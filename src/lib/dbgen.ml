type ('a, 'b) otm_pair =
  ('b Rpc.Types.ref, 'a) Rpc.Types.field
  * ('a Rpc.Types.ref list, 'b) Rpc.Types.field

type ('a, 'b) mtm_pair =
  ('b Rpc.Types.ref list, 'a) Rpc.Types.field
  * ('a Rpc.Types.ref list, 'b) Rpc.Types.field

type relation =
  | OtmRel : ('a, 'b) otm_pair -> relation
  | MtmRel : ('a, 'b) mtm_pair -> relation

module type DB = sig
  type t

  val t : t Rpc.Types.def

  val typ_of : t Rpc.Types.typ

  val typ_of_cls : 'a Rpc.Types.cls -> 'a Rpc.Types.typ

  val rels : relation list

  val find_objs : 'a Rpc.Types.cls -> ('a Rpc.Refmap.t, t) Rpc.Types.field

  val empty_db : t
end

module Make (DB : DB) = struct
  type db =
    { db: DB.t
    ; gen: int64
    ; st: DB.t Stat.wrapped_field Stat.StatTree.t }

  let empty =
    { db= DB.empty_db
    ; gen= 0L
    ; st= Stat.StatTree.empty}

  let operate :
         'a Rpc.Types.ref
      -> (   ('a Refmap.t, DB.t) Rpc.Types.field
          -> ('a option, 'a Refmap.t) Rpc.Types.field
          -> 'b)
      -> 'b =
   fun ref f ->
    let cls = Rpc.Types.cls_of_ref ref in
    let f' = DB.find_objs cls in
    match ref with
    | Rpc.Types.Ref f'' -> f f' f''
    | Rpc.Types.NullRef _ -> failwith "Null reference"

  let fld_update : ('a, DB.t) Rpc.Types.field -> ('a -> 'a) -> db -> db =
   fun fld f db ->
    let gen = Int64.add db.gen 1L in
    let otherfld = Stat.map_fld fld in
    { gen
    ; db = fld.fset (f (fld.fget db.db)) db.db
    ; st = otherfld.fset gen db.st }

  let add : 'a Rpc.Types.ref -> 'a -> db -> db =
   fun ref x ->
    (* Construct a 'field' type to access the object via the 'field_name' of 'ref' *)
    operate ref (fun db_field refmap_field ->
        fld_update (Rpc.Types.compose db_field refmap_field) (fun _ -> Some x)
    )

  let remove : 'a Rpc.Types.ref -> db -> db =
   fun ref ->
    operate ref (fun db_field refmap_field ->
        fld_update (Rpc.Types.compose db_field refmap_field) (fun _ -> None))

  let update_exn :
      'a Rpc.Types.ref -> ('b, 'a) Rpc.Types.field -> ('b -> 'b) -> db -> db =
    fun ref field f ->
    operate ref (fun tablefield objfield ->
        let fld =
          Rpc.Types.opt_compose (Rpc.Types.compose tablefield objfield) field
        in
        fld_update fld f )

  let update :
      'a Rpc.Types.ref -> ('b, 'a) Rpc.Types.field -> ('b -> 'b) -> db -> db =
    fun ref field f db ->
      try update_exn ref field f db with _ -> db
 
  let get : 'a Rpc.Types.ref -> ('b, 'a) Rpc.Types.field -> db -> 'b =
   fun ref field db ->
    operate ref (fun tablefield objfield ->
        let fld =
          Rpc.Types.(opt_compose (compose tablefield objfield) field)
        in
        fld.fget db.db )

  let get_obj : 'a Rpc.Types.ref -> db -> 'a option =
   fun ref db ->
    operate ref (fun tablefield objfield ->
        let fld = Rpc.Types.compose tablefield objfield in
        fld.fget db.db )

  let is_valid_ref : 'a Rpc.Types.ref -> db -> bool =
   fun ref db ->
    match ref with
    | Rpc.Types.NullRef _ -> false
    | _ -> ( match get_obj ref db with None -> false | Some _ -> true )

  let setrel : type a b.
      a Rpc.Types.ref -> (a, b) otm_pair -> b Rpc.Types.ref -> db -> db =
   fun refo (fieldo, fieldm) refm db ->
    let before = get refo fieldo db in
    db
    |> update refo fieldo (fun _ -> refm) 
    |> update before fieldm
         (List.filter (fun r ->
              Rpc.Types.name_of_ref r <> Rpc.Types.name_of_ref refo))
    |> update refm fieldm (fun x -> refo :: x)

  let setmtm : type a b.
      a Rpc.Types.ref -> (a, b) mtm_pair -> b Rpc.Types.ref list -> db -> db =
   fun refo (fieldo, fieldm) refms db ->
    let before = get refo fieldo db in
    db
    |> update refo fieldo (fun _ -> refms)
    |> List.fold_right
          (fun r db ->
            let r_name = Rpc.Types.name_of_ref r in
            if List.exists (fun r' -> Rpc.Types.name_of_ref r' = r_name) refms
            then (* It was there before, it's there now. Do nothing *) db
            else
              (* It was there before, not there after *)
              update r fieldm
                (List.filter (fun backref ->
                     Rpc.Types.name_of_ref backref
                     <> Rpc.Types.name_of_ref refo )) db)
          before
    |> List.fold_right (fun r db ->
          let r_name = Rpc.Types.name_of_ref r in
          if List.exists (fun r' -> Rpc.Types.name_of_ref r' = r_name) before
          then (* It was there before, it's there now. Do nothing *) db
          else
            (* It wasn't there before, but is there after *)
            update r fieldm (fun x ->
                refo
                :: List.filter
                     (fun backref ->
                       Rpc.Types.name_of_ref backref
                       <> Rpc.Types.name_of_ref refo )
                     x ) db )
        refms

  let set : type a b. a Rpc.Types.ref -> (b, a) Rpc.Types.field -> b -> db -> db =
   fun ref field x db ->
    let rel_setter =
      List.fold_left
        (fun acc r ->
          match r with
          | OtmRel (f1, f2) -> (
            match Rpc.Types.(eq_field field f1) with
            | Some Rpc.Types.Eq ->
                Some (setrel ref (f1, f2) x) ;
            | _ -> acc )
          | MtmRel (f1, f2) -> (
            match Rpc.Types.(eq_field field f1) with
            | Some Rpc.Types.Eq ->
                Some (setmtm ref (f1, f2) x)
            | _ -> acc ) )
        None DB.rels
    in
    match rel_setter with Some f -> f db | None -> update ref field (fun _ -> x) db

  let add_to :
         'a Rpc.Types.ref
      -> (('c * 'd) list, 'a) Rpc.Types.field
      -> 'c
      -> 'd 
      -> db -> db =
   fun ref field k v db ->
    update ref field (fun x -> (k, v) :: List.remove_assoc k x) db

  type 'a todo = ToDo : ('b, 'a) Rpc.Types.field * 'b -> 'a todo
 
  let add  : type s. s Rpc.Types.ref -> s -> db -> db =
   fun ref v db ->
    let cls = Rpc.Types.cls_of_ref ref in
    let fold_fn : (s * s todo list) -> relation -> (s * s todo list ) = fun (v, todos) b ->
      match b with
      | OtmRel (f1, f2) -> begin
        let ((newv : s), (todos : s todo list)) = match Rpc.Types.(eqcls cls f1.fcls) with
        | Some Rpc.Types.Eq -> let v = f1.fset (Rpc.Types.NullRef f2.fcls) v in (v, (ToDo (f1, f1.fget v))::todos)
        | None -> (v,todos)
        in
        let (newv : s) = match Rpc.Types.(eqcls cls f2.fcls) with
        | Some Rpc.Types.Eq -> f2.fset [] newv
        | None -> newv
        in
        (newv,todos)
        end 
      | MtmRel (f1, f2) ->
        let ((v : s), (todos : s todo list )) =
          match Rpc.Types.eqcls f1.fcls cls with
          | Some Rpc.Types.Eq -> (f1.fset [] v, (ToDo (f1, f1.fget v))::todos)
          | None -> (v,todos)
        in
        let (v : s) =
          match Rpc.Types.eqcls f2.fcls cls with
          | Some Rpc.Types.Eq -> f2.fset [] v
          | None -> v
        in
        (v,todos)
    in
    let (v, todos) = List.fold_left fold_fn (v, []) DB.rels in
    db
    |> add ref v
    |> List.fold_right (fun todo db ->
      match todo with
      | ToDo (f,v) -> set ref f v db) todos

  let update_obj : type a. a Rpc.Types.ref -> a -> db -> db =
    fun ref v db ->
      match get_obj ref db with
      | None -> add ref v db
      | Some cur -> begin
        let typ = DB.typ_of_cls (Rpc.Types.cls_of_ref ref) in
        match typ with
        | Rpc.Types.Struct s ->
          List.fold_right (fun field db -> 
            match field with
            | Rpc.Types.BoxedField f ->
              if Rpc.Types.(f.fget cur = f.fget v)
              then db
              else set ref f (f.fget v) db
          ) s.Rpc.Types.fields db
        | _ -> db
      end
  
  let dump_since gen db = (db.gen, Stat.dump_since gen db.db db.st)
end
