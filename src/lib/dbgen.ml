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
    { m: Mutex.t
    ; mutable db: DB.t
    ; mutable gen: int64
    ; mutable st: DB.t Stat.wrapped_field Stat.StatTree.t }

  let db =
    {m= Mutex.create (); db= DB.empty_db; gen= 0L; st= Stat.StatTree.empty}

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

  let fld_update : ('a, DB.t) Rpc.Types.field -> ('a -> 'a) -> unit =
   fun fld f ->
    let gen' = Int64.add db.gen 1L in
    let otherfld = Stat.map_fld fld in
    db.gen <- gen' ;
    db.db <- fld.fset (f (fld.fget db.db)) db.db ;
    db.st <- otherfld.fset gen' db.st

  let add : 'a Rpc.Types.ref -> 'a -> unit =
   fun ref x ->
    (* Construct a 'field' type to access the object via the 'field_name' of 'ref' *)
    operate ref (fun db_field refmap_field ->
        fld_update (Rpc.Types.compose db_field refmap_field) (fun _ -> Some x)
    )

  let remove : 'a Rpc.Types.ref -> unit =
   fun ref ->
    operate ref (fun db_field refmap_field ->
        fld_update (Rpc.Types.compose db_field refmap_field) (fun _ -> None) )

  let update :
      'a Rpc.Types.ref -> ('b, 'a) Rpc.Types.field -> ('b -> 'b) -> unit =
   fun ref field f ->
    operate ref (fun tablefield objfield ->
        let fld =
          Rpc.Types.opt_compose (Rpc.Types.compose tablefield objfield) field
        in
        fld_update fld f )

  let get : 'a Rpc.Types.ref -> ('b, 'a) Rpc.Types.field -> 'b =
   fun ref field ->
    operate ref (fun tablefield objfield ->
        let fld =
          Rpc.Types.(opt_compose (compose tablefield objfield) field)
        in
        fld.fget db.db )

  let get_obj : 'a Rpc.Types.ref -> 'a option =
   fun ref ->
    operate ref (fun tablefield objfield ->
        let fld = Rpc.Types.compose tablefield objfield in
        fld.fget db.db )

  let is_valid_ref : 'a Rpc.Types.ref -> bool =
   fun ref ->
    match ref with
    | Rpc.Types.NullRef _ -> false
    | _ -> ( match get_obj ref with None -> false | Some _ -> true )

  let setrel : type a b.
      a Rpc.Types.ref -> (a, b) otm_pair -> b Rpc.Types.ref -> unit =
   fun refo (fieldo, fieldm) refm ->
    let before = get refo fieldo in
    update refo fieldo (fun _ -> refm) ;
    ( try
        update before fieldm
          (List.filter (fun r ->
               Rpc.Types.name_of_ref r <> Rpc.Types.name_of_ref refo ))
      with _ -> () ) ;
    try update refm fieldm (fun x -> refo :: x) with _ -> ()

  let setmtm : type a b.
      a Rpc.Types.ref -> (a, b) mtm_pair -> b Rpc.Types.ref list -> unit =
   fun refo (fieldo, fieldm) refms ->
    let before = get refo fieldo in
    update refo fieldo (fun _ -> refms) ;
    ( try
        List.iter
          (fun r ->
            let r_name = Rpc.Types.name_of_ref r in
            if List.exists (fun r' -> Rpc.Types.name_of_ref r' = r_name) refms
            then (* It was there before, it's there now. Do nothing *) ()
            else
              (* It was there before, not there after *)
              update r fieldm
                (List.filter (fun backref ->
                     Rpc.Types.name_of_ref backref
                     <> Rpc.Types.name_of_ref refo )) )
          before
      with _ -> () ) ;
    try
      List.iter
        (fun r ->
          let r_name = Rpc.Types.name_of_ref r in
          if List.exists (fun r' -> Rpc.Types.name_of_ref r' = r_name) before
          then (* It was there before, it's there now. Do nothing *) ()
          else
            (* It wasn't there before, but is there after *)
            update r fieldm (fun x ->
                refo
                :: List.filter
                     (fun backref ->
                       Rpc.Types.name_of_ref backref
                       <> Rpc.Types.name_of_ref refo )
                     x ) )
        refms
    with _ -> ()

  let set : type a b. a Rpc.Types.ref -> (b, a) Rpc.Types.field -> b -> unit =
   fun ref field x ->
    let is_rel =
      List.fold_left
        (fun acc r ->
          match r with
          | OtmRel (f1, f2) -> (
            match Rpc.Types.(eq_field field f1) with
            | Some Rpc.Types.Eq ->
                setrel ref (f1, f2) x ;
                true
            | _ -> acc )
          | MtmRel (f1, f2) -> (
            match Rpc.Types.(eq_field field f1) with
            | Some Rpc.Types.Eq ->
                setmtm ref (f1, f2) x ;
                true
            | _ -> acc ) )
        false DB.rels
    in
    if not is_rel then update ref field (fun _ -> x)

  let add_to :
         'a Rpc.Types.ref
      -> (('c * 'd) list, 'a) Rpc.Types.field
      -> 'c
      -> 'd
      -> unit =
   fun ref field k v ->
    update ref field (fun x -> (k, v) :: List.remove_assoc k x)

  let add : type a. a Rpc.Types.ref -> a -> unit =
   fun ref v ->
    add ref v ;
    let cls = Rpc.Types.cls_of_ref ref in
    List.iter
      (fun r ->
        match r with
        | OtmRel (f1, f2) -> (
            ( match Rpc.Types.eqcls f1.fcls cls with
            | Some Rpc.Types.Eq ->
                (* OK, we've got a field where we're the 'one' in a one-to-many relation.
                   First check if the other end exists. *)
                if is_valid_ref (get ref f1) then
                  update (get ref f1) f2 (fun xs -> ref :: xs)
                  (* Yes, add us to the list *)
                else set ref f1 (NullRef f2.fcls) (* No, nuke the reference *)
            | None -> () ) ;
            match Rpc.Types.eqcls f2.fcls cls with
            | Some Rpc.Types.Eq ->
                (* We're the many end of a one-to-many relation. Nobody should be pointing
                   at us so let's nuke our list *)
                set ref f2 []
            | None -> () )
        | MtmRel (_f1, _f2) -> ()
        (* Should really do something here *) )
      DB.rels

  let update_obj : type a. a Rpc.Types.ref -> a -> unit =
    fun ref v ->
      match get_obj ref with
      | None -> add ref v
      | Some cur -> begin
        let typ = DB.typ_of_cls (Rpc.Types.cls_of_ref ref) in
        match typ with
        | Rpc.Types.Struct s ->
          List.iter (function | Rpc.Types.BoxedField f ->
            if Rpc.Types.(f.fget cur = f.fget v)
            then ()
            else set ref f (f.fget v)
          ) s.Rpc.Types.fields
        | _ -> ()
      end
  
  let dump_since gen = (db.gen, Stat.dump_since gen db.db db.st)
end
