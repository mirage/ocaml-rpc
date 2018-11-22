type v1 = One | Two of int | Three of string * string
[@@deriving rpcty]

type subrec = {
  another : int;
  record : (string * string) list;
} [@@deriving rpcty]

type ty1 = {
  f1 : string;
  f2 : string option;
  f3 : v1 option;
  f4 : subrec;
} [@@deriving rpcty]

type ty2 = {
  t2_f1 : string;
  t2_f2 : bool;
} [@@deriving rpcty]

module DB = struct
  type t = {
    ty1 : ty1 Rpc.Refmap.t;
    ty2 : ty2 Rpc.Refmap.t
  } [@@deriving rpcty]

  let rels = []

  let find_objs : type a. a Rpc.Types.cls -> (a Refmap.t, t) Rpc.Types.field = fun cls ->
    match cls with
    | TY1 -> t_ty1
    | TY2 -> t_ty2
    | _ -> failwith "Invalid cls"

  let typ_of_cls : type a. a Rpc.Types.cls -> a Rpc.Types.typ =
    fun cls ->
      match cls with
      | TY1 -> typ_of_ty1
      | TY2 -> typ_of_ty2
      | _ -> failwith "Invalid cls"

  let empty_db = {
    ty1 = Refmap.empty;
    ty2 = Refmap.empty;
  }
end

module M = Dbgen.Make(DB)

let mkref cls =
  let typ = DB.typ_of_cls cls in
  Rpc.Types.make_ref cls typ

let ns : (string * int) list ref = ref []
let rand cls =
  let c = match Rpc.Types.prcls cls with Some x -> x | None -> "none" in
  let num =
    try
      let before = List.assoc c !ns in
      ns := (c,before+1) :: (List.remove_assoc c !ns);
      before + 1
    with _ ->
      ns := (c,0):: !ns;
      0
  in
  Printf.sprintf "%s_%d" c num

let mkobj cls db =
  let typ = DB.typ_of_cls cls in
  let tests = Rpc_genfake.genall 3 "test" typ in
  List.fold_left (fun (db,refs) v ->
    let rf = mkref cls @@ rand cls in
    let db = M.add rf v db in
    (db,rf::refs))
    (db,[]) tests

type fld = Fld : 'a Rpc.Types.cls * ('b, 'a) Rpc.Types.field -> fld


let rndelt l =
  List.nth l (Random.int (List.length l))

let set_random db =
  let fld =
    match Random.int 6 with
    | 0 -> Fld (TY1, ty1_f1)
    | 1 -> Fld (TY1, ty1_f2)
    | 2 -> Fld (TY1, ty1_f3)
    | 3 -> Fld (TY1, ty1_f4)
    | 4 -> Fld (TY2, ty2_t2_f1)
    | 5 -> Fld (TY2, ty2_t2_f2)
  in
  let rndref : type a. a Rpc.Types.cls -> a Rpc.Types.ref = function
  | TY1 -> Rpc.Types.make_ref TY1 typ_of_ty1 (rndelt (Refmap.keys db.M.db.ty1))
  | TY2 -> Rpc.Types.make_ref TY2 typ_of_ty2 (rndelt (Refmap.keys db.M.db.ty2))
  in
  match fld with
  | Fld (TY1, f) ->
    let ref = rndref TY1 in
    let v = Rpc_genfake.genall 5 "foo" f.Rpc.Types.field |> rndelt in 
(*    Printf.printf "setting ty1 ref %s field %s to %s" (Rpc.Types.name_of_ref ref) (String.concat "." (f.Rpc.Types.fname)) (Rpc.to_string (Rpcmarshal.marshal f.Rpc.Types.field v)); *)
    M.set ref f v db
  | Fld (TY2, f) ->
    let ref = rndref TY2 in
    let v = Rpc_genfake.genall 5 "foo" f.Rpc.Types.field |> rndelt in
(*    Printf.printf "setting ty1 ref %s field %s to %s" (Rpc.Types.name_of_ref ref) (String.concat "." (f.Rpc.Types.fname)) (Rpc.to_string (Rpcmarshal.marshal f.Rpc.Types.field v)); *)
    M.set ref f v db

let test_delta_specific () =
  let db = M.empty in
  let (db,r1s) = mkobj TY1 db in
  let (db,r2s) = mkobj TY2 db in
  let r1 = List.hd r1s in
  let (gen,_) = M.dump_since 0L db in
  let db = M.set r1 (Rpc.Types.compose ty1_f4 subrec_another) 4 db in
  let gen,rpc = M.dump_since gen db in
  let db = M.set r1 (Rpc.Types.compose ty1_f4 subrec_another) 4 db in
  let _,rpc = M.dump_since gen db in
  match rpc with
  | Some r ->
    Printf.printf "rpc=%s\n" (Jsonrpc.to_string r);
    Printf.printf "StatTree: \n";
    Stat.StatTree.print db.M.st;
    let partials = Stat.StatTree.to_partial gen db.M.st in
    (match partials with
      | Some p -> Stat.StatTree.print_partials p
      | None -> Printf.printf "No partial!");
    failwith "foo"
  | None ->
    failwith "bar"


let test_deltas () =
  let db = M.empty in
  let (db,r1s) = mkobj TY1 db in
  let (db,r2s) = mkobj TY2 db in
  let r1 = List.hd r1s in
  let (gen,_) = M.dump_since 0L db in
  let db = M.set r1 ty1_f3 (Some (Two 4)) db in
  let _,rpc = M.dump_since gen db in
  Printf.printf "Put in %d and %d objects" (List.length r1s) (List.length r2s);
  let rec do_delta db replicated_db n =
    match n with
    | 0 -> ()
    | n -> begin
      let db = set_random db in
      (* Printf.printf "db.gen=%Ld\n" db.M.gen; *)
      let next =
        if Random.int 10 = 0
        then begin
          let (new_gen, deltas) = M.dump_since replicated_db.M.gen db in
          let new_replicated_db =
            match deltas with
            | Some rpc -> begin
              (*Printf.printf "since=%Ld new_gen=%Ld deltas: %s \n%!" (replicated_db.M.gen) new_gen (Jsonrpc.to_string rpc);*)
              match Rpcmarshal.unmarshal_partial DB.typ_of replicated_db.db rpc with
              | Ok db ->
                M.{gen=new_gen; db=db; st=Stat.StatTree.empty}
              | Error (`Msg m) ->
                failwith (Printf.sprintf "Failure: %s" m)
              end
            | None -> M.{gen=new_gen; db=replicated_db.db; st=Stat.StatTree.empty}
          in
          let rpc1 = Rpcmarshal.marshal DB.typ_of db.M.db in
          let rpc2 = Rpcmarshal.marshal DB.typ_of new_replicated_db.M.db in
          if not (rpc1 = rpc2) then begin
            Printf.printf "ERROR: difference detected!\n%!";
            Printf.printf "Original:\n%s\n" (Yojson.Basic.pretty_to_string (Yojson.Basic.from_string (Jsonrpc.to_string rpc1)));
            Printf.printf "Replicated:\n%s\n" (Yojson.Basic.pretty_to_string (Yojson.Basic.from_string (Jsonrpc.to_string rpc2)));
            Printf.printf "StatTree: \n";
            Stat.StatTree.print db.M.st;
            let partials = Stat.StatTree.to_partial replicated_db.M.gen db.M.st in
            (match partials with
            | Some p -> Stat.StatTree.print_partials p
            | None -> Printf.printf "No partial!");
            failwith "error"
          end;
          new_replicated_db
        end else
          replicated_db
      in
      
      do_delta db next (n-1)
    end
  in do_delta db M.{gen=(-1L); db=DB.empty_db; st=Stat.StatTree.empty} 1000000




let tests = [
  "Deltas", `Quick, test_delta_specific
]
