
module M = Dbgen.Make(struct
  type t = API.database
  let typ_of = API.typ_of_database
  let t = API.database
  let rels = API.rels
  let find_objs = API.find_objs
  let empty_db = API.empty_db 
end)

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

let mkobj cls =
  let typ = API.typ_of_cls cls in
  let rf = API.mkref cls @@ rand cls in
  let record = Rpc_genfake.gen_nice typ "generated" in
  M.add rf record;
  rf

let _ =
  let vm1 = mkobj API.VM_T in
  let vm2 = mkobj API.VM_T in
  let vbd1 = mkobj API.VBD_T in

  let vbds_str vm = 
    M.get vm API.VM.vBDs |>
    List.map Rpc.Types.name_of_ref |>
    String.concat ";"
  in

  let (gen,_recent) = M.dump_since 0L in

  Printf.printf "vm1 VBDs: [%s]  vm2 VBDs: [%s]\n" (vbds_str vm1) (vbds_str vm2);
  M.set vbd1 API.VBD.vM vm1;
  Printf.printf "vm1 VBDs: [%s]  vm2 VBDs: [%s]\n" (vbds_str vm1) (vbds_str vm2);
  M.set vbd1 API.VBD.vM vm2;
  Printf.printf "vm1 VBDs: [%s]  vm2 VBDs: [%s]\n" (vbds_str vm1) (vbds_str vm2);

  let (_gen,recent) = M.dump_since gen in

  Printf.printf "recent updates: %s\n" (recent |> Jsonrpc.to_string);
  ()

