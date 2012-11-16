type t = {
  foo : int option;
  bar : int list option;
  gni : int list;
  gna : int * (int option)
} with rpc

let _ =
  let t1 = { foo = None; bar = None; gni = []; gna = 1, None } in
  let t2 = { foo = None; bar = Some []; gni = [1]; gna = 1, None } in
  let r1 = rpc_of_t t1 in
  let r2 = rpc_of_t t2 in
  Printf.printf "r1 = %s\nr2 = %s\n" (Rpc.to_string r1) (Rpc.to_string r2);

  let t1' = t_of_rpc r1 in
  let t2' = t_of_rpc r2 in

  Printf.printf "t1 = t1' : %b\n%!" (t1=t1');
  assert (t1 = t1');

  Printf.printf "t2 = t2' : %b\n%!" (t2 = t2');
  assert (t2 = t2')
