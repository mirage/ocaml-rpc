(* OASIS_START *)
(* DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e) *)
(* OASIS_STOP *)
let () =
  Ocamlbuild_plugin.dispatch
    (fun hook ->
      Ocamlbuild_cppo.dispatcher hook ;
      dispatch_default hook;
    )
