(* OASIS_START *)
(* OASIS_STOP *)
let () =
  Ocamlbuild_plugin.dispatch
    (fun hook ->
      Ocamlbuild_cppo.dispatcher hook ;
      dispatch_default hook;
    )
