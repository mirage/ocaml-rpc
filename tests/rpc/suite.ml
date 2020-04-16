let () =
  Alcotest.run "rpc tests" [
      ("Client_new", Client_new.tests)
    ; ("Test_pythongen", Test_pythongen.tests) ];
  Lwt_main.run @@
  Alcotest_lwt.run "rpc lwt test"
    [ ("Client_lwt_new", Client_lwt_new.tests) ]
