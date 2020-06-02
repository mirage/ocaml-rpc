let () =
  Alcotest.run "rpc tests" [
      ("Client_new", Client_new.tests)
    ; ("Test_pythongen", Test_pythongen.tests) ]
