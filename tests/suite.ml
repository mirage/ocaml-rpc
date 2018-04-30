
let () =
  Alcotest.run
    "suite"
    [ "Client_server_test", Client_server_test.tests
    ]
