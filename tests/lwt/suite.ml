let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "rpclib-lwt suite"
       [ "Client_server_test", Client_server_test.tests ]
