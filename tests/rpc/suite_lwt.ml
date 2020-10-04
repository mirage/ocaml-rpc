;;
Lwt_main.run @@ Alcotest_lwt.run "rpc lwt test" [ "Client_lwt_new", Client_lwt_new.tests ]
