let detect_pylint () =
  if (abs @@ Sys.command "which pylint") + (abs @@ Sys.command "which pycodestyle") > 0 then
    (print_endline "Warning: pylint or pycodestyle not found... Skipping python linting tests!";
    false)
  else true

let () =
  Alcotest.run
    "rpc tests"
    @@ [ "Client_new", Client_new.tests ]
    @ if detect_pylint () then [ "Test_pythongen", Test_pythongen.tests ] else []
