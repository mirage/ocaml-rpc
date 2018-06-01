
module Interface(R : Idl.RPC) = struct
  open R

  let int_p_named_1 = Idl.Param.mk ~name:"int1" ~description:["first int param"] Rpc.Types.int
  let int_p_named_2 = Idl.Param.mk ~name:"int2" ~description:["second int param"] Rpc.Types.int
  let int_p_result = Idl.Param.mk ~name:"int" ~description:["int result"] Rpc.Types.int

  let add = R.declare "add"
      ["Add two numbers"]
      (int_p_named_1 @-> int_p_named_2 @-> returning int_p_result Idl.DefaultError.err)

  let implementation = implement
      { Idl.Interface.name = "Calc"; namespace = Some "Calc"; description = ["interface"]; version = (1,0,0) }
end

module IfCode = Interface(Codegen.Gen ())

let interfaces =
  Codegen.Interfaces.create
    ~name:"test_interface"
    ~title:"Test Interface"
    ~description:[{|Test Interface for integer arithmetic|}]
    ~interfaces:[IfCode.implementation ()]

let gen_python file =
  let oc = open_out file in
  output_string oc (Pythongen.of_interfaces interfaces |> Pythongen.string_of_ts);
  close_out oc

let run_linters file =
  let run msg cmd = Alcotest.(check int) msg 0 (Sys.command cmd) in
  run "pylint should exit with 0" ("pylint --errors-only " ^ file);
  run "pycodestyle should exit with 0" ("pycodestyle --ignore=E501 " ^ file)

let file = "bindings.py"

let lint_bindings () =
  gen_python file;
  run_linters file

let test_commandline () =
  gen_python file;
  let run ?input cmd =
    let inp, out = Unix.open_process cmd in
    begin match input with Some input -> output_string out input | None -> () end;
    close_out out;
    let l = input_line inp in
    close_in inp;
    l |> String.trim
  in
  let n = run "python calc.py 4 5" in
  Alcotest.(check string) "Calc.add with parameters passed on the command line" "9" n;
  let n = run ~input:{|{"int1":3,"int2":2}|} "python calc.py --json" in
  Alcotest.(check string) "Calc.add with parameters passed to stdin as JSON" "5" n

let tests =
  [ "Check generated test interface bindings with pylint & pycodestyle", `Slow, lint_bindings
  ; "Check generated commandline bindings", `Slow, test_commandline
  ]
