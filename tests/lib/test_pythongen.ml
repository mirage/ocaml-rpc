
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
  Alcotest.(check int) "pylint should exit with 0"
    0 (Sys.command ("pylint --errors-only " ^ file));
  Alcotest.(check int) "pycodestyle should exit with 0"
    0 (Sys.command ("pycodestyle --ignore=E501 " ^ file))

let run () =
  let file = "bindings.py" in
  gen_python file;
  run_linters file

let tests =
  [ "Check generated test interface bindings with pylint & pycodestyle", `Slow, run ]
