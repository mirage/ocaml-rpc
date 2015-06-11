(* Example IDL *)

module Old = struct 
  type return_record = {
    result : string;
    metadata : (int * int) list;
    extras : string option;
  }
end

type return_record_extended = {
  result : string;
  metadata : (int * int) list;
  extras : string option;
  new_field : string list;
}

let default_return_record_extended = {
  result = "hello";
  metadata = [(1,2); (3,4)];
  extras = None;
  new_field = ["1"; "3"; "5"];
}

let return_record_extended_of_rpc rpc = 
  Rpc.struct_extend rpc (rpc_of_return_record_extended default_return_record_extended) |> return_record_extended_of_rpc
  
type variant =
  | Foo of string list
  | Bar
  | Baz of float

external rpc1 : arg1:string -> int -> Old.return_record = ""
external rpc2 : ?opt:string -> variant -> unit = ""
external rpc3 : int64 -> int64 = ""

module SubModule = struct
  external rpc4 : int64 -> int64 = ""
end

