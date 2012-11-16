(* Example IDL *)

type return_record = {
  result : string;
  metadata : (int * int) list;
}

type variant =
  | Foo of string list
  | Bar
  | Baz of float

external rpc1 : arg1:string -> int -> return_record = ""
external rpc2 : ?opt:string -> variant -> unit = ""
external rpc3 : int64 -> int64 = ""

module SubModule = struct
  external rpc4 : int64 -> int64 = ""
end

