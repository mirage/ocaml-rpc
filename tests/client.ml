module type S = sig
	val foo : int -> int
	val bar : float -> ?foo:unit -> ?bar:(string * string) -> unit -> unit
end with_gen rpc
