type baz = {
  a : string list option;
  b : float;
} [@@deriving rpcty]
type foo = {
  x : int [@default 7];
  y : string;
  mybaz : baz;
} [@@deriving rpcty]

