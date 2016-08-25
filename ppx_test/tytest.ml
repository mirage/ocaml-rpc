type foo = {
  x : int [@default 7];
  y : string;
} [@@deriving rpcty]

