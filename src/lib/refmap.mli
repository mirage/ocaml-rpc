type 'a inner
type 'a t = { m : 'a inner }

val mem : string -> 'a t -> bool
val add : string -> 'a -> 'a t -> 'a t
val remove : string -> 'a t -> 'a t
val update : string -> 'a -> 'a t -> 'a t
val find : string -> 'a t -> 'a
val keys : 'a t -> string list
val empty : 'a t
