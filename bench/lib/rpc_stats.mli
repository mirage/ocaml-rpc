(** a flattened sequence of Rpc.t values *)
type t

(** empty sequence *)
val empty : t

(** [seq_of_rpc t x] is [x :: t]. *)
val seq_of_rpc : t -> Rpc.t -> t

(** statistics about a sequence of Rpc.t values *)
type stats

(** [merge a b] merges 2 statistics [a] and [b] *)
val merge : stats -> stats -> stats

(** [stats_of_seq seq] calculates statistics for the sequence [seq]. *)
val stats_of_seq : t -> stats

(** [pp_stats ppf stats] prints statistics in human-readable form, in OCaml syntax.
    This can be pasted into benchmark source code to create synthetic input resembling
    the real one. *)
val pp_stats : Format.formatter -> stats -> unit

(** [of_stats ~int ~int32 ~bool ~float ~string ~datetime ~enum_length ~dict_keys ~dict_length ~base64 ~null]
    creates Rpc.t distribution statistics from the distribution statistics of each Rpc.t variant.

    @param enum_length
      is a statistic on the distribution of enum lengths (their contents are assumed to be strings)
    @param dict_keys
      are statistics about the dictionary keys (both for nested dictionaries and the main dictionary)
    @param dict_length are statistics about the nested dictionary lengths (key counts) *)
val of_stats
  :  int:int
  -> int32:int
  -> bool:int
  -> float:int
  -> string:(int * int) array * (char * (int * int) array) list
  -> datetime:int
  -> enum_length:(int * int) array
  -> dict_keys:(int * int) array
  -> dict_length:(int * int) array
  -> base64:(int * int) array
  -> null:int
  -> stats

(** [rpc_of_stats stats] generates an Rpc.t value that matches the statistics [stats].

    [stats |> rpc_of_stats |> seq_of_rpc empty |> stats_of_seq = stats] *)
val rpc_of_stats : stats -> Rpc.t
