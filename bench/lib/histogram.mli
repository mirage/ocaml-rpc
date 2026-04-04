(** Histogram that consists of buckets, each tracking the count and sum of values within its range.
   Bucket widths are powers of 2, see {!module:ExponentialHistogramBucket}. *)
type t

(** [make ()] allocates enough space to store a histogram for all values
   [[0, Sys.max_string_length]].

   This is used for statistics on values that get serialized to strings, hence
   instead of [Int.max_int] we only support values up to [Sys.max_string_length]
 *)
val make : unit -> t

(** [update t value] updates the histogram with [value].

    [value] is typically the length of a string, the count of elements in an array/list/dict.
 *)
val update : t -> int -> unit

(** [merge a b] averages the non-empty buckets of the histograms [a] and [b]. *)
val merge : t -> t -> t

module Bucket : sig
  (** sum and count of values within the bucket's range.
      Internaly this type is mutable, but the interface only exposes immutable functions. *)
  type t

  (** [v ~sum ~count] construct a bucket *)
  val v : sum:int -> count:int -> t

  (** [sum t] is the sum of values within this bucket's range.

      This cannot overflow, because we must be able to serialize the entire value to a string,
      which must fit within {!val:Sys.max_string_length}
    *)
  val sum : t -> int

  (** [count t] is the number of values in this bucket's range. *)
  val count : t -> int

  (** [groups t] is the number of merges done with non-zero counts *)
  val groups : t -> int

  (** [generate t] will generate {!val:count} values that sum up to {!val:sum}. *)
  val generate : t -> int Seq.t

  (** [dump ppf t] print a debug representation of [t]  *)
  val dump : t Fmt.t
end

(** [to_seq t] returns a sequence of non-zero buckets. *)
val to_seq : t -> Bucket.t Seq.t

(** [of_seq seq] initializes an exponential histogram from the buckets in [seq]. *)
val of_seq : Bucket.t Seq.t -> t

(** [dump ppf t] print a debug representation of [t]  *)
val dump : t Fmt.t
