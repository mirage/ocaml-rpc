(** Gather statistics about the (shape) of the distribution of values.

    Given some [input] values, a benchmarked function [f], shape/length functions [to_shape: input -> int Seq.t], [of_shape: int Seq.t -> input] function:
    - Determine the distribution of values [let dist = Distribution.of_seq (shape input)]
    - Print these values, and use them as input to [rpclib_bench.ml]: [Distribution.(Summary.pp (dist |> to_summary))]
    - Generate synthetic benchmark size data: [let shape = Distribution.(Summary.v ... |> of_summary |> generate)]
    - Generate synthetic benchmark inputs: [let synthetic = of_shape shape]
    - The running time on the original data: [f input] should be similar to [f synthethic]

    We don't want to use the original data as input to the benchmark, as this may be large, and vary quite a bit between different systems.

    When we operate on strings, as a first approximation lets assume that operations on the string is at most linear
    in its length, and there might be some overhead if we process a lot of strings,
    i.e. a linear model with a constant overhead and constant linear factor [time(f(input)) = a + b * shape(input)].

    In that case to get equivalent timings on synthetic data it is sufficient to know the [count] and [sum] of the [shape]s
    (string lengths).
    [sum(time(f(input))) = sum(a + b * shape_i) = a * count + b * sum(shape_i)]

    The distribution of value lengths is not normal (there are some very large outliers on both ends), and in this case the outliers are always present,
    and not a measurement error (so dropping them would be incorrect).

    However running time might also be influenced by the magnitude of the string length (e.g. whether it exceeds certain thresholds or not):
    * IO_BUFFER_SIZE determines whether we can send a string in a single syscall, or multiple
    * len <= Max_young_wosize determines whether it is allocated on the minor or major heap
    * 0-sized strings may be treated specially
    * whether it fits in the L1 or L2 cache

    Performance may also be influenced by whether the string lengths are unique or not
    (see StringMap optimization), so when generating inputs we should avoid generating just the mean.

    All these thresholds are usually powers of 2, and an exponential histogram is a good representation for that.
    The very large outliers in string lengths are also best represented with an exponential histogram than a regular one.

    Therefore we currently capture a histogram with bucket sizes that are powers of 2, and we track both the sum and count of values in a bucket. *)

(** Describes the distribution of values in a sequence *)
type t

(** [of_const c] is the distribution that is always the value [c]. *)
val of_const : int -> t

(** [of_seq seq] is the distribution of values in the sequence [seq]. *)
val of_seq : int Seq.t -> t

(** [generate t] generates values that match the observed statistics on [t].

    [of_seq (generate t) = t] *)
val generate : t -> int Seq.t

(** [merge a b] is the distribution of the union of values [a] and [b]. *)
val merge : t -> t -> t

(** a compact represtation of the distribution *)
type view = (int * int) array

(** [view t] is a compact representation of the distribution [t]. *)
val to_view : t -> view

(** [of_view compact] rebuilds the distribution from the [compact] representation. *)
val of_view : view -> t

(** [dump_view view] prints a representation of [view] in OCaml syntax *)
val dump_view : view Fmt.t

module StringChars : sig
  (** statistics about string length and character distribution *)
  type t

  (** a compact represtation of the string and char distribution *)
  type nonrec view = view * (char * view) list

  (** [of_view view] constructs the distribution from the compact {!type:view}. *)
  val of_view : view -> t

  (** [to_view t] converts [t] to a compact {!type:view}. *)
  val to_view : t -> view

  (** [dump_view view] prints a representation of [view] in OCaml syntax *)
  val dump_view : view Fmt.t

  (** [of_seq ~track strings] computes statistics for [strings],
      tracking character distribution for characters determined by [track]. *)
  val of_seq : track:(char -> bool) -> string Seq.t -> t

  (** [generate t] generates strings that match the distribution [t] *)
  val generate : t -> string Seq.t

  (** [merge a b] merges 2 string distributions [a] and [b] *)
  val merge : t -> t -> t
end

(** [dump ppf t] prints a debug representation of [t]. *)
val dump : t Fmt.t
