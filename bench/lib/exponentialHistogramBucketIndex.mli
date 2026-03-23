(** {{:https://opentelemetry.io/docs/specs/otel/metrics/data-model/#scale-zero-extract-the-exponent} Opentelemetry Exponential Buckets} with [scale=-2], i.e. [base = 16].

   This scale was chosen, because:
   - it is a power of 2, so we can compute it with simple integer math (and test results exactly)
   - performance often depends on length thresholds that are powers of 2 in OCaml:
      - a cache line size is usually [64] bytes
      - page size, usually [4096] on [x86-64] 
      - [Max_young_wosize=256] that determines whether it is allocated in the minor or major heap
      - [IO_BUFFER_SIZE=65536] that determines whether 1 syscall or several are made during [read] or [write]
      - the size of CPU L1/L2 cache(s)
   - [256] would've been too coarse (see cache line and pagesize above), so the next available size [16] was more suitable         

   Usually bucket index 0 would contain values between [(1, base]], and there'd be a separate [zero_count].
   For simplicity we offset the index by 1, so that we can store this continously. We also use [zero_threshold = 1], to avoid creating a bucket just for the value [1],
   i.e. this is compatible with [of_int v = if v > zero_threshold then 1 + MapToIndex v else 0].

   TODO: also the uniqueness of the length values may matter, e.g. for the StringMap length compare optimization, track variance?
   Or just spread out

   This is its own module for easier testing.
*)

(** Histogram bucket index

   Each bucket counts how many values we've seen in its {!val:range} *)
type t = private int

(** [of_int v] determines a bucket index [i] for the value [v].

   See also {!val:range}.

   @return [i] such that 
   [let lo, hi = range i in lo < v && v <= hi]

   @raises Invalid_argument if [v < 0 || v > Sys.max_string_length]
*)
val of_int : int -> t

(** [to_index t] is equivalent to [t :> int], and is an index into an array of buckets. *)
val to_index : t -> int

(** [range index] is the range of values [(lo, hi]] that map to [index].

   @returns [-1, 1] when [v=0]
   @returns [16**(i-1), 16**i] otherwise
*)
val range : t -> int * int

(** [all] are all the valid indexes needed to represent
  lengths between [0, Sys.max_string_length] *)
val all : t Seq.t
