module Count : sig
  type +'a t

  val v : int -> _ t
  val of_seq : 'a Seq.t -> 'a t
  val merge : 'a t -> 'a t -> 'a t

  (* ensures that we update the correct stat for each Rpc variant, by requiring types to match *)
  val pp : _ t Fmt.t
  val generate : 'a -> 'a t -> 'a Seq.t
end = struct
  type +'a t =
    { mutable count : int
    ; groups : int
    }

  let view t = t.count / t.groups
  let v count = { count; groups = 1 }
  let of_seq s = { count = Seq.length s; groups = 1 }
  let merge a b = { count = a.count + b.count; groups = a.groups + b.groups }
  let pp fmt = Fmt.using view Fmt.int fmt
  let generate e t = Seq.init (view t) (fun _ -> e)
end

type t =
  { int : int64 Seq.t
  ; int32 : int32 Seq.t
  ; bool : bool Seq.t
  ; float : float Seq.t
  ; string : string Seq.t
  ; datetime : string Seq.t
  ; enum_length : int Seq.t
  ; dict_keys : string Seq.t
  ; dict_length : int Seq.t
  ; base64 : string Seq.t
  ; null : unit Seq.t
  }

let empty =
  { int = Seq.empty
  ; int32 = Seq.empty
  ; bool = Seq.empty
  ; float = Seq.empty
  ; string = Seq.empty
  ; datetime = Seq.empty
  ; enum_length = Seq.empty
  ; dict_keys = Seq.empty
  ; dict_length = Seq.empty
  ; base64 = Seq.empty
  ; null = Seq.empty
  }


let rec seq_of_rpc stats =
  let open Rpc in
  function
  | Int i -> { stats with int = Seq.cons i stats.int }
  | Int32 i -> { stats with int32 = Seq.cons i stats.int32 }
  | Bool i -> { stats with bool = Seq.cons i stats.bool }
  | Float i -> { stats with float = Seq.cons i stats.float }
  | String i -> { stats with string = Seq.cons i stats.string }
  | DateTime i -> { stats with datetime = Seq.cons i stats.datetime }
  | Enum l ->
    let stats = List.fold_left seq_of_rpc stats l in
    { stats with enum_length = Seq.cons (List.length l) stats.enum_length }
  | Dict d ->
    List.fold_left
      (fun stats (k, v) ->
         let stats = seq_of_rpc stats v in
         { stats with dict_keys = Seq.cons k stats.dict_keys })
      { stats with dict_length = Seq.cons (List.length d) stats.dict_length }
      d
  | Base64 i -> { stats with base64 = Seq.cons i stats.base64 }
  | Null -> { stats with null = Seq.cons () stats.null }


type stats =
  { int : int64 Count.t
  ; int32 : int32 Count.t
  ; bool : bool Count.t
  ; float : float Count.t
  ; string : Distribution.StringChars.t
  ; datetime : string Count.t
  ; enum_length : Distribution.t
  ; dict_keys : Distribution.t
  ; dict_length : Distribution.t
  ; base64 : Distribution.t
  ; null : unit Count.t
  }

let merge a b =
  { int = Count.merge a.int b.int
  ; int32 = Count.merge a.int32 b.int32
  ; bool = Count.merge a.bool b.bool
  ; float = Count.merge a.float b.float
  ; string = Distribution.StringChars.merge a.string b.string
  ; datetime = Count.merge a.datetime b.datetime
  ; enum_length = Distribution.merge a.enum_length b.enum_length
  ; dict_keys = Distribution.merge a.dict_keys b.dict_keys
  ; dict_length = Distribution.merge a.dict_length b.dict_length
  ; base64 = Distribution.merge a.base64 b.base64
  ; null = Count.merge a.null b.null
  }


let track = function
  | '<' | '>' | '"' -> true
  | _ -> false


let stats_of_seq (s : t) : stats =
  { int = Count.of_seq s.int
  ; int32 = Count.of_seq s.int32
  ; bool = Count.of_seq s.bool
  ; float = Count.of_seq s.float
  ; string = Distribution.StringChars.of_seq ~track s.string
  ; datetime = Count.of_seq s.datetime
  ; enum_length = Distribution.of_seq s.enum_length
  ; dict_keys = Distribution.of_seq (Seq.map String.length s.dict_keys)
  ; dict_length = Distribution.of_seq s.dict_length
  ; base64 = Distribution.of_seq (Seq.map String.length s.base64)
  ; null = Count.of_seq s.null
  }


let pp_distribution = Fmt.using Distribution.to_view Fmt.(Dump.(array @@ pair int int))
let pp_string_chars = Distribution.StringChars.(Fmt.using to_view dump_view)

let pp_stats ppf t =
  Fmt.pf
    ppf
    "of_stats@ ~int:%a@ ~int32:%a@ ~bool:%a@ ~float:%a@ ~string:%a@ ~datetime:%a@ \
     ~enum_length:%a@ ~dict_keys:%a@ ~dict_length:%a@ ~base64:%a@ ~null:%a"
    Count.pp
    t.int
    Count.pp
    t.int32
    Count.pp
    t.bool
    Count.pp
    t.float
    pp_string_chars
    t.string
    Count.pp
    t.datetime
    pp_distribution
    t.enum_length
    pp_distribution
    t.dict_keys
    pp_distribution
    t.dict_length
    pp_distribution
    t.base64
    Count.pp
    t.null


let of_stats
      ~int
      ~int32
      ~bool
      ~float
      ~string:(length, chars)
      ~datetime
      ~enum_length
      ~dict_keys
      ~dict_length
      ~base64
      ~null
  =
  { int = Count.v int
  ; int32 = Count.v int32
  ; bool = Count.v bool
  ; float = Count.v float
  ; string = Distribution.StringChars.of_view (length, chars)
  ; datetime = Count.v datetime
  ; enum_length = Distribution.(of_view enum_length)
  ; dict_keys = Distribution.(of_view dict_keys)
  ; dict_length = Distribution.(of_view dict_length)
  ; base64 = Distribution.(of_view base64)
  ; null = Count.v null
  }


let seq_of_stats (s : stats) : t =
  { int = Count.generate Int64.max_int s.int
  ; int32 = Count.generate Int32.max_int s.int32
  ; bool = Count.generate true s.bool
  ; float = Count.generate Float.epsilon s.float
  ; string = Distribution.StringChars.generate s.string
  ; datetime = Count.generate "20250320T12:00:00Z" s.datetime
  ; enum_length = Distribution.generate s.enum_length
  ; dict_keys =
      Distribution.generate s.dict_keys
      |> Seq.mapi (fun i len -> Printf.sprintf "%.*d" len i)
  ; dict_length = Distribution.generate s.dict_length
  ; base64 =
      Distribution.generate s.base64
      |> Seq.map (fun len -> String.init len (fun i -> Char.chr (i mod 255)))
  ; null = Count.generate () s.null
  }


let rpc_of_stats stats =
  let seq = seq_of_stats stats in
  (* move big strings to front, so they go into nested dicts *)
  let string_gen =
    seq.string
    |> Seq.map Rpc.rpc_of_string
    |> List.of_seq
    |> List.rev
    |> List.to_seq
    |> Seq.to_dispenser
  and dict_key_gen = seq.dict_keys |> Seq.to_dispenser in
  let nested_dict =
    seq.dict_length
    |> (Seq.map
        @@ fun len ->
        List.to_seq
          (List.of_seq
           @@ Seq.init len
           @@ fun _ ->
           ( dict_key_gen () |> Option.value ~default:"nk"
           , string_gen () |> Option.value ~default:(Rpc.String "") )))
    |> List.of_seq
    |> List.to_seq
  in
  let enum =
    seq.enum_length
    |> (Seq.map
        @@ fun len ->
        List.to_seq
          (List.of_seq
           @@ Seq.init len
           @@ fun _ -> string_gen () |> Option.value ~default:(Rpc.String "")))
    |> List.of_seq
    |> List.to_seq
  in
  (* remaining strings *)
  let strings = Seq.of_dispenser string_gen in
  (* TODO: doesn't quite work yet, generates too short dummy.. *)
  let dict_values =
    [ Seq.map Rpc.rpc_of_int64 seq.int
    ; Seq.map Rpc.rpc_of_int32 seq.int32
    ; Seq.map Rpc.rpc_of_bool seq.bool
    ; Seq.map Rpc.rpc_of_float seq.float
    ; Seq.map Rpc.rpc_of_base64 seq.base64
    ; Seq.map Rpc.rpc_of_unit seq.null
    ; strings
    ; Seq.map (fun e -> Rpc.Enum (List.of_seq e)) enum
    ; Seq.map (fun d -> Rpc.Dict (List.of_seq d)) nested_dict
    ]
    |> List.to_seq
    |> Seq.concat
    |> Seq.memoize
  in
  let dict =
    dict_values
    |> Seq.map (fun v -> dict_key_gen () |> Option.value ~default:"nk", v)
    |> List.of_seq
  in
  Rpc.Dict dict
