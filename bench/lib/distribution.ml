type t = Histogram.t

let of_seq s =
  let hist = Histogram.make () in
  let () =
    s
    |> Seq.iter
       @@ fun v ->
       assert (v >= 0);
       Histogram.(update hist v)
  in
  hist


type view = (int * int) array

let dump_view = Fmt.(Dump.(array @@ pair int int))
let of_const c = of_seq (Seq.return c)
let merge = Histogram.merge
let view_of_bucket b = Histogram.Bucket.sum b, Histogram.Bucket.count b
let to_view t = t |> Histogram.to_seq |> Seq.map view_of_bucket |> Array.of_seq
let bucket_of_view (sum, count) = Histogram.Bucket.v ~sum ~count
let of_view view = view |> Array.to_seq |> Seq.map bucket_of_view |> Histogram.of_seq

module Index = ExponentialHistogramBucketIndex

let generate t =
  let open Histogram in
  t |> to_seq |> Seq.concat_map Bucket.generate


module StringChars = struct
  module CharMap = Map.Make (Char)

  type nonrec t =
    { length : t
    ; chars : t CharMap.t
    }

  let merge a b =
    { length = merge a.length b.length
    ; chars = CharMap.union (fun _ a b -> Some (merge a b)) a.chars b.chars
    }


  let of_seq ~track strs =
    let length, chars =
      Seq.fold_left
        (fun (length, chars) str ->
           let char_counts = Array.make 256 0 in
           let () =
             str
             |> String.iter
                @@ fun c ->
                if track c
                then (
                  let i = Char.code c in
                  char_counts.(i) <- char_counts.(i) + 1)
           in
           let chars =
             char_counts
             |> Array.to_seqi
             |> Seq.fold_left
                  (fun chars (i, count) ->
                     if count > 0
                     then
                       CharMap.update
                         (Char.chr i)
                         (fun hist ->
                            let hist =
                              match hist with
                              | None -> Histogram.make ()
                              | Some t -> t
                            in
                            Histogram.update hist count;
                            Some hist)
                         chars
                     else chars)
                  chars
           in
           Seq.cons (String.length str) length, chars)
        (Seq.empty, CharMap.empty)
        strs
    in
    { length = of_seq length; chars }


  let generate t =
    let chars = CharMap.map generate t.chars in
    t.length
    |> generate
    |> Seq.mapi
       @@ fun i len ->
       let b = Bytes.of_string (if i = 0 then "" else Printf.sprintf "%.*d" len i) in
       let add_char char ~offset ~count =
         for i = 0 to count - 1 do
           Bytes.set b ((i * (len / count)) + offset) char
         done
       in
       let _, _, _ =
         CharMap.fold
           (fun char char_count (offset, str_length, chars) ->
              match Seq.uncons char_count with
              | Some (t, tl) when 2 * t < str_length ->
                add_char char ~offset ~count:t;
                offset + 1, str_length - t, CharMap.add char tl chars
              | _ -> offset, str_length, chars)
           chars
           (0, len, chars)
       in
       Bytes.unsafe_to_string b


  type nonrec view = view * (char * view) list

  let of_view (length, chars) =
    { length = length |> of_view
    ; chars =
        chars |> List.to_seq |> Seq.map (fun (c, s) -> c, s |> of_view) |> CharMap.of_seq
    }


  let to_view t =
    ( to_view t.length
    , t.chars |> CharMap.to_seq |> Seq.map (fun (c, t) -> c, t |> to_view) |> List.of_seq
    )


  let dump_view = Fmt.(Dump.(pair dump_view @@ list @@ pair char dump_view))
end

let dump = Histogram.dump
