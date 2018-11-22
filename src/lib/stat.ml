module Stat = struct
  type t = {
    created : int64;
    modified : int64;
  }
  let make g = { created = g; modified = g }
  let update g t = { t with modified = g }
end

include Stat

type 'a wrapped_field = Fld : (_, 'a) Rpc.Types.field -> 'a wrapped_field

(** StatTree - a data structure that keeps track of modification times
 *
 * Essentially a mapping from string list -> 'a, but keeping track of
 * when entries were added and when they are updated. 
 *
 * Crucially, it provides an efficient 'fold' method to fold a function
 * only over entries that have been modified after a supplied time. This
 * allows this to be used to track modifications to tree-like data and
 * replicate the data structure elsewhere
 *)

module StatTree = struct
  (* Each node has a 'Stat.t' with creation/modification information,
     optional data, and a list of children, stored as a (string, 'a t)
     association list *)
  type 'a t = Node of Stat.t * 'a option * (string * 'a t) list

  let empty = Node (Stat.make 0L, None, [])

  let rec get_stat : string list -> 'a t -> Stat.t = fun path t ->
    match path, t with
    | [], Node (st,_,_) -> st
    | s::ss, Node (_st,_,children) -> get_stat ss (List.assoc s children)

  let rec update_stat : int64 -> string list -> 'a option -> 'a t option -> 'a t = fun g path a t ->
    match path, t with
    | [], Some (Node (st, _x, xs)) -> Node (Stat.update g st, a, xs)
    | [], None -> Node (Stat.make g, a, [])
    | s::ss, None ->
      Node (Stat.make g, None, [s, update_stat g ss a None])
    | s::ss, Some (Node (st, thisa, xs)) ->
      let mine,others = (* List.partition (fun (x,_n) -> x=s) xs in *)
        let rec inner notmine l =
          match l with
          | (((x,_n) as f) ::xs) ->
            if x=s then (Some f,List.fold_left (fun acc x -> x::acc) xs notmine) else inner (f::notmine) xs
          | [] -> None, xs
        in
        inner [] xs
      in  
      let new_mine =
        match mine with
        | None -> (s, update_stat g ss a None)
        | Some (_, n) -> (s, update_stat g ss a (Some n))
      in
      Node (Stat.update g st, thisa, new_mine::others)

  let fold_over_recent : int64 -> (string list -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a = fun g f acc t ->
    let rec inner path acc t =
      match t with
      | Node (st, v, children) ->
        if st.Stat.modified > g then
          match v,children with
          | Some x , [] -> f path acc x
          | Some x , _children when st.Stat.created > g -> f path acc x
          | _ , _ ->
            let rec fold_left_recent f accu l =
              match l with
              | [] -> accu
              | (_, Node (st, _v, _children))::_l when st.modified <= g -> accu
              | a::l -> fold_left_recent f (f accu a) l
            in
            fold_left_recent (fun acc (p,x) -> inner (p::path) acc x) acc children
        else acc
    in inner [] acc t

  let to_partial gen t =
    let rec inner t =
      match t with
      | Node (st, _v, children) ->
          if st.Stat.created > gen
          then Some Rpcmarshal.Complete
          else if st.Stat.modified > gen then begin
            if List.length children = 0
            then Some Complete
            else 
              let modified_children = List.fold_left (fun acc (p, t) -> match inner t with Some t' -> (p,t')::acc | None -> acc) [] children in
              if List.length modified_children = 0
              then None
              else Some (Rpcmarshal.Partial modified_children)
            end
          else None
    in inner t

  let print st =
    let vprinter = function
    | Fld f -> String.concat "." f.fname
    in
    let rec inner name indent t =
      match t with
      | Node (st,v,cs) ->
        Printf.printf "%s===%s===\n" indent name;
        Printf.printf "%s: Node(gen=(%Ld,%Ld)\n" indent st.Stat.created st.Stat.modified; 
        Printf.printf "%s     v=%s\n" indent (match v with Some v -> vprinter v | None -> "<none>");
        Printf.printf "%s     children={\n" indent;
        List.iter (fun (n,t) -> inner n (Printf.sprintf "%s  " indent) t) cs;
        Printf.printf "%s     }\n" indent
    in inner "ROOT" "" st

  let print_partials t =
    let rec inner name indent t =
      Printf.printf "%s===%s===\n" indent name;
      match t with
      | Rpcmarshal.Complete -> Printf.printf "%sComplete\n" indent
      | Rpcmarshal.Partial cs ->
        Printf.printf "%sPartial {\n" indent;
        List.iter (fun (n,t) -> inner n (Printf.sprintf "%s  " indent) t) cs;
        Printf.printf "%s}\n" indent
    in inner "ROOT" "" t
end


type 'a Rpc.Types.cls += Stats : 'a wrapped_field StatTree.t Rpc.Types.cls

let map_fld : type b. (b, 'a) Rpc.Types.field -> (int64, 'a wrapped_field StatTree.t) Rpc.Types.field = fun f ->
  let path = f.fname in
  { fname = f.fname;
    field = Rpc.Types.(Basic Int64);
    fget = (fun st -> let s = StatTree.get_stat path st in s.Stat.modified);
    fset = (fun g st -> StatTree.update_stat g path (Some (Fld f)) (Some st));
    fcls = Stats;
    fdescription = [];
    fversion = None;
    fdefault = None;
    }

let dump_since : int64 -> 'b Rpc.Types.typ -> 'b -> 'b wrapped_field StatTree.t -> Rpc.t option = fun g typ db st ->
  let partial = StatTree.to_partial g st in
  match partial with
  | Some p -> Some (Rpcmarshal.marshal_partial typ p db)
  | None -> None
