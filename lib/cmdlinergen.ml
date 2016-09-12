open Idl

type description = Idl.Interface.description

let describe x = x

type 'a comp = 'a
type rpcfn = Rpc.call -> Rpc.response Rpc.Monad.error_or
type 'a res = rpcfn -> unit Cmdliner.Term.t * Cmdliner.Term.info

type _ fn =
  | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
  | Returning : 'a Param.t -> 'a comp fn

let returning a = Returning a
let (@->) = fun t f -> Function (t, f)

let pos = ref 0

let term_of_param : type a. a Param.t -> Rpc.t Cmdliner.Term.t = fun p ->
  let open Rpc.Types in
  let open Cmdliner in
  let pinfo = Cmdliner.Arg.info [] ~doc:p.Param.description ~docv:(p.Param.name) in
  let incr () =
    let p = !pos in
    incr pos;
    p
  in
  match p.Param.typedef.Rpc.Types.ty with
  | Basic Int ->
    Term.app
      (Term.pure Rpc.rpc_of_int64)
      (Cmdliner.Arg.(required & pos (incr ()) (some int64) None & pinfo))
  | Basic Int32 ->
    Term.app
      (Term.pure Rpc.rpc_of_int64)
      (Cmdliner.Arg.(required & pos (incr ()) (some int64) None & pinfo))
  | Basic Int64 ->
    Term.app
      (Term.pure Rpc.rpc_of_int64)
      (Cmdliner.Arg.(required & pos (incr ()) (some int64) None & pinfo))
  | Basic String ->
    Term.app
      (Term.pure Rpc.rpc_of_string)
      (Cmdliner.Arg.(required & pos (incr ()) (some string) None & pinfo))
  | Basic Bool ->
    Term.app
      (Term.pure Rpc.rpc_of_bool)
      (Cmdliner.Arg.(required & pos (incr ()) (some bool) None & pinfo))
  | Basic Float ->
    Term.app
      (Term.pure Rpc.rpc_of_float)
      (Cmdliner.Arg.(required & pos (incr ()) (some float) None & pinfo))
  | Basic Char ->
    Term.app
      (Term.pure (fun s -> Rpc.rpc_of_char s.[0]))
      (Cmdliner.Arg.(required & pos (incr ()) (some string) None & pinfo))
  | Unit -> Term.(const Rpc.Null)
  | DateTime ->
    Term.app
      (Term.pure (Rpc.rpc_of_dateTime))
      (Cmdliner.Arg.(required & pos (incr ()) (some string) None & pinfo))
  | Array _ ->
    Term.app
      (Term.pure (fun x ->
           let x = Jsonrpc.of_string x in
           match x with
           | Rpc.Enum _ -> x
           | _ -> failwith "Type error"))
      (Cmdliner.Arg.(required & pos (incr ()) (some string) None & pinfo))
  | List _ ->
    Term.app
      (Term.pure (fun x ->
           let x = Jsonrpc.of_string x in
           match x with
           | Rpc.Enum _ -> x
           | _ -> failwith "Type error"))
      (Cmdliner.Arg.(required & pos (incr ()) (some string) None & pinfo))
  | Dict _ ->
    Term.app
      (Term.pure (fun x ->
           let x = Jsonrpc.of_string x in
           match x with
           | Rpc.Dict _ -> x
           | _ -> failwith "Type error"))
      (Cmdliner.Arg.(required & pos (incr ()) (some string) None & pinfo))
  | Option _ -> Term.(const Rpc.Null)
  | Tuple (t1, t2) -> Term.const Rpc.Null
  | Struct {sname; fields} ->
    Term.app
      (Term.pure (fun x ->
           let x = Jsonrpc.of_string x in
           match x with
           | Rpc.Dict _ -> x
           | _ -> failwith "Type error"))
      (Cmdliner.Arg.(required & pos (incr ()) (some string) None & pinfo))
  | Variant {variants} ->
    Term.app
      (Term.pure (fun x ->
           let x = Jsonrpc.of_string x in
           match x with
           | Rpc.Enum _
           | Rpc.String _ -> x
           | _ -> failwith "Type error"))
      (Cmdliner.Arg.(required & pos (incr ()) (some string) None & pinfo))

let declare name descr ty rpc =
  let rec inner : type b. ((string * Rpc.t) list) Cmdliner.Term.t -> b fn -> unit Cmdliner.Term.t = fun cur f ->
    match f with
    | Function (t, f) ->
      let term = term_of_param t in
      let term = Cmdliner.Term.(const (fun x acc -> (t.Param.name, x)::acc) $ term $ cur) in
      inner term f
    | Returning ty ->
      let run args =
        let call = Rpc.call name [(Rpc.Dict args)] in
        ignore (Rpc.Monad.bind (rpc call) (function response ->
        match response.Rpc.contents with
        | x -> Printf.printf "%s\n" (Rpc.to_string x);
          Rpc.Monad.return ()))
      in
      Cmdliner.Term.(const (fun args -> run args) $ cur)
  in
  inner (Cmdliner.Term.pure []) ty, Cmdliner.Term.info name ~doc:descr
