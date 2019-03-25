(* Python generator *)
open Codegen
open Rpc.Types

type t = Block of t list | Line of string

let inline_defaults =
  {|
def success(result):
    """
    Called from each server dispatcher to return a successful response in case
    no exceptions were thrown.
    """
    return {"Status": "Success", "Value": result}


def handle_exception(exn):
    """
    Defines the behavior of the generated CLI code when arguments are passed as
    JSON and the dispatched function fails with an exception.
    """
    raise exn


class Rpc_light_failure(Exception):
    """Each error variant defined in the interface is a subclass of this exception."""

    def __init__(self, name, args):
        super(Rpc_light_failure, self).__init__(
            "Rpc_light_failure: {} ({})".format(name, args))
        self.name = name
        self.args = args

    def failure(self):
        """Returned from the top-level server dispatcher to indicate a failed RPC call."""

        # rpc-light marshals a single result differently to a list of results
        args = list(self.args)
        marshalled_args = args
        if len(args) == 1:
            marshalled_args = args[0]
        return {'Status': 'Failure',
                'ErrorDescription': [self.name, marshalled_args]}


class Unimplemented(Rpc_light_failure):
    """The called RPC method is not implemented."""
    def __init__(self, arg):
        super(Unimplemented, self).__init__("Unimplemented", [arg])


class InternalError(Rpc_light_failure):
    """
    Indicates either an error in the generated bindings to dispatch the RPC
    call, or an unknown exception raised by the implementation that is not
    declared in the interface.
    """
    def __init__(self, error):
        super(InternalError, self).__init__("Internal_error", [error])


class UnmarshalException(InternalError):
    """The input does not have the expected structure."""
    def __init__(self, thing, ty, desc):
        super(UnmarshalException, self).__init__(
            "UnmarshalException thing=%s ty=%s desc=%s" % (thing, ty, desc))


class TypeError(InternalError):
    """The type of an argument differs from the one defined in the interface."""
    def __init__(self, expected, actual):
        super(TypeError, self).__init__(
            "TypeError expected=%s actual=%s" % (expected, actual))


class UnknownMethod(InternalError):
    """The called method is not defined in the interface."""
    def __init__(self, name):
        super(UnknownMethod, self).__init__("Unknown method %s" % name)


class ListAction(argparse.Action):
    """
    Custom argparse action for specifying dictionaries on the command line.
    The syntax is <prog> --dict_name key1 value1 --dict_name key2 value2 ...
    """
    def __call__(self, parser, namespace, values, option_string=None):
        key = values[0]
        value = values[1]
        if (hasattr(namespace, self.dest) and
                getattr(namespace, self.dest) is not None):
            getattr(namespace, self.dest)[key] = value
        else:
            setattr(namespace, self.dest, {key: value})|}

let reserved_exns =
  [ "Rpc_light_failure"
  ; "Unimplemented"
  ; "InternalError"
  ; "UnmarshalException"
  ; "TypeError"
  ; "UnknownMethod" ]

let rec lines_of_t t =
  let indent = String.make 4 ' ' in
  match t with
  | Line x -> [x]
  | Block xs ->
      let all = List.concat (List.map lines_of_t xs) in
      List.map (function "" -> "" | x -> indent ^ x) all

let string_of_ts ts = String.concat "\n" (List.concat (List.map lines_of_t ts))

let output_doc description =
  let process descr =
    descr |> String.split_on_char '\n'
    |> List.map (function l -> Line (String.trim l))
  in
  let descr_lines =
    description |> List.map process |> List.concat
    |> function Line "" :: tl -> tl | lst -> lst
  in
  [Line {|"""|}] @ descr_lines @ [Line {|"""|}]

(* generate a fresh id *)
let fresh_id =
  let counter = ref 5 in
  fun () ->
    incr counter ;
    "tmp_" ^ string_of_int !counter

(** [typecheck ty v] returns a python fragment which checks
    [v] has type [ty] *)
let rec typecheck : type a. a typ -> string -> t list =
 fun ty v ->
  let open Printf in
  let raise_type_error =
    Line
      (sprintf {|raise TypeError("%s", repr(%s))|} (Rpcmarshal.ocaml_of_t ty) v)
  in
  let handle_basic b =
    let python_of_basic : type a. a basic -> string = function
      | Int64 -> "(int, long)"
      | Int32 -> "int"
      | Int -> "(int, long)"
      | Char -> "(str, unicode)"
      | String -> "(str, unicode)"
      | Float -> "float"
      | Bool -> "bool"
    in
    [ Line (sprintf "if not isinstance(%s, %s):" v (python_of_basic b))
    ; Block [raise_type_error] ]
  in
  match ty with
  | Basic Int64 -> handle_basic Int64
  | Basic String -> handle_basic String
  | Basic Int32 -> handle_basic Int32
  | Basic Int -> handle_basic Int
  | Basic Bool -> handle_basic Bool
  | Basic Float -> handle_basic Float
  | Basic Char -> handle_basic Char
  | DateTime -> handle_basic String
  | Struct {fields; _} ->
      let check boxedfield =
        let (BoxedField f) = boxedfield in
        let member = sprintf "%s['%s']" v (String.concat "." f.fname) in
        match f.field with
        | Option ty ->
            [ Line (sprintf "if '%s' in %s:" (String.concat "." f.fname) v)
            ; Block (typecheck ty member) ]
        | _ -> typecheck f.field member
      in
      List.concat (List.rev (List.map check (List.rev fields)))
  | Variant {variants; _} ->
      let check first boxed_tag =
        let (BoxedTag t) = boxed_tag in
        match t.tcontents with
        | Unit -> failwith "Can't happen: this has been filtered out"
        | ty ->
            [ Line
                (sprintf "%s %s[0] == '%s':"
                   (if first then "if" else "elif")
                   v t.tname)
            ; Block (typecheck ty (sprintf "%s[1]" v)) ]
      in
      let variants_to_check =
        List.filter
          (fun (BoxedTag t) ->
            match t.tcontents with Unit -> false | _ -> true )
          variants
      in
      let check_contents =
        List.fold_left
          (fun acc x -> List.concat [acc; check false x])
          (check true (List.hd variants_to_check))
          (List.tl variants_to_check)
      in
      let all_tags = List.map (fun (BoxedTag t) -> t.tname) variants in
      let pylist =
        sprintf "[%s]"
          (String.concat ", " (List.map (fun s -> sprintf {|"%s"|} s) all_tags))
      in
      [Line (sprintf "if %s[0] not in %s:" v pylist); Block [raise_type_error]]
      @ check_contents
  | Array t ->
      let id = fresh_id () in
      [ Line (sprintf "if not isinstance(%s, list):" v)
      ; Block [raise_type_error]
      ; Line (sprintf "for %s in %s:" id v)
      ; Block (typecheck t id) ]
  | List t ->
      let id = fresh_id () in
      [ Line (sprintf "if not isinstance(%s, list):" v)
      ; Block [raise_type_error]
      ; Line (sprintf "for %s in %s:" id v)
      ; Block (typecheck t id) ]
  | Dict (key, va) ->
      let id_k = fresh_id () in
      let id_v = fresh_id () in
      [ Line (sprintf "if not isinstance(%s, dict):" v)
      ; Block [raise_type_error]
      ; Line (sprintf "for %s, %s in %s.items():" id_k id_v v)
      ; Block (typecheck (Basic key) id_k)
      ; Block (typecheck va id_v) ]
  | Unit -> [Line (sprintf "if %s is not None:" v); Block [raise_type_error]]
  | Option t -> [Line (sprintf "if %s is not None:" v); Block (typecheck t v)]
  | Tuple (a, b) ->
      [ Line (sprintf "if not (isinstance(%s, tuple) and len(%s) == 2):" v v)
      ; Block [raise_type_error]
      ; Line (sprintf "v1, v2 = %s" v) ]
      @ typecheck a (Printf.sprintf "v1")
      @ typecheck b (Printf.sprintf "v2")
  | Tuple3 (a, b, c) ->
      [ Line (sprintf "if not (isinstance(%s, tuple) and len(%s) == 3):" v v)
      ; Block [raise_type_error]
      ; Line (sprintf "v1, v2, v3 = %s" v) ]
      @ typecheck a (Printf.sprintf "v1")
      @ typecheck b (Printf.sprintf "v2")
      @ typecheck c (Printf.sprintf "v3")
  | Tuple4 (a, b, c, d) ->
      [ Line (sprintf "if not (isinstance(%s, tuple) and len(%s) == 4):" v v)
      ; Block [raise_type_error]
      ; Line (sprintf "v1, v2, v3, v4 = %s" v) ]
      @ typecheck a (Printf.sprintf "v1")
      @ typecheck b (Printf.sprintf "v2")
      @ typecheck c (Printf.sprintf "v3")
      @ typecheck d (Printf.sprintf "v4")
  | Abstract _ -> failwith "Abstract types cannot be typechecked by pythongen"
  | Refv _ ->
    failwith "Refv types cannot be typechecked by pythongen"
  | Refmap _ ->
    failwith "Refmap types cannot be typechecked by pythongen"

let rec value_of : type a. a typ -> string =
  let open Printf in
  function
    | Basic Int64 -> "0L"
    | Basic Int -> "0L"
    | Basic Int32 -> "0"
    | Basic Char -> "'c'"
    | Basic String -> {|"string"|}
    | Basic Float -> "1.1"
    | Basic Bool -> "True"
    | DateTime -> {|"19700101T00:00:00Z"|}
    | Struct {fields; _} ->
        let member boxed_field =
          let (BoxedField f) = boxed_field in
          sprintf {|"%s": %s|} (String.concat "." f.fname) (value_of f.field)
        in
        sprintf "{%s}" (String.concat ", " (List.map member fields))
    | Variant _ -> "None"
    | Array t -> sprintf "[%s]" (value_of t)
    | List t -> sprintf "[%s]" (value_of t)
    | Dict (key, va) -> sprintf "{%s: %s}" (value_of (Basic key)) (value_of va)
    | Unit -> "None"
    | Option _ -> "None"
    | Tuple _ -> "[]"
    | Tuple3 _ -> "[]"
    | Tuple4 _ -> "[]"
    | Abstract _ -> failwith "Cannot get default value for abstract types"
    | Refv _ -> failwith "Cannot get default value for Refv type"
    | Refmap _ -> failwith "Cannot get default value for Refmap type"

let exn_var myarg =
  let open Printf in
  let inner : type a b. (a, b) tag -> t list = function
    | tag ->
        let has_arg = match tag.tcontents with Unit -> false | _ -> true in
        (* Avoid generating a duplicate version of an existing exception defined in
       the helpers *)
        if List.mem tag.tname reserved_exns then []
        else
          [ Line ""
          ; Line ""
          ; Line (sprintf "class %s(Rpc_light_failure):" tag.tname)
          ; Block
              ( output_doc tag.tdescription
              @ [Line ""]
              @
              if not has_arg then
                [ Line "def __init__(self)"
                ; Block
                    [ Line
                        (sprintf {|super(%s, self).__init__("%s", [])|}
                           tag.tname tag.tname) ] ]
              else
                [ Line (sprintf "def __init__(self, arg_0):")
                ; Block
                    ( [ Line
                          (sprintf {|super(%s, self).__init__("%s", [arg_0])|}
                             tag.tname tag.tname) ]
                    @ typecheck tag.tcontents "arg_0"
                    @ [Line "self.arg_0 = arg_0"] ) ] ) ]
  in
  match myarg with
  | BoxedDef {ty= Variant {variants; _}; _} ->
      List.concat (List.map (fun (BoxedTag t) -> inner t) variants)
  | BoxedDef {ty; _} ->
      failwith
        (Printf.sprintf "Unable to handle non-variant exceptions (%s)"
           (Rpcmarshal.ocaml_of_t ty))

let skeleton_method unimplemented i (BoxedFunction m) =
  let inputs = Method.(find_inputs m.ty) in
  let output = Method.(find_output m.ty) in
  let inputs =
    List.filter
      (function
          | Idl.Param.Boxed {Idl.Param.typedef; _} ->
            match typedef.ty with Unit -> false | _ -> true)
      inputs
  in
  let open Printf in
  let output_py (Idl.Param.Boxed a) =
    let value = value_of a.Idl.Param.typedef.ty in
    match (a.Idl.Param.typedef.ty, a.Idl.Param.name) with
    | Unit, _ -> []
    | _, _ -> [Line (sprintf "return %s" value)]
  in
  [ Line ""
  ; Line
      (sprintf "def %s(self%s):" m.Method.name
         (String.concat ""
            (List.map
               (fun x -> ", " ^ x)
               (List.map
                  (fun (Idl.Param.Boxed x) ->
                    match x.Idl.Param.name with
                    | Some n -> n
                    | None ->
                        failwith
                          (Printf.sprintf
                             "Parameter names required for python generation \
                              (%s)"
                             m.Method.name) )
                  inputs))))
  ; Block
      ( output_doc m.Method.description
      @
      if unimplemented then
        [ Line
            (sprintf {|raise Unimplemented("%s.%s")|}
               i.Interface.details.Idl.Interface.name m.Method.name) ]
      else output_py output ) ]

let example_stub_user i (BoxedFunction m) =
  let open Printf in
  [ Line ""
  ; Line "# import necessary libraries if needed"
  ; Line
      "# we assume that your library providing the client is called myclient \
       and it provides a connect method"
  ; Line "import myclient"
  ; Line ""
  ; Line {|if __name__ == "__main__":|}
  ; Block
      [ Line "c = myclient.connect()"
      ; Line
          (Printf.sprintf "results = c.%s.%s({ %s })"
             i.Interface.details.Idl.Interface.name m.Method.name
             (String.concat ", "
                (List.map
                   (fun (Idl.Param.Boxed a) ->
                     sprintf "%s: %s"
                       ( match a.Idl.Param.name with
                       | Some x -> x
                       | None ->
                           failwith
                             (Printf.sprintf
                                "Parameter names required for python \
                                 generation (%s)"
                                m.Method.name) )
                       (value_of a.Idl.Param.typedef.ty) )
                   Method.(find_inputs m.ty))))
      ; Line "print (repr(results))" ] ]

let example_skeleton_user i m =
  let open Printf in
  [ Line ""
  ; Line "# import additional libraries if needed"
  ; Line ""
  ; Line
      (sprintf "class %s_myimplementation(%s_skeleton):"
         i.Interface.details.Idl.Interface.name
         i.Interface.details.Idl.Interface.name)
  ; Block
      ( [ Line "# by default each method will return a Not_implemented error"
        ; Line "# ..." ] @ skeleton_method false i m @ [Line "# ..."] ) ]

let skeleton_of_interface unimplemented suffix i =
  let open Printf in
  [ Line ""
  ; Line ""
  ; Line
      (sprintf "class %s_%s(object):" i.Interface.details.Idl.Interface.name
         suffix)
  ; Block
      ( output_doc i.Interface.details.Idl.Interface.description
      @ [Line ""; Line "def __init__(self):"; Block [Line "pass"]]
      @ List.concat
          (List.map (skeleton_method unimplemented i) i.Interface.methods) ) ]

let test_impl_of_interface = skeleton_of_interface false "test"

let skeleton_of_interface = skeleton_of_interface true "skeleton"

let server_of_interface i =
  let open Printf in
  let typecheck_method_wrapper (BoxedFunction m) =
    let inputs = Method.(find_inputs m.ty) in
    let inputs =
      List.filter
        (function
            | Idl.Param.Boxed {Idl.Param.typedef; _} ->
              match typedef.ty with Unit -> false | _ -> true)
        inputs
    in
    let output = Method.(find_output m.ty) in
    let extract_input (Idl.Param.Boxed arg) =
      let arg_name =
        match arg.Idl.Param.name with
        | Some x -> x
        | None ->
            failwith
              (Printf.sprintf
                 "Parameter names requred for python generation (%s)"
                 m.Method.name)
      in
      [ Line (sprintf {|if "%s" not in args:|} arg_name)
      ; Block
          [ Line
              (sprintf "raise UnmarshalException('argument missing', '%s', '')"
                 arg_name) ]
      ; Line (sprintf {|%s = args["%s"]|} arg_name arg_name) ]
      @ typecheck arg.Idl.Param.typedef.ty arg_name
    in
    let check_output (Idl.Param.Boxed arg) =
      match arg.Idl.Param.typedef.ty with
      | Unit -> []
      | _ ->
          (* The ocaml rpc-light doesn't actually support named results, instead we
           have single anonymous results only. *)
          typecheck arg.Idl.Param.typedef.ty "results"
    in
    [ Line ""
    ; Line (sprintf "def %s(self, args):" m.Method.name)
    ; Block
        ( [ Line
              {|"""type-check inputs, call implementation, type-check outputs and return"""|}
          ; Line "if not isinstance(args, dict):"
          ; Block
              [Line "raise UnmarshalException('arguments', 'dict', repr(args))"]
          ]
        @ List.concat (List.map extract_input inputs)
        @ [ Line
              (sprintf "results = self._impl.%s(%s)" m.Method.name
                 (String.concat ", "
                    (List.map
                       (fun (Idl.Param.Boxed x) ->
                         match x.Idl.Param.name with
                         | Some n -> n
                         | None ->
                             failwith
                               "Parameter names required for python generation"
                         )
                       inputs))) ]
        @ List.concat (List.map check_output [output])
        @ [Line "return results"] ) ]
  in
  let dispatch_method (BoxedFunction m) comma =
    Line
      (sprintf {|    "%s.%s": self.%s%s|}
         i.Interface.details.Idl.Interface.name m.Method.name m.Method.name
         comma)
  in
  let dispatch_dict methods =
    let rec intersperse_commas (list: Codegen.boxed_fn list) =
      match list with
      | [] -> []
      | [x] -> [dispatch_method x ""]
      | x :: y :: tl -> dispatch_method x "," :: intersperse_commas (y :: tl)
    in
    [Line "self._dispatcher_dict = {"] @ intersperse_commas methods @ [Line "}"]
  in
  [ Line ""
  ; Line ""
  ; Line
      (sprintf "class %s_server_dispatcher(object):"
         i.Interface.details.Idl.Interface.name)
  ; Block
      ( output_doc i.Interface.details.Idl.Interface.description
      @ [ Line ""
        ; Line "def __init__(self, impl):"
        ; Block
            ( [ Line
                  {|"""impl is a proxy object whose methods contain the implementation"""|}
              ; Line "self._impl = impl" ]
            @ dispatch_dict i.Interface.methods ) ]
      @ List.concat (List.map typecheck_method_wrapper i.Interface.methods)
      @ [ Line ""
        ; Line "def _dispatch(self, method, params):"
        ; Block
            [ Line
                {|"""type check inputs, call implementation, type check outputs and return"""|}
            ; Line "args = params[0]"
            ; Line "return success(self._dispatcher_dict[method](args))" ] ] )
  ]

let test_impl_of_interfaces i =
  let open Printf in
  [ Line ""
  ; Line ""
  ; Line
      (sprintf "class %s_server_test(%s_server_dispatcher):" i.Interfaces.name
         i.Interfaces.name)
  ; Block
      [ Line
          {|"""Create a server which will respond to all calls, returning arbitrary values. This is intended as a marshal/unmarshal test."""|}
      ; Line ""
      ; Line "def __init__(self):"
      ; Block
          [ Line
              (sprintf "%s_server_dispatcher.__init__(self%s)"
                 i.Interfaces.name
                 (String.concat ""
                    (List.map
                       (fun i ->
                         sprintf ", %s_server_dispatcher(%s_test())"
                           i.Interface.details.Idl.Interface.name
                           i.Interface.details.Idl.Interface.name )
                       i.Interfaces.interfaces))) ] ] ]

let commandline_parse _ (BoxedFunction m) =
  let open Printf in
  let inputs = Method.(find_inputs m.ty) in
  let inputs =
    List.filter
      (function
          | Idl.Param.Boxed {Idl.Param.typedef; _} ->
            match typedef.ty with Unit -> false | _ -> true)
      inputs
  in
  [ Line ""
  ; Line (sprintf "def _parse_%s(self):" m.Method.name)
  ; Block
      ( output_doc m.Method.description
      @ [ Line "# in --json mode we don't have any other arguments"
        ; Line "if ('--json' in sys.argv or '-j' in sys.argv):"
        ; Block
            [ Line "jsondict = json.loads(sys.stdin.readline(),)"
            ; Line "jsondict['json'] = True"
            ; Line "return jsondict" ]
        ; Line
            (sprintf "parser = argparse.ArgumentParser(description='%s')"
               (String.concat " " m.Method.description))
        ; Line
            "parser.add_argument('-j', '--json', action='store_const', \
             const=True, default=False, help='Read json from stdin, print \
             json to stdout', required=False)" ]
      @ List.map
          (fun (Idl.Param.Boxed a) ->
            let name =
              match a.Idl.Param.name with
              | Some s -> s
              | None ->
                  failwith
                    (Printf.sprintf
                       "Parameter names required for python generation (%s)"
                       m.Method.name)
            in
            match a.Idl.Param.typedef.ty with
            | Dict (_, _) ->
                Line
                  (sprintf
                     "parser.add_argument('--%s', default={}, nargs=2, \
                      action=ListAction, help='%s')"
                     name
                     (String.concat " " a.Idl.Param.description))
            | _ ->
                Line
                  (sprintf
                     "parser.add_argument('%s', action='store', help='%s'%s)"
                     name
                     (String.concat " " a.Idl.Param.description)
                     ( match a.Idl.Param.typedef.ty with
                     | Basic Int -> ", type=long"
                     | Basic Int64 -> ", type=long"
                     | Basic Int32 -> ", type=int"
                     | Basic Bool -> ", type=lambda x: json.loads(x.lower())"
                     | Basic Float -> ", type=float"
                     | _ -> "" )) )
          inputs
      @ [Line "return vars(parser.parse_args())"] ) ]

let commandline_run _ (BoxedFunction m) =
  let open Printf in
  [ Line ""
  ; Line (sprintf "def %s(self):" m.Method.name)
  ; Block
      ( output_doc m.Method.description
      @ [ Line "use_json = False"
        ; Line "try:"
        ; Block
            [ Line (sprintf "request = self._parse_%s()" m.Method.name)
            ; Line "use_json = 'json' in request and request['json']"
            ; Line
                (sprintf "results = self.dispatcher.%s(request)" m.Method.name)
            ; Line "print json.dumps(results)" ]
        ; Line "except Exception as exn:"
        ; Block
            [ Line "if use_json:"
            ; Block [Line "handle_exception(exn)"]
            ; Line "else:"
            ; Block [Line "traceback.print_exc()"; Line "raise exn"] ] ] ) ]

let commandline_of_interface i =
  let open Printf in
  [ Line ""
  ; Line ""
  ; Line
      (sprintf "class %s_commandline(object):"
         i.Interface.details.Idl.Interface.name)
  ; Block
      ( [ Line
            {|"""Parse command-line arguments and call an implementation."""|}
        ; Line ""
        ; Line "def __init__(self, impl):"
        ; Block
            [ Line "self.impl = impl"
            ; Line
                (sprintf "self.dispatcher = %s_server_dispatcher(self.impl)"
                   i.Interface.details.Idl.Interface.name) ] ]
      @ List.concat (List.map (commandline_parse i) i.Interface.methods)
      @ List.concat (List.map (commandline_run i) i.Interface.methods) ) ]

let of_interfaces ?(helpers= inline_defaults) i =
  let open Printf in
  let dispatch_class i comma =
    Line
      (sprintf {|    "%s": self.%s._dispatch%s|}
         i.Interface.details.Idl.Interface.name
         i.Interface.details.Idl.Interface.name comma)
  in
  let dispatch_dict methods =
    let rec intersperse_commas list =
      match list with
      | [] -> []
      | [x] -> [dispatch_class x ""]
      | x :: y :: tl -> dispatch_class x "," :: intersperse_commas (y :: tl)
    in
    [Line "self._dispatcher_dict = {"] @ intersperse_commas methods @ [Line "}"]
  in
  [ Line
      (sprintf {|"""Bindings generated for interface %s by rpclib"""|}
         i.Interfaces.name)
  ; Line ""
  ; Line "import argparse"
  ; Line "import json"
  ; Line "import logging"
  ; Line "import sys"
  ; Line "import traceback"
  ; Line "" ]
  @ (helpers |> String.split_on_char '\n' |> List.map (fun line -> Line line))
  @ ( try List.map exn_var i.Interfaces.error_decls |> List.flatten with e ->
        Printf.fprintf stderr "Error while generating exceptions of %s"
          i.Interfaces.name ;
        raise e )
  @ List.fold_left
      (fun acc i ->
        acc @ server_of_interface i @ skeleton_of_interface i
        @ test_impl_of_interface i @ commandline_of_interface i )
      [] i.Interfaces.interfaces
  @ [ Line ""
    ; Line ""
    ; Line (sprintf "class %s_server_dispatcher(object):" i.Interfaces.name)
    ; Block
        [ Line {|"""Demux calls to individual interface server_dispatchers"""|}
        ; Line ""
        ; Line
            (sprintf "def __init__(self%s):"
               (String.concat ""
                  (List.map
                     (fun x -> ", " ^ x ^ "=None")
                     (List.map
                        (fun i -> i.Interface.details.Idl.Interface.name)
                        i.Interfaces.interfaces))))
        ; Block
            ( List.map
                (fun i ->
                  Line
                    (sprintf "self.%s = %s"
                       i.Interface.details.Idl.Interface.name
                       i.Interface.details.Idl.Interface.name) )
                i.Interfaces.interfaces
            @ dispatch_dict i.Interfaces.interfaces )
        ; Line ""
        ; Line "def _dispatch(self, method, params):"
        ; Block
            [ Line "try:"
            ; Block
                [ Line
                    {|logging.log("method = %s params = %s", method, repr(params))|}
                ; (* str.split is never empty in python *)
                  Line "class_ = method.split('.')[0]"
                ; Line "if class_ in self._dispatcher_dict:"
                ; Block
                    [ Line
                        (sprintf
                           "return self._dispatcher_dict[class_](method, \
                            params)") ]
                ; Line "raise UnknownMethod(method)" ]
            ; Line "except Rpc_light_failure as rpc_exn:"
            ; Block
                [ Line {|logging.log("caught %s", rpc_exn)|}
                ; Line "traceback.print_exc()"
                ; Line {|logging.log("returning %s", repr(rpc_exn.failure()))|}
                ; Line "return rpc_exn.failure()" ]
            ; Line "except Exception as exn:"
            ; Block
                [ Line {|logging.log("caught %s", exn)|}
                ; Line "traceback.print_exc()"
                ; Line "return InternalError(str(exn)).failure()" ] ] ] ]
  @ test_impl_of_interfaces i @ [Line ""]
