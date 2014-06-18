(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Camlp4
open PreCast
open Ast

module Param = struct
  let rpc = Gram.Entry.mk "rpc"
      EXTEND Gram
      GLOBAL: rpc;
    pair: [[ x=STRING; "->"; y=STRING -> (x, y ) ]];
    rpc:  [[ l = LIST0 [ pair ] SEP "," -> l]];
    END
end

let _ =
  (* type generator *)
  Pa_type_conv.add_generator_with_arg "rpc" Param.rpc P4_rpc.RpcNormal.gen;

  (* module generator *)
  Pa_module_conv.add_generator "rpc" (fun mt -> P4_rpc.RpcNormal.gen_module mt)
