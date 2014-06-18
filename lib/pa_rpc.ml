(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2006-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
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
