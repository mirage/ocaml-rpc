open Ppxlib

module Ast_builder = struct
  module type S = sig
    include Ast_builder.S

    val pexp_function_cases : Ast.cases -> expression
  end

  let make loc =
    (module struct
      include (val Ast_builder.make loc)

      let pexp_function_cases = pexp_function
    end : S)
end
