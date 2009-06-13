module BuiltinLibrary :
  sig
    val exported :
      (string list * string list * (Ast.symbol_table -> unit)) list
  end
