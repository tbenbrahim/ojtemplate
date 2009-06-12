module BuiltinLibrary :
  sig
    val exported :
      (string * Ast.variable_name list * (Ast.symbol_table -> unit)) list
  end
