module StringLibrary :
  sig
    val exported :
      (string * Ast.variable_name list * (Ast.symbol_table -> 'a)) list
  end
