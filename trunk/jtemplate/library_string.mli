module StringLibrary :
  sig
    val exported :
      (string list * string list * (Ast.symbol_table -> 'a)) list
  end
