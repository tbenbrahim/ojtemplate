val register_library :
  (string * Ast.variable_name list * (Ast.symbol_table -> unit)) list ->
  Ast.symbol_table -> unit
