val register_library :
  (string list * string list * (Ast.symbol_table -> unit)) list ->
  Ast.symbol_table -> unit
