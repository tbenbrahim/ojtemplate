module AstInfo :
  sig 
    val statement_description : Ast.statement -> string
    val var_description : Ast.variable_name -> string
    val expr_description : Ast.expression -> string
    val print_ast : Ast.statement -> unit
  end
