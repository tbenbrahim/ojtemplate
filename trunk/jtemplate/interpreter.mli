module Interpreter :
  sig
    exception EIncompatibleTypes of string * string
    exception EInvalidCast of string * string
    exception EInvalidOperation of Ast.operator * string
    exception EInvalidComparaison of Ast.comparator * string
    exception EMismatchedTypeInCompare of string * string
    exception CFReturn of Ast.variable_value
    exception CFBreak
    exception CFContinue
    type cast_type = IntegerCast | FloatCast | StringCast
    val casting_type : Ast.variable_value -> Ast.variable_value -> cast_type
    val cast_to_string : Ast.variable_value -> string
    val cast_to_integer : Ast.variable_value -> int
    val cast_to_float : Ast.variable_value -> float
    val make_map :
      (string * Ast.expression) list ->
      Ast.symbol_table -> (string, Ast.variable_value) Hashtbl.t
    val make_array :
      Ast.expression list ->
      Ast.symbol_table -> (string, Ast.variable_value) Hashtbl.t
    val compare_same_type :
      Ast.variable_value ->
      Ast.comparator -> Ast.variable_value -> Ast.variable_value
    val restricted_compare :
      Ast.variable_value ->
      Ast.comparator -> Ast.variable_value -> Ast.variable_value
    val evaluate_expression :
      Ast.expression -> Ast.symbol_table -> Ast.variable_value
    val interpret_statement : Ast.statement -> Ast.symbol_table -> unit
    val interpret_statements : Ast.statement list -> Ast.symbol_table -> unit
  end
