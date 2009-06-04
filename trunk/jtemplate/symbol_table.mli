module SymbolTable :
  sig
    module StringMap :
      sig
        type key = String.t
        type 'a t = 'a Map.Make(String).t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val find : key -> 'a t -> 'a
        val remove : key -> 'a t -> 'a t
        val mem : key -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      end
    exception InconsistentSymbolTableStatePopLocal
    exception InconsistentSymbolTableStatePopBlock
    exception ReferenceToUndefinedVariable of string
    type variable_value =
        IntegerValue of int
      | FloatValue of float
      | StringValue of string
      | BooleanValue of bool
      | FunctionValue of Ast.variable_name list * Ast.statement list
      | ArrayValue of variable_value array
      | MapValue of variable_value StringMap.t
    type symbol_table = {
      local_scope_stack : symbol_table list;
      parent_table : symbol_table option;
      values : variable_value StringMap.t;
    }
    val initialize : symbol_table
    val enter_local_scope : symbol_table -> symbol_table
    val exit_local_scope : symbol_table -> symbol_table
    val enter_block_scope : symbol_table -> symbol_table
    val exit_block_scope : symbol_table -> symbol_table
    val initialize_variable :
      symbol_table ->
      Ast.variable_name -> variable_value -> variable_value StringMap.t
    val assign_variable :
      symbol_table ->
      Ast.variable_name -> variable_value -> variable_value StringMap.t
  end
