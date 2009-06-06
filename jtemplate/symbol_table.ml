open Stringmap

module SymbolTable =
struct
	
	(** Implementation of a symbol table. This implementation takes
      advantage of the immutable properties of maps to implement
			nested blocks, sacrificing space for ease of implementation
			
			Usage:
			The initial symbol table is created by calling SymbolTable.initialize
			scopes are entered and exited by creating copies of the symbol table:
			
			let old_symbol_table=current_symbol_table in
			let current_symbol_table=SymbolTable.new_scope
			in process_nested_block 
			in let current_symbol_table=old_symbol_table
			 
	@author tbenbrahim
	*)
	
	(** This exception indicates a jtemplate error where access was made to an
	undefined variable *)
	exception ReferenceToUndefinedVariable of string
	
	(** type definition for the four scalar values (int, float, string, bool)
	for a function definition, and for maps. Arrays are syntactic sugar
	for maps, for example foo.bar[1] is equivalent to foo.bar.1
	Map values are implemented as a recursive structure of Maps. To
	resolved foo.bar.1, foo is looked up in the symbol table. It must be
	a MapValue. In the MapValue hashtable, bar is looked up. It must be also
	be a MapValue. Finally, the last component is looked up in the hashtable
	and it can be of any type. *)
	type variable_value =
			IntegerValue of int
		| FloatValue of float
		| StringValue of string
		| BooleanValue of bool
		| FunctionValue of Ast.variable_name list * Ast.statement list
		| MapValue of variable_value StringMap.t
	
	(** Definition for a symbol table. *)
	type symbol_table ={
		values: variable_value StringMap.t (* variable values for this scope *)
	}
	
	(** creates a new empty symbol table. Called only at the start of a program,
	creating other symbol tables is accomplished by entering a new scope.
	@return an empty symbol table.
	*)
	let initialize =
			{ values = StringMap.empty } 
	
	(** creates a new symbol table for a nested scope. *)
	let new_scope symbol_table =
		{
			values=symbol_table.values
		}
	
	let fullname = function
			Ast.CompoundName(lst) ->
				List.fold_left(fun acc el -> acc^"."^el) "" lst
		| Ast.Name(name) -> name
	
end