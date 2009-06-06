open Stringmap

module SymbolTable =
struct
	
	(** Implementation of a symbol table. This implementation takes
	advantage of the immutable properties of maps to implement
	nested blocks, sacrificing space for ease of implementation
	
	Usage:
	The initial symbol table is created by calling SymbolTable.initialize
	scopes are entered and exited by creating copies of the symbol table:
	
	let old_symbol_table = current_symbol_table in
	let current_symbol_table = SymbolTable.new_scope
	in process_nested_block
	in let current_symbol_table = old_symbol_table
	
	@author tbenbrahim
	*)
	
	(** This exception indicates a jtemplate error where access was made to an
	undefined variable *)
	exception ReferenceToUndefinedVariable of string
	exception ReferenceToUndefinedMapVariable of string * string
	exception AssignmentToNonMap of string * string
	
	(** internal exception to indicate assignment to something that should be a
	map but is not, rethrown as AssignmentToNonMap of string *)
	exception ENotAMap of string
	(** internal exception to indicate that a compound name had less than 2 elements *)
	exception EUnexpectedCompoundName
	(** internal exception to indicate that part of a compound name was not found *)
	exception ENotFound of string
	
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
			values = symbol_table.values
		}
	
	(** returns the printable name of a variable name
	@param name an Ast.variable_name type
	@return a string specifying the variable name
	*)
	let fullname = function
			Ast.CompoundName(lst) ->
				List.fold_left(fun acc el -> acc^"."^el) "" lst
		| Ast.Name(name) -> name
	
	let lookup sname map =
		try
			StringMap.find sname map
		with
			Not_found -> raise (ENotFound sname)
	
	let rec replace containername namelist container value =
		match namelist with
			lastelem::[] ->
		(* this is the last element, it must be a MapValue *)
				(match container with
						MapValue(map) -> MapValue(StringMap.add lastelem value map)
					| _ -> raise (ENotAMap containername))
		| [] -> raise EUnexpectedCompoundName (* my error, should never happen, could only happen if a coumpound name had fewer than 2 elements *)
		| elem :: tail ->
		(* middle element, it must be a MapValue *)
				(match container with
						MapValue(map) -> MapValue(StringMap.add elem (replace elem tail (lookup elem map) value) map)
					| _ -> raise (ENotAMap containername))
	
	let declare name value symbol_table =
		try(
				match name with
					Ast.Name(varname) -> { values = StringMap.add varname value symbol_table.values	}
				| Ast.CompoundName(lst) ->
						(match lst with
								varname:: tl -> { values = StringMap.add varname (replace varname tl (lookup varname (symbol_table.values)) value) symbol_table.values }
							| _ -> raise EUnexpectedCompoundName)
			)with
			ENotFound ename -> raise (ReferenceToUndefinedMapVariable (ename , fullname name))
		| ENotAMap ename -> raise (AssignmentToNonMap (ename , fullname name))
	
end