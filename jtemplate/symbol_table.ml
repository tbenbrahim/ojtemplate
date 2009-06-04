module SymbolTable =
struct
	
	(* a symbol table. may not be the most efficient, but should get me      *)
	(* started                                                               *)
	
	(* may not need the exit functions, double check after implementing*)
	
	(****

    TODO replace Map with HashTbl, Map is immutable!
	
	****)
	
	module StringMap = Map.Make (String)
	
	exception InconsistentSymbolTableStatePopLocal
	exception InconsistentSymbolTableStatePopBlock
	exception ReferenceToUndefinedVariable of string
	
	(* TODO look at syntactic sugar for map and array to avoid having to     *)
	(* convert other ast values                                              *)
	type variable_value =
			IntegerValue of int
		| FloatValue of float
		| StringValue of string
		| BooleanValue of bool
		| FunctionValue of Ast.variable_name list * Ast.statement list
		| ArrayValue of variable_value array
		| MapValue of variable_value StringMap.t
	
	type symbol_table ={
		local_scope_stack: symbol_table list; (* stack of local scopes *)
		parent_table: symbol_table option; (* reference to block scope parent *)
		values: variable_value StringMap.t; (* variable values for this scope *)
	}
	
	let initialize =
		{ local_scope_stack =[];
			parent_table = None;
			values = StringMap.empty }
	
	let enter_local_scope symbol_table =
		(* add old symbol table to local scope stack *)
		{ local_scope_stack = symbol_table:: symbol_table.local_scope_stack;
			(* parent block scope stays the same *)
			parent_table = symbol_table.parent_table;
			(* new empty symbol table for the local scope *)
			values = StringMap.empty }
	
	let exit_local_scope symbol_table =
		(* pop the old local scope *)
		match symbol_table.local_scope_stack with
			[]-> raise InconsistentSymbolTableStatePopLocal
		| old_scope:: tl -> old_scope
	
	let enter_block_scope symbol_table =
		(* no change to local scope stack *)
		{ local_scope_stack = symbol_table.local_scope_stack;
			(* point parent to old table *)
			parent_table = Some symbol_table;
			(* new empty symbol table for the local scope *)
			values = StringMap.empty }
	
	let exit_block_scope symbol_table =
		match symbol_table.parent_table with
			None -> raise InconsistentSymbolTableStatePopBlock
		| Some table -> table
	
	let fullname = function
			Ast.CompoundName(lst) ->
				List.fold_left(fun acc el -> acc^"."^el) "" lst
		| Ast.Name(name) -> name
	
	let rec initialize_map_value map name value varname =
		match name with
			hd::[] -> StringMap.add hd value map
		| [] -> raise (ReferenceToUndefinedVariable varname)
		| hd:: tl -> try let variable = StringMap.find hd map in match variable with
						MapValue(newmap) -> initialize_map_value newmap tl value varname
					| _ -> raise (ReferenceToUndefinedVariable varname)
				with Not_found -> raise (ReferenceToUndefinedVariable varname)
	
	let initialize_variable symbol_table name value =
		match name with
			Ast.Name(varname) -> StringMap.add varname value symbol_table.values
		| Ast.CompoundName(varname) -> initialize_map_value symbol_table.values varname value (fullname name)
	
	let rec assign_map_value map name value varname =
		match name with
			hd::[] -> let _=StringMap.find hd map in StringMap.add hd value map
		| [] -> raise (ReferenceToUndefinedVariable varname)
		| hd:: tl -> let variable = StringMap.find hd map in match variable with
						MapValue(newmap) -> initialize_map_value newmap tl value varname
					| _ -> raise (ReferenceToUndefinedVariable varname)
		
	
	let assign_variable symbol_table name value =
		try
			match name with
				Ast.Name(varname) ->
					let _ = StringMap.find varname symbol_table.values in StringMap.add varname value symbol_table.values
			| Ast.CompoundName(varname) -> assign_map_value symbol_table.values varname value (fullname name)
		with Not_found -> raise (ReferenceToUndefinedVariable (fullname name))
end