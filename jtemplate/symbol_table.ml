module SymbolTable =
struct
	
	open Stringmap
	
	(** Implementation of a symbol table. This implementation takes
	advantage of the immutable properties of maps to implement
	nested blocks, sacrificing space and speed for ease of implementation
	
	Usage:
	The initial symbol table is created by calling SymbolTable.initialize
	scopes are entered and exited by creating copies of the symbol table:
	
	let symbol_table = SymbolTable.push_scope symbol_table
	in process_nested_block
	in let symbol_table = SymbolTable.pop_scope symbol_table
	
	function calls get the same scope as the scope they are declare in. Example:
	[{
	var b = 1;
	var x = function() {
	..var a = 1;
	..var y = function() {
	..};
	..{
	....var d = 2;
	....var w = function() {
	....};
	..}
	..w();// a, b, c, d, w, x, y, z are in scope
	..y();// a, b, x, y, z are in scope
	..z();// b, x, z are in scope
	};
	var z = function() {
	};
	x();// b, x, z are in scope
	]}
	@author tbenbrahim
	*)
	
	(** This exception indicates a jtemplate error where access was made to an
	undefined variable *)
	exception ReferenceToUndefinedVariable of string
	exception ReferenceToUndefinedMapVariable of string * string
	exception NotAMap of string * string
	
	(** internal exception to indicate assignment to something that should be a
	map but is not, rethrown as AssignmentToNonMap of string *)
	exception ENotAMap of string
	(** internal exception to indicate that a compound name had less than 2 elements *)
	exception EUnexpectedCompoundName
	(** internal exception to indicate that part of a compound name was not found *)
	exception ENotFound of string
	(** internal exception to indicate trying to exit top level scope *)
	exception ECannotPopTopmostScope
	
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
	
	(**
	returns the printable name of a variable name
	@param name an Ast.variable_name type
	@return a string specifying the variable name
	*)
	let fullname = function
			Ast.CompoundName(lst) ->
				(match lst with
					| el:: lst -> List.fold_left(fun acc el -> acc^"."^el) el lst
					| [] -> "")
		| Ast.Name(name) -> name
	
	let string_of_args args =
		match args with
		| [] -> "()"
		| el:: tl -> "(" ^ (List.fold_left (fun acc el -> acc^","^(fullname el)) (fullname el) tl) ^")"
	
	let string_of_symbol_value = function
		| IntegerValue(i) -> string_of_int i
		| FloatValue(f) -> string_of_float f
		| BooleanValue(b) -> string_of_bool b
		| StringValue(s) -> "'" ^ s ^ "'"
		| FunctionValue(args, _) -> "function"^(string_of_args args)
		| MapValue(map) -> "{}"
	
	(** Definition for a symbol table. *)
	type symbol_table ={
		values: variable_value StringMap.t; (* variable values for this scope *)
		parent_table: symbol_table option
	}
	
	let rec print_symbol_map map prefix =
        let _ = (StringMap.mapi (fun key var -> print_string (prefix^key^"="^(string_of_symbol_value var)^"\n");
                                (match var with
                                    | MapValue(map) -> let _ = print_symbol_map map (prefix^key^".") in ()
                                    | _ -> ()
                                )) map) in ()
    
    let rec print_symbol_table symbol_table =
        print_string "SYMBOL TABLE:\n";
        print_symbol_map symbol_table.values "";
        match symbol_table.parent_table with
        | None -> ()
        | Some table -> (print_string "PARENT "; print_symbol_table table)
	
	(** creates a new empty symbol table. Called only at the start of a program,
	creating other symbol tables is accomplished by entering a new scope.
	@return an empty symbol table.
	*)
	let initialize =
		{ values = StringMap.empty; parent_table = None }
	
	(** creates a new symbol table for a nested scope. *)
	let push_scope symbol_table =
		{
			values = StringMap.empty; parent_table = Some symbol_table
		}
	
	let pop_scope symbol_table =
		match symbol_table.parent_table with
		| Some table -> table
		| None -> raise ECannotPopTopmostScope
	
	let lookup sname map =
		try
			StringMap.find sname map
		with
			Not_found -> raise (ENotFound sname)
	
	let rec replace_map_value assignment containername namelist container value =
		match namelist with
			lastelem::[] ->
		(* this is the last element, it must be a MapValue *)
				(match container with
						MapValue(map) ->
							if assignment then
								try
									let _ = StringMap.find lastelem map in MapValue(StringMap.add lastelem value map)
								with
								| Not_found -> raise (ENotFound lastelem)
							else
								MapValue(StringMap.add lastelem value map)
					| _ -> raise (ENotAMap containername))
		| [] -> raise EUnexpectedCompoundName (* my error, should never happen, could only happen if a coumpound name had fewer than 2 elements *)
		| elem :: tail ->
		(* middle element, it must be a MapValue *)
				(match container with
						MapValue(map) -> MapValue(StringMap.add elem (replace_map_value assignment elem tail (lookup elem map) value) map)
					| _ -> raise (ENotAMap containername))
	
	let rec resolve_replace assignment name symbol_table value =
		match symbol_table with
		| None -> raise (ReferenceToUndefinedVariable (fullname name))
		| Some table -> (
					match name with
					| Ast.Name(varname) -> (
								if assignment then( (* it is an assignment we must find the declaration *)
									try (* it is in the current scope *)
										let _ = StringMap.find varname table.values in 
										{ values = StringMap.add varname value table.values; parent_table = table.parent_table }
									with
										Not_found ->  (* it may be in a parent scope *)
											{ values = table.values; parent_table = Some (resolve_replace assignment name table.parent_table value) }
								)
								else (* it is a declaration, always use current scope *)
								{ values = StringMap.add varname value table.values; parent_table = table.parent_table }
							)
					| Ast.CompoundName(lst) -> (
								(match lst with
									| [] -> raise EUnexpectedCompoundName
									| el:: lst ->
											if assignment then ( (*it is an assignment, we must first find the declaration of the first element *)
												try (* it is in the current scope *)
													let v = StringMap.find el table.values in
													match v with
													| MapValue(_) -> { values = StringMap.add el (replace_map_value assignment el lst v value) table.values ; parent_table = table.parent_table }
													| _ -> raise (NotAMap (el, fullname name)) (* found something with same name, but wrong type *)
												with
												| Not_found -> { values = table.values; parent_table = Some (resolve_replace assignment name table.parent_table value) }
												| ENotAMap(comp) -> raise (NotAMap(comp, fullname name))
												| ENotFound(comp) -> raise (ReferenceToUndefinedMapVariable(comp, fullname name))
											)
											else( (*it is a declaration, always use current scope *)
												try
													{ values = StringMap.add el ((replace_map_value assignment el lst (StringMap.find el table.values) value)) table.values; parent_table = table.parent_table }
												with
												| Not_found -> raise (ReferenceToUndefinedMapVariable(el, fullname name))
												| ENotAMap(comp) -> raise (NotAMap(comp, fullname name))
												| ENotFound(comp) -> raise (ReferenceToUndefinedMapVariable(comp, fullname name))
											)
								)
							)
				)
	
	(**
	module private function to recusively resolve a compound name list to a value in a map
	@param name the name of the segment of the compound name being processed
	@param value value the value of the segment of the compound name being processed
	@param name_list the part of compound name list following name
	@return the value of the last segment of the compound name list
	@raise EUnexpectedCompoundName if the name_list is empty (should never happen)
	@raise ENotFound if any intermediate name is not defined
	@raise ENotAMap if any intermediate name is not a MapValue
	*)
	let rec get_map_value name value name_list =
		match value with
		| MapValue(map) ->
				(match name_list with
					| el::[] -> (try StringMap.find el map with Not_found -> raise (ENotFound el)) (* last element, return value *)
					| el:: tl -> get_map_value el
								(try StringMap.find el map with Not_found -> raise (ENotFound el)) tl (* intermediate element, recurse *)
					| _ -> raise EUnexpectedCompoundName (* could only happen if compound name had less than 2 elements *)
				)
		| _ -> raise (ENotAMap name)
	
	let rec resolve name symbol_table =
		match symbol_table with
		| None ->
				(match name with
					| Ast.CompoundName(el:: _) -> raise (ReferenceToUndefinedMapVariable(el, fullname name))
					| _ -> raise (ReferenceToUndefinedVariable (fullname name))
				)
		| Some table -> (
					try
						match name with
							Ast.Name(varname) ->
								(try
									StringMap.find varname table.values
								with
									Not_found -> resolve name table.parent_table)
						| Ast.CompoundName(lst) ->
								(match lst with
									| el:: tl -> (try
												get_map_value el (StringMap.find el table.values) tl
											with
												Not_found -> resolve name table.parent_table)
									| _ -> raise EUnexpectedCompoundName
								)
					with
						Not_found -> raise (ReferenceToUndefinedVariable (fullname name))
					| ENotFound ename -> raise (ReferenceToUndefinedMapVariable (ename , fullname name))
					| ENotAMap ename -> raise (NotAMap (ename, fullname name))
				)
	
	let declare name value symbol_table = resolve_replace false name (Some symbol_table) value
	
	let assign name value symbol_table = resolve_replace true name (Some symbol_table) value
	
	
	
	(**
	retrieve a name's value from a symbol table
	@param name an Ast.Name or Ast.CompoundName for which to retrieve the value
	@param symbol_table the symbol from which to retrieve the value
	@return a SymbolTable.variable_value
	@raise ReferenceToUndefinedVariable if the variable is not defined
	@raise ReferenceToUndefinedMapVariable when part of map variable name is not defined
	@raise NotAMap when part of the map variable name (except the last) is not a MapValue
	@raise EUnexpectedCompoundName if a coumpound name has fewer than 2 elements (should never happen)
	*)
	let get_value name symbol_table = resolve name (Some symbol_table)
	
end