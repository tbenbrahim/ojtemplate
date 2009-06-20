module SymbolTable =
struct
	
	open RuntimeError
	open Ast
	
	(** Implementation of a symbol table.
	
	Usage:
	The initial symbol table is created by calling SymbolTable.initialize
	scopes are entered and exited thusly:
	
	let symbol_table = SymbolTable.push_scope symbol_table
	in process_nested_block symbol_table
	in let symbol_table = SymbolTable.pop_scope symbol_table
	
	function calls get the same scope as the scope the function is declared in (static scoping). Example:
	
	@author tbenbrahim
	*)
	
	(** internal exception to indicate assignment to something that should be a
	map but is not, rethrown as AssignmentToNonMap of string *)
	exception ENotAMap of string
	(** internal exception to indicate that a compound name had less than 2 elements *)
	exception EUnexpectedCompoundName
	(** internal exception to indicate that part of a compound name was not found *)
	exception ENotFound of string
	(** internal exception to indicate trying to exit top level scope *)
	exception ECannotPopTopmostScope
	(** internal exception to indicate illegal reassignment of value of different type *)
	exception ETypeMismatchInAssignment of string * string * string (* name , current_type, new_type *)
	
	exception EUnexpectedName
	
	exception EInvalidArrayIndex of string  (*key, map*)
	
	exception EUnexpected
	
	let rec string_of_symbol_value = function
		| IntegerValue(i) -> string_of_int i
		| FloatValue(f) -> string_of_float f
		| BooleanValue(b) -> string_of_bool b
		| StringValue(s) -> s
		| FunctionValue(args, _) -> "function"^(string_of_args args)
		| LibraryFunction(args, _, _) -> "library call"^(string_of_args args)
		| MapValue(map, MapSubtype) -> "{}"
		| MapValue(map, ArraySubtype _) -> "[]"
		| Void -> "Void"
		| NaN -> "NaN"
	and
	(**
	returns the printable name of a variable name
	@param name an Ast.variable_name type
	@return a string specifying the variable name
	*)
	fullname = function
		| Ast.Name(name) -> name
	
	and string_of_args args =
		match args with
		| [] -> "()"
		| _ -> "(" ^ (let x = (List.fold_left (fun acc el -> acc^","^(fullname el)) "" args) in
					String.sub x 1 ((String.length x) - 1)) ^")"
	
	let string_of_symbol_type = function
		| IntegerValue(_) -> "integer"
		| FloatValue(_) -> "float"
		| BooleanValue(_) -> "boolean"
		| StringValue(_) -> "string"
		| FunctionValue(_, _) -> "function"
		| LibraryFunction(_, _, _) -> "library call"
		| MapValue(_, MapSubtype) -> "map"
		| MapValue(_, ArraySubtype _) -> "array"
		| Void -> "void"
		| NaN -> "NaN"
	
	type valuetype =
		| IntegerType
		| FloatType
		| BooleanType
		| StringType
		| FunctionType
		| LibraryCallType
		| MapType
		| ArrayType
		| VoidType
		| NaNType
	
	let value_type = function
		| IntegerValue(_) -> IntegerType
		| FloatValue(_) -> FloatType
		| BooleanValue(_) -> BooleanType
		| StringValue(_) -> StringType
		| FunctionValue(_, _) -> FunctionType
		| LibraryFunction(_, _, _) -> LibraryCallType
		| MapValue(_, MapSubtype) -> MapType
		| MapValue(_, ArraySubtype _) -> ArrayType
		| Void -> VoidType
		| NaN -> NaNType
	
	let rec print_symbol_map map prefix incl_lib =
		let _ = (Hashtbl.iter (fun key var ->
								(match var with
									| LibraryFunction(_, _, _) -> (if incl_lib then print_string (prefix^key^"="^(string_of_symbol_value var)^"\n") else ())
									| _ ->	print_string (prefix^key^"="^(string_of_symbol_value var)^"\n"));
								(match var with
									| MapValue(map, _) -> let _ = print_symbol_map map (prefix^key^".") incl_lib in ()
									| _ -> ()
								)) map) in ()
	
	let rec print_symbol_table symbol_table incl_lib =
		print_string "SYMBOL TABLE:\n";
		print_symbol_map symbol_table.values "" incl_lib;
		match symbol_table.parent_table with
		| None -> ()
		| Some table -> (print_string "PARENT "; print_symbol_table table incl_lib)
	
	(** creates a new empty symbol table. Called only at the start of a program,
	creating other symbol tables is accomplished by entering a new scope.
	@return an empty symbol table.
	*)
	let initialize () =
		{ values = Hashtbl.create 10 ; parent_table = None ;
			env = { parse_callback = (fun s -> Ast.Noop);
				loaded_imports = []; current_stmt = ("", 0);
				stack_trace =[]; } }
	
	let initialize_environment environment =
		{ values = Hashtbl.create 10 ; parent_table = None ; env = environment }
	
	let valid_array_index ind =
		(ind ="length") ||
		(try let _ = int_of_string ind in true with Failure _ -> false)
	
	(** creates a new symbol table for a nested scope. *)
	let push_scope symbol_table =
		{ values = Hashtbl.create 10 ; parent_table = Some symbol_table; env = symbol_table.env	}
	
	let pop_scope symbol_table =
		match symbol_table.parent_table with
		| Some table -> table
		| None -> raise ECannotPopTopmostScope
	
	let lookup hashtbl sname =
		try
			Hashtbl.find hashtbl sname
		with Not_found ->
				raise (ENotFound sname)
	
	let rec replace_map_value assignment containername namelist container value =
		match namelist with
			lastelem::[] ->
		(* this is the last element, it must be a MapValue *)
				(match container with
						MapValue(hashtbl, subtype) ->
							if subtype = ArraySubtype && not(valid_array_index lastelem)then
								raise (EInvalidArrayIndex(lastelem))
							else
								();
							if assignment then
								try
									let old_value = (Hashtbl.find hashtbl lastelem ) in
									let old_value_type = value_type old_value in
									let new_value_type = value_type value in
									if new_value_type = old_value_type then
										Hashtbl.replace hashtbl lastelem value
									else
										raise (ETypeMismatchInAssignment(lastelem, string_of_symbol_type old_value,
													string_of_symbol_type value))
								with
								| Not_found -> raise (ENotFound lastelem)
							else
								Hashtbl.replace hashtbl lastelem value;
					| _ -> raise (ENotAMap containername))
		| [] -> raise EUnexpectedCompoundName (* my error, should never happen, could only happen if a coumpound name had fewer than 2 elements *)
		| elem :: tail ->
		(* middle element, it must be a MapValue *)
				(match container with
						MapValue(hashtbl, _) -> replace_map_value assignment elem tail (lookup hashtbl elem) value
					| _ -> raise (ENotAMap containername))
	
	let rec resolve_replace assignment name symbol_table value =
		match symbol_table with
		| None -> raise (ReferenceToUndefinedVariable (fullname name))
		| Some table -> (
					match name with
					| Ast.Name(varname) -> (
								if assignment then( (* it is an assignment we must find the original scope of the declaration *)
									try (* it is in the current scope *)
										let old_value_type = string_of_symbol_type (Hashtbl.find table.values varname ) in
										let new_value_type = string_of_symbol_type value in
										if new_value_type = old_value_type then
											Hashtbl.replace table.values varname value
										else
											raise ( TypeMismatchInAssignment(varname, old_value_type, new_value_type))
									with
										Not_found -> resolve_replace assignment name table.parent_table value
								)
								else
									Hashtbl.replace table.values varname value (* it is a declaration, always use current scope *)
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
		| MapValue(map, subtype) ->
				(match name_list with
					| el::[] ->
							if subtype = ArraySubtype && not(valid_array_index el) then
								raise (EInvalidArrayIndex(el))
							else
								();
							(try Hashtbl.find map el with Not_found -> raise (ENotFound el)) (* last element, return value *)
					| el:: tl -> get_map_value el
								(try Hashtbl.find map el with Not_found -> raise (ENotFound el)) tl (* intermediate element, recurse *)
					| _ -> raise EUnexpectedCompoundName (* could only happen if compound name had less than 2 elements *)
				)
		| _ -> raise (ENotAMap name)
	
	let rec resolve name symbol_table =
		match symbol_table with
		| None -> raise (ReferenceToUndefinedVariable (fullname name))
		| Some table -> (
					try
						match name with
						| Ast.Name(varname) ->
								(try
									Hashtbl.find table.values varname
								with
									Not_found -> resolve name table.parent_table)
					with
						Not_found -> raise (ReferenceToUndefinedVariable (fullname name))
					| ENotFound ename -> raise (ReferenceToUndefinedMapVariable (ename , fullname name))
					| ENotAMap ename -> raise (NotAMap (ename, fullname name))
					| EInvalidArrayIndex comp -> raise (InvalidArrayIndex (comp, fullname name))
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
	let get_value name symbol_table =
		match name with (* handle of exception for dumpSymbolTable since we need the current table *)
		| _ -> resolve name (Some symbol_table)
	
	let is_defined name symbol_table =
		try
			let _ = get_value name symbol_table in true
		with
			_ -> false
	
	let is_undefined name symbol_table = not (is_defined name symbol_table)
	
	let rec add_vals_to_arr arr values cnt =
		match values with
		| [] -> cnt
		| el:: rest_vals -> Hashtbl.add arr (string_of_int cnt) el; add_vals_to_arr arr rest_vals (cnt + 1)
	
	let rec put_args_in_scope args vals scope =
		match args with
		| [] -> if vals != [] then raise RuntimeError.MismatchedCallArgs else ()
		| Name(last)::[] -> (* only place where vararg is allowed *)
				if is_vararg last then
					let h = (Hashtbl.create 10) in
					Hashtbl.add h "length" (IntegerValue(add_vals_to_arr h vals 0));
					declare (Name(vararg_formalname last)) (MapValue(h, ArraySubtype)) scope
				else(
					if vals =[] then raise RuntimeError.MismatchedCallArgs (* ran out of values *)
					else declare (Name last) (List.hd vals) scope; put_args_in_scope [] (List.tl vals) scope
				)
		| Name(el):: rest_args ->
				if el.[0]='[' then
					raise RuntimeError.VarArgsMustbeLast
				else(
					if vals =[] then raise RuntimeError.MismatchedCallArgs (* ran out of values *)
					else(
						declare (Name el) (List.hd vals) scope;
						put_args_in_scope rest_args (List.tl vals) scope
					))
	
	let new_function_call_scope scope arglist vallist =
		let scope =	push_scope scope in
		put_args_in_scope arglist vallist scope; scope
	
	let list_of_array arr =
		match arr with
		| MapValue(h, ArraySubtype) -> (
					match Hashtbl.find h "length" with
					| IntegerValue(0) ->[]
					| IntegerValue(len) ->
							let rec loop lst ind =
								(let lst = (Hashtbl.find h (string_of_int ind)):: lst in
									if ind = 0 then lst else (loop lst (ind - 1)))
							in loop [] (len - 1)
					| _ -> raise EUnexpected
				)
		| _ -> raise EUnexpected
	
end