open Ast

(**
Module environment defines the runtime and analysis environments
@author Tony BenBrahim
*)

module StringMap = Map.Make(String)

(**
@param field1 value
@param field2 index into scope
@param field3 unique id
*)
type var_info = (runtime_variable_value * int * int)

(**
represents variables map in a global or local scope, and reference to parent scope
*)
type rec_varmap ={
	variable_map: var_info StringMap.t;
	parent: rec_varmap option;
}

(**
Properties of variable locations
*)
type var_prop ={
	written_after_declared: bool;
	read_after_declared: bool;
	inlineable: bool;
	tail_callble: bool;
}

(**
The analysis environment
*)
type analysis_env ={
	globals: rec_varmap;
	num_globals: int;
	locals: rec_varmap list; (* one item for each stackframe depth, head is current *)
	num_locals: int list;
	sdepth: int;
	max_depth: int;
	errors: string list;
	warnings: string list;
	unique_id: int;
	names: string list;
	varprops: (int, var_prop) Hashtbl.t;
	imported: string list;
}

(**
returns a newly initialized analysis environment
@return analysis_env
*)
let new_analysis_environment () =
	{
		globals ={ variable_map = StringMap.empty; parent = None };
		num_globals = 0;
		locals =[];
		num_locals = [];
		sdepth = 0;
		max_depth = 0;
		errors =[];
		warnings =[];
		unique_id = 0;
		varprops = Hashtbl.create 10;
		names =[];
		imported =[];
	}

(**
declare a variable if it does not exist
or create a new entry and return new index
@param name name of variable to declare
@param env analysis environment
@return the modified environment
*)
let declare_variable_and_value env name value =
	let find_or_declare varmaps nextind uid =
		try let _ = StringMap.find name varmaps in (varmaps, 0)
		with Not_found -> (StringMap.add name (value, nextind, uid) varmaps, 1)
	in
	match env.locals with
	| [] ->
			let ( map, num_added) = find_or_declare env.globals.variable_map env.num_globals env.unique_id
			in
			{ globals ={ variable_map = map; parent = env.globals.parent };
				num_globals = env.num_globals + num_added;
				locals = env.locals;
				num_locals = env.num_locals;
				sdepth = env.sdepth;
				max_depth = env.max_depth;
				errors = env.errors;
				warnings = env.warnings;
				unique_id = env.unique_id + num_added;
				names = if num_added = 0 then env.names else name:: env.names;
				varprops = env.varprops;
				imported = env.imported;
			}
	| _ ->
			let (map, num_added) = find_or_declare (List.hd env.locals).variable_map (List.hd env.num_locals) env.unique_id
			in
			{ globals = env.globals;
				num_globals = env.num_globals;
				locals ={ variable_map = map; parent = (List.hd env.locals).parent }:: List.tl env.locals;
				num_locals = ((List.hd env.num_locals) + num_added):: List.tl env.num_locals;
				sdepth = env.sdepth;
				max_depth = env.max_depth;
				errors = env.errors;
				warnings = env.warnings;
				unique_id = env.unique_id + num_added;
				names = if num_added = 0 then env.names else name:: env.names;
				varprops = env.varprops;
				imported = env.imported;
			}

(**
declare a variable, return index of existing variable in current scope if found
or create a new entry and return new index
@param name name of variable to declare
@param env analysis environment
@return the modified environment
*)
let declare_variable name env =
	declare_variable_and_value env name RUndefined

exception Variable_not_found of string

(**
Find variable in analysis scope
@param name the variable name
@param env the analysis environment
@return a tuple with the value and location
@throws Vaariable_not_found when the variable is not found
*)
let resolve_variable name env =
	let rec find scopes =
		try
			let (value, ind, uid) = StringMap.find name scopes.variable_map
			in (value, uid, ind)
		with Not_found ->
				(match scopes.parent with
					| Some parent -> find parent
					| None -> raise Not_found)
	in
	let rec find_in_stackframes = function
		| [] -> raise Not_found
		| scope:: tl ->
				try
					let (value, uid, ind) = find scope
					in (value, LocalVar(uid, List.length tl, ind))
				with
				| Not_found -> find_in_stackframes tl
	in
	try
		match env.locals with
		| [] -> let (value, uid, ind) = find env.globals in (value, GlobalVar(uid, ind))
		| _ -> (try
					find_in_stackframes env.locals
				with Not_found ->
						let (value, uid, ind) = find env.globals in (value, GlobalVar(uid, ind)))
	with Not_found -> raise (Variable_not_found name)
(**
Setups a new scope within the same global or local scope
@param env analysis environment
@return a new analysis environment setup for the new scope
*)
let new_analysis_scope env =
	match env.locals with
	| [] ->{
				globals = { variable_map = StringMap.empty; parent = Some env.globals };
				num_globals = env.num_globals;
				locals =[];
				num_locals = env.num_locals;
				sdepth = env.sdepth;
				max_depth = env.max_depth;
				errors = env.errors;
				warnings = env.warnings;
				unique_id = env.unique_id;
				names = env.names;
				varprops = env.varprops;
				imported = env.imported;
			}
	| hd:: tl -> {
				globals = env.globals;
				num_globals = env.num_globals;
				locals ={ variable_map = StringMap.empty; parent = Some hd }:: tl;
				num_locals = env.num_locals; (* on new scope, same number of locals *)
				sdepth = env.sdepth;
				max_depth = env.max_depth;
				errors = env.errors;
				warnings = env.warnings;
				unique_id = env.unique_id;
				names = env.names;
				varprops = env.varprops;
				imported = env.imported;
			}
(**
Pops the analysis scope
@param env analysis environment
@param old old analysis environment
@return a new environment with the last scope popped
*)
let pop_scope env =
	match env.locals with
	| [] ->
			(match env.globals.parent with
				| Some old_globals -> {
							globals = old_globals;
							num_globals = env.num_globals;
							locals = env.locals;
							num_locals = env.num_locals;
							sdepth = env.sdepth;
							max_depth = env.max_depth;
							errors = env.errors;
							warnings = env.warnings;
							unique_id = env.unique_id;
							names = env.names;
							varprops = env.varprops;
							imported = env.imported;
						}
				| None -> raise (RuntimeError.InternalError "popping a top level scope"))
	| local:: tl ->
			match local.parent with
			| Some old_parent -> {
						globals = env.globals;
						num_globals = env.num_globals;
						locals = old_parent:: tl;
						num_locals = env.num_locals; (* preserve, we are still in the same stack frame *)
						sdepth = env.sdepth;
						max_depth = env.max_depth;
						errors = env.errors;
						warnings = env.warnings;
						unique_id = env.unique_id;
						names = env.names;
						varprops = env.varprops;
						imported = env.imported;
					}
			| None -> {
						globals = env.globals;
						num_globals = env.num_globals;
						locals = tl;
						num_locals = List.tl env.num_locals; (* exiting stack frame, restore old number of locals *)
						sdepth = env.sdepth - 1;
						max_depth = env.max_depth;
						errors = env.errors;
						warnings = env.warnings;
						unique_id = env.unique_id;
						names = env.names;
						varprops = env.varprops;
						imported = env.imported;
					}

(**
Create a new stackframe
@param env analysis environment
@return a new analysis environment with a new stackframe
*)
let new_analysis_stackframe env =
	{
		globals = env.globals;
		num_globals = env.num_globals;
		locals ={ variable_map = StringMap.empty; parent = None }:: env.locals;
		num_locals = 0:: env.num_locals; (* push new stackframe number of locals *)
		sdepth = env.sdepth + 1;
		max_depth = if env.sdepth + 1 > env.max_depth then env.sdepth + 1 else env.max_depth;
		errors = env.errors;
		warnings = env.warnings;
		unique_id = env.unique_id;
		names = env.names;
		varprops = env.varprops;
		imported = env.imported;
	}

(**
Returns the depth of the current stack frame
@param env analysis environment
@return the depth of the current stack frame, 0 indexed
*)
let get_depth env =
	(List.length env.locals) - 1

(**
Add an error to the analysis environemnt
@param env the analysis environment
@param codeloc a filename, line number tuple
@param message the error message
@return an analysis environment with the error added
*)
let add_error env codeloc message =
	let (filename, line_number) = codeloc
	in	{
		globals = env.globals;
		num_globals = env.num_globals;
		locals = env.locals;
		num_locals = env.num_locals;
		errors = ("At line "^(string_of_int line_number)^" in "^filename^": "^message):: env.errors;
		sdepth = env.sdepth;
		max_depth = env.max_depth;
		warnings = env.warnings;
		unique_id = env.unique_id;
		names = env.names;
		varprops = env.varprops;
		imported = env.imported;
	}

(**
Add a warning to the analysis environemnt
@param env the analysis environment
@param codeloc a filename, line number tuple
@param message the warning message
@return an analysis environment with the warning added
*)
let add_warning env codeloc message =
	let (filename, line_number) = codeloc
	in	{
		globals = env.globals;
		num_globals = env.num_globals;
		locals = env.locals;
		num_locals = env.num_locals;
		sdepth = env.sdepth;
		max_depth = env.max_depth;
		errors = env.errors;
		warnings = ("At line "^(string_of_int line_number)^" in "^filename^": "^message):: env.warnings;
		unique_id = env.unique_id;
		names = env.names;
		varprops = env.varprops;
		imported = env.imported;
	}

(**
Returns true if there are errors in the environment
@param env the analysis environment
@return true if there are errors, false otherwise
*)
let has_errors env =
	env.errors!=[]

(**
adds an import to the list of imports
@param env analysis environment
@param filename the filename
@return the modified environment
*)
let add_import env filename =
	{
		globals = env.globals;
		num_globals = env.num_globals;
		locals = env.locals;
		num_locals = env.num_locals;
		sdepth = env.sdepth;
		max_depth = env.max_depth;
		errors = env.errors;
		warnings = env.warnings;
		unique_id = env.unique_id;
		names = env.names;
		varprops = env.varprops;
		imported = filename:: env.imported;
	}

(**
checks if a file has already been imported
@param env analysis environment
@param filename the filename to check
@return true if already imported, false otherwise
*)
let has_import env filename =
	let rec loop = function
		| [] -> false
		| s:: tl -> if s = filename then true else loop tl
	in loop env.imported

(**
Retrieves a value at a location
@param env a runtime environment
@param loc the location of the variable
@return the value at the selected location
*)
let get_value env = function
	| GlobalVar(uid, ind) -> let (_, value) = env.heap.(ind) in value
	| LocalVar(uid, depth, ind) -> env.stackframes.(depth).(ind)

(**
Sets a value at a location
@param env a runtime environment
@param value the value to set
@param loc the location of the variable
@return the value that was set
*)
let set_value env value = function
	| GlobalVar(uid, ind) -> env.heap.(ind) <- (uid, value); value
	| LocalVar(uid, depth, ind) -> env.stackframes.(depth).(ind) <- value; value

(**
Returns the name of a location
@param env the runtime environment
@param loc the location of the variable
@return the name of the variable at location loc
*)
let get_loc_name env = function
	| GlobalVar(uid, _)	| LocalVar(uid, _, _) -> env.gnames.(uid)

