(**
Operations on AST analysis and runtime environments.

@author Tony BenBrahim < tony.benbrahim at gmail.com >
*)

(* This program is free software; you can redistribute it and / or modify  *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation; version 3 of the License. This program is *)
(* distributed in the hope that it will be useful, but WITHOUT ANY         *)
(* WARRANTY; without even the implied warranty of MERCHANTABILITY or       *)
(* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details.                                                       *)

open Ast

module StringMap = Map.Make(String)

(** Variable information, tuple of index into scope and unique id *)
type var_info = (int * int )

(**
represents variables map in a global or local scope, and reference to parent scope
*)
type rec_varmap ={
	variable_map: var_info StringMap.t; (** map of variable name to variable info *)
	parent: rec_varmap option; (** parent scope variable map, or None if top level scope *)
}

(**
Properties of variable locations
*)
type var_prop ={
	written_after_declared: bool; (** is the variable assigned after it is declared *)
	read_after_declared: bool; (** is the variable read after declared *)
	declaration_loc: string * int; (** tuple of file where variable is declared and line number*)
}

(** position of a label with within a template spec, tuple of start begin, start end,
end begin, end ending *)
type label_pos = int * int * int * int

(** definition of a template specidifcation, used during validity checking.
tuple of sepecfication list and map of labels to label position *)
type template_spec_def = (template_spec list *	(string , label_pos ) Hashtbl.t * (string * int))

(**
The analysis environment
*)
type analysis_env ={
	globals: rec_varmap; (** map of global variables *)
	num_globals: int; (** number of globals *)
	locals: rec_varmap list; (** recursive list of stack frames *)
	num_locals: int list; (** number of locals in current stack frame *)
	sdepth: int; (** current stack depth *)
	max_depth: int; (** maximum stack depth encountered *)
	errors: string list; (** list of errors found during analysis *)
	warnings: string list; (** list of warning generated during analysis *)
	unique_id: int; (** counter for next unique id *)
	names: string list; (** list of names encountered *)
	varprops: (int, var_prop) Hashtbl.t; (** properties of variables *)
	imported: string list; (** list of files already imported *)
	templates: (string, template_spec_def) Hashtbl.t; (** map of template names to template definitions *)
	constants: (int, runtime_variable_value) Hashtbl.t; (** map of variables unique id to declared value *)
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
		templates = Hashtbl.create 1;
		constants = Hashtbl.create 10;
	}

(**
gets the constant value for a variable
@param env analysis environment
@param uid unique id of variable
@return runtime value of variable
*)
let get_constant_value env uid =
	try
		Hashtbl.find env.constants uid
	with
		| Not_found -> raise (RuntimeError.InternalError ("Unable to get constant "^(string_of_int uid)))
(**
sets the declaration value for a variable
@param env analysis environment
@param uid unique id of variable
@param value runtime value of variable
@return unit
*)
let set_constant_value env uid value =
	Hashtbl.replace env.constants uid value



(**
returns whether the variable is a constant
@param env analysis environment
@param uid unique id of variable
@return true if the variable is a constant
*)
let is_constant env uid =
	let varprop = Hashtbl.find env.varprops uid
	in let (_, line) = varprop.declaration_loc
	in not varprop.written_after_declared && line!= 0

(**
declare a variable if it does not exist or create a new entry and return new index
@param name name of variable to declare
@param env analysis environment
@return a tuple of the modified environment and uid
*)
let declare_variable env name =
	let find_or_declare varmaps nextind uid =
		try let (_, uid) = StringMap.find name varmaps in (varmaps, 0, uid)
		with Not_found -> (StringMap.add name (nextind, uid) varmaps, 1, uid)
	in
	match env.locals with
	| [] ->
			let ( map, num_added, uid) = find_or_declare env.globals.variable_map env.num_globals env.unique_id
			in
			({ globals ={ variable_map = map; parent = env.globals.parent };
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
					templates = env.templates;
					constants = env.constants;
				}, uid)
	| _ ->
			let (map, num_added, uid) = find_or_declare (List.hd env.locals).variable_map (List.hd env.num_locals) env.unique_id
			in
			({ globals = env.globals;
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
					templates = env.templates;
					constants = env.constants;
				}, uid)

(**
declare a variable if it does not exist or create a new entry and return new index,
then sets constant value
@param name name of variable to declare
@param env analysis environment
@param value the value to initialize the variable with
@return the modified environment
*)
let declare_variable_and_value name env value =
	let (env, uid) = declare_variable name env
	in set_constant_value env uid value; env

(** internal exception used during analysis *)
exception Variable_not_found of string

(**
Find variable in analysis scope
@param name the variable name
@param env the analysis environment
@return location
@raise Variable_not_found when the variable is not found
*)
let resolve_variable name env =
	let rec find scopes =
		try
			let (ind, uid) = StringMap.find name scopes.variable_map
			in (uid, ind)
		with Not_found ->
				(match scopes.parent with
					| Some parent -> find parent
					| None -> raise Not_found)
	in
	let rec find_in_stackframes = function
		| [] -> raise Not_found
		| scope:: tl ->
				try
					let (uid, ind) = find scope
					in (LocalVar(uid, List.length tl, ind))
				with
				| Not_found -> find_in_stackframes tl
	in
	try
		match env.locals with
		| [] -> let (uid, ind) = find env.globals in (GlobalVar(uid, ind))
		| _ -> (try
					find_in_stackframes env.locals
				with Not_found ->
						let (uid, ind) = find env.globals in (GlobalVar(uid, ind)))
	with Not_found -> raise (Variable_not_found name)

(**
returns uid from location
@param loc the variable location
@return the unique id of the variable
*)
let uid_from_loc = function
	| GlobalVar(uid, _) -> uid
	| LocalVar(uid, _, _) -> uid

(**
Find variable and value in analysis scope
@param name the variable name
@param env the analysis environment
@return tuple of value and location
@raise Variable_not_found when the variable is not found
*)
let resolve_variable_value name env =
	let loc = resolve_variable name env
	in let uid = uid_from_loc loc
	in ( get_constant_value env uid, loc)

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
				templates = env.templates;
				constants = env.constants;
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
				templates = env.templates;
				constants = env.constants;
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
							templates = env.templates;
							constants = env.constants;
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
						templates = env.templates;
						constants = env.constants;
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
						templates = env.templates;
						constants = env.constants;
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
		templates = env.templates;
		constants = env.constants;
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
		errors = ("At line "^(string_of_int line_number)^" in "^(Filename.basename filename)^": "^message):: env.errors;
		sdepth = env.sdepth;
		max_depth = env.max_depth;
		warnings = env.warnings;
		unique_id = env.unique_id;
		names = env.names;
		varprops = env.varprops;
		imported = env.imported;
		templates = env.templates;
		constants = env.constants;
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
		warnings = ("At line "^(string_of_int line_number)^" in "^(Filename.basename filename)^": "^message):: env.warnings;
		unique_id = env.unique_id;
		names = env.names;
		varprops = env.varprops;
		imported = env.imported;
		templates = env.templates;
		constants = env.constants;
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
		templates = env.templates;
		constants = env.constants;
	}

(**
type of operation performed on variable
*)
type var_op_type =
	| ReadOp  (** variable is read *)
	| WriteOp (** variable is written *)
	| DeclareOp of (string * int) (** variable is declared *)
	| DeclareWriteOp of (string * int) (** variable is declared and written, used for function args *)

(**
Records a variables property
@param env analysis environment
@param loc variable location
@param operation the operation on the variable
@return unit
*)
let record_usage env loc op =
	let uid = match loc with
		| GlobalVar(uid, _) -> uid
		| LocalVar(uid, _, _) -> uid
	in let props =
		try Hashtbl.find env.varprops uid
		with Not_found ->
				{ written_after_declared = false;
					read_after_declared = false;
					declaration_loc = ("", 0);
				}
	in let new_props =
		match op with
		| ReadOp ->
				{ written_after_declared = props.written_after_declared;
					read_after_declared = true;
					declaration_loc = props.declaration_loc;
				}
		| WriteOp ->
				{	written_after_declared = true;
					read_after_declared = props.read_after_declared;
					declaration_loc = props.declaration_loc;
				}
		| DeclareOp(loc) ->
				(match props.declaration_loc with
					| ("", 0) ->
							{ written_after_declared = false;
								read_after_declared = props.read_after_declared;
								declaration_loc = loc
							}
					| _ ->
							{ written_after_declared = true;
								read_after_declared = props.read_after_declared;
								declaration_loc = props.declaration_loc
							})
		| DeclareWriteOp(loc) ->
				match props.declaration_loc with
				| ("", 0) ->
						{ written_after_declared = true;
							read_after_declared = true;
							declaration_loc = loc
						}
				| _ ->
						{ written_after_declared = true;
							read_after_declared = true;
							declaration_loc = props.declaration_loc
						}
	in Hashtbl.replace env.varprops uid new_props

(**
Adds a template to the environment
@param runtime environment
@param name template name
@param spec_list list of line specifications
@param labels label positions
@return a new environment
**)
let add_template env name spec_list labels cloc =
	let env = if Hashtbl.mem env.templates name then
			add_warning env cloc ("Duplicate template definition '" ^ name ^ "'")
		else
			env
	in Hashtbl.replace env.templates name (spec_list, labels, cloc); env

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
	| LocalVar(uid, depth, ind) ->
			match env.closure_vars with
			| None -> env.stackframes.(depth).(ind)
			| Some h ->
					try match Hashtbl.find h (depth, ind) with
						| RUndefined -> env.stackframes.(depth).(ind) (* needed for recursive function defs *)
						| value -> value
					with Not_found -> env.stackframes.(depth).(ind)

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

