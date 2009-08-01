open Ast
open Environment

(**
Registers all library functions and params in an analysis environment
@param env analysis environment
@return a modified environment with all library functions registered
*)
let register_for_analysis env =
	let env = Environment.declare_variable_and_value env "args" (RMapValue(Hashtbl.create 10, ArraySubtype))
	in let env = Environment.declare_variable_and_value env "String" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Integer" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Float" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Boolean" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Function" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Void" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Nan" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Array" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Map" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "void" RVoid
	in let rec define_map_lib_call h libdef = function
		| name::[] -> Hashtbl.replace h name libdef
		| name:: tl ->
				let h = (try
						match Hashtbl.find h name with
						| RMapValue(h, MapSubtype) -> h
						| _ -> raise (RuntimeError.InternalError "inconsistent library call definition")
					with Not_found ->
							let nh = Hashtbl.create 10
							in Hashtbl.replace h name (RMapValue(nh, MapSubtype)); nh)
				in define_map_lib_call h libdef tl
		| [] -> raise (RuntimeError.InternalError "invalid library call definition")
	in let rec loop env = function
		| [] -> env
		| def:: tl ->
				(match def.name with
					| name::[] ->
							let env = Environment.declare_variable_and_value env name (RLibraryFunction(def))
							in loop env tl
					| name:: name_rest ->
							let (h, env) = (try
									match resolve_variable_value name env with
									| (RMapValue(h, MapSubtype), loc) -> (h, env)
									| _ -> raise (RuntimeError.InternalError "inconsistent library call definition")
								with
								| Variable_not_found(_) ->
										let h = Hashtbl.create 10
										in let env = Environment.declare_variable_and_value env name (RMapValue(h, MapSubtype))
										in (h, env)
								)
							in define_map_lib_call h (RLibraryFunction(def)) name_rest;
							loop env tl
					| [] -> raise (RuntimeError.InternalError "invalid library call definition")
				)
	in let (exported, env) = Library_builtin.initialize env
	in let env = loop env exported
	in let (exported, env) = Library_string.initialize env
	in let env = loop env exported
	in let (exported, env) = Library_io.initialize env
	in loop env exported

(**
Registers library functions into a runtime environment
@param env analysis environment from which definitions will be transferred
@param renv runtime environment into which definitions will be transferred
@return unit
*)
let register_for_runtime env renv =
	let rec process rmap =
		StringMap.fold(fun k v _ ->
						let (ind, uid) = v
						in try
							print_string("LIBRARY TRYING:"^(string_of_int uid)^"\n");
							let value = get_constant_value env uid
							in print_string("LIBRARY GOT:"^(string_of_int uid)^"\n");renv.heap.(ind) <- (uid, value)
						with Not_found -> ()
			) rmap.variable_map ();
		match rmap.parent with
		| None -> ()
		| Some m -> process m
	in process env.globals
	