(**
This program is free software; you can redistribute it and / or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

Create an optimized AST from the parsing phase AST

Pass 1:
- resolve all variable references, including closure variables
- process imports and add declarations to AST
- builds runtime AST
- convert template definitions / instructions
- evaluate operations on constants and replace with value in AST
- determine if variables are initialized with a constant value (rather than an expression),
if a variable is written after being declared and if it is ever read after being declared
- determine if a function is inlineable

Pass 2:
The second pass replaces all non function variables whose value have not been modified
with a constant value, and evaluates operations on constants , eliminates assignment
statements on constant values when the variable is not reassigned and not written,
inline functions

@author Tony BenBrahim < tony.benbrahim at gmail.com >

*)

open Ast
open Environment
open Parser_util
open RuntimeError

(**
Prints all errors in an analysis environment and raises FatalExit if there are errors
@param env analysis environment
@returns unit
@raise FatalExit if there are errors in the environment
*)
let check_errors env =
	List.fold_left (fun _ error -> print_string ("ERROR: "^error^"\n")) () (List.rev env.errors);
	if List.length env.errors > 0 then raise RuntimeError.FatalExit
	else ()

(**
Generates additional warnings about unused variables, then prints all warnings
@param env analysis environment
@returns analysis environment with newly added warnings
*)
let check_warnings env =
	(* check for unused templates *)
	let env = Hashtbl.fold (fun name spec env ->
						let (_, _, cloc) = spec
						in Environment.add_warning env cloc ("Template definition '" ^ name ^ "' is never used.")
			)	env.templates env
	(* check variables usage *)
	in let rec loop_names env max names = function
		| n when n = max -> env
		| n ->
				let env =
					try
						let varprop = Hashtbl.find env.varprops n
						in let (_, line) = varprop.declaration_loc
						in match line with
						| 0 -> env
						| _ ->
								let env = if varprop.read_after_declared = false && line!= 0 then
										add_warning env varprop.declaration_loc ("Variable "^(List.hd names)^" is never read.")
									else
										env
								in env
					with Not_found ->
							env
				in loop_names env max (List.tl names) (n + 1)
	in let env = loop_names env (List.length env.names) env.names 0
	(* print warnings *)
	in List.fold_left (fun _ warning -> print_string ("WARNING: "^warning^"\n")) () (List.rev env.warnings);
	env

(**
Prints information about names found during analysis
@env analysis environment
@return unit
*)
let print_name_info env =
	let _ = List.fold_left (fun ind name ->
						print_int ind;
						let (read, written, analyzed, (file, line)) =
							try
								let vp = Hashtbl.find env.varprops ind
								in (vp.read_after_declared, vp.written_after_declared, true, vp.declaration_loc)
							with Not_found -> (false, false, false, ("", 0))
						in
						print_string (" "^name^" read="^(string_of_bool read)^" written="^(string_of_bool written)
								^ " analyzed="^(string_of_bool analyzed)^" ("
								^( Filename.basename file)^","^(string_of_int line)^")\n") ;
						ind + 1) 0 env.names
	in ()

(**
**********************************************************************************************
* FIRST PASS
* - resolve all variable references, including closure variables
* - process imports and add declarations to AST
* - builds runtime AST
* - convert template definitions / instructions
* - evaluate operations on constants and replace with value in AST
* - determine if variables are initialized with a constant value (rather than an expression)
* - determine if a variable is written after being declared and if it is ever read after being declared
* - determine if a function is inlineable
**********************************************************************************************
*)

(**internal exception to signal an error in template processing. *)
exception TemplateError of string

(**
Checks for invalid nesting in a template specification
@param template_spec the template spec to check
@return an list of tuples containing the label and line offset where conflicts where found
**)
let rec check_template_nesting template_spec =
	let labels = Hashtbl.create 10
	(* get start and end for each label *)
	in let (_, errors) = List.fold_left(fun acc spec ->
						let (line, errors) = acc
						in let (name_opt, _) = spec
						in match name_opt with
						| None -> (line + 1, errors)
						| Some label ->
								try
									let (start, end_start, ending, end_ending) = Hashtbl.find labels label
									in let (new_pos, errors) =
										if line = end_start + 1 then ((start, line, line, line), errors)
										else if line = end_ending + 1 then ((start, end_start, ending, line), errors)
										else if ending = end_start then ((start, end_start, line, line), errors)
										else ((start, end_start, ending, end_ending), (label, line):: errors)
									in Hashtbl.replace labels label new_pos;
									(line + 1, errors)
								with Not_found ->
										Hashtbl.add labels label (line, line, line, line);
										(line + 1, errors))
			(0,[]) template_spec
	(* find overlapping labels *)
	in let errors = Hashtbl.fold ( fun label pos list ->
						let (start1, _, _, ending1) = pos
						in Hashtbl.fold(fun label pos list ->
										let (start2, _, _, ending2) = pos
										in if start2 > start1 && start2 < ending1 && ending2 > ending1 then (label, ending2):: list
										else list
							) labels list
			) labels errors
	in (labels, errors)

(**
Generate a set of statements corresponding to a template instruction
@param instruction instruction AST
@env runtime environment
@return a runtime statement for the instruction defining a function
*)
and generate_template_instr_function instruction env =
	let generate_instructions template_specs labels replacement_list cloc args =
		let find_replacement name =
			let rec loop = function
				| [] -> raise Not_found
				| (n, condexpr, repl):: tl when n = name -> (condexpr, repl)
				| hd:: tl -> loop tl
			in loop replacement_list
		in let make_repl_vars replacements =
			let rec loop substrings expressions = function
				| [] -> (ArrayExpr(List.rev substrings), ArrayExpr(List.rev expressions))
				| hd:: tl ->
						let (string, expr) = hd
						in loop (Value(StringValue(string)):: substrings) (expr:: expressions) tl
			in loop [] [] replacements
		in let array = Array.of_list template_specs
		in	let rec loop name result index endindex args target_label substrings_var repl_var =
			if index = endindex then
				let result = (Return(Id("result"), cloc)):: result
				in ExpressionStatement(Declaration(Id(name), Value(FunctionValue(args, List.rev result))), cloc)
			else
				let (label_opt, line) = array.(index)
				in match label_opt with
				| None ->
						loop name (ExpressionStatement(Assignment(Id("result"), BinaryOp(Id("result"), Plus, Value(StringValue(line)))), cloc):: result) (index + 1) endindex args target_label substrings_var repl_var
				| Some label ->
						if label = target_label then
							let call = FunctionCall(MemberExpr(Value(StringValue(line)), Value(StringValue("mreplace"))),[substrings_var; repl_var])
							in loop name (ExpressionStatement(Assignment(Id("result"), BinaryOp(Id("result"), Plus, call)), cloc):: result) (index + 1) endindex args target_label substrings_var repl_var
						else
							try
								let (condexpr, replacements) = find_replacement label
								in let (substrings, replexprs) = make_repl_vars replacements
								in let (start, _, _, ending) = Hashtbl.find labels label
								in let arg_array = match condexpr with
									| Once | When(_) -> []
									| Loop(iname, _) | CondLoop(_, iname, _) -> [iname]
								in let earg_array = match condexpr with
									| Once | When(_) -> []
									| Loop(iname, _) | CondLoop(_, iname, _) -> [Id(iname)]
								in let stmt1 = loop label [ExpressionStatement(Declaration(Id("result"), Value(StringValue(""))), cloc)]
										start (ending + 1) arg_array label substrings replexprs
								in let call = ExpressionStatement(Assignment(Id("result"), BinaryOp(Id("result"), Plus, FunctionCall(Id(label), earg_array))), cloc)
								in let stmt2 = match condexpr with
									| Once -> call
									| When(cexpr) -> If(cexpr, call, Noop, cloc)
									| Loop(iname, iexpr) -> ForEach(iname, iexpr, call, cloc)
									| CondLoop(cexpr, iname, iexpr) -> If(cexpr, ForEach(iname, iexpr, call, cloc), Noop, cloc)
								in loop name (stmt2:: stmt1:: result) (ending + 1) endindex args target_label substrings_var repl_var
							with Not_found ->
									raise(TemplateError ("could not find instruction for label '"^label^"'"))
		in let (name, args, specs, cloc) = instruction
		in loop name [ExpressionStatement(Declaration(Id("result"), Value(StringValue(""))), cloc)]
			0 (List.length template_specs) args "" (ArrayExpr([])) (ArrayExpr([]))
	in let (name, args, replacements, cloc) = instruction
	in	try
		let (template_specs, labels, _) = Hashtbl.find env.templates name
		in let (rstmt, env) =
			try
				let stmt = generate_instructions template_specs labels replacements cloc args
				in analyze_variables env stmt
			with TemplateError message ->
					(RNoop, Environment.add_error env cloc message)
		in Hashtbl.remove env.templates name; (rstmt, env)
	with
	| Not_found -> (RNoop, env)
	| Variable_not_found(name) -> (RNoop, env)
(**
Filters an ast, returning only a list of declaration and import statement
@param stmts the statement list to process
@return a statement list containing only declarations and imports
*)
and filter_imported_ast stmts =
	let rec loop result = function
		| [] -> List.rev result
		| stmt:: tl ->
				(match stmt with
					| ExpressionStatement(Declaration(_, _), _) | Import (_, _) ->
							loop (stmt:: result) tl
					| _ -> loop result tl
				) in
	loop [] stmts

(** find declarations and resolve references to variables. Since declarations are
visible in the entire scope in which they are defined, and not just after they are
declared, a breadth first search is necessary before recursively processing children
statements
@param env an analysis environment
@param ast the intermediate ast
@return an ast where all variables have been resolved to an absolute location, either
on a stack or in the global heap and an environment containing information about all
variables
*)
and analyze_variables env ast =
	(**
	recursively searches an expression for declarations
	@param env the analysis environment
	@param expr the expression to search
	@param cloc the location in code of the expression
	@return an environment with any declared name added
	*)
	let rec find_decl_in_expr env expr cloc =
		match expr with
		| Declaration(expr1, expr2) ->
				let env = (match expr1 with
						| Id(name) -> let (env, uid) = Environment.declare_variable env name in env
						| MemberExpr(_, _) -> env
						| _ -> Environment.add_error env cloc "Left side cannot be assigned to")
				in find_decl_in_expr (find_decl_in_expr env expr1 cloc) expr2 cloc
		| Not(expr) | PostFixSum (expr, _) ->
				find_decl_in_expr env expr cloc
		| Assignment(expr1, expr2) | BinaryOp(expr1, _, expr2) | CompOp(expr1, _, expr2)
		| MemberExpr(expr1, expr2) ->
				find_decl_in_expr (find_decl_in_expr env expr1 cloc) expr2 cloc
		| TernaryCond(expr1, expr2, expr3) ->
				find_decl_in_expr(find_decl_in_expr (find_decl_in_expr env expr1 cloc) expr2 cloc) expr3 cloc
		| FunctionCall(expr, expr_list) ->
				List.fold_left (fun env expr ->
								find_decl_in_expr env expr cloc) (find_decl_in_expr env expr cloc) expr_list
		| MapExpr(proplist) ->
				List.fold_left (fun env prop -> let (_, expr) = prop in find_decl_in_expr env expr cloc) env proplist
		| ArrayExpr(expr_list) ->
				List.fold_left (fun env expr -> find_decl_in_expr env expr cloc) env expr_list
		| Value(_) | UnboundVar(_) | Id(_) | VarArg(_) -> env
	(** finds all the declarations in the statement and registers	them in the environment
	@param env analysis environment
	@param stmt the statement to be analyzed
	@return an analysis environment where all names found in assigments are registered
	*)
	and find_declarations_in_stmt env stmt =
		match stmt with
		| ExpressionStatement(expr, cloc) | Throw(expr, cloc) | Switch(expr, _, cloc) | Case(Some expr, cloc)
		| If(expr, _, _, cloc) | Return(expr, cloc) | ForEach(_, expr, _, cloc) ->
				find_decl_in_expr env expr cloc
		| TryFinally(_, _, _) | TryCatch(_, _, _, _) | StatementBlock(_) | Case(None, _) | Continue(_)
		| Break(_) | Noop | Program(_) | Import(_) | For(_, _, _, _, _) -> env
		| Instructions(name, _, _, _) -> let (env, _) = Environment.declare_variable env name in env
		| TemplateDef(name, spec_list , cloc) ->
				let (labels, errors) = check_template_nesting spec_list
				in match errors with
				| [] -> Environment.add_template env name spec_list labels cloc
				| error_list ->
						List.fold_left (fun env label_offset ->
										let (label, offset) = label_offset
										in let (file, line) = cloc
										in Environment.add_error env (file, line + offset + 1) ("Invalid nesting of labels for label '"^label^"'")
							) env error_list
	(**
	Find all all variables in statement list whose stack depth is lower than that given
	@param stmt_list a list of statements to be searched recursively
	@param stack_depth the stack depth for the given statement
	@return Some map of unique id to Void of all variables found or None if none where found
	*)
	and get_closure_vars stmt_list stack_depth =
		let add loc = function
			| None -> let t = Hashtbl.create 2 in Hashtbl.replace t loc RUndefined; Some t
			| Some t -> Hashtbl.replace t loc RUndefined; Some t
		in
		let rec find_in_expr result = function
			| RVariable(LocalVar(_, sdepth, ind)) | RVarArg(LocalVar(_, sdepth, ind)) ->
					if sdepth = stack_depth then result else add (sdepth, ind) result
			| RValue(_) | RVariable(GlobalVar(_, _)) | RVarArg(GlobalVar(_, _)) -> result
			| RPostFixSum(e, _) | RNot(e) -> find_in_expr result e
			| RBinaryOp(e1, _, e2) | RCompOp(e1, _, e2) | RAssignment(e1, e2) | RDeclaration(e1, e2)
			| RMemberExpr(e1, e2) ->
					find_in_expr (find_in_expr result e1) e2
			| RTernaryCond(e1, e2, e3) ->
					find_in_expr(find_in_expr (find_in_expr result e1) e2) e3
			| RArrayExpr(elist) -> List.fold_left (fun r e -> find_in_expr r e) result elist
			| RMapExpr(proplist) ->
					List.fold_left (fun r prop -> let (_, e) = prop
									in find_in_expr r e) result proplist
			| RFunctionCall(e, elist) ->
					List.fold_left (fun r e -> find_in_expr r e)
						(find_in_expr result e) elist
		and process result = function
			| RNoop | RContinue(_) | RCase(None, _) | RBreak(_) | RFastIterator _ -> result
			| RStatementBlock(slist) -> loop result slist
			| RTryCatch(s1, _, s2, _) | RTryFinally(s1, s2, _) ->
					process (process result s1) s2
			| RReturn (e, _) | RThrow(e, _) | RCase(Some e, _) | RExpressionStatement(e, _) ->
					find_in_expr result e
			| RFor(e1, e2, e3, stmt, _) ->
					process (find_in_expr(find_in_expr (find_in_expr result e1) e2) e3) stmt
			| RIf(e, stmt1, stmt2, _) -> process(process(find_in_expr result e) stmt1)stmt2
			| RSwitch(e, stmts, _) -> loop (find_in_expr result e) stmts
			| RForEach(_, e, stmt, _) -> process (find_in_expr result e) stmt
			| RProgram(_) -> raise (RuntimeError.InternalError "unexpected statement in closure processing")
		and loop result = function
			| [] -> result
			| stmt:: list -> loop (process result stmt) list
		in loop None stmt_list
	(**
	Convert a parsing value to a runtime value
	*)
	and convert_value env cloc = function
		| StringValue(v) -> (RStringValue(v), env)
		| IntegerValue(v) -> (RIntegerValue(v), env)
		| FloatValue(v) -> (RFloatValue(v), env)
		| BooleanValue(v) -> (RBooleanValue(v), env)
		| Void -> (RVoid, env)
		| MapValue(h, s) ->
				(RMapValue(Hashtbl.fold (fun k v h ->
										let (repl, env) = convert_value env cloc v
										in Hashtbl.replace h k repl ; h ) h (Hashtbl.create 10), s), env)
		| FunctionValue(arg_list, stmt_list) ->
				let rec analyze_vararg has_vararg namelist env = function
					| [] -> (has_vararg, List.rev namelist, env)
					| name::[] when is_vararg name -> analyze_vararg true ((vararg_formalname name):: namelist) env []
					| name:: tl ->
							let env =
								(if is_vararg name then
										Environment.add_error env cloc "vararg must be last argument"
									else
										env)
							in analyze_vararg has_vararg (name:: namelist) env tl
				in let (has_vararg, arg_list, env) = analyze_vararg false [] env arg_list
				in let env = Environment.new_analysis_stackframe env
				in let (env, _) = Environment.declare_variable env "this"
				in let _ = Environment.record_usage env (LocalVar(env.unique_id - 1, 0, 0)) (DeclareOp cloc)
				in let _ = Environment.record_usage env (LocalVar(env.unique_id - 1, 0, 0)) WriteOp
				in let _ = Environment.record_usage env (LocalVar(env.unique_id - 1, 0, 0)) ReadOp
				in let env = List.fold_left
						(fun env name ->
									let (env, _) = Environment.declare_variable env name
									in let _ = Environment.record_usage env (LocalVar(env.unique_id - 1 , 0, 0)) (DeclareOp cloc)
									in let _ = Environment.record_usage env (LocalVar(env.unique_id - 1, 0, 0)) WriteOp
									in env) env arg_list
				in let (stmt_list, env) = analyze_variables_in_block env stmt_list
				in let closure_vars = get_closure_vars stmt_list (Environment.get_depth env)
				in let inline_expr = match (stmt_list, closure_vars) with
					| (RReturn(expr, _)::[], None) | (RExpressionStatement(expr, _)::[], None) -> Some expr
					| ([], None) -> Some (RValue(RVoid))
					| _ -> None
				in (RFunctionValue(List.hd env.num_locals, Environment.get_depth env, List.length arg_list, has_vararg,
						stmt_list, closure_vars, inline_expr), Environment.pop_scope env)
	(**
	Replaces all variables in expression with absolute locations
	Also sets up function definitions
	@param env analysis environment
	@param expr expression
	@param location in code
	@return new expression with variables replaced with absolute location
	*)
	and resolve_expr env expr cloc =
		let rec resolve_expr_sub env expr op_type =
			match expr with
			| Id(name) ->
					let loc = Environment.resolve_variable name env
					in let _ = record_usage env loc op_type
					in (RVariable(loc), env)
			| VarArg(name) ->
					let loc = Environment.resolve_variable name env
					in (RVarArg(loc), env)
			| BinaryOp(e1, op, e2) ->
					let (expr1, env) = resolve_expr_sub env e1 ReadOp
					in let (expr2, env) = resolve_expr_sub env e2 ReadOp
					in (match (expr1, expr2) with
						| (RValue(v1), RValue(v2)) ->
								(try
									(RValue(Expression.evaluate_op v1 v2 op), env)
								with
								| Division_by_zero -> (RBinaryOp(expr1, op, expr2), Environment.add_error env cloc "division by zero")
								| EInvalidOperation(_, t) ->
										(RBinaryOp(expr1, op, expr2), Environment.add_error env cloc ("invalid operation for "^t^" types"))
								| EIncompatibleTypes(t1, t2) ->
										(RBinaryOp(expr1, op, expr2), Environment.add_error env cloc ("incompatible types "^t1^" and "^t2))
								)
						| _ -> (RBinaryOp(expr1, op, expr2), env))
			| CompOp(e1, op, e2) ->
					let (expr1, env) = resolve_expr_sub env e1 ReadOp
					in let (expr2, env) = resolve_expr_sub env e2 ReadOp
					in (match (expr1, expr2) with
						| (RValue(v1), RValue(v2)) ->
								(try
									(RValue(Expression.compare v1 op v2), env)
								with
								| EInvalidComparaison(_, _, _) ->
										(RCompOp(expr1, op, expr2), Environment.add_error env cloc ("invalid  comparaison"))
								)
						| _ -> (RCompOp(expr1, op, expr2), env))
			| Not(e) ->
					let (expr, env) = resolve_expr_sub env e ReadOp
					in (match expr with
						| RValue(RBooleanValue(b)) -> (RValue(RBooleanValue(not b)), env)
						| _ -> (RNot(expr), env))
			| FunctionCall(e, el) ->
					let rec has_unbound_var = function
						| [] -> false
						| UnboundVar(_):: tl -> true
						| _:: tl -> has_unbound_var tl
					in if has_unbound_var el then
						let rec unbound_list result = function
							| [] -> List.rev result
							| UnboundVar(name):: tl -> unbound_list (name:: result) tl
							| _:: tl -> unbound_list result tl
						in let rec bound_list result = function
							| [] -> List.rev result
							| UnboundVar(name):: tl ->
									let variable =
										if is_vararg name then
											VarArg(vararg_formalname name)
										else
											Id(name)
									in bound_list (variable:: result) tl
							| expr:: tl -> bound_list (expr:: result) tl
						in resolve_expr_sub env (Value(FunctionValue(unbound_list [] el,[Return(FunctionCall(e, bound_list [] el), cloc)]))) ReadOp
					else
						let (expr, env) = resolve_expr_sub env e ReadOp
						in let (expr_list, env) = List.fold_left(fun acc expr -> let (lst, env) = acc
											in let (expr, env) = resolve_expr_sub env expr ReadOp in (expr:: lst, env)) ([], env) el
						in (RFunctionCall(expr, List.rev expr_list), env)
			| MapExpr(prop_list) ->
					let (prop_list, env) = List.fold_left(fun acc prop -> let (lst, env) = acc
										in let (name, expr) = prop
										in let (expr, env) = resolve_expr_sub env expr ReadOp
										in ((name, expr):: lst, env)) ([], env) prop_list
					in (RMapExpr(List.rev prop_list), env)
			| ArrayExpr(el) ->
					let (expr_list, env) = List.fold_left(fun acc expr -> let (lst, env) = acc
										in let (expr, env) = resolve_expr_sub env expr ReadOp
										in (expr:: lst, env)) ([], env) el
					in (RArrayExpr(List.rev expr_list), env)
			| Value(v) -> let (repl, env) = convert_value env cloc v in (RValue(repl), env)
			| Assignment(e1, e2) ->
					let (expr1, env) = resolve_expr_sub env e1 WriteOp
					in let (expr2, env) = resolve_expr_sub env e2 ReadOp
					in (RAssignment(expr1, expr2), env)
			| Declaration(e1, e2) ->
					let (expr1, env) = resolve_expr_sub env e1 (DeclareOp cloc)
					in let (expr2, env) = resolve_expr_sub env e2 ReadOp
					in let _ = match (expr1, expr2) with
						| (RVariable(loc), RValue(value)) ->
								let uid = uid_from_loc loc
								in Environment.set_constant_value env uid value
						| _ -> ()
					in (RDeclaration(expr1, expr2), env)
			| TernaryCond(e1, e2, e3) ->
					let (e1, env) = resolve_expr_sub env e1 ReadOp
					in let (e2, env) = resolve_expr_sub env e2 ReadOp
					in let (e3, env) = resolve_expr_sub env e3 ReadOp
					in (RTernaryCond(e1, e2, e3), env)
			| MemberExpr(e1, e2) ->
					let (expr1, env) = match op_type with
						| DeclareOp(loc) -> resolve_expr_sub env e1 (DeclareWriteOp(loc))
						| _ -> resolve_expr_sub env e1 op_type
					in let (expr2, env) = resolve_expr_sub env e2 ReadOp
					in (RMemberExpr(expr1, expr2), env)
			| PostFixSum(e, inc) ->
					let (expr, env) = resolve_expr_sub env e WriteOp
					in (RPostFixSum(expr, inc), env)
			| UnboundVar(_) ->
					(RValue(RVoid), Environment.add_error env cloc "Unexpected unbound var")
		in
		try
			resolve_expr_sub env expr ReadOp
		with
		| Variable_not_found(name) -> (RValue(RVoid), Environment.add_error env cloc ("Undefined variable '"^name^"'"))
	(**
	Replace variables in all statements in a list with an absolute location
	@param env analysis environment
	@param stmt_list the statement list to process
	@return a tuple consisting of a new statement list with all variables replaced with an absolute location
	and an updated environment, possibly containing new errrors
	*)
	and replace_variables_in_block env stmt =
		let rec loop env stmt_list new_list =
			match stmt_list with
			| [] -> (List.rev new_list, env)
			| stmt:: tl ->
					let (stmt, env) = analyze_variables env stmt
					in loop env tl (stmt:: new_list)
		in loop env stmt []
	(**
	In the given statement list, replace all top level import statements
	@param env analysis enviroment
	@param stmt_list the statement list
	@return a tuple of the statement list with imports replaced by imported statements and
	a new environment, possibly containing new errors
	*)
	and process_imports env stmt_list =
		let rec loop result env = function
			| [] -> (List.rev result, env)
			| Import(filename, cloc):: tl ->
					(if Environment.has_import env filename then
							loop (Noop:: result) env tl
						else (
							let env = Environment.add_import env filename
							in let stmt = Parser_util.parse_filename filename
							in (match stmt with
								| Program(stmts) ->
										let (stmts, env) = process_imports env (filter_imported_ast stmts)
										in let result = List.fold_left (fun lst stmt -> stmt:: lst) result stmts
										in loop result env tl
								| _ -> raise (RuntimeError.InternalError "Unexpected node from import"))))
			| stmt:: tl -> loop (stmt:: result) env tl
		in loop [] env stmt_list
	(**
	In the given statement list, find all top level declarations, then resolve all top
	level variables, then recurse into nested statement blocks
	@param env analysis environment
	@param stmt_list the statement list to process
	@return a tuple of modified statements and the modified analysis environment
	**)
	and analyze_variables_in_block env stmt_list =
		let (stmt_list, env) = process_imports env stmt_list
		in let env = List.fold_left(fun env stmt -> find_declarations_in_stmt env stmt) env stmt_list
		in replace_variables_in_block env stmt_list
	and analyze_variables_in_stmt env stmt =
		let env = find_declarations_in_stmt env stmt
		in analyze_variables env stmt
	in
	match ast with
	| Program(stmt_list) ->
			let (ast, env) = analyze_variables_in_block env stmt_list
			in (RProgram(ast), env)
	| StatementBlock(stmt_list) ->
			let newenv = Environment.new_analysis_scope env
			in let (ast, newenv) = analyze_variables_in_block newenv stmt_list
			in (RStatementBlock(ast), Environment.pop_scope newenv)
	| Switch(expr, stmt_list, cloc) ->
			let newenv = Environment.new_analysis_scope env
			in let (expr, newenv) = resolve_expr newenv expr cloc
			in let (ast, newenv) = analyze_variables_in_block newenv stmt_list
			in (RSwitch(expr, ast, cloc), Environment.pop_scope newenv)
	| TryCatch(stmt1, name, stmt2, cloc) ->
			let newenv = Environment.new_analysis_scope env
			in let (stmt1, newenv) = analyze_variables_in_stmt newenv stmt1
			in let newenv = Environment.new_analysis_scope (Environment.pop_scope newenv)
			in let (newenv, _) = Environment.declare_variable newenv name
			in let (stmt2, newenv) = analyze_variables_in_stmt newenv stmt2
			in let loc = Environment.resolve_variable name newenv
			in (RTryCatch(stmt1, loc, stmt2, cloc), Environment.pop_scope newenv)
	| If(expr, stmt1, stmt2, cloc) ->
			let (expr, env) = resolve_expr env expr cloc
			in let newenv = Environment.new_analysis_scope env
			in let (stmt1, newenv) = analyze_variables_in_stmt newenv stmt1
			in let newenv = Environment.new_analysis_scope (Environment.pop_scope newenv)
			in let (stmt2, newenv) = analyze_variables_in_stmt newenv stmt2
			in (RIf(expr, stmt1, stmt2, cloc), Environment.pop_scope newenv)
	| TryFinally(stmt1, stmt2, cloc) ->
			let newenv = Environment.new_analysis_scope env
			in let (stmt1, newenv) = analyze_variables_in_stmt newenv stmt1
			in let newenv = Environment.new_analysis_scope (Environment.pop_scope newenv)
			in let (stmt2, newenv) = analyze_variables_in_stmt newenv stmt2
			in (RTryFinally(stmt1, stmt2, cloc), Environment.pop_scope newenv)
	| ForEach(name, expr, stmt, cloc) ->
			let (newenv, _) = Environment.declare_variable (Environment.new_analysis_scope env) name
			in let (expr, newenv) = resolve_expr newenv expr cloc
			in let (stmt, newenv) = analyze_variables_in_stmt newenv stmt
			in let loc = Environment.resolve_variable name newenv
			in (RForEach(loc, expr, stmt, cloc), Environment.pop_scope newenv)
	| For(expr1, expr2, expr3, stmt, cloc) ->
			let newenv = Environment.new_analysis_scope env
			in let newenv = find_decl_in_expr (find_decl_in_expr (find_decl_in_expr newenv expr1 cloc) expr2 cloc) expr3 cloc
			in let (expr1, newenv) = resolve_expr newenv expr1 cloc
			in let (expr2, newenv) = resolve_expr newenv expr2 cloc
			in let (expr3, newenv) = resolve_expr newenv expr3 cloc
			in let (stmt, newenv) = analyze_variables_in_stmt newenv stmt
			in (RFor(expr1, expr2, expr3, stmt, cloc), Environment.pop_scope newenv)
	| Noop -> (RNoop, env)
	| ExpressionStatement(e, cloc) ->
			let (expr, env) = resolve_expr env e cloc
			in (RExpressionStatement(expr, cloc), env)
	| Return(e, cloc) ->
			let (e, env) = resolve_expr env e cloc
			in (RReturn(e, cloc), env)
	| Case(Some e, cloc) ->
			let (e, env) = resolve_expr env e cloc
			in (RCase(Some e, cloc), env)
	| Case(None, cloc) -> (RCase(None, cloc), env)
	| Throw(e, cloc) ->
			let (e, env) = resolve_expr env e cloc
			in (RThrow(e, cloc), env)
	| Break(cloc) -> (RBreak(cloc), env)
	| Continue(cloc) -> (RContinue(cloc), env)
	| Import(_, _) | TemplateDef(_, _, _) -> (RNoop, env)
	| Instructions(name, args, specs, cloc) ->
			generate_template_instr_function (name, args, specs, cloc) env

(**
**********************************************************************************************
* SECOND PASS
* - replace all constant declarations with Noop
* - replace all constant variables with their value
* - replace all constant expressions with the computed value
* - replace all calls to inlineable functions with an expression
**********************************************************************************************
*)

(** replaces an expression from an inlined function with the corresponding
values from a function call expression list
@param depth the stack depth, for sanity checking
@param numargs the number of arguments
@param repl_exprs the expressions used in the call invocation
@param expr the inline expression
@return the inline expression with the arguments replacing the former local args
*)
let rec inline_expr_replace depth numargs repl_expr expr =
	let rec loop = function
		| RVariable(LocalVar(uid, d, ind)) when d = depth && ind <= numargs ->
				List.nth repl_expr (ind - 1)
		| RVariable(_) | RValue(_) | RVarArg(_) as e -> e
		| RBinaryOp(expr1, op, expr2) -> RBinaryOp(loop expr1, op, loop expr2)
		| RCompOp(e1, op, e2) -> RCompOp(loop e1, op, loop e2)
		| RNot(e) -> RNot(loop e)
		| RTernaryCond(e1, e2, e3) -> RTernaryCond(loop e1, loop e2, loop e3)
		| RDeclaration(e1, e2) -> RDeclaration(loop e1, loop e2)
		| RAssignment(e1, e2) -> RAssignment(loop e1, loop e2)
		| RMapExpr(props) -> RMapExpr(List.map (fun p -> let (name, e) = p in (name, loop e)) props)
		| RArrayExpr(elist) -> RArrayExpr(List.map(fun e -> loop e) elist)
		| RFunctionCall(e, elist) -> RFunctionCall(loop e, List.map(fun e -> loop e) elist)
		| RMemberExpr(e1, e2) -> RMemberExpr(loop e1, loop e2)
		| RPostFixSum(e, i) -> RPostFixSum(loop e, i)
	in loop expr
and
(**
Replace non modified variables with their declared value
@param env analysis environment
@param inline_uids list of inlined functions to avoid recursively inlining recursive inlinable functions
@param expression expression to process
@return an expression with constant variables replaced by their value
*)
replace_constant env inline_uids = function
	|	RVariable(loc) ->
			let uid = uid_from_loc loc
			in if Environment.is_constant env uid then
				try
					match (Environment.get_constant_value env uid) with
					| RFunctionValue(_) -> RVariable(loc)
					| value -> RValue(value)
				with Not_found -> RVariable(loc)
			else RVariable(loc)
	| RNot(expr) -> RNot(replace_constant env inline_uids expr)
	| RBinaryOp(expr1, op, expr2) ->
			let (e1, e2) = (replace_constant env inline_uids expr1, replace_constant env inline_uids expr2)
			in (try match (e1, e2) with
				| (RValue(v1), RValue(v2)) -> RValue(Expression.evaluate_op v1 v2 op)
				| _ -> RBinaryOp(e1, op, e2)
			with _ -> RBinaryOp(e1, op, e2))
	| RCompOp(expr1, op, expr2) ->
			RCompOp(replace_constant env inline_uids expr1, op, replace_constant env inline_uids expr2)
	| RValue(RFunctionValue(locals, depth, args, vararg, stmts, closvars, inline)) ->
			RValue(RFunctionValue(locals, depth, args, vararg, List.map(fun stmt -> pass2 env stmt) stmts, closvars, inline))
	| RValue(_) | RPostFixSum(_) | RVarArg(_) as value -> value
	| RFunctionCall(expr, expr_list) ->
			let e = replace_constant env inline_uids expr
			in let e_list = List.map(fun e -> replace_constant env inline_uids e) expr_list
			in (match e with
				| RVariable(GlobalVar(uid, _))
				| RVariable(LocalVar(uid, _, _)) when is_constant env uid ->
						(match get_constant_value env uid with
							| RFunctionValue(_, depth, numargs, false, _, None, Some expr) ->
									if List.exists (fun i -> i == uid) inline_uids then
										RFunctionCall(e, e_list)
									else
										replace_constant env (uid:: inline_uids) (inline_expr_replace depth numargs e_list expr)
							| _ -> RFunctionCall(e, e_list))
				| _ -> RFunctionCall(e, e_list))
	| RAssignment(expr1, expr2) -> RAssignment(expr1, replace_constant env inline_uids expr2)
	| RDeclaration(expr1, expr2) ->
			let expr2 = replace_constant env inline_uids expr2
			in (match (expr1, expr2) with
				| (RVariable(loc), RValue(value)) ->
						let uid = uid_from_loc loc
						in if is_constant env uid then
							match value with
							| RFunctionValue(_) -> RDeclaration(expr1, expr2)
							| _ -> (Environment.set_constant_value env uid value; RValue(RUndefined))
						else
							RDeclaration(expr1, expr2)
				| _ -> RDeclaration(expr1, expr2))
	| RMemberExpr(expr1, expr2) ->
			RMemberExpr(replace_constant env inline_uids expr1, replace_constant env inline_uids expr2)
	| RArrayExpr(expr_list) ->
			RArrayExpr(List.map(fun e -> replace_constant env inline_uids e) expr_list)
	| RMapExpr(prop_list) ->
			RMapExpr(List.map (fun prop -> let (name, e) = prop in (name, replace_constant env inline_uids e)) prop_list)
	| RTernaryCond(expr1, expr2, expr3) ->
			RTernaryCond(replace_constant env inline_uids expr1, replace_constant env inline_uids expr2,
				replace_constant env inline_uids expr3)
(**
Looks for expressions where constants can be substituted
@param env analysis environment
@param stmt statement
*)
and pass2 env = function
	| RProgram(stmts) -> RProgram(List.map (fun stmt -> pass2 env stmt) stmts)
	| RStatementBlock(stmts) ->	RStatementBlock(List.map (fun stmt -> pass2 env stmt) stmts)
	| RThrow(expr, cloc) -> RThrow(replace_constant env [] expr, cloc)
	| RCase(Some expr, cloc) -> RCase(Some (replace_constant env [] expr), cloc)
	| RReturn(expr, cloc) -> RReturn(replace_constant env [] expr, cloc)
	| RContinue(_) | RBreak(_) | RCase(None, _) | RNoop | RFastIterator _ as stmt -> stmt
	| RExpressionStatement(expr, cloc) ->
			(match replace_constant env [] expr with
				| RValue(RUndefined) -> RNoop
				| expr -> RExpressionStatement(expr, cloc))
	| RFor(expr1, expr2, expr3, stmt, cloc) ->
			let expr1 = replace_constant env [] expr1
			in let expr2 = replace_constant env [] expr2
			in let expr3 = replace_constant env [] expr3
			in let stmt = pass2 env stmt
			in (match (expr1, expr2, expr3) with
				| (RDeclaration(RVariable(vloc1), RValue(RIntegerValue(start))),
				RCompOp(RVariable(vloc2), LessThan, RValue(RIntegerValue(max))),
				RPostFixSum(RVariable(vloc3), inc)) when vloc1 = vloc2 && vloc1 = vloc3 ->
						RFastIterator(vloc1, start , max , inc, stmt , cloc)
				| (RDeclaration(RVariable(vloc1), RValue(RIntegerValue(start))),
				RCompOp(RVariable(vloc2), LessThan, RValue(RIntegerValue(max))),
				RAssignment(RVariable(vloc3), RBinaryOp(RVariable(vloc4), Plus, RValue(RIntegerValue(inc)))))
				when vloc1 = vloc2 && vloc1 = vloc3 & vloc1 = vloc4 ->
						RFastIterator(vloc1, start , max , inc, stmt , cloc)
				| _ -> RFor(expr1, expr2 , expr3 , stmt , cloc))
	| RIf(expr, stmt1, stmt2, cloc) -> RIf(replace_constant env [] expr, pass2 env stmt1, pass2 env stmt2, cloc)
	| RTryFinally(stmt1, stmt2, cloc) -> RTryFinally(pass2 env stmt1, pass2 env stmt2, cloc)
	| RTryCatch(stmt1, v, stmt2, cloc) -> RTryCatch(pass2 env stmt1, v, pass2 env stmt2, cloc)
	| RSwitch(expr, stmts, cloc) -> RSwitch(replace_constant env [] expr,
				(List.map (fun stmt -> pass2 env stmt) stmts), cloc)
	| RForEach(v, expr, stmt, cloc) -> RForEach(v, replace_constant env [] expr, pass2 env stmt, cloc)

(**
Analyzes an AST, generates a runtime AST
@param ast a parsing AST
@return a tuple of the runtime AST and analysis environment
*)
let analyze ast =
	let analyze_all env ast =
		let (rast, env) = analyze_variables env ast
		in let (rast, env) =
			(rast, {
					globals = env.globals;
					num_globals = env.num_globals;
					locals = env.locals;
					num_locals = env.num_locals;
					sdepth = env.sdepth;
					max_depth = env.max_depth;
					errors = env.errors;
					warnings = env.warnings;
					unique_id = env.unique_id;
					names = List.rev env.names;
					varprops = env.varprops;
					imported = env.imported;
					templates = env.templates;
					constants = env.constants;
				})
		in let (rast, env) = (pass2 env rast, env)
		in let _ = check_errors env
		in let env = check_warnings env
		in (rast, env)
	in let env = Environment.new_analysis_environment()
	in let env = Library.register_for_analysis env
	in analyze_all env ast
