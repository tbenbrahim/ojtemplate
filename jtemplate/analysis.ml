(**
Create an optimized AST from the parsing phase AST

The first pass replaces all symbolic variable names with absolute locations,
either on a stack or in the global heap, detects and registers closure variables,
processes imports

The second pass replaces all non function variables whose value have not been modified
with a constant value. Example:
let a = 1; let a = 1;
let b = a + 1; -----> let b = 1 + 1;

The third pass replacess all evaluatable expressions with a value. Example:
let b = 1 + 1; -----> let b = 2;

The fourth pass eliminates any variable which is never read after it is declared,
and generates a warning. Also eliminates expression statements with no side effects
Example:
let a = 1;
let a = a + 1;
print("Hello world"); -----> print("Hello world");

expand varagrs

TODO detect use before intialization

@author Tony BenBrahim

*)

open Ast
open Environment
open Parser_util

(**
Prints all errors in an analysis environment and raises FatalExit if there are errors
@param env analysis environment
@returns unit
@raise FatalExit if there are errors in the environment
*)
let check_errors env =
	List.fold_left (fun _ error -> print_string ("ERROR: "^error^"\n")) () (List.rev env.errors);
	if List.length env.errors > 0 then raise (RuntimeError.FatalExit RuntimeError.AnalysisErrors)
	else ()

(**
******************************************************************************************
*
* FIRST PASS - resolve all variable references, including closure variables
*
******************************************************************************************
*)

(**
Filters an ast, returning only a list of declaration and import statement
@param stmts the statement list to process
@return a statement list containing only declarations and imports
*)
let filter_imported_ast stmts =
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
declared, a breadth first search is necessary before receusively processing children
statements
@param env an analysis environment
@param ast the intermediate ast
@return an ast where all variables have been resolved to an absolute location, either
on a stack or in the global heap and an environment containing information about all
variables
*)
let rec analyze_variables env ast =
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
						| Id(name) -> Environment.declare_variable name env
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
		| Instructions(_, _, _, _) | TemplateDef(_, _, _) -> raise(RuntimeError.InternalError "TODO") (*/*TODO*/*)
	(**
	Find all all variables in statement list whose stack depth is lower than that given
	@param stmt_list a list of statements to be searched recursively
	@param stack_depth the stack depth for the given statement
	@return Some map of unique id to Void of all variables found or None if none where found
	*)
	and get_closure_vars stmt_list stack_depth =
		let add uid = function
			| None -> let t = Hashtbl.create 2 in Hashtbl.replace t uid RUndefined; Some t
			| Some t -> Hashtbl.replace t uid RUndefined; Some t
		in
		let rec find_in_expr result = function
			| RVariable(LocalVar(uid, sdepth, _)) | RVarArg(LocalVar(uid, sdepth, _)) ->
					if sdepth = stack_depth then result else add uid result
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
			| RNoop | RContinue(_) | RCase(None, _) | RBreak(_) -> result
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
	Replaces all variables in expression with absolute locations
	Also sets up function definitions
	@param env analysis environment
	@param expr expression
	@param location in code
	@return new expression with variables replaced with absolute location
	*)
	and resolve_expr env expr cloc =
		let rec convert_value env = function
			| StringValue(v) -> (RStringValue(v), env)
			| IntegerValue(v) -> (RIntegerValue(v), env)
			| FloatValue(v) -> (RFloatValue(v), env)
			| BooleanValue(v) -> (RBooleanValue(v), env)
			| Void -> (RVoid, env)
			| NaN -> (RNaN, env)
			| MapValue(h, s) ->
					(RMapValue(Hashtbl.fold (fun k v h ->
											let (repl, env) = convert_value env v
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
					in let newenv = Environment.new_analysis_stackframe env
					in let newenv = Environment.declare_variable "this" newenv
					in let newenv = List.fold_left (fun env name -> Environment.declare_variable name env) newenv arg_list
					in let (stmt_list, newenv) = analyze_variables_in_block newenv stmt_list
					in let closure_vars = get_closure_vars stmt_list (Environment.get_depth newenv)
					in (RFunctionValue(List.hd newenv.num_locals, Environment.get_depth newenv, List.length arg_list, has_vararg, stmt_list, closure_vars), Environment.pop_scope newenv)
		and resolve_expr_sub env expr =
			match expr with
			| Id(name) ->
					let (_, loc) = Environment.resolve_variable name env
					in (RVariable(loc), env)
			| VarArg(name) ->
					let (_, loc) = Environment.resolve_variable name env
					in (RVarArg(loc), env)
			| BinaryOp(e1, op, e2) ->
					let (expr1, env) = resolve_expr_sub env e1
					in let (expr2, env) = resolve_expr_sub env e2
					in (RBinaryOp(expr1, op, expr2), env)
			| CompOp(e1, op, e2) ->
					let (expr1, env) = resolve_expr_sub env e1
					in let (expr2, env) = resolve_expr_sub env e2
					in (RCompOp(expr1, op, expr2), env)
			| Not(e) ->
					let (expr, env) = resolve_expr_sub env e
					in (RNot(expr), env)
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
						in resolve_expr_sub env (Value(FunctionValue(unbound_list [] el,[Return(FunctionCall(e, bound_list [] el), cloc)])))
					else
						let (expr, env) = resolve_expr_sub env e
						in let (expr_list, env) = List.fold_left(fun acc expr -> let (lst, env) = acc
											in let (expr, env) = resolve_expr_sub env expr in (expr:: lst, env)) ([], env) el
						in (RFunctionCall(expr, List.rev expr_list), env)
			| MapExpr(prop_list) ->
					let (prop_list, env) = List.fold_left(fun acc prop -> let (lst, env) = acc
										in let (name, expr) = prop
										in let (expr, env) = resolve_expr_sub env expr
										in ((name, expr):: lst, env)) ([], env) prop_list
					in (RMapExpr(List.rev prop_list), env)
			| ArrayExpr(el) ->
					let (expr_list, env) = List.fold_left(fun acc expr -> let (lst, env) = acc
										in let (expr, env) = resolve_expr_sub env expr
										in (expr:: lst, env)) ([], env) el
					in (RArrayExpr(List.rev expr_list), env)
			| Value(v) -> let (repl, env) = convert_value env v in (RValue(repl), env)
			| Assignment(e1, e2) ->
					let (expr1, env) = resolve_expr_sub env e1
					in let (expr2, env) = resolve_expr_sub env e2
					in (RAssignment(expr1, expr2), env)
			| Declaration(e1, e2) ->
					let (expr1, env) = resolve_expr_sub env e1
					in let (expr2, env) = resolve_expr_sub env e2
					in (RDeclaration(expr1, expr2), env)
			| TernaryCond(e1, e2, e3) ->
					let (e1, env) = resolve_expr_sub env e1
					in let (e2, env) = resolve_expr_sub env e2
					in let (e3, env) = resolve_expr_sub env e3
					in (RTernaryCond(e1, e2, e3), env)
			| MemberExpr(e1, e2) ->
					let (expr1, env) = resolve_expr_sub env e1
					in let (expr2, env) = resolve_expr_sub env e2
					in (RMemberExpr(expr1, expr2), env)
			| PostFixSum(e, inc) ->
					let (expr, env) = resolve_expr_sub env e
					in (RPostFixSum(expr, inc), env)
			| UnboundVar(_) ->
					(RValue(RVoid), Environment.add_error env cloc "Unexpected unbound var")
		in
		try
			resolve_expr_sub env expr
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
			(* if env.locals =[] && env.globals.parent = None then *)
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
			(* else let env = Environment.add_error env cloc "Import must be at  *)
			(* the top level of the global scope" in loop (Noop:: result) env tl *)
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
		(*in let (stmt_list, env) = *)in replace_variables_in_block env stmt_list
	(* in analyze_variables_nested_stmt env stmt_list [] *)
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
			in let (ast, newenv) = analyze_variables_in_block newenv stmt_list
			in let (expr, env) = resolve_expr env expr cloc
			in (RSwitch(expr, ast, cloc), Environment.pop_scope newenv)
	| TryCatch(stmt1, name, stmt2, cloc) ->
			let newenv = Environment.new_analysis_scope env
			in let (stmt1, newenv) = analyze_variables_in_stmt newenv stmt1
			in let newenv = Environment.new_analysis_scope (Environment.pop_scope newenv)
			in let newenv = Environment.declare_variable name newenv
			in let (stmt2, newenv) = analyze_variables_in_stmt newenv stmt2
			in let (_, loc) = Environment.resolve_variable name newenv
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
			let newenv = Environment.declare_variable name (Environment.new_analysis_scope env)
			in let (expr, newenv) = resolve_expr newenv expr cloc
			in let (stmt, newenv) = analyze_variables_in_stmt newenv stmt
			in let (_, loc) = Environment.resolve_variable name newenv
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
	| Import(_, cloc) -> (RNoop, env)
	| TemplateDef(_, _, _) | Instructions(_, _, _, _) -> (* TODO *)
			raise (RuntimeError.InternalError "unexpexted statement")

let analyze ast =
	let analyze_all env ast =
		let (ast, env) = analyze_variables env ast in
		check_errors env; (ast, env)
	in let env = Environment.new_analysis_environment()
	in let env = Library.register_for_analysis env
	in analyze_all env ast