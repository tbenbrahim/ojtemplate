(**
This program is free software; you can redistribute it and / or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

The Jtemplate interpreter

@author Tony BenBrahim < tony.benbrahim at gmail.com >

*)

open Ast
open Expression
open RuntimeError

(**
Interpret a runtime AST
@param env a runtime environment
@param ast the runtime AST to interpret
@return unit
*)
let rec interpret env = function
	| RProgram(stmts) | RStatementBlock(stmts) ->
			List.iter (fun stmt -> interpret env stmt) stmts
	| RExpressionStatement(expr, cloc) ->
			env.current_line <- cloc;
			let _ = evaluate env expr in ()
	| RReturn (expr, cloc) ->
			env.current_line <- cloc;
			let (_, value) = evaluate env expr
			in raise (CFReturn value)
	| RFor (preloop, condexpr, postloop, stmt, cloc) ->
			env.current_line <- cloc;
			let _ = evaluate env preloop
			in let rec loop () =
				let (env, value) = evaluate env condexpr
				in match value with
				| RBooleanValue(false) -> ()
				| RBooleanValue(true) | RVoid ->
						(try
							interpret env stmt
						with
						| CFContinue -> ()
						);
						let _ = evaluate env postloop
						in loop ()
				| value ->
						raise (EInvalidCast(string_of_value value,"boolean"))
			in (try
				loop ()
			with
			| CFBreak -> ())
	| RFastIterator (vloc, start, max, inc, stmt, cloc) ->
			env.current_line <- cloc;
			let rec loop ind =
				let _ = Environment.set_value env (RIntegerValue(ind)) vloc
				in if ind < max then
					((try
							interpret env stmt
						with
						| CFContinue -> ()
						);
						loop (ind + inc))
				else
					()
			in (try
				loop start
			with
			| CFBreak -> ())
	| RForEach (vloc, expr, stmt, cloc) ->
			env.current_line <- cloc;
			let list =
				let (env, value) = evaluate env expr
				in match value with
				| RMapValue(h, MapSubtype) -> Hashtbl.fold (fun k v lst -> RStringValue(k):: lst) h []
				| RMapValue(_, ArraySubtype) as v -> list_of_array v
				| _ as v -> raise(ENotACollectionType("the second argument of forEach", string_of_value_type v))
			in let rec loop = function
				| [] -> ()
				| hd:: tl ->
						let _ = Environment.set_value env hd vloc
						in (try interpret env stmt with CFContinue -> ());
						loop tl
			in
			(try loop list with CFBreak -> () )
	| RIf(condexpr, if_stmt, else_stmt, cloc) ->
			env.current_line <- cloc;
			let (env, value) = evaluate env condexpr
			in (match value with
				| RBooleanValue(true) -> interpret env if_stmt
				| RBooleanValue(false) -> interpret env else_stmt
				| value -> raise (EInvalidCast(string_of_value value,"boolean"))
			)
	| RSwitch(expr, stmtlist, cloc) ->
			env.current_line <- cloc;
			let rec find_cases caselist defaultfound = function
				| []-> caselist
				| RCase(Some expr, _):: tl ->
						if defaultfound then
							raise EDefaultCaseShouldBeLast
						else(
							let caselist = (Some expr, tl):: caselist in
							find_cases caselist false tl)
				| RCase(None, _):: tl ->
						if defaultfound then
							raise EDefaultCaseShouldBeLast
						else(
							let caselist = (None, tl):: caselist in
							find_cases caselist true tl)
				| _:: tl -> find_cases caselist defaultfound tl
			(* match a value with a case and return a statement list *)
			in let rec match_case expr1 = function
				| [] -> []
				| (Some expr2, stmts):: tl ->
						let (env, value) = evaluate env (RCompOp(expr1, Equal, expr2))
						in if value = RBooleanValue(true) then
							stmts
						else
							match_case expr1 tl
				| (None, stmts):: tl -> stmts
			in let caselist = List.rev (find_cases [] false stmtlist)
			in let (env, value) = evaluate env expr
			in let stmts = match_case (RValue(value)) caselist
			in (try
				List.iter (fun stmt -> interpret env stmt) stmts
			with
			| CFBreak -> ())
	| RTryCatch(stmt1, vloc, stmt2, cloc) ->
			env.current_line <- cloc;
			(try
				interpret env stmt1;
			with
			| CFUserException(e, _) ->
					let _ = Environment.set_value env e vloc
					in interpret env stmt2
			| CFBreak | CFContinue as exn -> raise exn
			| CFReturn(v) -> raise (CFReturn v)
			| exn ->
					let _ = Environment.set_value env (RStringValue(RuntimeError.string_of_error exn)) vloc
					in interpret env stmt2
			)
	| RTryFinally(stmt1, stmt2, cloc) ->
			env.current_line <- cloc;
			(try
				interpret env stmt1
			with
			| _ as e -> interpret env stmt2; raise e)
	| RContinue(cloc) ->
			env.current_line <- cloc;
			raise CFContinue
	| RBreak(cloc) ->
			env.current_line <- cloc;
			raise CFBreak;
	| RThrow(expr, cloc) ->
			env.current_line <- cloc;
			let (_, value) = evaluate env expr
			in raise (CFUserException(value, string_of_value value))
	| RNoop | RCase(_, _) -> ()

(**
Interprets a list of statements
@param env runtime environments
@param stmts list of statements
*)
and interpret_stmts env = function
	| [] -> ()
	| stmt:: tl ->
			interpret env stmt; interpret_stmts env tl
(**
Evaluates an expression
@param env runtime environment
@param expr expression to be evaluated
@return a value
*)
and evaluate env = function
	| RVariable(loc) -> (env, Environment.get_value env loc)
	| RValue(v) ->
			(match v with (* look for closure vars*)
				| RFunctionValue(framesize, depth, argslen, has_varargs, statements, Some closure_vars, _) ->
						let closure_vals = Hashtbl.create 10
						in let _ = Hashtbl.fold(
									fun k _ _ -> let (d, i) = k
											in let (_, value) = evaluate env (RVariable(LocalVar(0, d, i)))
											in Hashtbl.replace closure_vals (d, i) value) closure_vars ()
						in (env, RFunctionValue(framesize, depth, argslen, has_varargs, statements, Some closure_vals, None))
				| _ -> (env, v))
	| RPostFixSum(expr, inc) ->
			let (env, v) = evaluate env expr
			in let _ = evaluate env (RAssignment(expr, RBinaryOp(RValue(v), Plus, RValue(RIntegerValue(inc)))))
			in (env, v)
	| RArrayExpr(expr_list) ->
			let value_list = List.map (fun e -> let (_, v) = evaluate env e in v) expr_list
			in let len = (List.length value_list)
			in let t = Hashtbl.create len
			in let _ = List.fold_left (fun ind v -> Hashtbl.add t (string_of_int ind) v; ind + 1) 0 value_list
			in let _ = Hashtbl.add t "length" (RIntegerValue(len))
			in (env, RMapValue(t, ArraySubtype))
	| RMapExpr(prop_list) ->
			let t = Hashtbl.create (List.length prop_list)
			in let _ = List.iter(fun prop -> let (name, e) = prop in Hashtbl.add t name (let (_, v) = evaluate env e in v)) prop_list
			in (env, RMapValue(t, MapSubtype))
	| RBinaryOp(e1, op, e2) ->
			let (env, v1) = evaluate env e1
			in let (env, v2) = evaluate env e2
			in (env, evaluate_op v1 v2 op)
	| RCompOp(e1, op, e2) ->
			let (env, v1) = evaluate env e1
			in let (env, v2) = evaluate env e2
			in (env, compare v1 op v2)
	| RTernaryCond(e1, e2, e3) ->
			let (env, value) = evaluate env e1
			in (match value with
				| RBooleanValue(true) -> evaluate env e2
				| RBooleanValue(false) -> evaluate env e3
				| v -> raise (EIncompatibleTypes ("boolean" , string_of_value_type v)))
	| RMemberExpr(left, index) ->
			let (env, left_map) = evaluate env left
			in let (key, is_int) = evaluate_memb_expr_index env index
			in (match left_map with
				| RMapValue(h, ArraySubtype) ->
						if (not is_int) then
							raise (EInvalidArrayIndex("string", key))
						else
							(try
								(env, Hashtbl.find h key)
							with Not_found ->
									raise (EArrayIndexOutOfBounds key))
				| RMapValue(h, MapSubtype) ->
						(try
							(env, Hashtbl.find h key)
						with Not_found ->
								raise (EUndefinedMapMember key))
				| _ -> raise (ELeftSideIsNotAMap(string_of_value_type left_map, string_of_value left_map)))
	| RNot(expr) ->
			let (env, v) = evaluate env expr in
			(match v with
				| RBooleanValue(b) -> (env, RBooleanValue(not b))
				| _ -> raise (EIncompatibleTypes(string_of_value_type v, "boolean"))
			)
	| RDeclaration(left, right) ->
			let (env, value) = evaluate env right in
			(match left with
				| RVariable(loc) -> (env, Environment.set_value env value loc)
				| RMemberExpr(expr, key) ->
						let (h, index) = get_member_expr_map env expr key
						in Hashtbl.replace h index value; (env, value)
				| _ -> raise ELeftSideCannotBeAssigned
			)
	| RAssignment(left, right) ->
			let (env, value) = evaluate env right in
			(match left with
				| RVariable(loc) ->
						let oldvalue = Environment.get_value env loc
						in (if value_type oldvalue = value_type value then
								(env, (Environment.set_value env value loc))
							else
								raise ( ETypeMismatchInAssignment(Environment.get_loc_name env loc, string_of_value_type oldvalue, string_of_value_type value)))
				
				| RMemberExpr(expr, key) ->
						let (h, index) = get_member_expr_map env expr key
						in let oldvalue = Hashtbl.find h index
						in (if value_type oldvalue = value_type value then
								(Hashtbl.replace h index value; (env, value))
							else
								raise ( ETypeMismatchInAssignment(index, string_of_value_type oldvalue, string_of_value_type value)))
				| _ -> raise ELeftSideCannotBeAssigned
			)
	| RFunctionCall(fexpr, args_expr) ->
			let (this, func) = resolve_func_this env fexpr
			in let value_list = (evaluate_expr_list env args_expr)
			in run_function env value_list this func
	| RVarArg(_) ->
			raise (RuntimeError.InternalError "unexpected expression in evaluate")
(**
Resolves a function call by an expression into a function and a this object
@param env runtime environment
@param fexpr the expression to analyze
@return a tuple of the this object and the function
*)
and resolve_func_this env fexpr =
	let find_prototype h =
		match Hashtbl.find h "prototype" with
		| RMapValue(h, MapSubtype) -> h
		| _ -> raise Not_found
	in let find_func h name =
		match Hashtbl.find h name with
		| RFunctionValue(_, _, _, _, _, _, _) | RLibraryFunction(_) as v -> v
		| _ -> raise Not_found
	in let rec find_map_func h = function
		| "prototype":: tl -> find_map_func (find_prototype h) tl
		| name:: tl -> find_func h name
		| [] -> raise (RuntimeError.InternalError "map function find")
	in match fexpr with
	| RMemberExpr(this_expr, funcname) ->
			let (env, this) = evaluate env this_expr
			in let f = match this with
				| RUndefined -> raise (RuntimeError.InternalError "unexpected undefined this in function resolution")
				| RStringValue(_) ->
						let (env, v) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(1, 1)), RValue(RStringValue("prototype"))), funcname))
						in v
				| RIntegerValue(_) ->
						let (env, v) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(2, 2)), RValue(RStringValue("prototype"))), funcname))
						in v
				| RFloatValue(_) ->
						let (env, v) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(3, 3)), RValue(RStringValue("prototype"))), funcname))
						in v
				| RBooleanValue(_) ->
						let (env, v) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(4, 4)), RValue(RStringValue("prototype"))), funcname))
						in v
				| RFunctionValue(_, _, _, _, _, _, _) | RLibraryFunction(_) ->
						let (env, v) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(5, 5)), RValue(RStringValue("prototype"))), funcname))
						in v
				| RVoid ->
						let (env, v) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(6, 6)), RValue(RStringValue("prototype"))), funcname))
						in v
				| RNaN ->
						let (env, v) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(7, 7)), RValue(RStringValue("prototype"))), funcname))
						in v
				| RMapValue(_, ArraySubtype) ->
						let (env, v) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(8, 8)), RValue(RStringValue("prototype"))), funcname))
						in v
				| RMapValue(h, MapSubtype) ->
						let (env, value) = evaluate env funcname
						in let name = string_of_value value
						in try find_func h name
						with | Not_found ->
								try find_map_func h ["prototype"; name]
								with | Not_found ->
										try find_map_func h ["prototype";"prototype"; name]
										with | Not_found ->
												try
													let (env, value) = evaluate env (RMemberExpr(RMemberExpr(RVariable(GlobalVar(9, 9)), RValue(RStringValue("prototype"))), funcname))
													in value
												with EUndefinedMapMember _ -> raise (EUndefinedMapMember name)
			in (this, f)
	| _ ->
			let (env, v) = evaluate env fexpr
			in (RVoid, v)
(**
Runs a function
@param env runtime environment
@value_list list of values to pass as arguments
@this this pointer
@func function
@return a tuple of the environemt and return value
*)
and run_function env value_list this func =
	match func with
	| RFunctionValue(framesize, depth, argslen, vararg, stmts, closure_vars, _) ->
			let old_frame = env.stackframes.(depth)
			in let _ = env.stackframes.(depth) <- make_stackframe framesize argslen vararg value_list this
			in let old_closure_vars = env.closure_vars
			in let old_skip_callstack_pop = env.skip_callstack_pop
			in let _ = env.closure_vars <- closure_vars
			in (try
				(if Stack.is_empty env.callstack or Stack.top env.callstack!= env.current_line then
						(Stack.push env.current_line env.callstack;
							env.skip_callstack_pop <- false)
					else
						env.skip_callstack_pop <- true
				);
				interpret_stmts env stmts;
				(if env.skip_callstack_pop then ()
					else let _ = Stack.pop env.callstack in ());
				env.skip_callstack_pop <- old_skip_callstack_pop;
				env.stackframes.(depth) <- old_frame;
				env.closure_vars <- old_closure_vars;
				(env, RVoid)
			with
			| CFReturn value ->
					(if env.skip_callstack_pop then ()
						else let _ = Stack.pop env.callstack in ());
					env.skip_callstack_pop <- old_skip_callstack_pop;
					env.stackframes.(depth) <- old_frame;
					env.closure_vars <- old_closure_vars;
					(env, value)
			| ex ->
					(if env.skip_callstack_pop then ()
						else let _ = Stack.pop env.callstack in ());
					env.skip_callstack_pop <- old_skip_callstack_pop;
					env.stackframes.(depth) <- old_frame;
					env.closure_vars <- old_closure_vars;
					raise ex)
	| RLibraryFunction(def) ->
			let old_frame = env.stackframes.(0)
			in let old_skip_callstack_pop = env.skip_callstack_pop
			in env.stackframes.(0) <- make_stackframe def.num_args def.num_args def.vararg value_list this;
			(try
				(if Stack.is_empty env.callstack or Stack.top env.callstack!= env.current_line then
						(Stack.push env.current_line env.callstack;
							env.skip_callstack_pop <- false)
					else
						env.skip_callstack_pop <- true
				);
				def.code env;
				(if env.skip_callstack_pop then ()
					else let _ = Stack.pop env.callstack in ());
				env.skip_callstack_pop <- old_skip_callstack_pop;
				env.stackframes.(0) <- old_frame;
				(env, RVoid)
			with
			| CFReturn value ->
					(if env.skip_callstack_pop then ()
						else let _ = Stack.pop env.callstack in ());
					env.skip_callstack_pop <- old_skip_callstack_pop;
					env.stackframes.(0) <- old_frame;
					(env, value)
			| ex ->
					(if env.skip_callstack_pop then ()
						else let _ = Stack.pop env.callstack in ());
					env.skip_callstack_pop <- old_skip_callstack_pop;
					env.stackframes.(0) <- old_frame;
					raise ex)
	| _ -> raise ENotAFunction

(**
Determines the value and type of expression for the last member of a member expression
@param env the runtime environment
@param index the expression to evaluate
@return a tuple with the index of the expression and a boolean indicating
whether it is an integer
*)
and evaluate_memb_expr_index env index =
	(match evaluate env index with
		| (_, RStringValue(s)) -> (s, false)
		| (_, RIntegerValue(i)) -> (string_of_int i, true)
		| (_, v) -> raise (EInvalidMember(string_of_value_type v, string_of_value v))
	)
(**
Returns the hashmap that corresponds to the member expression
@env the runtime environment
@param expr the member expression (without the last member)
@param index the index (the last member of the member expression)
@return the hashmap that corresponds to the member expression
*)
and get_member_expr_map env expr index =
	let (env, left) = evaluate env expr
	in let (index, is_int) = evaluate_memb_expr_index env index
	in (match left with
		| RMapValue(h, ArraySubtype) ->
				(if not is_int then raise (EInvalidArrayIndex("string", index))
					else
						try
							let _ = Hashtbl.find h index in (h, index)
						with
						| Not_found -> raise (EArrayIndexOutOfBounds index)
				)
		| RMapValue(h, MapSubtype) -> (h, index)
		| _ -> raise (ELeftSideIsNotAMap(string_of_value_type left, string_of_value left))
	)
(**
Evaluates a list of expressions
@param env the runtime environment
@param expr_list an expression list
@param a list of the corresponding value for each expression
*)
and evaluate_expr_list env expr_list =
	let rec loop result = function
		| [] -> List.rev result
		| RVarArg(loc):: tl ->
				loop (List.concat [
							(let (env, v) = evaluate env (RVariable(loc))
								in match v with
								| RMapValue(_, ArraySubtype) as arr -> List.rev (list_of_array arr)
								| _ -> raise (RuntimeError.InternalError "expected array while expanding args")); result]) tl
		| expr:: tl -> let (env, v) = evaluate env expr in loop (v:: result) tl
	in loop [] expr_list
