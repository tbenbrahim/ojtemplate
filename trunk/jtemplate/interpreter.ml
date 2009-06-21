module Interpreter =
struct
	open Ast
	open Symbol_table
	open RuntimeError
	
	exception EIncompatibleTypes of string * string (* type1, type2 *)
	exception EInvalidCast of string * string (* value, typename *)
	exception EInvalidOperation of Ast.operator * string  (* operator, typename *)
	exception EInvalidComparaison of Ast.comparator * string (* comparator, typename *)
	exception EMismatchedTypeInCompare of string * string (* type1, type2 *)
	exception ENotAFunction of string (* variable name *)
	exception ENoScopeInFunctionCall
	
	(* control flow exceptions *)
	exception CFReturn of variable_value
	exception CFBreak
	exception CFContinue
	
	type cast_type = | IntegerCast of int * int | FloatCast of float * float | StringCast of string * string | BoolCast of bool * bool
	
	let cast_to_bool value =
		match value with
		| BooleanValue(b) -> b
		| _ -> raise (EInvalidCast (SymbolTable.string_of_symbol_value value,"boolean"))
	
	let cast_to_string value =
		SymbolTable.string_of_symbol_value value
	
	let cast_to_integer value =
		match value with
		| IntegerValue(i) -> i
		| _ -> raise (EInvalidCast (SymbolTable.string_of_symbol_value value,"integer"))
	
	let cast_to_float value =
		match value with
		| FloatValue(f) -> f
		| IntegerValue(i) -> float_of_int i
		| _ -> raise (EInvalidCast (SymbolTable.string_of_symbol_value value,"float"))
	
	let casting_type value1 value2 =
		let type1 = SymbolTable.value_type value1 in
		let type2 = SymbolTable.value_type value2 in
		if type1 = SymbolTable.StringType || type2 = SymbolTable.StringType then
			StringCast(cast_to_string value1, cast_to_string value2)
		else if (type1 = SymbolTable.FloatType && (type2 = SymbolTable.IntegerType || type2 = SymbolTable.FloatType))
		or (type2 = SymbolTable.FloatType && (type1 = SymbolTable.IntegerType || type1 = SymbolTable.FloatType)) then
			FloatCast(cast_to_float value1, cast_to_float value2)
		else if type1 = SymbolTable.IntegerType && type2 = SymbolTable.IntegerType then
			IntegerCast(cast_to_integer value1, cast_to_integer value2)
		else if type1 = SymbolTable.BooleanType && type2 = SymbolTable.BooleanType then
			BoolCast(cast_to_bool value1, cast_to_bool value2)
		else
			raise (EIncompatibleTypes(SymbolTable.string_of_symbol_type value1,
						SymbolTable.string_of_symbol_type value2))
	
	(** given a function call's arglist, resolve to either a
	list of values if the arglist has no unbound values, or	a list of expression (values are Value, UnboundVars are Variable)
	if the arglist has unbound values. Invoked with arglist, returns (boolean, ((exprlist, formal_args, has_vararg), valuelist,)
	if the boolean is true, unbound names were found and exprlist	should be used. if the boolean is false, all names were bound
	and valuelist should be used. formal_args will contain a list of unbound names. has_vararg will be true if a vararg was found *)
	let rec resolve_arg_list arglist symbol_table =
		let rec resolve arglist exprlist valuelist formal_args has_unbound has_vararg =
			match arglist with
			| [] -> (has_unbound, (List.rev exprlist, List.rev formal_args, has_vararg), List.rev valuelist)
			| expr:: tl ->
					match expr with
					| UnboundVar(name) -> (*from this point on, valuelist is no longer valid *)
							let varname = SymbolTable.fullname name in
							let isvararg = is_vararg varname in
							let formalname = (if isvararg then vararg_formalname varname else varname) in
							(if isvararg && tl <>[] then raise RuntimeError.VarArgsMustbeLast else ());
							resolve tl (VariableExpr(Name(formalname)):: exprlist) (valuelist) (name:: formal_args) true isvararg
					| _ ->
							let value = evaluate_expression expr symbol_table
							in resolve tl (Value(value):: exprlist) (value:: valuelist) formal_args has_unbound false
		in resolve arglist [] [] [] false false
	
	and make_map str_expr_list symbol_table =
		let map = Hashtbl.create (1 + List.length str_expr_list) in
		List.fold_left
			(fun acc el ->
						let (name, expr) = el in
						Hashtbl.replace map name (evaluate_expression expr symbol_table)
			) () str_expr_list;
		map
	and
	make_array expr_list symbol_table =
		let map = Hashtbl.create (1 + List.length expr_list) in
		let lastindex = List.fold_left
				(fun index expr ->
							Hashtbl.replace map (string_of_int index) (evaluate_expression expr symbol_table);
							index + 1) 0 expr_list in
		Hashtbl.replace map "length" (IntegerValue(lastindex));
		map
	and
	compare_same_type v1 op v2 =
		BooleanValue(
			match op with
			| Equal -> v1 = v2
			| NotEqual -> v1 <> v2
			| LessThan -> v1 < v2
			| LessThanEqual -> v1 <= v2
			| GreaterThan -> v1 > v2
			| GreaterThanEqual -> v1 >= v2)
	and
	restricted_compare v1 op v2 =
		BooleanValue(
			match v1 with
			| MapValue(h, ArraySubtype) -> (SymbolTable.list_of_array v1) = (SymbolTable.list_of_array v2)
			| _ ->
					match op with
					| Equal -> v1 = v2
					| NotEqual -> v1 <> v2
					| _ -> raise (EInvalidComparaison(op, SymbolTable.string_of_symbol_type v1))
		)
	and	resolve_variable_name varname symbol_table =
		match varname with
		| Name(_) -> varname
	(* and addFunctionScope value symbol_table = match value with |          *)
	(* FunctionValue(args, stmts) -> ScopedFunctionValue(args, stmts,        *)
	(* symbol_table) | v -> v                                                *)
	and evaluate_memb_expr_index index symbol_table =
		(match index with
			| Id(name) -> (name, false)
			| IndexExpr(expr) -> (match evaluate_expression expr symbol_table with
						| StringValue(s) -> (s, false)
						| IntegerValue(i) -> (string_of_int i, true)
						| _ as v -> raise (InvalidMember(SymbolTable.string_of_symbol_type v,
											cast_to_string v)))
			| _ -> raise (RuntimeError.InternalError "unexpeted index, not an Id and not an IndexExpr")
		)
	and get_lhs expr index symbol_table =
		let left = evaluate_expression expr symbol_table
		in let (index, int) = evaluate_memb_expr_index index symbol_table
		in (match left with
			| MapValue(h, ArraySubtype) -> (if not int then raise (InvalidArrayIndex("string", index))
						else
							try
								let _ = Hashtbl.find h index in (h, index)
							with
							| Not_found -> raise (ArrayIndexOutOfBounds index)
					)
			| MapValue(h, MapSubtype) -> (h, index)
			| _ -> raise (LeftSideIsNotAMap(SymbolTable.string_of_symbol_type left,
								cast_to_string left))
		)
	and get_evaluated_lhs expr index symbol_table =
		let left = evaluate_expression expr symbol_table
		in let (index, int) = evaluate_memb_expr_index index symbol_table
		in (match left with
			| MapValue(h, ArraySubtype) -> (if not int then raise (InvalidArrayIndex("string", index))
						else
							try
								let _ = Hashtbl.find h index in (h, index)
							with
							| Not_found -> raise (ArrayIndexOutOfBounds index)
					)
			| MapValue(h, MapSubtype) -> (h, index)
			| _ -> raise (LeftSideIsNotAMap(SymbolTable.string_of_symbol_type left,
								cast_to_string left))
		)
	and evaluate_expression expr symbol_table =
		match expr with
		| Id(name) -> SymbolTable.get_value (Name(name)) symbol_table
		| IndexExpr(expr) -> raise (RuntimeError.InternalError "index expression evaluated outside of member expression")
		| MemberExpr(expr, key) ->
		(** left side has to evaluate to a map, right side must be a string or integer or Id *)
				let (h, index) = get_evaluated_lhs expr key symbol_table
				in(
					try
						Hashtbl.find h index
					with
					| Not_found -> raise (UndefinedMapMember index )
				)
		| Assignment(left, right) ->
				let v = evaluate_expression right symbol_table in
				(match left with
					| Id(name) -> SymbolTable.assign (Name(name)) v symbol_table; v
					| MemberExpr(expr, key) ->
							let (h, index) = get_lhs expr key symbol_table
							in let oldvalue = Hashtbl.find h index
							in (if SymbolTable.value_type oldvalue = SymbolTable.value_type v then (Hashtbl.replace h index v; v)
								else raise ( TypeMismatchInAssignment(index, SymbolTable.string_of_symbol_type oldvalue, SymbolTable.string_of_symbol_type v)))
					| _ -> raise LeftSideCannotBeAssigned
				)
		| Declaration(left, right) ->
				let v = evaluate_expression right symbol_table in
				(match left with
					| Id(name) -> SymbolTable.declare (Name(name)) v symbol_table; v
					| MemberExpr(expr, key) ->
							let (h, index) = get_lhs expr key symbol_table
							in Hashtbl.replace h index v; v
					| _ -> raise LeftSideCannotBeAssigned
				)
		| PostFixSum(expr, incr) ->
				let value = let x = evaluate_expression expr symbol_table in
					(match x with
						| IntegerValue(i) -> i
						| _ -> raise (RuntimeError.NotAnInteger "expected an integer in postfix operation"))
				in let _ = evaluate_expression (Assignment(expr, (Value(IntegerValue(value + incr))))) symbol_table in
				IntegerValue(value)
		| FunctionCall(expr, args) ->
				let (has_unbound, (exprlist, formal_args, has_vararg), value_list) = resolve_arg_list args symbol_table in
				if has_unbound then
					let stmts =
						(if has_vararg then
								let varargname = vararg_formalname (SymbolTable.fullname (List.hd (List.rev formal_args))) in
								[Return(FunctionCallExpandVarArg(expr, exprlist, varargname), symbol_table.env.current_stmt)]
							else
								[Return(FunctionCall(expr, exprlist), symbol_table.env.current_stmt)]) in
					ScopedFunctionValue(formal_args, stmts, symbol_table)
				else
					let run_func f this =
						(match f with
							| FunctionValue(_, _) -> raise (InternalError "unscoped function invoked")
							| ScopedFunctionValue(arglist, stmts, scope) ->
									let old_stack = symbol_table.env.stack_trace in
									(try
										symbol_table.env.stack_trace <- symbol_table.env.current_stmt:: symbol_table.env.stack_trace;
										interpret_statements stmts (SymbolTable.new_function_call_scope scope (Name("this"):: arglist) (this:: value_list));
										symbol_table.env.stack_trace <- old_stack;
										Void
									with
									| CFReturn value -> symbol_table.env.stack_trace <- old_stack; value)
							| LibraryFunction( arglist , code, scope) ->
									(try
										code (SymbolTable.new_function_call_scope scope (Name("this"):: arglist) (this:: value_list));
										Void
									with
									| CFReturn value -> value)
							| _ -> raise RuntimeError.NotAFunction
						)
					in let (f, this) =
						(match expr with
							| MemberExpr(left, key) ->
									let (index, int) = evaluate_memb_expr_index key symbol_table in
									let v = evaluate_expression left symbol_table in
									(match v with
										| StringValue(_) ->
												(evaluate_expression (MemberExpr(MemberExpr(Id("String"), Id("prototype")), Id(index))) symbol_table, v)
										| IntegerValue(_) ->
												(evaluate_expression (MemberExpr(MemberExpr(Id("Integer"), Id("prototype")), Id(index))) symbol_table, v)
										| NaN | FloatValue(_) ->
												(evaluate_expression (MemberExpr(MemberExpr(Id("Float"), Id("prototype")), Id(index))) symbol_table, v)
										| BooleanValue(_) ->
												(evaluate_expression (MemberExpr(MemberExpr(Id("Boolean"), Id("prototype")), Id(index))) symbol_table, v)
										| Void -> (evaluate_expression (MemberExpr(MemberExpr(Id("Void"), Id("prototype")), Id(index))) symbol_table, Void)
										| ScopedFunctionValue(_, _, _) | FunctionValue(_, _) | LibraryFunction(_, _, _) ->
												(evaluate_expression (MemberExpr(MemberExpr(Id("Function"), Id("prototype")), Id(index))) symbol_table, v)
										| MapValue(h, ArraySubtype) ->
												(if not int then
														(evaluate_expression (MemberExpr(MemberExpr(Id("Array"), Id("prototype")), key)) symbol_table, v)
													else
														(try (Hashtbl.find h index, Void)
														with | Not_found -> raise (ArrayIndexOutOfBounds index)
														)
												)
										| MapValue(h, MapSubtype) ->
												let rec mapExists h lst =
													(match lst with
														| hd::[] -> (try (true, Hashtbl.find h hd) with | Not_found -> (false, Void))
														| hd:: tl -> (try (match Hashtbl.find h hd with | MapValue(m, MapSubtype) -> mapExists m tl | _ -> (false, Void))
																with | Not_found -> (false, Void))
														| _ -> (false, Void))
												in
												let (exists, fv) = mapExists h [index] in
												if exists then (fv, Void)
												else let (exists, fv) = mapExists h ["prototype";"prototype"; index] in
													if exists then (fv, v)
													else (evaluate_expression (MemberExpr(MemberExpr(Id("Map"), Id("prototype")), key)) symbol_table, v)
									)
							| _ -> (evaluate_expression expr symbol_table, Void)
						)
					in run_func f this
		| FunctionCallExpandVarArg(variable, exprlist, varargname) ->
		(* get the vararg value array, remove the append the values to the     *)
		(* expression list (minus the last, which is the vararg array name),   *)
		(* and call the function                                               *)
				let (last, start) = match List.rev exprlist with
					| [] -> raise (InternalError "expected at least the vararg variable in the expression list")
					| last:: start -> (last, start) in
				let arr = SymbolTable.get_value (Name varargname) symbol_table in
				let value_list = SymbolTable.list_of_array arr in
				let v_exprlist = List.map (fun value -> Value(value)) value_list in
				evaluate_expression (FunctionCall(variable, (List.append (List.rev start) v_exprlist))) symbol_table
		| MapExpr(str_expr_list) ->
				MapValue(make_map str_expr_list symbol_table, MapSubtype)
		| ArrayExpr(expr_list) ->
				MapValue(make_array expr_list symbol_table, ArraySubtype)
		| VariableExpr(variable) ->
				SymbolTable.get_value (resolve_variable_name variable symbol_table) symbol_table
		| Value(value) -> (match value
					with
					| FunctionValue(arglist, stmts) -> ScopedFunctionValue(arglist, stmts, symbol_table) | _ -> value)
		| UnboundVar(name) -> raise (UnexpectedUnboundVar (SymbolTable.fullname name))
		
		| Not(expr) ->
				let v = evaluate_expression expr symbol_table in
				(match v with
					| BooleanValue(b) -> BooleanValue(not b)
					| _ -> raise (EIncompatibleTypes(SymbolTable.string_of_symbol_type v,
										"boolean"))
				)
		| BinaryOp(expr1 , operator , expr2) ->
				let value1 = evaluate_expression expr1 symbol_table in
				let value2 = evaluate_expression expr2 symbol_table in
				(match casting_type value1 value2 with
					| BoolCast(b1, b2) ->
							(match operator with
								| And -> BooleanValue(b1 && b2)
								| Or -> BooleanValue(b1 || b2)
								| _ -> raise (EInvalidOperation (operator,"boolean"))
							)
					| StringCast(s1, s2) ->
							(match operator with
								| Plus -> StringValue(s1 ^ s2)
								| _ -> raise (EInvalidOperation (operator,"string"))
							)
					| FloatCast(f1, f2) -> (let f = (match operator with
										| Plus -> f1 +. f2
										| Minus -> f1 -. f2
										| Times -> f1 *. f2
										| Divide -> f1 /. f2
										| _ -> raise (EInvalidOperation (operator,"float"))) in
								if f = infinity || f = neg_infinity || f = nan then NaN
								else FloatValue(f)
							)
					| IntegerCast(i1, i2) -> (match operator with
								| Plus -> IntegerValue( i1 + i2 )
								| Minus -> IntegerValue( i1 - i2)
								| Times -> IntegerValue( i1 * i2)
								| Divide -> if i2 <> 0 then IntegerValue( i1 / i2) else NaN
								| Modulo -> if i2 <> 0 then IntegerValue( i1 mod i2) else NaN
								| _ -> raise (EInvalidOperation (operator,"integer"))
							)
				)
		| CompOp (expr1, comparator, expr2) ->
				let value1 = evaluate_expression expr1 symbol_table in
				let value2 = evaluate_expression expr2 symbol_table in
				let type1 = SymbolTable.value_type value1 in
				let type2 = SymbolTable.value_type value2 in
				(if type1 = type2 then
						match type1 with
						| SymbolTable.IntegerType | SymbolTable.StringType | SymbolTable.FloatType -> compare_same_type value1 comparator value2
						| SymbolTable.BooleanType | SymbolTable.MapType | SymbolTable.FunctionType
						| SymbolTable.LibraryCallType | SymbolTable.VoidType | SymbolTable.NaNType
						| SymbolTable.ArrayType -> restricted_compare value1 comparator value2
					else
						try
							match casting_type value1 value2 with
							| FloatCast(f1, f2) -> compare_same_type (FloatValue(f1)) comparator (FloatValue(f2))
							| _ ->	BooleanValue(false)
						with
						| EIncompatibleTypes(_, _) -> BooleanValue(false)
				)
	and
	evaluate_exprs exprlist symbol_table =
		List.map (fun expr -> evaluate_expression expr symbol_table ) exprlist
	and
	stack_trace = function
		| [] -> ()
		| (file, line):: tl ->
				prerr_string ("Called from line "^(string_of_int line)^" in file "^ (Filename.basename file)^":\n");
				stack_trace tl
	and
	interpret_program statement symbol_table =
		try
			interpret_statement statement symbol_table
		with
		(**		| CFReturn v -> raise (CFReturn(v))
		| CFContinue -> raise CFContinue
		| CFBreak -> raise CFBreak *)
		| FatalExit e -> raise (FatalExit e)
		| e ->
				flush_all ();
				let (file, line) = symbol_table.env.current_stmt in
				prerr_string ("At line "^(string_of_int line)^" in file "^ (Filename.basename file)^": ");
				prerr_string (RuntimeError.string_of_error e); prerr_newline();
				stack_trace symbol_table.env.stack_trace;
				flush_all();
				raise (FatalExit e)
	and
	interpret_statement statement symbol_table =
		match statement with
		| TryCatch(tryblock, excp, catchblock, env) ->
				symbol_table.env.current_stmt <- env;
				(try
					interpret_statement tryblock symbol_table
				with
				| UserException e ->
						let s = SymbolTable.push_scope symbol_table in
						SymbolTable.declare excp e s;
						interpret_statement catchblock s
				| exn ->
						let s = SymbolTable.push_scope symbol_table in
						SymbolTable.declare excp (StringValue(string_of_error exn)) s;
						interpret_statement catchblock s
				)
		| TryFinally(stmt, finally, env) ->
				symbol_table.env.current_stmt <- env;
				(try
					interpret_statement stmt symbol_table;
					interpret_statement finally symbol_table;
				with
				| exn -> interpret_statement finally symbol_table; raise exn
				)
		| Throw(expr, env) ->
				symbol_table.env.current_stmt <- env;
				let e = evaluate_expression expr symbol_table in
				raise (UserException e);
		| ExpressionStatement(expression, env) ->
				symbol_table.env.current_stmt <- env;
				let _ = evaluate_expression expression symbol_table in ()
		| Noop -> ()
		| For (preloop, condexpr, postloop, stmt, env) ->
				symbol_table.env.current_stmt <- env;
				(try
					let symbol_table = SymbolTable.push_scope symbol_table in
					let _ = evaluate_expression preloop symbol_table in
					let runloop () =
						match (evaluate_expression condexpr symbol_table) with
						| BooleanValue(true) | Void -> true
						| BooleanValue(false) -> false
						| value ->
								raise (EInvalidCast(SymbolTable.string_of_symbol_value value,"boolean")) in
					let rec loop () = (
							if runloop () then
								((try
										interpret_statement stmt symbol_table
									with
									| CFContinue -> ());
									let _ = evaluate_expression postloop symbol_table in
									loop()
								) else ()
						) in loop ()
				with
					CFBreak -> ()
				)
		| ForEach (varname, expr, stmt, env) ->
				symbol_table.env.current_stmt <- env;
				(try
					let map = evaluate_expression expr symbol_table in
					let list =
						match map with
						| MapValue(h, MapSubtype) -> Hashtbl.fold (fun k v lst -> v:: lst) h []
						| MapValue(_, ArraySubtype) -> SymbolTable.list_of_array map
						| _ -> raise(NotACollectionType("the second argument of forEach", SymbolTable.string_of_symbol_type map))
					in
					let symbol_table = SymbolTable.push_scope symbol_table in
					let runloop lst =
						match lst with
						| [] -> (false,[])
						| el:: tl -> SymbolTable.declare varname el symbol_table; (true, tl)
					in
					let rec loop lst = (
							let (continue, rest_list) = runloop lst in
							if continue then
								((try
										interpret_statement stmt (SymbolTable.push_scope symbol_table)
									with
										CFContinue -> ());
									loop rest_list
								) else ()
						) in loop list
				with
					CFBreak -> ()
				)
		| Case(_, _) -> () (* ignore, preprocessed in switch *)
		| Switch(expr, stmtlist, env) ->
				symbol_table.env.current_stmt <- env;
				(* make a list of case expressions and succeeding statement *)
				let rec find_cases stmtlist caselist defaultfound =
					match stmtlist with
					| []-> caselist
					| Case(Some expr, _):: tl ->
							if defaultfound then
								raise DefaultCaseShouldBeLast
							else(
								let caselist = (Some expr, tl):: caselist in
								find_cases tl caselist false)
					| Case(None, _):: tl ->
							let caselist = (None, tl):: caselist in
							find_cases tl caselist true
					| _:: tl -> find_cases tl caselist false
				(* match a value with a case and return a statement list *)
				in let rec match_case expr1 caselist =
					match caselist with
					| [] -> []
					| (Some expr2, stmts):: tl ->
							if evaluate_expression (CompOp(expr1, Equal, expr2)) symbol_table = BooleanValue(true) then
								stmts
							else
								match_case expr1 tl
					| (None, stmts):: tl -> stmts
				in let caselist = List.rev (find_cases stmtlist [] false)
				in let value = evaluate_expression expr symbol_table
				in let stmts = match_case (Value(value)) caselist
				in (try
					interpret_statements stmts (SymbolTable.push_scope symbol_table)
				with
				| CFBreak -> ())
		| If(condexpr, if_stmt, else_stmt, env) ->
				symbol_table.env.current_stmt <- env;
				(match (evaluate_expression condexpr symbol_table) with
					| BooleanValue(true) -> interpret_statement if_stmt (SymbolTable.push_scope symbol_table)
					| BooleanValue(false) -> interpret_statement else_stmt (SymbolTable.push_scope symbol_table)
					| value -> raise (EInvalidCast(SymbolTable.string_of_symbol_value value,"boolean"))
				)
		| StatementBlock(statements) -> interpret_statements statements (SymbolTable.push_scope symbol_table)
		| Program(statements) -> interpret_statements statements symbol_table
		| Import((filename, descr), env) ->
				symbol_table.env.current_stmt <- env;
				if descr.loaded then
					()
				else
					(
						if List.mem filename symbol_table.env.loaded_imports then
							(descr.loaded <- true; ())
						else(
							let ast = symbol_table.env.parse_callback filename in
							descr.loaded <- true;
							symbol_table.env.loaded_imports <- filename:: symbol_table.env.loaded_imports;
							match ast with
							| Program(stmts) -> interpret_import stmts symbol_table
							| _ -> raise (InternalError "expected a Program from parse")
						)
					)
		| Instructions(_, _, _, env) -> symbol_table.env.current_stmt <- env; ()
		| TemplateDef(_, _, env) -> symbol_table.env.current_stmt <- env; ()
		| Return(expression, env) -> symbol_table.env.current_stmt <- env;
				raise (CFReturn(evaluate_expression expression symbol_table))
		| Continue(env) -> symbol_table.env.current_stmt <- env;
				raise CFContinue
		| Break(env) -> symbol_table.env.current_stmt <- env;
				raise CFBreak
	and
	interpret_statements statement_list symbol_table =
		List.fold_left (fun _ stmt -> interpret_statement stmt symbol_table) () statement_list
	and interpret_import stmts symbol_table = (* only import decl and imports *)
		List.fold_left (fun _ stmt ->
						match stmt with
						| ExpressionStatement( Declaration(_, _), _) | Import(_, _) -> interpret_statement stmt symbol_table
						| _ -> ()) () stmts
	
end