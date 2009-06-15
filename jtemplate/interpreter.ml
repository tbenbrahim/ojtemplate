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
							(if isvararg && tl <>[] then raise (RuntimeError.VarArgsMustbeLast formalname) else ());
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
			match op with
			| Equal -> v1 = v2
			| NotEqual -> v1 <> v2
			| _ -> raise (EInvalidComparaison(op, SymbolTable.string_of_symbol_type v1))
		)
	and
	resolve_variable_name varname symbol_table =
		match varname with
		| Name(_) | CompoundName(_) -> varname
		| ArrayIndex(_, _) | EvaluatedName(_) ->
				let rec resolve variable =
					match variable with
					| ArrayIndex(name, expr) ->
							[name; cast_to_string(evaluate_expression expr symbol_table)]
					| Name(name) -> [name]
					| CompoundName(lst) -> lst
					| EvaluatedName(lst) ->
							List.fold_left (fun acc el -> List.append acc (resolve el)) [] lst
				in CompoundName(resolve varname)
	and
	evaluate_expression expr symbol_table =
		match expr with
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
		| FunctionCall(variable, exprlist) ->
				let (has_unbound, (exprlist, formal_args, has_vararg), value_list) = resolve_arg_list exprlist symbol_table in
				if has_unbound then
					let stmts =
						(if has_vararg then
								let varargname = vararg_formalname (SymbolTable.fullname (List.hd (List.rev formal_args))) in
								[Return(FunctionCallExpandVarArg(variable, exprlist, varargname), symbol_table.env.current_stmt)]
							else
								[Return(FunctionCall(variable, exprlist), symbol_table.env.current_stmt)]) in
					FunctionValue(formal_args, stmts)
				else
					(match SymbolTable.get_value (resolve_variable_name variable symbol_table) symbol_table with
						| ScopedFunctionValue(arglist, stmts, scope) ->
								let old_stack = symbol_table.env.stack_trace in
								(try
									symbol_table.env.stack_trace <- symbol_table.env.current_stmt:: symbol_table.env.stack_trace;
									interpret_statements stmts (SymbolTable.new_function_call_scope variable scope arglist value_list);
									symbol_table.env.stack_trace <- old_stack;
									Void
								with
								| CFReturn value -> symbol_table.env.stack_trace <- old_stack; value)
						| LibraryFunction(arglist, code, scope) ->
								(try
									code (SymbolTable.new_function_call_scope variable scope arglist value_list);
									Void
								with
								| CFReturn value -> value)
						| _ -> raise (ENotAFunction (SymbolTable.fullname variable))
					)
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
		| DirectFunctionCall(expr, exprlist) ->
				let value_list = evaluate_exprs exprlist symbol_table in
				(match expr with
					| Value(FunctionValue(arglist, stmts)) ->
							let old_stack = symbol_table.env.stack_trace in
							(try
								symbol_table.env.stack_trace <- symbol_table.env.current_stmt:: symbol_table.env.stack_trace;
								interpret_statements stmts (SymbolTable.new_function_call_scope (Name("anon")) symbol_table arglist value_list);
								symbol_table.env.stack_trace <- old_stack;
								Void
							with
							| CFReturn value -> symbol_table.env.stack_trace <- old_stack; value)
					| _ -> raise (ENotAFunction "anon")
				)
		| MapExpr(str_expr_list) ->
				MapValue(make_map str_expr_list symbol_table, MapSubtype)
		| ArrayExpr(expr_list) ->
				MapValue(make_array expr_list symbol_table, ArraySubtype)
		| VariableExpr(variable) ->
				SymbolTable.get_value (resolve_variable_name variable symbol_table) symbol_table
		| Value(value) -> value
		| UnboundVar(name) -> raise (UnexpectedUnboundVar (SymbolTable.fullname name))
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
	interpret_statement statement symbol_table =
		try
			interpret_stmt statement symbol_table
		with
		| CFReturn v -> raise (CFReturn(v))
		| CFContinue -> raise CFContinue
		| CFBreak -> raise CFBreak
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
	interpret_stmt statement symbol_table =
		match statement with
		| Assignment(varname, expression, env) ->
				symbol_table.env.current_stmt <- env;
				SymbolTable.assign (resolve_variable_name varname symbol_table)
					(evaluate_expression expression symbol_table) symbol_table
		| Declaration(varname, expression, env) ->
				symbol_table.env.current_stmt <- env;
				SymbolTable.declare (resolve_variable_name varname symbol_table)
					(evaluate_expression expression symbol_table) symbol_table
		| ExpressionStatement(expression, env) ->
				symbol_table.env.current_stmt <- env;
				let _ = evaluate_expression expression symbol_table in ()
		| Noop -> ()
		| For (preloop, condexpr, endstmt, stmtlist, env) ->
				symbol_table.env.current_stmt <- env;
				(try
					let symbol_table = SymbolTable.push_scope symbol_table in
					interpret_statement preloop symbol_table;
					let runloop () =
						match (evaluate_expression condexpr symbol_table) with
						| BooleanValue(true) -> true
						| BooleanValue(false) -> false
						| value ->
								raise (EInvalidCast(SymbolTable.string_of_symbol_value value,"boolean")) in
					let rec loop () = (
							if runloop () then
								((try
										interpret_statements stmtlist symbol_table
									with
										CFContinue -> ());
									interpret_statement endstmt (SymbolTable.push_scope symbol_table);
									loop()
								) else ()
						) in loop ()
				with
					CFBreak -> ()
				)
		| ForEach (varname, expr, stmtlist, env) ->
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
										interpret_statements stmtlist (SymbolTable.push_scope symbol_table)
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
		| If(condexpr, if_stmts, else_stmts, env) ->
				symbol_table.env.current_stmt <- env;
				(try (match (evaluate_expression condexpr symbol_table) with
						| BooleanValue(true) -> interpret_statements if_stmts (SymbolTable.push_scope symbol_table)
						| BooleanValue(false) -> interpret_statements else_stmts (SymbolTable.push_scope symbol_table)
						| value -> raise (EInvalidCast(SymbolTable.string_of_symbol_value value,"boolean"))
					)with
				| CFBreak -> ())
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
							| StatementBlock(stmts) -> interpret_import stmts symbol_table
							| _ -> raise (InternalError "expected a StatementBlock from parse")
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
						| Declaration(_, _, _) | Import(_, _) -> interpret_statement stmt symbol_table
						| _ -> ()) () stmts
	
end