module Interpreter =
struct
	open Ast
	open Symbol_table
	
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
	
	let rec make_map str_expr_list symbol_table =
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
						match casting_type value1 value2 with
						| FloatCast(f1, f2) -> compare_same_type (FloatValue(f1)) comparator (FloatValue(f2))
						| _ ->	raise (EMismatchedTypeInCompare(SymbolTable.string_of_symbol_type value1,
											SymbolTable.string_of_symbol_type value2))
				)
		| FunctionCall(variable, exprlist) ->
				let value_list = evaluate_exprs exprlist symbol_table in
				(match SymbolTable.get_value (resolve_variable_name variable symbol_table) symbol_table with
					| FunctionValue(arglist, stmts, scope) ->
							(try
								interpret_statements stmts (SymbolTable.new_function_call_scope variable scope arglist value_list); Void
							with
							| CFReturn value -> value)
					| LibraryFunction(arglist, code, scope) ->
							(try
								code (SymbolTable.new_function_call_scope variable scope arglist value_list); Void
							with
							| CFReturn value -> value)
					| _ -> raise (ENotAFunction (SymbolTable.fullname variable))
				)
		| MapExpr(str_expr_list) ->
				MapValue(make_map str_expr_list symbol_table, MapSubtype)
		| ArrayExpr(expr_list) ->
				MapValue(make_array expr_list symbol_table, ArraySubtype)
		| VariableExpr(variable) ->
				SymbolTable.get_value (resolve_variable_name variable symbol_table) symbol_table
		| Value(value) -> value
	and
	evaluate_exprs exprlist symbol_table =
		List.map (fun expr -> evaluate_expression expr symbol_table ) exprlist
	and
	interpret_statement statement symbol_table =
		match statement with
		| Assignment(varname, expression) ->
				SymbolTable.assign (resolve_variable_name varname symbol_table)
					(evaluate_expression expression symbol_table) symbol_table
		| Declaration(varname, expression) ->
				SymbolTable.declare (resolve_variable_name varname symbol_table)
					(evaluate_expression expression symbol_table) symbol_table
		| ExpressionStatement expression ->
				let _ = evaluate_expression expression symbol_table in ()
		| Noop -> ()
		| For (preloop, condexpr, endstmt, stmtlist) ->
				(try
					let symbol_table = SymbolTable.push_scope symbol_table in
					interpret_statement preloop symbol_table;
					let rec loop () = (
							(try
								interpret_statements stmtlist symbol_table
							with
								CFContinue -> ());
							interpret_statement endstmt symbol_table;
							match (evaluate_expression condexpr symbol_table) with
							| BooleanValue(true) -> loop ()
							| BooleanValue(false) -> ()
							| value ->
									raise (EInvalidCast(SymbolTable.string_of_symbol_value value,"boolean"))
						) in
					loop ()
				with
					CFBreak -> ()
				)
		| If(condexpr, if_stmts, else_stmts) ->
				(try (match (evaluate_expression condexpr symbol_table) with
						| BooleanValue(true) -> interpret_statements if_stmts (SymbolTable.push_scope symbol_table)
						| BooleanValue(false) -> interpret_statements else_stmts (SymbolTable.push_scope symbol_table)
						| value -> raise (EInvalidCast(SymbolTable.string_of_symbol_value value,"boolean"))
					)with
				| CFBreak -> ())
		| StatementBlock(statements) -> interpret_statements statements symbol_table
		| ForEach (_, _, _) -> ()
		| Instructions(_, _, _) -> ()
		| TemplateDef(_, _) -> ()
		| Return expression -> raise (CFReturn(evaluate_expression expression symbol_table))
		| Continue ->	raise CFContinue
		| Break -> raise CFBreak
	and
	interpret_statements statement_list symbol_table =
		List.fold_left (fun _ stmt -> interpret_statement stmt symbol_table) () statement_list
	
end