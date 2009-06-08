module Interpreter =
struct
	open Ast
	open Symbol_table
	
	exception EIncompatibleTypes of string * string (* type1, type2 *)
	exception EInvalidCast of string * string (* value, typename *)
	exception EInvalidOperation of Ast.operator * string  (* operator, typename *)
	exception EInvalidComparaison of Ast.comparator * string (* comparator, typename *)
	exception EMismatchedTypeInCompare of string * string (* type1, type2 *)
	
	(* control flow exceptions *)
	exception CFReturn of variable_value
	exception CFBreak
	exception CFContinue
	
	type cast_type = IntegerCast | FloatCast | StringCast
	
	let casting_type value1 value2 =
		let type1 = SymbolTable.value_type value1 in
		let type2 = SymbolTable.value_type value2 in
		if type1 = SymbolTable.StringType || type2 = SymbolTable.StringType then
			StringCast
		else if (type1 = SymbolTable.FloatType && (type2 = SymbolTable.IntegerType || type2 = SymbolTable.FloatType))
		or (type2 = SymbolTable.FloatType && (type1 = SymbolTable.IntegerType || type1 = SymbolTable.FloatType)) then
			FloatCast
		else if type1 = SymbolTable.IntegerType && type2 = SymbolTable.IntegerType then
			IntegerCast
		else
			raise (EIncompatibleTypes(SymbolTable.string_of_symbol_type value1,
						SymbolTable.string_of_symbol_type value2))
	
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
	evaluate_expression expr symbol_table =
		match expr with
		| BinaryOp(expr1 , operator , expr2) ->
				let value1 = evaluate_expression expr1 symbol_table in
				let value2 = evaluate_expression expr2 symbol_table in
				(match casting_type value1 value2 with
					| StringCast ->
							(match operator with
								| Plus -> StringValue((cast_to_string value1) ^ (cast_to_string value2))
								| _ -> raise (EInvalidOperation (operator,"string"))
							)
					| FloatCast -> (match operator with
								| Plus -> FloatValue( (cast_to_float value1) +. (cast_to_float value2))
								| Minus -> FloatValue( (cast_to_float value1) -. (cast_to_float value2))
								| Times -> FloatValue( (cast_to_float value1) *. (cast_to_float value2))
								| Divide -> let divisor = cast_to_float value2 in
										if divisor <> 0.0 then FloatValue( (cast_to_float value1) /. divisor)
										else NaN
								| Modulo -> raise (EInvalidOperation (operator,"float"))
							)
					| IntegerCast -> (match operator with
								| Plus -> IntegerValue( (cast_to_integer value1) + (cast_to_integer value2))
								| Minus -> IntegerValue( (cast_to_integer value1) - (cast_to_integer value2))
								| Times -> IntegerValue( (cast_to_integer value1) * (cast_to_integer value2))
								| Divide -> let divisor = cast_to_integer value2 in
										if divisor <> 0 then IntegerValue( (cast_to_integer value1) / divisor)
										else NaN
								| Modulo -> let divisor = cast_to_integer value2 in
										if divisor <> 0 then IntegerValue( (cast_to_integer value1) mod divisor)
										else NaN
							)
				)
		| CompOp (expr1, comparator, expr2) ->
				let value1 = evaluate_expression expr1 symbol_table in
				let value2 = evaluate_expression expr2 symbol_table in
				let type1 = SymbolTable.value_type value1 in
				let type2 = SymbolTable.value_type value2 in
				(if type1 = type2 then
						match type1 with
						| SymbolTable.IntegerType -> compare_same_type value1 comparator value2
						| SymbolTable.StringType -> compare_same_type value1 comparator value2
						| SymbolTable.FloatType -> compare_same_type value1 comparator value2
						| SymbolTable.BooleanType -> restricted_compare value1 comparator value2
						| SymbolTable.MapType -> restricted_compare value1 comparator value2
						| SymbolTable.FunctionType -> restricted_compare value1 comparator value2
						| SymbolTable.VoidType -> restricted_compare value1 comparator value2
						| SymbolTable.NaNType -> restricted_compare value1 comparator value2
					else
						match casting_type value1 value2 with
						| FloatCast -> compare_same_type (FloatValue(cast_to_float value1)) comparator (FloatValue(cast_to_float value2))
						| _ ->	raise (EMismatchedTypeInCompare(SymbolTable.string_of_symbol_type value1,
											SymbolTable.string_of_symbol_type value2))
				)
		| FunctionCall(variable, exprs) -> Void
		| MapExpr(str_expr_list) ->
				MapValue(make_map str_expr_list symbol_table)
		| ArrayExpr(expr_list) ->
				MapValue(make_array expr_list symbol_table)
		| VariableExpr(variable) -> SymbolTable.get_value variable symbol_table
		| Value(value) -> value
	and
	interpret_statement statement symbol_table =
		match statement with
		| Assignment(varname, expression) ->
				SymbolTable.assign varname
					(evaluate_expression expression symbol_table) symbol_table
		| Declaration(varname, expression) ->
				SymbolTable.declare varname
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
		| If(_, _, _) -> ()
		| ForEach (_, _, _) -> ()
		| Instructions(_, _, _) -> ()
		| TemplateDef(_, _) -> ()
		| Return expression ->
				raise (CFReturn(evaluate_expression expression symbol_table))
		| Continue ->	raise CFContinue
		| Break -> raise CFBreak
	and
	interpret_statements statement_list symbol_table =
		List.fold_left (fun _ stmt -> interpret_statement stmt symbol_table) () statement_list
	
end