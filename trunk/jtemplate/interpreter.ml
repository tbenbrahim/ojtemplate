module Interpreter =
struct
	open Ast
	open Symbol_table
	open Stringmap
	
	exception EIncompatibleTypes of string * string (* type1, type2 *)
	exception EInvalidCast of string * string (* value, typename *)
	exception EInvalidOperation of Ast.operator * string  (* operator, typename *)
	
	type cast_type = IntegerCast | FloatCast | StringCast
	
	let casting_type value1 value2 =
		let type1 = SymbolTable.string_of_symbol_type value1 in
		let type2 = SymbolTable.string_of_symbol_type value2 in
		if type1 ="string" || type2 ="string" then
			StringCast
		else if (type1 ="float" && (type2 ="integer" || type2 ="float"))
		or (type2 ="float" && (type1 ="integer" || type1 ="float")) then
			FloatCast
		else if type1 ="integer" && type2 ="integer" then
			IntegerCast
		else
			raise (EIncompatibleTypes(type1, type2))
	
	let cast_to_string value =
		SymbolTable.string_of_symbol_value value
	
	let cast_to_integer value =
		match value with
		| SymbolTable.IntegerValue(i) -> i
		| _ -> raise (EInvalidCast (SymbolTable.string_of_symbol_value value,"integer"))
	
	let cast_to_float value =
		match value with
		| SymbolTable.FloatValue(f) -> f
		| SymbolTable.IntegerValue(i) -> float_of_int i
		| _ -> raise (EInvalidCast (SymbolTable.string_of_symbol_value value,"float"))
	
	let rec make_map str_expr_list map symbol_table =
		List.fold_left
			(fun acc el ->
						let (name, expr) = el in
						StringMap.add name (evaluate_expression expr symbol_table) map
			) map str_expr_list
	and
	make_array expr_list map symbol_table =
		let (map, length) = List.fold_left
				(fun acc expr ->
							let (map, index) = acc in
							(StringMap.add (string_of_int index) (evaluate_expression expr symbol_table) map,
								index + 1)
				) (map, 0) expr_list in
		StringMap.add "length" (SymbolTable.IntegerValue(length)) map
	and
	evaluate_expression expr symbol_table =
		match expr with
		| BinaryOp(expr1 , operator , expr2) ->
				let value1 = evaluate_expression expr1 symbol_table in
				let value2 = evaluate_expression expr2 symbol_table in
				(match casting_type value1 value2 with
					| StringCast ->
							(match operator with
								| Plus -> SymbolTable.StringValue((cast_to_string value1) ^ (cast_to_string value2))
								| _ -> raise (EInvalidOperation (operator,"string"))
							)
					| FloatCast -> (match operator with
								| Plus -> SymbolTable.FloatValue( (cast_to_float value1) +. (cast_to_float value2))
								| Minus -> SymbolTable.FloatValue( (cast_to_float value1) -. (cast_to_float value2))
								| Times -> SymbolTable.FloatValue( (cast_to_float value1) *. (cast_to_float value2))
								| Divide -> let divisor = cast_to_float value2 in
										if divisor <> 0.0 then SymbolTable.FloatValue( (cast_to_float value1) /. divisor)
										else SymbolTable.NaN
								| Modulo -> raise (EInvalidOperation (operator,"float"))
							)
					| IntegerCast -> (match operator with
								| Plus -> SymbolTable.IntegerValue( (cast_to_integer value1) + (cast_to_integer value2))
								| Minus -> SymbolTable.IntegerValue( (cast_to_integer value1) - (cast_to_integer value2))
								| Times -> SymbolTable.IntegerValue( (cast_to_integer value1) * (cast_to_integer value2))
								| Divide -> let divisor = cast_to_integer value2 in
										if divisor <> 0 then SymbolTable.IntegerValue( (cast_to_integer value1) / divisor)
										else SymbolTable.NaN
								| Modulo -> let divisor = cast_to_integer value2 in
										if divisor <> 0 then SymbolTable.IntegerValue( (cast_to_integer value1) mod divisor)
										else SymbolTable.NaN
							)
				)
		| CompOp (expr1, comparator, expr2) -> SymbolTable.Void
		| FunctionCall(variable, exprs) -> SymbolTable.Void
		| Value(value) -> (match value with
					| Integer(i) -> SymbolTable.IntegerValue(i)
					| Float(f) -> SymbolTable.FloatValue(f)
					| String(s) -> SymbolTable.StringValue(s)
					| Boolean(b) -> SymbolTable.BooleanValue(b)
					| Function(varlist, stmtlist) -> SymbolTable.FunctionValue(varlist, stmtlist, None)
					| Map(str_expr_list) ->
							SymbolTable.MapValue(make_map str_expr_list StringMap.empty symbol_table)
					| Array(expr_list) ->
							SymbolTable.MapValue(make_array expr_list StringMap.empty symbol_table)
					| Expression(expr) -> evaluate_expression expr symbol_table
					| Variable(variable) -> SymbolTable.get_value variable symbol_table
				)
	and
	interpret_ast ast symbol_table =
		match ast with
			Declaration(varname, expression) ->	()
end