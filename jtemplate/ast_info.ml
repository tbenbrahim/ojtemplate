module AstInfo =
struct
	
	open Ast
	
	let rec string_of_symbol_value = function
		| RIntegerValue(i) -> string_of_int i
		| RFloatValue(f) -> string_of_float f
		| RBooleanValue(b) -> string_of_bool b
		| RStringValue(s) -> s
		| RFunctionValue(stacksize, depth, numargs, varargs, _, closure_vars) ->
				"Function("^(string_of_int stacksize)^","^(string_of_int depth)^","^(string_of_int numargs)
				^","^(string_of_bool varargs)^","^(get_closure_vars closure_vars)^")"
		| RLibraryFunction(def) -> "library call"^(string_of_args def.args)
		| RMapValue(map, MapSubtype) -> "{}"
		| RMapValue(map, ArraySubtype _) -> "[]"
		| RVoid -> "Void"
		| RNaN -> "NaN"
		| RUndefined -> "undefined"
	and string_of_args args =
		match args with
		| [] -> "()"
		| _ -> "(" ^ (let x = (List.fold_left (fun acc el -> acc^","^el) "" args) in
					String.sub x 1 ((String.length x) - 1)) ^")"
	and get_closure_vars = function
		| None -> "None"
		| Some t -> (Hashtbl.fold (fun n _ s -> let (d,i)=n in s^"Local("^(string_of_int d)^","^(string_of_int i)^"),") t "[")^"]"
	
	let prefix level =
		match level with
		| 0 -> ""
		| _ -> (String.make ((level - 1) * 3) ' ') ^ "+--"
	
	let opname =
		function
		| Plus -> "+"
		| Minus -> "-"
		| Divide -> "/"
		| Times -> "*"
		| Modulo -> "%"
		| And -> "&&"
		| Or -> "||"
	
	let compopname =
		function
		| LessThan -> "<"
		| LessThanEqual -> ">"
		| Equal -> "=="
		| GreaterThan -> ">"
		| GreaterThanEqual -> ">="
		| NotEqual -> "!="
	
	let rec statement_descriptionl level statement =
		match statement with
		| RStatementBlock(stmts) ->
				List.fold_left(fun pre stmt -> pre^(statement_descriptionl (level + 1) stmt))
					((prefix level) ^"Statement block\n") stmts
		| RProgram(stmts) ->
				List.fold_left(fun pre stmt -> pre^(statement_descriptionl (level + 1) stmt))
					((prefix level) ^"Program\n") stmts
		| RTryFinally(stmt1, stmt2, env) ->
				(prefix level)^"Try\n"^(statement_descriptionl (level + 1) stmt1)^
				(prefix level)^"finally\n"^(statement_descriptionl (level + 1) stmt2)
		| RTryCatch(stmt1, vloc, stmt2, env) ->
				(prefix level)^"Try\n"^(statement_descriptionl (level + 1) stmt1)^
				(prefix level)^"catch "^(match vloc with
					| GlobalVar(uid, ind) ->"Global("^(string_of_int uid)^","^(string_of_int ind)^")"
					| LocalVar(uid, d, ind) ->"Local("^(string_of_int uid)^","^(string_of_int d)^","^(string_of_int ind)^")"
				)^"\n"^(statement_descriptionl (level + 1) stmt2)
		| RSwitch(expr, stmts, env) ->
				List.fold_left(fun pre stmt -> pre^(statement_descriptionl (level + 1) stmt))
					((prefix level)^"Switch\n"^(expr_descriptionl (level + 1) expr)) stmts
		| RForEach(vloc, expr, stmt, env) ->
				(prefix level)^"RForEach "^(match vloc with
					| GlobalVar(uid, ind) ->"Global("^(string_of_int uid)^","^(string_of_int ind)^")"
					| LocalVar(uid, d, ind) ->"Local("^(string_of_int uid)^","^(string_of_int d)^","^(string_of_int ind)^")"
				)^"\n"^(expr_descriptionl (level + 1) expr)^
				(statement_descriptionl (level + 1) stmt)
		| RIf (expr, iflist, elselist, env) ->
				(prefix level) ^
				("If/Else\n" ^
					((expr_descriptionl (level + 1) expr) ^
						((statement_descriptionl (level + 1) iflist) ^
							(statement_descriptionl (level + 1) elselist))))
		| RReturn( expr, env) ->
				(prefix level) ^
				("Return\n" ^ (expr_descriptionl (level + 1) expr))
		| RExpressionStatement (expr , env) -> expr_descriptionl level expr
		| RContinue(env) -> (prefix level) ^ "Continue\n"
		| RBreak(env) -> (prefix level) ^ "Break\n"
		| RFor (expr1, expr2, expr3, stmt_list, env) ->
				(prefix level) ^
				("For\n" ^
					((expr_descriptionl (level + 1) expr1) ^
						((expr_descriptionl (level + 1) expr2) ^
							((expr_descriptionl (level + 1) expr3) ^
								(statement_descriptionl (level + 1) stmt_list)))))
		| RNoop -> (prefix level) ^ "Noop\n"
		| RThrow(expr, env) -> (prefix level)^"Throw\n"^(expr_descriptionl (level + 1) expr)
		| RCase(Some expr, env) -> (prefix level)^"Case\n"^(expr_descriptionl (level + 1) expr)
		| RCase(None, env) -> (prefix level)^"DefaultCase\n"
	
	and var_descriptionl level var =
		(prefix level) ^ ("Variable " ^ (var ^ "\n"))
	and expr_descriptionl level expr =
		match expr with
		| RBinaryOp (op1, op, op2) ->
				(prefix level) ^
				("BinOp " ^
					((opname op) ^
						("\n" ^
							((expr_descriptionl (level + 1) op1) ^
								(expr_descriptionl (level + 1) op2)))))
		| RCompOp (op1, op, op2) ->
				(prefix level) ^
				("CompOp " ^
					((compopname op) ^
						("\n" ^
							((expr_descriptionl (level + 1) op1) ^
								(expr_descriptionl (level + 1) op2)))))
		| RValue value ->
				(prefix level) ^ "Value reference\n" ^ value_descriptionl (level + 1) value
		| RMapExpr v ->
				(prefix level) ^ ("Map\n" ^ ((property_list (level + 1) v) ^ "\n"))
		| RArrayExpr v ->
				(prefix level) ^ ("Array\n" ^ (expression_list (level + 1) v))
		| RVariable(loc) ->
				(prefix level) ^"Variable " ^(match loc with
					| GlobalVar(uid, ind) ->"Global("^(string_of_int uid)^","^(string_of_int ind)^")\n"
					| LocalVar(uid, d, ind) ->"Local("^(string_of_int uid)^","^(string_of_int d)^","^(string_of_int ind)^")\n"
				)
		| RVarArg(loc) ->
				(prefix level) ^ "VarArg" ^(match loc with
					| GlobalVar(uid, ind) ->"Global("^(string_of_int uid)^","^(string_of_int ind)^")\n"
					| LocalVar(uid, d, ind) ->"Local("^(string_of_int uid)^","^(string_of_int d)^","^(string_of_int ind)^")\n"
				)
		| RNot(expr) ->
				(prefix level) ^ "Not\n"^(expr_descriptionl (level + 1) expr)
		| RDeclaration(expr1, expr2) ->
				(prefix level)^"Declare\n"^
				(expr_descriptionl (level + 1) expr1)^(expr_descriptionl (level + 1) expr2)
		| RAssignment(expr1, expr2) ->
				(prefix level)^"Assignment\n"^
				(expr_descriptionl (level + 1) expr1)^(expr_descriptionl (level + 1) expr2)
		| RPostFixSum(expr, inc) -> (prefix level)^"++("^(string_of_int inc)^")\n"^
				(expr_descriptionl (level + 1) expr)
		| RMemberExpr(expr1, expr2) ->
				(prefix level)^"Member\n"^
				(expr_descriptionl (level + 1) expr1)^(expr_descriptionl (level + 1) expr2)
		| RTernaryCond(expr1, expr2, expr3) ->
				(prefix level)^"Ternary condition\n"^
				(expr_descriptionl (level + 1) expr1)^(expr_descriptionl (level + 1) expr2)^
				(expr_descriptionl (level + 1) expr3)
		| RFunctionCall(expr, exprl) ->
				(prefix level)^"FunctionCall\n"^(expr_descriptionl (level + 1) expr)^
				(expression_list (level + 1) exprl)
	
	and value_descriptionl level value =
		match value with
		| RIntegerValue v ->
				(prefix level) ^ ("Integer " ^ ((string_of_int v) ^ "\n"))
		| RFunctionValue (stacksize, depth, numargs, varargs, stmts, closure_vars) ->
				(prefix level) ^
				"RFunction("^(string_of_int stacksize)^","^(string_of_int depth)^","^(string_of_int numargs)
				^","^(string_of_bool varargs)^","^(get_closure_vars closure_vars)^")\n"
				^ (statement_list (level + 1) stmts)
		| RLibraryFunction(_) -> "" (* never in ast *)
		| RBooleanValue v ->
				(prefix level) ^ ("Boolean " ^ ((string_of_bool v) ^ "\n"))
		| RStringValue v -> (prefix level) ^ ("String " ^ (v ^ "\n"))
		| RFloatValue v ->
				(prefix level) ^ ("Float " ^ ((string_of_float v) ^ "\n"))
		| RMapValue( _, _) -> "" (* Not in AST *)
		| RVoid -> (prefix level) ^"void\n"
		| RNaN -> (prefix level) ^"NaN\n"
		| RUndefined -> (prefix level) ^"Undefined\n"
	
	and statement_list level stmt_list =
		List.fold_left (fun acc el -> acc ^ (statement_descriptionl level el))
			"" stmt_list
	
	and expression_list level expr_list =
		List.fold_left (fun acc el -> acc ^ (expr_descriptionl level el)) ""
			expr_list
	
	and property_list level prop_list =
		List.fold_left
			(fun acc el ->
						let (name, expr) = el
						in
						acc ^
						((prefix level) ^
							("Property " ^
								(name ^ ("\n" ^ (expr_descriptionl (level + 1) expr))))))
			"" prop_list
	
	let statement_description = statement_descriptionl 0
	
	let var_description = var_descriptionl 0
	
	let expr_description = expr_descriptionl 0
	
	let print_ast statement = print_string (statement_description statement)
	
end
