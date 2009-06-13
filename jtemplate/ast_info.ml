module AstInfo =
struct
	
	let prefix level =
		match level with
		| 0 -> ""
		| _ -> (String.make ((level - 1) * 3) ' ') ^ "+--"
	
	let opname =
		function
		| Ast.Plus -> "+"
		| Ast.Minus -> "-"
		| Ast.Divide -> "/"
		| Ast.Times -> "*"
		| Ast.Modulo -> "%"
		| Ast.And -> "&&"
		| Ast.Or -> "||"
	
	let compopname =
		function
		| Ast.LessThan -> "<"
		| Ast.LessThanEqual -> ">"
		| Ast.Equal -> "=="
		| Ast.GreaterThan -> ">"
		| Ast.GreaterThanEqual -> ">="
		| Ast.NotEqual -> "!="
	
	let rec statement_descriptionl level statement =
		match statement with
		| Ast.StatementBlock(stmts) ->
				List.fold_left(fun pre stmt -> pre^(statement_descriptionl (level + 1) stmt))
					((prefix level) ^"Statement block\n") stmts
		| Ast.Assignment (var, expr) ->
				(prefix level) ^
				("Assignment\n" ^
					((var_descriptionl (level + 1) var) ^
						(expr_descriptionl (level + 1) expr)))
		| Ast.Declaration (var, expr) ->
				(prefix level) ^
				("Declaration\n" ^
					((var_descriptionl (level + 1) var) ^
						(expr_descriptionl (level + 1) expr)))
		| Ast.Instructions (var, varlist, specs) ->
				(prefix level) ^
				("Instructions\n" ^
					((var_descriptionl (level + 1) var) ^
						((List.fold_left
									(fun acc el -> el ^ "," ^ acc) "" varlist)
							^
							(List.fold_left
									(fun acc el ->
												let (Ast.Label label, condspec, repllist) = el
												in
												acc ^
												((prefix (level + 1)) ^
													("Label " ^
														(label ^
															("\n" ^
																((match condspec with
																		| Ast.Once ->
																				(prefix (level + 2)) ^
																				"once\n"
																		| Ast.When expr ->
																				(prefix (level + 2)) ^
																				("when\n" ^
																					(expr_descriptionl
																							(level + 3) expr))
																		| Ast.CondLoop (cond, var, expr)
																		->
																				(prefix (level + 2)) ^
																				("foreach when\n" ^
																					(expr_descriptionl
																							(level + 3) cond) ^
																					((var_descriptionl
																								(level + 3) var)
																						^
																						(expr_descriptionl
																								(level + 3) expr)))
																		| Ast.Loop (var, expr) ->
																				(prefix (level + 2)) ^
																				("foreach\n" ^
																					((var_descriptionl
																								(level + 3) var)
																						^
																						(expr_descriptionl
																								(level + 3) expr))))
																	^
																	(List.fold_left
																			(fun acc el ->
																						let (text, expr) = el
																						in
																						acc ^
																						((prefix (level + 3))
																							^
																							(text ^
																								("=\n" ^
																									(expr_descriptionl
																											(
																												level + 4)
																											expr)))))
																			"" repllist)))))))
									"" specs))))
		| Ast.TemplateDef (var, specs) ->
				(prefix level) ^
				("Template Definition\n" ^
					((var_descriptionl (level + 1) var) ^
						(List.fold_left
								(fun acc el ->
											let (optlabel, text) = el
											in
											match optlabel with
											| Some (Ast.Label label) ->
													acc ^ (label ^ (":\t" ^ (text ^ "\n")))
											| None -> acc ^ (":\t" ^ (text ^ "\n")))
								"" specs)))
		| Ast.If (expr, iflist, elselist) ->
				(prefix level) ^
				("If/Else\n" ^
					((expr_descriptionl (level + 1) expr) ^
						((statement_list (level + 1) iflist) ^
							(statement_list (level + 1) elselist))))
		| Ast.Return expr ->
				(prefix level) ^
				("Return\n" ^ (expr_descriptionl (level + 1) expr))
		| Ast.ExpressionStatement expr -> expr_descriptionl level expr
		| Ast.ForEach (var, expr, stmt) ->
				(prefix level) ^
				("ForEach\n" ^
					((var_descriptionl (level + 1) var) ^
						((expr_descriptionl (level + 1) expr) ^
							(statement_list (level + 1) stmt))))
		| Ast.Continue -> (prefix level) ^ "Continue\n"
		| Ast.Break -> (prefix level) ^ "Break\n"
		| Ast.For (stmt1, expr, stmt3, stmt_list) ->
				(prefix level) ^
				("For\n" ^
					((statement_descriptionl (level + 1) stmt1) ^
						((expr_descriptionl (level + 1) expr) ^
							((statement_descriptionl (level + 1) stmt3) ^
								(statement_list (level + 1) stmt_list)))))
		| Ast.Noop -> (prefix level) ^ "Noop\n"
	
	and var_descriptionl level var =
		match var with
		| Ast.Name(name) -> (prefix level) ^ ("Variable " ^ (name ^ "\n"))
		| Ast.CompoundName (name_list) ->
				(prefix level) ^
				("MapReference " ^
					((List.fold_left (fun acc el -> el ^ ("." ^ acc)) "" name_list)
						^ "\n"))
		| Ast.ArrayIndex(name, expr) ->
				(prefix level) ^
				"ArrayIndex " ^name^"[]\n"^
				(expr_descriptionl (level + 1) expr)
		| Ast.EvaluatedName(lst) ->
				(prefix level) ^"EvaluatedName\n"^
				(List.fold_left (fun acc el -> acc ^ (var_descriptionl (level + 1) el)) "" lst)
	and expr_descriptionl level expr =
		match expr with
		| Ast.BinaryOp (op1, op, op2) ->
				(prefix level) ^
				("BinOp " ^
					((opname op) ^
						("\n" ^
							((expr_descriptionl (level + 1) op1) ^
								(expr_descriptionl (level + 1) op2)))))
		| Ast.CompOp (op1, op, op2) ->
				(prefix level) ^
				("CompOp " ^
					((compopname op) ^
						("\n" ^
							((expr_descriptionl (level + 1) op1) ^
								(expr_descriptionl (level + 1) op2)))))
		| Ast.Value value ->
				(prefix level) ^ "Value reference\n" ^ value_descriptionl (level + 1) value
		| Ast.FunctionCall (var, expr_list) ->
				(prefix level) ^
				("FunctionCall\n" ^
					((var_descriptionl (level + 1) var) ^
						(expression_list (level + 1) expr_list)))
		| Ast.MapExpr v ->
				(prefix level) ^ ("Map\n" ^ ((property_list (level + 1) v) ^ "\n"))
		| Ast.ArrayExpr v ->
				(prefix level) ^ ("Array\n" ^ (expression_list (level + 1) v))
		| Ast.VariableExpr v ->
				(prefix level) ^ "Variable reference\n" ^ var_descriptionl (level + 1) v
	
	and value_descriptionl level value =
		match value with
		| Ast.IntegerValue v ->
				(prefix level) ^ ("Integer " ^ ((string_of_int v) ^ "\n"))
		| Ast.FunctionValue (arglist, stmts, _) ->
				(prefix level) ^
				("Function\n" ^
					((List.fold_left
								(fun acc el -> el^ "," ^ acc) "" arglist)
						^ (statement_list (level + 1) stmts)))
		| Ast.LibraryFunction(_, _, _) -> "" (* never in ast *)
		| Ast.BooleanValue v ->
				(prefix level) ^ ("Boolean " ^ ((string_of_bool v) ^ "\n"))
		| Ast.StringValue v -> (prefix level) ^ ("String " ^ (v ^ "\n"))
		| Ast.FloatValue v ->
				(prefix level) ^ ("Float " ^ ((string_of_float v) ^ "\n"))
		| Ast.MapValue( _, _) -> "" (* Not in AST *)
		| Ast.Void -> "void\n"
		| Ast.NaN -> "NaN\n"
	
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