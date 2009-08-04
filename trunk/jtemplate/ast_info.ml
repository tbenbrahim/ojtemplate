(**
This program is free software; you can redistribute it and / or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

Pretty prints the runtime AST

@author Tony BenBrahim < tony.benbrahim at gmail.com >

*)

module AstInfo =
struct
	
	open Ast
	
	(**
	Returns a pretty printed representation of the runtime AST
	@param statement the top level statement (program)
	@return a string with the pretty printed AST
	*)
	let statement_description statement =
		let rec statement_descriptionl level statement =
			let rec get_closure_vars = function
				| None -> "None"
				| Some t -> (Hashtbl.fold (fun n _ s -> let (d, i) = n in s^"Local("^(string_of_int d)^","^(string_of_int i)^"),") t "[")^"]"
			
			and prefix = function
				| 0 -> ""
				| level -> (String.make (level * 3) ' ') ^ "+--"
			
			and location_name = function
				| GlobalVar(uid, ind) ->"Global("^(string_of_int uid)^","^(string_of_int ind)^")\n"
				| LocalVar(uid, d, ind) ->"Local("^(string_of_int uid)^","^(string_of_int d)^","^(string_of_int ind)^")\n"
			
			and opname = function
				| Plus -> "+"
				| Minus -> "-"
				| Divide -> "/"
				| Times -> "*"
				| Modulo -> "%"
				| And -> "&&"
				| Or -> "||"
			
			and compopname = function
				| LessThan -> "<"
				| LessThanEqual -> ">"
				| Equal -> "=="
				| GreaterThan -> ">"
				| GreaterThanEqual -> ">="
				| NotEqual -> "!="
			
			and expr_descriptionl level = function
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
						(prefix level) ^"Variable " ^(location_name loc)
				| RVarArg(loc) ->
						(prefix level) ^ "VarArg" ^(location_name loc)
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
			
			and value_descriptionl level = function
				| RIntegerValue v ->
						(prefix level) ^ ("Integer " ^ ((string_of_int v) ^ "\n"))
				| RFunctionValue (stacksize, depth, numargs, varargs, stmts, closure_vars, inline) ->
						(prefix level) ^
						"RFunction("^(string_of_int stacksize)^","^(string_of_int depth)^","^(string_of_int numargs)
						^","^(string_of_bool varargs)^","^(get_closure_vars closure_vars)^
						", inline "^(match inline with | Some _ -> "true" | None -> "false")^
						")\n"
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
			
			in match statement with
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
					(prefix level)^"catch "^(location_name vloc)^(statement_descriptionl (level + 1) stmt2)
			| RSwitch(expr, stmts, env) ->
					List.fold_left(fun pre stmt -> pre^(statement_descriptionl (level + 1) stmt))
						((prefix level)^"Switch\n"^(expr_descriptionl (level + 1) expr)) stmts
			| RForEach(vloc, expr, stmt, env) ->
					(prefix level)^"RForEach "^(location_name vloc)^"\n"^(expr_descriptionl (level + 1) expr)^
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
			| RFastIterator(vloc, start, max, incr, stmt, env) ->
					(prefix level)^ ("FastIterator "^" "^(string_of_int start)^" "
						^(string_of_int max)^" "^(string_of_int incr)^" "
						^(location_name vloc)^(statement_descriptionl (level + 1) stmt))
			| RNoop -> (prefix level) ^ "Noop\n"
			| RThrow(expr, env) -> (prefix level)^"Throw\n"^(expr_descriptionl (level + 1) expr)
			| RCase(Some expr, env) -> (prefix level)^"Case\n"^(expr_descriptionl (level + 1) expr)
			| RCase(None, env) -> (prefix level)^"DefaultCase\n"
		
		in statement_descriptionl 0 statement
	
	(**
	Pretty prints the representation of the runtime AST
	@param statement the top level statement (program)
	@return unit
	*)
	let print_ast statement = print_string (statement_description statement)
	
end
