module TestInterpreter =
struct
	open Test_helper
	open Symbol_table
	open RuntimeError
	open Ast
	open Interpreter
	
	let test_suite = ("Interpreter",[
			
			("integer arithmetic?", fun() ->
						let v1 = Value(IntegerValue(7)) in
						let v2 = Value(IntegerValue(2)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s = IntegerValue(9) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Minus, v2)) s = IntegerValue(5) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Times, v2)) s = IntegerValue(14) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Divide, v2)) s = IntegerValue(3) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Modulo, v2)) s = IntegerValue(1)
			);
			("integer division/modulo by 0 should return NaN", fun() ->
						let v1 = Value(IntegerValue(7)) in
						let v2 = Value(IntegerValue(0)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Divide, v2)) s = NaN &&
						Interpreter.evaluate_expression (BinaryOp(v1, Modulo, v2)) s = NaN
			);
			("integer comparaison", fun() ->
						let v1 = Value(IntegerValue(7)) in
						let v2 = Value(IntegerValue(2)) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, LessThan, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, GreaterThan, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, LessThanEqual, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, GreaterThanEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, LessThanEqual, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, GreaterThanEqual, v3)) s = BooleanValue(true)
			);
			("floating point arithmetic", fun() ->
						let v1 = Value(FloatValue(2.5)) in
						let v2 = Value(FloatValue(0.5)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s = FloatValue(3.0) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Minus, v2)) s = FloatValue(2.0) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Times, v2)) s = FloatValue(1.25) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Divide, v2)) s = FloatValue(5.0)
			);
			("floating point division by 0 should return NaN", fun() ->
						let v1 = Value(FloatValue(2.5)) in
						let v2 = Value(FloatValue(0.)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Divide, v2)) s = NaN
			);
			("floating point comparaison", fun() ->
						let v1 = Value(FloatValue(7.1)) in
						let v2 = Value(FloatValue(2.5)) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, LessThan, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, GreaterThan, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, LessThanEqual, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, GreaterThanEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, LessThanEqual, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, GreaterThanEqual, v3)) s = BooleanValue(true)
			);
			("string comparaison", fun() ->
						let v1 = Value(StringValue("abc")) in
						let v2 = Value(StringValue("XYZ")) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, LessThan, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, GreaterThan, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, LessThanEqual, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, GreaterThanEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, LessThanEqual, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, GreaterThanEqual, v3)) s = BooleanValue(true)
			);
			("boolean comparaison", fun() ->
						let v1 = Value(BooleanValue(true)) in
						let v2 = Value(BooleanValue(false)) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, NotEqual, v3)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Equal, v2)) s = BooleanValue(false)
			);
			("mixed floating point/integer arithmetic ", fun() ->
						let v1 = Value(FloatValue(3.5)) in
						let v2 = Value(IntegerValue(2)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s = FloatValue(5.5) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Minus, v2)) s = FloatValue(1.5) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Times, v2)) s = FloatValue(7.0) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Divide, v2)) s = FloatValue(1.75)
			);
			("mixed floating point/integer comparaison", fun() ->
						let v1 = Value(FloatValue(7.1)) in
						let v2 = Value(IntegerValue(2)) in
						let v3 = Value(FloatValue(2.0)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, LessThan, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, GreaterThan, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, LessThanEqual, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, GreaterThanEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, LessThanEqual, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, GreaterThanEqual, v3)) s = BooleanValue(true)
			);
			("string addition", fun() ->
						let v1 = Value(StringValue("2")) in
						let v2 = Value(StringValue("3")) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s = StringValue("23")
			);
			("integer/string addition", fun() ->
						let v1 = Value(IntegerValue(2)) in
						let v2 = Value(StringValue("3")) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s = StringValue("23")
			);
			("string/float addition", fun() ->
						let v1 = Value(FloatValue(2.5)) in
						let v2 = Value(StringValue("3")) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s = StringValue("2.53")
			);
			("expression modulo 3.0 and 2.0 throws EInvalidOperation(Modulo, \"float\")", fun() ->
						let v1 = Value(FloatValue(3.0)) in
						let v2 = Value(FloatValue(2.0)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Modulo, v2)) s in false
						with
						| Interpreter.EInvalidOperation(Modulo, "float") -> true
						| _ -> false
			);
			("expression modulo 3.0 and 2 throws EInvalidOperation(Modulo, \"float\")", fun() ->
						let v1 = Value(FloatValue(3.0)) in
						let v2 = Value(IntegerValue(2)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Modulo, v2)) s in false
						with
						| Interpreter.EInvalidOperation(Modulo, "float") -> true
						| _ -> false
			);
			("expression add 2 and true throws EIncompatibleTypes(\"integer\", \"boolean\")", fun() ->
						let v1 = Value(IntegerValue(2)) in
						let v2 = Value(BooleanValue(true)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s in
							false
						with
						| Interpreter.EIncompatibleTypes("integer", "boolean") -> true
						| _ -> false
			);
			("expression add {} and true throws EIncompatibleTypes(\"map\", \"boolean\")", fun() ->
						let v1 = MapExpr([]) in
						let v2 = Value(BooleanValue(true)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s in
							false
						with
						| Interpreter.EIncompatibleTypes("map", "boolean") -> true
						| _ -> false
			);
			("expression add 2.5 and true throws EIncompatibleTypes(\"float\", \"boolean\")", fun() ->
						let v1 = Value(FloatValue(2.5)) in
						let v2 = Value(BooleanValue(true)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Plus, v2)) s in
							false
						with
						| Interpreter.EIncompatibleTypes("float", "boolean") -> true
						| _ -> false
			);
			("expression subtract 'xx' and 'x' throws EInvalidOperation(Minus, \"string\")", fun() ->
						let v1 = Value(StringValue("xx")) in
						let v2 = Value(StringValue("x")) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Minus, v2)) s in
							false
						with
						| Interpreter.EInvalidOperation(Minus, "string") -> true
						| _ -> false
			);
			("expression multiply 'xx' and 'x' throws EInvalidOperation(Times, \"string\")", fun() ->
						let v1 = Value(StringValue("xx")) in
						let v2 = Value(StringValue("x")) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Times, v2)) s in
							false
						with
						| Interpreter.EInvalidOperation(Times, "string") -> true
						| _ -> false
			);
			("expression divide 'xx' and 'x' throws EInvalidOperation(Divide, \"string\")", fun() ->
						let v1 = Value(StringValue("xx")) in
						let v2 = Value(StringValue("x")) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Divide, v2)) s in
							false
						with
						| Interpreter.EInvalidOperation(Divide, "string") -> true
						| _ -> false
			);
			("expression modulo 'xx' and 'x' throws EInvalidOperation(Modulo, \"string\")", fun() ->
						let v1 = Value(StringValue("xx")) in
						let v2 = Value(StringValue("x")) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Modulo, v2)) s in
							false
						with
						| Interpreter.EInvalidOperation(Modulo, "string") -> true
						| _ -> false
			);
			("boolean < comparaison throws EInvalidComparaison(LessThan, \"boolean\")", fun() ->
						let v2 = Value(BooleanValue(false)) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (CompOp(v2, LessThan, v3)) s in false
						with
							Interpreter.EInvalidComparaison(LessThan, "boolean") -> true
						| _ -> false
			);
			("comparaison of two mismtached types throws EMismatchedTypeInCompare(\"integer\", \"string\")", fun() ->
						let v1 = Value(StringValue("3")) in
						let v2 = Value(IntegerValue(1)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (CompOp(v2, LessThan, v1)) s in false
						with
							Interpreter.EMismatchedTypeInCompare("integer", "string") -> true
						| _ -> false
			);
			("scalar declaration: var a=1;a=1?", fun() ->
						let stmt = Declaration(Name("a"), Value(IntegerValue(1))) in
						let s = SymbolTable.initialize () in
						Interpreter.interpret_statement stmt s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(1)
			);
			("expression declaration: var a=1;var b=a+2;b=3?", fun() ->
						let stmts =
							[Declaration(Name("a"), Value(IntegerValue(1)));
							Declaration(Name("b"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))))
							] in
						let s = SymbolTable.initialize () in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("b")) s = IntegerValue(3)
			);
			("expression assignment: var a=1;var a=a+2;a=3?", fun() ->
						let stmts =
							[Declaration(Name("a"), Value(IntegerValue(1)));
							Declaration(Name("a"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))))
							] in
						let s = SymbolTable.initialize () in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(3)
			);
			("array length test: var a=[1,2,3];a.length=3?", fun () ->
						let s = SymbolTable.initialize() in
						let expr = ArrayExpr([Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3));]) in
						let stmt = Declaration(Name("a"), expr) in
						Interpreter.interpret_statement stmt s;
						(match SymbolTable.get_value (Name("a")) s with
							| MapValue(h, ArraySubtype) -> Hashtbl.find h "length" = IntegerValue(3)
							| _ -> false)
			);
			("for loop: var a=0;for(var i=0;i<10000;i=i+1){a=a+2} a=20000? i not in scope?", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							For(Declaration(Name("i"), Value(IntegerValue(0))),
								CompOp(VariableExpr(Name("i")), LessThan, Value(IntegerValue(10000))),
								Assignment(Name("i"), BinaryOp(VariableExpr(Name("i")), Plus, Value(IntegerValue(1)))),
								[
								Assignment(Name("a"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))));
								])
							] in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(20000) &&
						SymbolTable.is_undefined (Name("i")) s
			);
			("for loop with break: var a=0;for(var i=0;i<10000;i=i+1){a=a+2;break} a=2? i not in scope?", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							For(Declaration(Name("i"), Value(IntegerValue(0))),
								CompOp(VariableExpr(Name("i")), LessThan, Value(IntegerValue(10000))),
								Assignment(Name("i"), BinaryOp(VariableExpr(Name("i")), Plus, Value(IntegerValue(1)))),
								[
								Assignment(Name("a"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))));
								Break;
								])
							] in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(2) &&
						SymbolTable.is_undefined (Name("i")) s
			);
			("for loop with continue: var a=0;for(var i=0;i<10000;i=i+1){a=a+2;continue;a=a+2} a=20000? i not in scope?", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							For(Declaration(Name("i"), Value(IntegerValue(0))),
								CompOp(VariableExpr(Name("i")), LessThan, Value(IntegerValue(10000))),
								Assignment(Name("i"), BinaryOp(VariableExpr(Name("i")), Plus, Value(IntegerValue(1)))),
								[
								Assignment(Name("a"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))));
								Continue;
								Assignment(Name("a"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))));
								])
							] in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(20000) &&
						SymbolTable.is_undefined (Name("i")) s
			);
			("if test: var a=0;for(var i=0;i<10000;i=i+1){if (i%2=1){continue};a=a+2} a=10000? i not in scope?", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							For(Declaration(Name("i"), Value(IntegerValue(0))),
								CompOp(VariableExpr(Name("i")), LessThan, Value(IntegerValue(10000))),
								Assignment(Name("i"), BinaryOp(VariableExpr(Name("i")), Plus, Value(IntegerValue(2)))),
								[
								If(CompOp(BinaryOp(VariableExpr(Name("i")), Modulo, Value(IntegerValue(1))),
										Equal, Value(IntegerValue(1))),
									[ Continue ],[ Noop ]);
								Assignment(Name("a"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))));
								])
							] in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(10000) &&
						SymbolTable.is_undefined (Name("i")) s
			);
			("static scoping test: var x=2;var f=function(){return x;};\n\t\tvar g=function(){var x=1;return f();} var y=g();y=2?", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("x"), Value(IntegerValue(2)));
							Declaration(Name("f"), Value(FunctionValue([],
										[Return(VariableExpr(Name("x")))], SymbolTable.dummy_table)));
							Declaration(Name("g"), Value(FunctionValue([],[
										Declaration(Name("x"), Value(IntegerValue(1)));
										Return(FunctionCall(Name("f"),[]));], SymbolTable.dummy_table)));
							Declaration(Name("y"), FunctionCall(Name("g"),[]));
							] in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("y")) s = IntegerValue(2)
			);
			("function call args are assigned correctely: var a=0;var b=0; var f=function(x,y){a=x;b=y;}\n\t f(1,2); a=1? b=2?", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							Declaration(Name("b"), Value(IntegerValue(0)));
							Declaration(Name("f"), Value(FunctionValue(["x";"y"],
										[ Assignment(Name("a"), VariableExpr(Name("x")));
										Assignment(Name("b"), VariableExpr(Name("y")))], SymbolTable.dummy_table)));
							Ast.ExpressionStatement(FunctionCall(Name("f"),[Value(IntegerValue(1)); Value(IntegerValue(2))]));
							] in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(1) && SymbolTable.get_value (Name("b")) s = IntegerValue(2)
			);
			("function call with too few arguments should throw MismatchedCallArgs", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							Declaration(Name("b"), Value(IntegerValue(0)));
							Declaration(Name("f"), Value(FunctionValue(["x";"y"],
										[ Assignment(Name("a"), VariableExpr(Name("x")));
										Assignment(Name("b"), VariableExpr(Name("y")))], SymbolTable.dummy_table)));
							Ast.ExpressionStatement(FunctionCall(Name("f"),[Value(IntegerValue(1))]));
							] in
						try
							Interpreter.interpret_statements stmts s; false
						with
						| MismatchedCallArgs("f") -> true
						| e -> raise e
				
			);
			("function call with too many arguments should throw MismatchedCallArgs", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							Declaration(Name("b"), Value(IntegerValue(0)));
							Declaration(Name("f"), Value(FunctionValue(["x";"y"],
										[ Assignment(Name("a"), VariableExpr(Name("x")));
										Assignment(Name("b"), VariableExpr(Name("y")))], SymbolTable.dummy_table)));
							Ast.ExpressionStatement(FunctionCall(Name("f"),
									[Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3))]));
							] in
						try
							Interpreter.interpret_statements stmts s; false
						with
						| MismatchedCallArgs("f") -> true
						| e -> raise e
				
			);
			("call function with vararg", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							Declaration(Name("b"), Value(MapValue(Hashtbl.create 10, ArraySubtype)));
							Declaration(Name("f"), Value(FunctionValue(["x";"[y"],
										[ Assignment(Name("a"), VariableExpr(Name("x")));
										Assignment(Name("b"), VariableExpr(Name("y")))], SymbolTable.dummy_table)));
							Ast.ExpressionStatement(FunctionCall(Name("f"),
									[Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3))]));
							] in
						Interpreter.interpret_statements stmts s;
						IntegerValue(1) = SymbolTable.get_value (Name "a") s &&
						IntegerValue(2) = SymbolTable.get_value (CompoundName(["b";"0"])) s &&
						IntegerValue(3) = SymbolTable.get_value (CompoundName(["b";"1"])) s &&
						match SymbolTable.get_value (Name "b") s with
						| MapValue(h, ArraySubtype) -> Hashtbl.find h "length" = IntegerValue(2)
						| _ -> false
			);
			("call function with vararg, fewer values than formal arguments", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							Declaration(Name("b"), Value(MapValue(Hashtbl.create 10, ArraySubtype)));
							Declaration(Name("f"), Value(FunctionValue(["x";"[y"],
										[ Assignment(Name("a"), VariableExpr(Name("x")));
										Assignment(Name("b"), VariableExpr(Name("y")))], SymbolTable.dummy_table)));
							Ast.ExpressionStatement(FunctionCall(Name("f"),
									[Value(IntegerValue(1))]));
							] in
						Interpreter.interpret_statements stmts s;
						IntegerValue(1) = SymbolTable.get_value (Name "a") s &&
						match SymbolTable.get_value (Name "b") s with
						| MapValue(h, ArraySubtype) -> Hashtbl.find h "length" = IntegerValue(0)
						| _ -> false
			);
			("function with vararg not in last position should throw VarArgsMustbeLast", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Declaration(Name("a"), Value(IntegerValue(0)));
							Declaration(Name("b"), Value(MapValue(Hashtbl.create 10, ArraySubtype)));
							Declaration(Name("f"), Value(FunctionValue(["[x";"y"],
										[ Assignment(Name("a"), VariableExpr(Name("x")));
										Assignment(Name("b"), VariableExpr(Name("y")))], SymbolTable.dummy_table)));
							Ast.ExpressionStatement(FunctionCall(Name("f"),
									[Value(IntegerValue(1))]));
							] in
						try
							Interpreter.interpret_statements stmts s; false
						with
						| VarArgsMustbeLast _ -> true
						| e -> raise e
			);
			
			])
	
end