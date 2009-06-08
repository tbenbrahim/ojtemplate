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
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s = IntegerValue(9) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Minus, v2)) s = IntegerValue(5) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Times, v2)) s = IntegerValue(14) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Divide, v2)) s = IntegerValue(3) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Modulo, v2)) s = IntegerValue(1)
			);
			("integer comparaison", fun() ->
						let v1 = Value(IntegerValue(7)) in
						let v2 = Value(IntegerValue(2)) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Ast.Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.LessThan, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.GreaterThan, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.LessThanEqual, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.GreaterThanEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.LessThanEqual, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.GreaterThanEqual, v3)) s = BooleanValue(true)
			);
			("floating point arithmetic", fun() ->
						let v1 = Value(FloatValue(2.5)) in
						let v2 = Value(FloatValue(0.5)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s = FloatValue(3.0) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Minus, v2)) s = FloatValue(2.0) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Times, v2)) s = FloatValue(1.25) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Divide, v2)) s = FloatValue(5.0)
			);
			("floating point comparaison", fun() ->
						let v1 = Value(FloatValue(7.1)) in
						let v2 = Value(FloatValue(2.5)) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Ast.Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.LessThan, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.GreaterThan, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.LessThanEqual, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.GreaterThanEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.LessThanEqual, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.GreaterThanEqual, v3)) s = BooleanValue(true)
			);
			("string comparaison", fun() ->
						let v1 = Value(StringValue("abc")) in
						let v2 = Value(StringValue("XYZ")) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Ast.Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.LessThan, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.GreaterThan, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.LessThanEqual, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.GreaterThanEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.LessThanEqual, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.GreaterThanEqual, v3)) s = BooleanValue(true)
			);
			("boolean comparaison", fun() ->
						let v1 = Value(BooleanValue(true)) in
						let v2 = Value(BooleanValue(false)) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Ast.Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.NotEqual, v3)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.Equal, v2)) s = BooleanValue(false)
			);
			("mixed floating point/integer arithmetic ", fun() ->
						let v1 = Value(FloatValue(3.5)) in
						let v2 = Value(IntegerValue(2)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s = FloatValue(5.5) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Minus, v2)) s = FloatValue(1.5) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Times, v2)) s = FloatValue(7.0) &&
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Divide, v2)) s = FloatValue(1.75)
			);
			("mixed floating point/integer comparaison", fun() ->
						let v1 = Value(FloatValue(7.1)) in
						let v2 = Value(IntegerValue(2)) in
						let v3 = Value(FloatValue(2.0)) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (CompOp(v2, Ast.Equal, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.NotEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.LessThan, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.GreaterThan, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.LessThanEqual, v2)) s = BooleanValue(false) &&
						Interpreter.evaluate_expression (CompOp(v1, Ast.GreaterThanEqual, v2)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.LessThanEqual, v3)) s = BooleanValue(true) &&
						Interpreter.evaluate_expression (CompOp(v2, Ast.GreaterThanEqual, v3)) s = BooleanValue(true)
			);
			("string addition", fun() ->
						let v1 = Value(StringValue("2")) in
						let v2 = Value(StringValue("3")) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s = StringValue("23")
			);
			("integer/string addition", fun() ->
						let v1 = Value(IntegerValue(2)) in
						let v2 = Value(StringValue("3")) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s = StringValue("23")
			);
			("string/float addition", fun() ->
						let v1 = Value(FloatValue(2.5)) in
						let v2 = Value(StringValue("3")) in
						let s = SymbolTable.initialize() in
						Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s = StringValue("2.53")
			);
			("expression modulo 3.0 and 2.0 throws EInvalidOperation(Ast.Modulo, \"float\")", fun() ->
						let v1 = Value(FloatValue(3.0)) in
						let v2 = Value(FloatValue(2.0)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Modulo, v2)) s in false
						with
						| Interpreter.EInvalidOperation(Ast.Modulo, "float") -> true
						| _ -> false
			);
			("expression modulo 3.0 and 2 throws EInvalidOperation(Ast.Modulo, \"float\")", fun() ->
						let v1 = Value(FloatValue(3.0)) in
						let v2 = Value(IntegerValue(2)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Modulo, v2)) s in false
						with
						| Interpreter.EInvalidOperation(Ast.Modulo, "float") -> true
						| _ -> false
			);
			("expression add 2 and true throws EIncompatibleTypes(\"integer\", \"boolean\")", fun() ->
						let v1 = Value(IntegerValue(2)) in
						let v2 = Value(BooleanValue(true)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s in
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
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s in
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
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Plus, v2)) s in
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
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Minus, v2)) s in
							false
						with
						| Interpreter.EInvalidOperation(Ast.Minus, "string") -> true
						| _ -> false
			);
			("expression multiply 'xx' and 'x' throws EInvalidOperation(Times, \"string\")", fun() ->
						let v1 = Value(StringValue("xx")) in
						let v2 = Value(StringValue("x")) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Times, v2)) s in
							false
						with
						| Interpreter.EInvalidOperation(Ast.Times, "string") -> true
						| _ -> false
			);
			("expression divide 'xx' and 'x' throws EInvalidOperation(Divide, \"string\")", fun() ->
						let v1 = Value(StringValue("xx")) in
						let v2 = Value(StringValue("x")) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Divide, v2)) s in
							false
						with
						| Interpreter.EInvalidOperation(Ast.Divide, "string") -> true
						| _ -> false
			);
			("expression modulo 'xx' and 'x' throws EInvalidOperation(Modulo, \"string\")", fun() ->
						let v1 = Value(StringValue("xx")) in
						let v2 = Value(StringValue("x")) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (BinaryOp(v1, Ast.Modulo, v2)) s in
							false
						with
						| Interpreter.EInvalidOperation(Ast.Modulo, "string") -> true
						| _ -> false
			);
			("boolean < comparaison throws EInvalidComparaison(LessThan, \"boolean\")", fun() ->
						let v2 = Value(BooleanValue(false)) in
						let v3 = v2 in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (CompOp(v2, Ast.LessThan, v3)) s in false
						with
							Interpreter.EInvalidComparaison(Ast.LessThan, "boolean") -> true
						| _ -> false
			);
			("comparaison of two mismtached types throws EMismatchedTypeInCompare(\"integer\", \"string\")", fun() ->
						let v1 = Value(StringValue("3")) in
						let v2 = Value(IntegerValue(1)) in
						let s = SymbolTable.initialize() in
						try
							let _ = Interpreter.evaluate_expression (CompOp(v2, Ast.LessThan, v1)) s in false
						with
							Interpreter.EMismatchedTypeInCompare("integer", "string") -> true
						| _ -> false
			);
			("scalar declaration: var a=1;a=1?", fun() ->
						let stmt = Ast.Declaration(Name("a"), Value(IntegerValue(1))) in
						let s = SymbolTable.initialize () in
						Interpreter.interpret_statement stmt s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(1)
			);
			("expression declaration: var a=1;var b=a+2;b=3?", fun() ->
						let stmts =
							[Ast.Declaration(Name("a"), Value(IntegerValue(1)));
							Ast.Declaration(Name("b"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))))
							] in
						let s = SymbolTable.initialize () in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("b")) s = IntegerValue(3)
			);
			("expression assignment: var a=1;var a=a+2;a=3?", fun() ->
						let stmts =
							[Ast.Declaration(Name("a"), Value(IntegerValue(1)));
							Ast.Declaration(Name("a"), BinaryOp(VariableExpr(Name("a")), Plus, Value(IntegerValue(2))))
							] in
						let s = SymbolTable.initialize () in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(3)
			);
			("array length test: var a=[1,2,3];a.length=3?", fun () ->
						let s = SymbolTable.initialize() in
						let expr = ArrayExpr([Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3));]) in
						let stmt = Ast.Declaration(Name("a"), expr) in
						Interpreter.interpret_statement stmt s;
						SymbolTable.get_value (CompoundName(["a";"length"])) s = IntegerValue(3)
			);
			("for loop: var a=0;for(var i=0;i<10000-;i=i+1){a=a+2} a=20000? i not in scope?", fun() ->
						let s = SymbolTable.initialize() in
						let stmts = [
							Ast.Declaration(Name("a"), Value(IntegerValue(0)));
							Ast.For(Ast.Declaration(Name("i"), Ast.Value(IntegerValue(0))),
								Ast.CompOp(Ast.VariableExpr(Name("i")), Ast.LessThan, Ast.Value(IntegerValue(10000))),
								Ast.Assignment(Name("i"), Ast.BinaryOp(Ast.VariableExpr(Name("i")), Ast.Plus, Ast.Value(IntegerValue(1)))),
								[
								    Ast.Assignment(Name("a"),Ast.BinaryOp(Ast.VariableExpr(Name("a")),Ast.Plus,Ast.Value(IntegerValue(2))));
								])
							] in
						Interpreter.interpret_statements stmts s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(20000) &&
						SymbolTable.is_undefined (Name("i")) s
			);
			
			])
	
end