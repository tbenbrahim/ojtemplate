module TestSymbolTable =
struct
	open Test_helper
	open Symbol_table
	open Stringmap
	
	let test_suite = ("Symbol Table",[
			("simple declaration a=1, b=2", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let _ = SymbolTable.declare (Ast.Name("b")) (SymbolTable.IntegerValue(2)) s in
						true
			);
			("simple retrieval a=1, b=2", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.declare (Ast.Name("b")) (SymbolTable.IntegerValue(2)) s in
						match SymbolTable.get_value (Ast.Name("a")) s with
							SymbolTable.IntegerValue(1) -> (
									match SymbolTable.get_value (Ast.Name("b")) s with
										SymbolTable.IntegerValue(2) -> true
									| _ -> false)
						| _ -> false
			);
			("compound declaration a={}, a.b={}, a.b.c=1", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.MapValue(StringMap.empty)) s in
						let _ = SymbolTable.declare (Ast.CompoundName(["a";"b";"c"])) (SymbolTable.IntegerValue(1)) s in
						true
			);
			("compound retrieval a={}, a.b={}, a.b.c=1", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.MapValue(StringMap.empty)) s in
						let name = Ast.CompoundName(["a";"b";"c"]) in
						let s = SymbolTable.declare name (SymbolTable.IntegerValue(1)) s in
						(match SymbolTable.get_value name s with
							|	SymbolTable.IntegerValue(1) -> true
							| _ -> false)
			);
			("multiple declaration to map preserves values a={}, a.b={}, a.b.c=1, a.b.d=2", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.MapValue(StringMap.empty)) s in
						let name1 = Ast.CompoundName(["a";"b";"c"]) in
						let name2 = Ast.CompoundName(["a";"b";"d"]) in
						let s = SymbolTable.declare name1 (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.declare name2 (SymbolTable.IntegerValue(2)) s in
						match SymbolTable.get_value name1 s with
							SymbolTable.IntegerValue(1) -> (
									match SymbolTable.get_value name2 s with
										SymbolTable.IntegerValue(2) -> true
									| _ -> false)
						| _ -> false
			);
			("a=1,a.b=2 should throw NotAMap(\"a\" \"a.b\")", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						try
							let _ = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(2)) s in
							false
						with
						| SymbolTable.NotAMap ("a", "a.b") -> true
						| _ -> false
			);
			("a={},a.b=2, a.b.c=1 should throw NotAMap(\"b\" \"a.b.c\")", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(2)) s in
						try
							let _ = SymbolTable.declare (Ast.CompoundName(["a";"b";"c"])) (SymbolTable.IntegerValue(1)) s in
							false
						with
						| SymbolTable.NotAMap ("b", "a.b.c") -> true
						| _ -> false
			);
			("a.b=1 should throw ReferenceToUndefinedMapVariable(\"a\",\"a.b\") ", fun() ->
						let s = SymbolTable.initialize in
						try
							let _ = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(1)) s in
							false
						with
						| SymbolTable.ReferenceToUndefinedMapVariable ("a", "a.b") -> true
						| _ -> false
			);
			("a={} a.b.c=1 should throw ReferenceToUndefinedMapVariable(\"b\",\"a.b.c\") ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						try
							let _ = SymbolTable.declare (Ast.CompoundName(["a";"b";"c"])) (SymbolTable.IntegerValue(1)) s in
							false
						with
						| SymbolTable.ReferenceToUndefinedMapVariable ("b", "a.b.c") -> true
						| _ -> false
			);
			("reference to undeclared a should throw ReferenceToUndefinedVariable(\"a\") ", fun() ->
						let s = SymbolTable.initialize in
						try
							let _ = SymbolTable.get_value (Ast.Name("a")) s in
							false
						with
						| SymbolTable.ReferenceToUndefinedVariable ("a") -> true
						| _ -> false
			);
			("reference to undeclared a.b should throw ReferenceToUndefinedMapVariable(\"a\",\"a.b\") ", fun() ->
						let s = SymbolTable.initialize in
						try
							let _ = SymbolTable.get_value (Ast.CompoundName(["a";"b"])) s in
							false
						with
						| SymbolTable.ReferenceToUndefinedMapVariable ("a", "a.b") -> true
						| _ -> false
			);
			("reference to undeclared a.b when a={} should throw ReferenceToUndefinedMapVariable(\"b\",\"a.b\") ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						try
							let _ = SymbolTable.get_value (Ast.CompoundName(["a";"b"])) s in
							false
						with
						| SymbolTable.ReferenceToUndefinedMapVariable ("b", "a.b") -> true
						| _ -> false
			);
			("reference to undeclared a.b.c when a={},a.b={} should throw ReferenceToUndefinedMapVariable(\"c\",\"a.b.c\") ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.MapValue(StringMap.empty)) s in
						try
							let _ = SymbolTable.get_value (Ast.CompoundName(["a";"b";"c"])) s in
							false
						with
						| SymbolTable.ReferenceToUndefinedMapVariable ("c", "a.b.c") -> true
						| _ -> false
			);
			("reference to undeclared a.b.c when a={} should throw ReferenceToUndefinedMapVariable(\"b\",\"a.b.c\") ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						try
							let _ = SymbolTable.get_value (Ast.CompoundName(["a";"b";"c"])) s in
							false
						with
						| SymbolTable.ReferenceToUndefinedMapVariable ("b", "a.b.c") -> true
						| _ -> false
			);
			("reference to undeclared a.b.c when a={},a.b=1 should throw NotAMap(\"b\",\"a.b.c\") ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(1)) s in
						try
							let _ = SymbolTable.get_value (Ast.CompoundName(["a";"b";"c"])) s in
							false
						with
						| SymbolTable.NotAMap ("b", "a.b.c") -> true
						| _ -> false
			);
			("redeclaration of a: a=1, a=2 ", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(2)) s in
						(match SymbolTable.get_value (Ast.Name("a")) s with
							| SymbolTable.IntegerValue(2) -> true
							| _ -> false)
			);
			("visibility of a in nested scope : a=1 { a=1? } ", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.push_scope s in
						(match SymbolTable.get_value (Ast.Name("a")) s with
							| SymbolTable.IntegerValue(1) -> true
							| _ -> false)
			);
			
			("visibility of map in nested scope : a={}, a.b=1 { a.b=1? }", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.push_scope s in
						match SymbolTable.get_value (Ast.CompoundName(["a";"b"])) s with
						| SymbolTable.IntegerValue(1) -> true
						| _ -> false
			);
			
			("adding key to map in nested scope : a={} { a.b=1 } should throw ReferenceToUndefinedMapVariable(\"a\",\"a.b\")", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.push_scope s in
						try
							let _ = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(1)) s in
							false
						with
							SymbolTable.ReferenceToUndefinedMapVariable("a","a.b") -> true
			);
			
			("assigning key value to map in nested scope: a={},a.b=1 { { a.b=2 a.b=2?}}", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.assign (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(2)) s in
						match SymbolTable.get_value (Ast.CompoundName(["a";"b"])) s with
						| SymbolTable.IntegerValue(2) -> true
						| _ -> false
			);
			
			("assigning key value to map in nested scope: a={},a.b=1 { { a.b=2 }} a.b=2?", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.assign (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(2)) s in
						let s = SymbolTable.pop_scope s in
						let s = SymbolTable.pop_scope s in
						match SymbolTable.get_value (Ast.CompoundName(["a";"b"])) s with
						| SymbolTable.IntegerValue(2) -> true
						| _ -> false
			);
			
			("redeclaration of a in nested scope : a=1 { a=2 } a=1? ", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(2)) s in
						let s = SymbolTable.pop_scope s in
						(match SymbolTable.get_value (Ast.Name("a")) s with
							| SymbolTable.IntegerValue(1) -> true
							| _ -> false)
			);
			
			("assignment of a in nested scope : a=1 { a=2 } a=2? ", fun () ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.assign (Ast.Name("a")) (SymbolTable.IntegerValue(2)) s in
						let s = SymbolTable.pop_scope s in
						(match SymbolTable.get_value (Ast.Name("a")) s with
							| SymbolTable.IntegerValue(2) -> true
							| _ -> false)
			);
			
			("function call symbol table: a=1 { b=2;x=function(){}; x(); } a, b and x are in scope of x() ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.declare (Ast.Name("b")) (SymbolTable.IntegerValue(2)) s in
						let s = SymbolTable.declare (Ast.Name("x")) (SymbolTable.FunctionValue([],[], None)) s in
						match SymbolTable.get_value (Ast.Name("x")) s with
						| SymbolTable.FunctionValue(_, _, Some functionScope) ->
								(match SymbolTable.get_value (Ast.Name("b")) functionScope with
									| SymbolTable.IntegerValue(2) -> true
									| _ -> false
								) &&
								(match SymbolTable.get_value (Ast.Name("x")) functionScope with
									| SymbolTable.FunctionValue(_, _, _) -> true
									| _ -> false
								) &&
								(match SymbolTable.get_value (Ast.Name("a")) functionScope with
									| SymbolTable.IntegerValue(1) -> true
									| _ -> false
								)
						| _ -> false
			);
			
			("function call symbol table: a=1;x=function(){} { b=2; x(); } a and x are in scope of x(),b is not ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.declare (Ast.Name("x")) (SymbolTable.FunctionValue([],[], None)) s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.declare (Ast.Name("b")) (SymbolTable.IntegerValue(2)) s in
						match SymbolTable.get_value (Ast.Name("x")) s with
						| SymbolTable.FunctionValue(_, _, Some functionScope) ->
								(try let _ = SymbolTable.get_value (Ast.Name("b")) functionScope in false
								with
								| SymbolTable.ReferenceToUndefinedVariable("b") -> true
								| _ -> false
								) &&
								(match SymbolTable.get_value (Ast.Name("x")) functionScope with
									| SymbolTable.FunctionValue(_, _, _) -> true
									| _ -> false
								) &&
								(match SymbolTable.get_value (Ast.Name("a")) functionScope with
									| SymbolTable.IntegerValue(1) -> true
									| _ -> false
								)
						| _ -> false
			);
			
			("function call symbol table: a=1;x=function(){} { b=2; } x(); a and x are in scope of x(),b is not ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.declare (Ast.Name("x")) (SymbolTable.FunctionValue([],[], None)) s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.declare (Ast.Name("b")) (SymbolTable.IntegerValue(2)) s in
						let s = SymbolTable.pop_scope s in
						match SymbolTable.get_value (Ast.Name("x")) s with
						| SymbolTable.FunctionValue(_, _, Some functionScope) ->
								(try let _ = SymbolTable.get_value (Ast.Name("b")) functionScope in false
								with
								| SymbolTable.ReferenceToUndefinedVariable("b") -> true
								| _ -> false
								) &&
								(match SymbolTable.get_value (Ast.Name("x")) functionScope with
									| SymbolTable.FunctionValue(_, _, _) -> true
									| _ -> false
								) &&
								(match SymbolTable.get_value (Ast.Name("a")) functionScope with
									| SymbolTable.IntegerValue(1) -> true
									| _ -> false
								)
						| _ -> false
			);
			
			("map function call symbol table: a=1 { b={}; b.x=function(){}; b.x(); } a, b and b.x are in scope of b.x() ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.declare (Ast.Name("b")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["b";"x"])) (SymbolTable.FunctionValue([],[], None)) s in
						match SymbolTable.get_value (Ast.CompoundName(["b";"x"])) s with
						| SymbolTable.FunctionValue(_, _, Some functionScope) ->
								(match SymbolTable.get_value (Ast.Name("b")) functionScope with
									| SymbolTable.MapValue(_) -> true
									| _ -> false
								) &&
								(match SymbolTable.get_value (Ast.CompoundName(["b";"x"])) functionScope with
									| SymbolTable.FunctionValue(_, _, _) -> true
									| _ -> false
								) &&
								(match SymbolTable.get_value (Ast.Name("a")) functionScope with
									| SymbolTable.IntegerValue(1) -> true
									| _ -> false
								)
						| _ -> false
			);
			
			("redeclaration to different type: a=1, a='1' ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						let _ = SymbolTable.declare (Ast.Name("a")) (SymbolTable.StringValue("1")) s in
						true
			);
			
			("reassignment to different type: a=1, a='1' should fail with TypeMismatchInAssignment(\"a\",\"integer\",\"string\")", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.IntegerValue(1)) s in
						try
							let _ = SymbolTable.assign (Ast.Name("a")) (SymbolTable.StringValue("1")) s in
							false
						with
							SymbolTable.TypeMismatchInAssignment("a","integer","string") -> true
						| _ -> false
			);
			
			("map redeclaration to different type: a={b:1}, a.b='1' ", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(1)) s in
						let _ = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.StringValue("1")) s in
						true
			);
			
			("reassignment to different type: a={b:1}, a.b='1'  should fail with TypeMismatchInMapAssignment(\"a\",\"a.b\",\"integer\",\"string\")", fun() ->
						let s = SymbolTable.initialize in
						let s = SymbolTable.declare (Ast.Name("a")) (SymbolTable.MapValue(StringMap.empty)) s in
						let s = SymbolTable.declare (Ast.CompoundName(["a";"b"])) (SymbolTable.IntegerValue(1)) s in
						try
							let _ = SymbolTable.assign (Ast.CompoundName(["a";"b"])) (SymbolTable.StringValue("1")) s in
							false
						with
							SymbolTable.TypeMismatchInMapAssignment("b","a.b","integer","string") -> true
						| _ -> false
			);
			
			])
	
end