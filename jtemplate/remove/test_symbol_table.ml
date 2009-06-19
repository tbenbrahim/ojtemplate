module TestSymbolTable =
struct
	open Test_helper
	open Symbol_table
	open RuntimeError
	open Ast
	
	let test_suite = ("Symbol Table",[
			("simple declaration a=1, b=2", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						SymbolTable.declare (Name("b")) (IntegerValue(2)) s;
						true
			);
			("simple retrieval a=1, b=2", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						SymbolTable.declare (Name("b")) (IntegerValue(2)) s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(1)
			);
			("compound declaration a={}, a.b={}, a.b.c=1", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue ((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b";"c"])) (IntegerValue(1)) s;
						true
			);
			("compound retrieval a={}, a.b={}, a.b.c=1", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						let name = CompoundName(["a";"b";"c"]) in
						SymbolTable.declare name (IntegerValue(1)) s;
						SymbolTable.get_value name s = IntegerValue(1)
			);
			("multiple declaration to map preserves values a={}, a.b={}, a.b.c=1, a.b.d=2", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						let name1 = CompoundName(["a";"b";"c"]) in
						let name2 = CompoundName(["a";"b";"d"]) in
						SymbolTable.declare name1 (IntegerValue(1)) s;
						SymbolTable.declare name2 (IntegerValue(2)) s;
						SymbolTable.get_value name1 s = IntegerValue(1) && SymbolTable.get_value name2 s = IntegerValue(2)
			);
			("a=1,a.b=2 should throw NotAMap(\"a\" \"a.b\")", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						try
							SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(2)) s;
							false
						with
						| NotAMap ("a", "a.b") -> true
						| _ -> false
			);
			("a={},a.b=2, a.b.c=1 should throw NotAMap(\"b\" \"a.b.c\")", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(2)) s;
						try
							SymbolTable.declare (CompoundName(["a";"b";"c"])) (IntegerValue(1)) s;
							false
						with
						| NotAMap ("b", "a.b.c") -> true
						| _ -> false
			);
			("a.b=1 should throw ReferenceToUndefinedVariable(\"a.b\") ", fun() ->
						let s = SymbolTable.initialize() in
						try
							let _ = SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(1)) s in
							false
						with
						| ReferenceToUndefinedVariable ( "a.b") -> true
						| _ -> false
			);
			("a={} a.b.c=1 should throw ReferenceToUndefinedMapVariable(\"b\",\"a.b.c\") ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						try
							SymbolTable.declare (CompoundName(["a";"b";"c"])) (IntegerValue(1)) s ;
							false
						with
						| ReferenceToUndefinedMapVariable ("b", "a.b.c") -> true
						| _ -> false
			);
			("reference to undeclared a should throw ReferenceToUndefinedVariable(\"a\") ", fun() ->
						let s = SymbolTable.initialize() in
						try
							let _ = SymbolTable.get_value (Name("a")) s in
							false
						with
						| ReferenceToUndefinedVariable ("a") -> true
						| _ -> false
			);
			("reference to undeclared a.b should throw ReferenceToUndefinedMapVariable(\"a\",\"a.b\") ", fun() ->
						let s = SymbolTable.initialize() in
						try
							let _ = SymbolTable.get_value (CompoundName(["a";"b"])) s in
							false
						with
						| ReferenceToUndefinedMapVariable ("a", "a.b") -> true
						| _ -> false
			);
			("reference to undeclared a.b when a={} should throw ReferenceToUndefinedMapVariable(\"b\",\"a.b\") ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						try
							let _ = SymbolTable.get_value (CompoundName(["a";"b"])) s in
							false
						with
						| ReferenceToUndefinedMapVariable ("b", "a.b") -> true
						| _ -> false
			);
			("reference to undeclared a.b.c when a={},a.b={} should throw ReferenceToUndefinedMapVariable(\"c\",\"a.b.c\") ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						try
							let _ = SymbolTable.get_value (CompoundName(["a";"b";"c"])) s in
							false
						with
						| ReferenceToUndefinedMapVariable ("c", "a.b.c") -> true
						| _ -> false
			);
			("reference to undeclared a.b.c when a={} should throw ReferenceToUndefinedMapVariable(\"b\",\"a.b.c\") ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						try
							let _ = SymbolTable.get_value (CompoundName(["a";"b";"c"])) s in
							false
						with
						| ReferenceToUndefinedMapVariable ("b", "a.b.c") -> true
						| _ -> false
			);
			("reference to undeclared a.b.c when a={},a.b=1 should throw NotAMap(\"b\",\"a.b.c\") ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(1)) s;
						try
							let _ = SymbolTable.get_value (CompoundName(["a";"b";"c"])) s in
							false
						with
						| NotAMap ("b", "a.b.c") -> true
						| _ -> false
			);
			("redeclaration of a: a=1, a=2 ", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						SymbolTable.declare (Name("a")) (IntegerValue(2)) s;
						SymbolTable.get_value (Name("a")) s = IntegerValue(2)
			);
			("visibility of a in nested scope : a=1 { a=1? } ", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						let s = SymbolTable.push_scope s in
						SymbolTable.get_value (Name("a")) s = IntegerValue(1)
			);
			
			("visibility of map in nested scope : a={}, a.b=1 { a.b=1? }", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(1)) s;
						let s = SymbolTable.push_scope s in
						SymbolTable.get_value (CompoundName(["a";"b"])) s = IntegerValue(1)
			);
			
			("adding key to map in nested scope : a={} { a.b=1 } ", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						let s = SymbolTable.push_scope s in
						let _ = SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(1)) s in
						IntegerValue(1) = SymbolTable.get_value (CompoundName(["a";"b"])) s
			);
			
			("assigning key value to map in nested scope: a={},a.b=1 { { a.b=2 a.b=2?}}", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(1)) s;
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.push_scope s in
						SymbolTable.assign (CompoundName(["a";"b"])) (IntegerValue(2)) s;
						SymbolTable.get_value (CompoundName(["a";"b"])) s = IntegerValue(2)
			);
			
			("assigning key value to map in nested scope: a={},a.b=1 { { a.b=2 }} a.b=2?", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(1)) s;
						let s = SymbolTable.push_scope s in
						let s = SymbolTable.push_scope s in
						SymbolTable.assign (CompoundName(["a";"b"])) (IntegerValue(2)) s;
						let s = SymbolTable.pop_scope s in
						let s = SymbolTable.pop_scope s in
						SymbolTable.get_value (CompoundName(["a";"b"])) s = IntegerValue(2)
			);
			
			("redeclaration of a in nested scope : a=1 { a=2 } a=1? ", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						let s = SymbolTable.push_scope s in
						SymbolTable.declare (Name("a")) (IntegerValue(2)) s;
						let s = SymbolTable.pop_scope s in
						SymbolTable.get_value (Name("a")) s = IntegerValue(1)
			);
			
			("assignment of a in nested scope : a=1 { a=2 } a=2? ", fun () ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						let s = SymbolTable.push_scope s in
						SymbolTable.assign (Name("a")) (IntegerValue(2)) s;
						let s = SymbolTable.pop_scope s in
						SymbolTable.get_value (Name("a")) s = IntegerValue(2)
			);
			
			("function call symbol table: a=1 { b=2;x=function(){}; x(); } a, b and x are in scope of x() ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s ;
						let s = SymbolTable.push_scope s in
						SymbolTable.declare (Name("b")) (IntegerValue(2)) s;
						SymbolTable.declare (Name("x")) (FunctionValue([],[])) s;
						match SymbolTable.get_value (Name("x")) s with
						| ScopedFunctionValue(_, _, functionScope) ->
								SymbolTable.get_value (Name("b")) functionScope = IntegerValue(2)
								&& SymbolTable.get_value (Name("a")) functionScope = IntegerValue(1)
								&&
								(match SymbolTable.get_value (Name("x")) functionScope with
									| ScopedFunctionValue(_, _, _) -> true
									| _ -> false
								)
						| _ -> false
			);
			
			("function call symbol table: a=1;x=function(){} { b=2; x(); } a and x are in scope of x(),b is not ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s ;
						SymbolTable.declare (Name("x")) (FunctionValue([],[])) s ;
						let s = SymbolTable.push_scope s in
						SymbolTable.declare (Name("b")) (IntegerValue(2)) s;
						match SymbolTable.get_value (Name("x")) s with
						| ScopedFunctionValue(_, _, functionScope) ->
								(try let _ = SymbolTable.get_value (Name("b")) functionScope in false
								with
								| ReferenceToUndefinedVariable("b") -> true
								| _ -> false
								) &&
								(match SymbolTable.get_value (Name("x")) functionScope with
									| ScopedFunctionValue(_, _, _) -> true
									| _ -> false
								) &&
								SymbolTable.get_value (Name("a")) functionScope = IntegerValue(1)
						| _ -> false
			);
			
			("function call symbol table: a=1;x=function(){} { b=2; } x(); a and x are in scope of x(),b is not ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						SymbolTable.declare (Name("x")) (FunctionValue([],[])) s;
						let s = SymbolTable.push_scope s in
						SymbolTable.declare (Name("b")) (IntegerValue(2)) s;
						let s = SymbolTable.pop_scope s in
						match SymbolTable.get_value (Name("x")) s with
						| ScopedFunctionValue(_, _, functionScope) ->
								(try let _ = SymbolTable.get_value (Name("b")) functionScope in false
								with
								| ReferenceToUndefinedVariable("b") -> true
								| _ -> false
								) &&
								(match SymbolTable.get_value (Name("x")) functionScope with
									| ScopedFunctionValue(_, _, _) -> true
									| _ -> false
								) &&
								SymbolTable.get_value (Name("a")) functionScope = IntegerValue(1)
						| _ -> false
			);
			
			("map function call symbol table: a=1 { b={}; b.x=function(){}; b.x(); } a, b and b.x are in scope of b.x() ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						let s = SymbolTable.push_scope s in
						SymbolTable.declare (Name("b")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["b";"x"])) (FunctionValue([],[])) s;
						match SymbolTable.get_value (CompoundName(["b";"x"])) s with
						| ScopedFunctionValue(_, _, functionScope) ->
								(match SymbolTable.get_value (Name("b")) functionScope with
									| MapValue(_) -> true
									| _ -> false
								) &&
								(match SymbolTable.get_value (CompoundName(["b";"x"])) functionScope with
									| ScopedFunctionValue(_, _, _) -> true
									| _ -> false
								) &&
								SymbolTable.get_value (Name("a")) functionScope = IntegerValue(1)
						| _ -> false
			);
			
			("redeclaration to different type: a=1, a='1' ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						SymbolTable.declare (Name("a")) (StringValue("1")) s;
						true
			);
			
			("reassignment to different type: a=1, a='1' should fail with TypeMismatchInAssignment(\"a\",\"integer\",\"string\")", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s;
						try
							SymbolTable.assign (Name("a")) (StringValue("1")) s;
							false
						with
							TypeMismatchInAssignment("a","integer","string") -> true
						| _ -> false
			);
			
			("map redeclaration to different type: a={b:1}, a.b='1' ", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(1)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (StringValue("1")) s;
						true
			);
			
			("reassignment to different type: a={b:1}, a.b='1'  should fail with TypeMismatchInMapAssignment(\"b\",\"a.b\",\"integer\",\"string\")", fun() ->
						let s = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (MapValue((Hashtbl.create 10), MapSubtype)) s;
						SymbolTable.declare (CompoundName(["a";"b"])) (IntegerValue(1)) s;
						try
							let _ = SymbolTable.assign (CompoundName(["a";"b"])) (StringValue("1")) s in
							false
						with
							TypeMismatchInMapAssignment("b","a.b","integer","string") -> true
						| _ -> false
			);
			
			("assignment of a in nested scope visible in original table : a=1 { a=2 } a=2? ", fun () ->
						let s1 = SymbolTable.initialize() in
						SymbolTable.declare (Name("a")) (IntegerValue(1)) s1;
						let s2 = SymbolTable.push_scope s1 in
						SymbolTable.assign (Name("a")) (IntegerValue(2)) s2;
						SymbolTable.get_value (Name("a")) s1 = IntegerValue(2)
			);
			
			])
	
end