module TestStringLibrary =
struct
	open Test_helper
	open Symbol_table
	open RuntimeError
	open Ast
	open Library_helper
	open Library_string
	open Interpreter
	
	let test_suite = ("String Library",[
			("toUppercase()", fun () -> 
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr = FunctionCall(CompoundName(["String";"toUppercase"]),[Value(StringValue("test"))]) in
						StringValue("TEST") = Interpreter.evaluate_expression expr symbol_table
			);
			("toLowercase()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr = FunctionCall(CompoundName(["String";"toLowercase"]),[Value(StringValue("TeSt"))]) in
						StringValue("test") = Interpreter.evaluate_expression expr symbol_table
			);
			("toFirstUpper()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr = FunctionCall(CompoundName(["String";"toFirstUpper"]),[Value(StringValue("test"))]) in
						StringValue("Test") = Interpreter.evaluate_expression expr symbol_table
			);
			("toFirstLower()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr = FunctionCall(CompoundName(["String";"toFirstLower"]),[Value(StringValue("TeSt"))]) in
						StringValue("teSt") = Interpreter.evaluate_expression expr symbol_table
			);
			("length()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr1 = FunctionCall(CompoundName(["String";"length"]),[Value(StringValue(""))]) in
						let expr2 = FunctionCall(CompoundName(["String";"length"]),[Value(StringValue("test"))]) in
						IntegerValue(0) = Interpreter.evaluate_expression expr1 symbol_table &&
						IntegerValue(4) = Interpreter.evaluate_expression expr2 symbol_table
			);
			("charAt()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr1 = FunctionCall(CompoundName(["String";"charAt"]),[Value(StringValue("abcd")); Value(IntegerValue(0))]) in
						let expr2 = FunctionCall(CompoundName(["String";"charAt"]),[Value(StringValue("abcd")); Value(IntegerValue(3))]) in
						let expr3 = FunctionCall(CompoundName(["String";"charAt"]),[Value(StringValue("abcd")); Value(IntegerValue(5))]) in
						let expr4 = FunctionCall(CompoundName(["String";"charAt"]),[Value(StringValue("abcd")); Value(StringValue("a"))]) in
						StringValue("a") = Interpreter.evaluate_expression expr1 symbol_table &&
						StringValue("d") = Interpreter.evaluate_expression expr2 symbol_table &&
						(try let _ = Interpreter.evaluate_expression expr3 symbol_table in false with LibraryError _ -> true | _ -> false) &&
						(try let _ = Interpreter.evaluate_expression expr4 symbol_table in false with LibraryError _ -> true | _ -> false)
			);
			("indexOf()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr1 = FunctionCall(CompoundName(["String";"indexOf"]),[Value(StringValue("abcdef")); Value(StringValue("cde"))]) in
						let expr2 = FunctionCall(CompoundName(["String";"indexOf"]),[Value(StringValue("test")); Value(StringValue("cde"))]) in
						IntegerValue(2) = Interpreter.evaluate_expression expr1 symbol_table &&
						IntegerValue(- 1) = Interpreter.evaluate_expression expr2 symbol_table
			);
			("substr()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr1 = FunctionCall(CompoundName(["String";"substr"]),[Value(StringValue("abcdef")); Value(IntegerValue(2)); Value(IntegerValue(3))]) in
						let expr2 = FunctionCall(CompoundName(["String";"substr"]),[Value(StringValue("abcdef")); Value(IntegerValue(9)); Value(IntegerValue(3))]) in
						StringValue("cde") = Interpreter.evaluate_expression expr1 symbol_table &&
						(try let _ = Interpreter.evaluate_expression expr2 symbol_table in false with LibraryError _ -> true | _ -> false)
			);
			("startsWith()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr1 = FunctionCall(CompoundName(["String";"startsWith"]),[Value(StringValue("abcdef")); Value(StringValue("abc"))]) in
						let expr2 = FunctionCall(CompoundName(["String";"startsWith"]),[Value(StringValue("abcdef")); Value(StringValue("cde"))]) in
						BooleanValue(true) = Interpreter.evaluate_expression expr1 symbol_table &&
						BooleanValue(false) = Interpreter.evaluate_expression expr2 symbol_table
			);
			("endsWith()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr1 = FunctionCall(CompoundName(["String";"endsWith"]),[Value(StringValue("abcdef")); Value(StringValue("def"))]) in
						let expr2 = FunctionCall(CompoundName(["String";"endsWith"]),[Value(StringValue("abcdef")); Value(StringValue("cde"))]) in
						BooleanValue(true) = Interpreter.evaluate_expression expr1 symbol_table &&
						BooleanValue(false) = Interpreter.evaluate_expression expr2 symbol_table
			);
			("replaceAll()", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr1 = FunctionCall(CompoundName(["String";"replaceAll"]),[Value(StringValue("abcdabefab")); Value(StringValue("ab")); Value(StringValue("xyz"))]) in
						let expr2 = FunctionCall(CompoundName(["String";"replaceAll"]),[Value(StringValue("abcdabefab")); Value(StringValue("xy")); Value(StringValue("xx"))]) in
						StringValue("xyzcdxyzefxyz") = Interpreter.evaluate_expression expr1 symbol_table &&
						StringValue("abcdabefab") = Interpreter.evaluate_expression expr2 symbol_table
			);
			("split()", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library StringLibrary.exported symbol_table;
						let expr1 = FunctionCall(CompoundName(["String";"split"]),[Value(StringValue("xabcdabefab")); Value(StringValue("ab"))]) in
						let expr2 = FunctionCall(CompoundName(["String";"split"]),[Value(StringValue("abcdabefab")); Value(StringValue("xy"))]) in
						(match Interpreter.evaluate_expression expr1 symbol_table with
							| MapValue(hashtbl,ArraySubtype) -> (StringValue("x") = Hashtbl.find hashtbl "0") &&
									(StringValue("cd") = Hashtbl.find hashtbl "1") &&
									(StringValue("ef") = Hashtbl.find hashtbl "2") &&
									(StringValue("") = Hashtbl.find hashtbl "3") &&
									(IntegerValue(4) = Hashtbl.find hashtbl "length")
							| _ -> false) &&
						(match Interpreter.evaluate_expression expr2 symbol_table with
							| MapValue(hashtbl,ArraySubtype) -> (StringValue("abcdabefab") = Hashtbl.find hashtbl "0") &&
									(IntegerValue(1) = Hashtbl.find hashtbl "length")
							| _ -> false)
			);
			])
	
end