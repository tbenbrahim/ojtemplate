module TestIOLibrary =
struct
	open Test_helper
	open Symbol_table
	open RuntimeError
	open Ast
	open Library_helper
	open Library_io
	open Interpreter
	
	let test_suite = ("I/O library",[
			("writeFile(filename, data)", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("writeFile"),
									[Value(StringValue("test.txt")); Value(StringValue("this is line 1\nLine 2"))]))
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						true
			);
			("data=readFile(filename)", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("data"), FunctionCall(Name("readFile"),
									[Value(StringValue("test.txt"))]))
							] in 
						let _ = Interpreter.interpret_statements stmts symbol_table in
						StringValue("this is line 1\nLine 2\n") = SymbolTable.get_value (Name("data")) symbol_table
			);
			
			])
	
end
