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
			("print(data)", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("print"),
									[Value(StringValue("test of print\n"))]))
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						true
			);
			("println(data)", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("println"),
									[Value(StringValue("test of println"))]))
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						true
			);
			("writeFile(filename, data)", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("writeFile"),
									[Value(StringValue("test.txt")); Value(StringValue("this is line 1\nLine 2\n"))]))
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
			("open/write/close file", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForWriting"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(Name("write"),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						true
			);
			("open/readln/eof/close file", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(Name("eof"),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						StringValue("this is a test") = SymbolTable.get_value (Name("data")) symbol_table &&
						BooleanValue(true) = SymbolTable.get_value (Name("atEnd")) symbol_table
			);
			("readln of last line without trailing \\n succeeds", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForWriting"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(Name("write"),[Value(IntegerValue(1));
									Value(StringValue("this is a test"))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						StringValue("this is a test") = SymbolTable.get_value (Name("data")) symbol_table
			);
			("reuse of closed handle", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForWriting"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(Name("write"),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(Name("eof"),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						StringValue("this is a test") = SymbolTable.get_value (Name("data")) symbol_table &&
						BooleanValue(true) = SymbolTable.get_value (Name("atEnd")) symbol_table
			);
			("open for read with already open handle throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForWriting"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(Name("write"),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(Name("eof"),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("open for write with already open handle throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(Name("eof"),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(Name("openFileForWriting"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(Name("write"),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("reading from handle opened for write throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForWriting"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(Name("write"),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(Name("eof"),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("writing to handle opened for read throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(Name("write"),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("opening non existent file throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test123.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(2))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("reading from unknown handle throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(2))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("writing to unknwown handle throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForWriting"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(Name("write"),[Value(IntegerValue(2));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(1));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("closing unknown handle throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("openFileForReading"),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(Name("readln"),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(Name("closeFile"),[Value(IntegerValue(2));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("fileExists()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("shouldbetrue"), FunctionCall(Name("fileExists"),[Value(StringValue("test1.txt"))]));
							Declaration(Name("shouldbefalse"), FunctionCall(Name("fileExists"),[Value(StringValue("test1234.txt"))]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						BooleanValue(true) = SymbolTable.get_value (Name("shouldbetrue")) symbol_table &&
						BooleanValue(true) = SymbolTable.get_value (Name("shouldbetrue")) symbol_table
			);
			])
	
end
