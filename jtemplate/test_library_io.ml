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
			("print(data...)", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("print"),
									[Value(StringValue("test of print\n"))]))
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						true
			);
			("println(data...)", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(Name("println"),
									[Value(StringValue("test of println"))]))
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						true
			);
			("open/write/close file", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForWriting"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"write"]),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						true
			);
			("open/readln/eof/close file", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(CompoundName(["File";"eof"]),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						StringValue("this is a test") = SymbolTable.get_value (Name("data")) symbol_table &&
						BooleanValue(true) = SymbolTable.get_value (Name("atEnd")) symbol_table
			);
			("readln of last line without trailing \\n succeeds", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForWriting"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"write"]),[Value(IntegerValue(1));
									Value(StringValue("this is a test"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						StringValue("this is a test") = SymbolTable.get_value (Name("data")) symbol_table
			);
			("reuse of closed handle", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForWriting"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"write"]),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(CompoundName(["File";"eof"]),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						StringValue("this is a test") = SymbolTable.get_value (Name("data")) symbol_table &&
						BooleanValue(true) = SymbolTable.get_value (Name("atEnd")) symbol_table
			);
			("open for read with already open handle throws LibraryError", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForWriting"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"write"]),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(CompoundName(["File";"eof"]),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
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
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(CompoundName(["File";"eof"]),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForWriting"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"write"]),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
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
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForWriting"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"write"]),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(1))]));
							Declaration(Name("atEnd"), FunctionCall(CompoundName(["File";"eof"]),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
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
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"write"]),[Value(IntegerValue(1));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
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
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test123.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(2))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
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
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(2))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
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
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForWriting"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"write"]),[Value(IntegerValue(2));
									Value(StringValue("this is a test\n"))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(1));]));
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
							ExpressionStatement(FunctionCall(CompoundName(["File";"openForReading"]),[Value(IntegerValue(1));
									Value(StringValue("test1.txt"))]));
							Declaration(Name("data"), FunctionCall(CompoundName(["File";"readln"]),[Value(IntegerValue(1))]));
							ExpressionStatement(FunctionCall(CompoundName(["File";"close"]),[Value(IntegerValue(2));]));
							] in
						try
							let _ = Interpreter.interpret_statements stmts symbol_table in false
						with
						| LibraryError _ -> true
						| _ -> false
			);
			("File.exists()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library IOLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("shouldbetrue"), FunctionCall(CompoundName(["File";"exists"]),[Value(StringValue("test1.txt"))]));
							Declaration(Name("shouldbefalse"), FunctionCall(CompoundName(["File";"exists"]),[Value(StringValue("test1234.txt"))]));
							] in
						let _ = Interpreter.interpret_statements stmts symbol_table in
						BooleanValue(true) = SymbolTable.get_value (Name("shouldbetrue")) symbol_table &&
						BooleanValue(true) = SymbolTable.get_value (Name("shouldbetrue")) symbol_table
			);
			])
	
end
