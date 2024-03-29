module TestBuiltinLibrary =
struct
	open Test_helper
	open Symbol_table
	open RuntimeError
	open Ast
	open Library_helper
	open Library_builtin
	open Interpreter
	
	let test_suite = ("Builtin Library",[
			("Array.push()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("arr"), ArrayExpr([Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3));]),("",0));
							ExpressionStatement(FunctionCall(CompoundName(["Array";"push"]),[VariableExpr(Name("arr")); Value(StringValue("x"))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						StringValue("x") = SymbolTable.get_value (CompoundName(["arr";"3"])) symbol_table &&
						(match SymbolTable.get_value (Name("arr")) symbol_table with
							| MapValue(h, ArraySubtype) -> Hashtbl.find h "length" = IntegerValue(4)
							| _ -> false)
			);
			("Array.pop()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("arr"), ArrayExpr([Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3));]),("",0));
							Declaration(Name("hd"), FunctionCall(CompoundName(["Array";"pop"]),[VariableExpr(Name("arr"))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						IntegerValue(3) = SymbolTable.get_value (Name "hd") symbol_table &&
						(match SymbolTable.get_value (Name("arr")) symbol_table with
							| MapValue(h, ArraySubtype) -> Hashtbl.find h "length" = IntegerValue(2)
							| _ -> false)
			);
			("Array.length()", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("arr"), ArrayExpr([Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3));]),("",0));
							Declaration(Name("len"), FunctionCall(CompoundName(["Array";"length"]),[VariableExpr(Name("arr"))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						IntegerValue(3) = SymbolTable.get_value (Name "len") symbol_table
			);
			("Array.pop() on empty array throws LibraryException", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("arr"), ArrayExpr([]),("",0));
							Declaration(Name("hd"), FunctionCall(CompoundName(["Array";"pop"]),[VariableExpr(Name("arr"))]),("",0));
							] in
						try
							Interpreter.interpret_statements stmts symbol_table; false
						with
						| FatalExit (LibraryError _) -> true
						| _ -> false
			);
			("Array.push() on non array throws LibraryException", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("arr"), MapExpr([("1", Value(IntegerValue(1))); ("2", Value(IntegerValue(2))); ("3", Value(IntegerValue(4)));]),("",0));
							ExpressionStatement(FunctionCall(CompoundName(["Array";"push"]),[VariableExpr(Name("arr")); Value(StringValue("x"))]),("",0));
							] in
						try
							Interpreter.interpret_statements stmts symbol_table; false
						with
						| FatalExit (LibraryError _) -> true
						| _ -> false
			);
			("Array.pop() on non array throws LibraryException", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("arr"), MapExpr([("1", Value(IntegerValue(1))); ("2", Value(IntegerValue(2))); ("3", Value(IntegerValue(4)));]),("",0));
							Declaration(Name("hd"), FunctionCall(CompoundName(["Array";"pop"]),[VariableExpr(Name("arr"))]),("",0));
							] in
						try
							Interpreter.interpret_statements stmts symbol_table; false
						with
						| FatalExit (LibraryError _) -> true
						| _ -> false
			);
			("Array.length() on non array throws LibraryException", fun () ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("arr"), MapExpr([("1", Value(IntegerValue(1))); ("2", Value(IntegerValue(2))); ("3", Value(IntegerValue(4)));]),("",0));
							Declaration(Name("len"), FunctionCall(CompoundName(["Array";"length"]),[VariableExpr(Name("arr"))]),("",0));
							] in
						try
							Interpreter.interpret_statements stmts symbol_table; false
						with
						| FatalExit (LibraryError _)-> true
						| _ -> false
			);
			("Map.remove()", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("map"), MapExpr([("1", Value(IntegerValue(1))); ("2", Value(IntegerValue(2))); ("3", Value(IntegerValue(4)));]),("",0));
							ExpressionStatement(FunctionCall(CompoundName(["Map";"remove"]),[VariableExpr(Name("map")); Value(StringValue("1"))]),("",0));
							ExpressionStatement(FunctionCall(CompoundName(["Map";"remove"]),[VariableExpr(Name("map")); Value(StringValue("3"))]),("",0));
							Declaration(Name("arr"), FunctionCall(CompoundName(["Map";"keys"]),[VariableExpr(Name("map"))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						StringValue("2") = SymbolTable.get_value (CompoundName(["arr";"0"])) symbol_table &&
						(match SymbolTable.get_value (Name("arr")) symbol_table with
							| MapValue(h, ArraySubtype) -> Hashtbl.find h "length" = IntegerValue(1)
							| _ -> false)
			);
			("Map.contains()", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("map"), MapExpr([("1", Value(IntegerValue(1))); ("2", Value(IntegerValue(2))); ("3", Value(IntegerValue(4)));]),("",0));
							Declaration(Name("shouldBeTrue"), FunctionCall(CompoundName(["Map";"contains"]),[VariableExpr(Name("map")); Value(StringValue("1"))]),("",0));
							Declaration(Name("shouldBeFalse"), FunctionCall(CompoundName(["Map";"contains"]),[VariableExpr(Name("map")); Value(StringValue("X"))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						BooleanValue(true) = SymbolTable.get_value (Name("shouldBeTrue")) symbol_table &&
						BooleanValue(false) = SymbolTable.get_value (Name("shouldBeFalse")) symbol_table
			);
			("Map.keys()", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("map"), MapExpr([("1", Value(IntegerValue(1))); ("2", Value(IntegerValue(2))); ("3", Value(IntegerValue(4)));]),("",0));
							Declaration(Name("arr"), FunctionCall(CompoundName(["Map";"keys"]),[VariableExpr(Name("map"))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						StringValue("1") = SymbolTable.get_value (CompoundName(["arr";"0"])) symbol_table &&
						StringValue("2") = SymbolTable.get_value (CompoundName(["arr";"1"])) symbol_table &&
						StringValue("3") = SymbolTable.get_value (CompoundName(["arr";"2"])) symbol_table &&
						(match SymbolTable.get_value (Name("arr")) symbol_table with
							| MapValue(h, ArraySubtype) -> Hashtbl.find h "length" = IntegerValue(3)
							| _ -> false)
			);
			("Map.keys() call on non map throws LibraryException", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("map"), ArrayExpr([Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3));]),("",0));
							Declaration(Name("arr"), FunctionCall(CompoundName(["Map";"keys"]),[VariableExpr(Name("map"))]),("",0));
							] in
						try
							Interpreter.interpret_statements stmts symbol_table; false
						with
						| FatalExit (LibraryError _) -> true
						| _ -> false
			);
			("Map.contains() call on non map throws LibraryException", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("map"), ArrayExpr([Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3));]),("",0));
							Declaration(Name("x"), FunctionCall(CompoundName(["Map";"contains"]),[VariableExpr(Name("map")); Value(StringValue("x"))]),("",0));
							] in
						try
							Interpreter.interpret_statements stmts symbol_table; false
						with
						| FatalExit (LibraryError _)-> true
						| _ -> false
			);
			("Map.remove() call on non map throws LibraryException", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("map"), ArrayExpr([Value(IntegerValue(1)); Value(IntegerValue(2)); Value(IntegerValue(3));]),("",0));
							ExpressionStatement(FunctionCall(CompoundName(["Map";"remove"]),[VariableExpr(Name("map"));Value(StringValue("1"))]),("",0));
							] in
						try
							Interpreter.interpret_statements stmts symbol_table; false
						with
						| FatalExit (LibraryError _) -> true
						| _ -> false
			);
			("Integer.parse()", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("good"), FunctionCall(CompoundName(["Integer";"parse"]),[Value(StringValue("1234"))]),("",0));
							Declaration(Name("bad"), FunctionCall(CompoundName(["Integer";"parse"]),[Value(StringValue("1234a"))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						IntegerValue(1234) = SymbolTable.get_value (Name("good")) symbol_table &&
						Void = SymbolTable.get_value (Name("bad")) symbol_table
			);
			("Float.parse()", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("good"), FunctionCall(CompoundName(["Float";"parse"]),[Value(StringValue("12.34"))]),("",0));
							Declaration(Name("bad"), FunctionCall(CompoundName(["Float";"parse"]),[Value(StringValue("12.34a"))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						FloatValue(12.34) = SymbolTable.get_value (Name("good")) symbol_table &&
						Void = SymbolTable.get_value (Name("bad")) symbol_table
			);
			("Float.round()", fun() ->
						let symbol_table = SymbolTable.initialize () in
						register_library BuiltinLibrary.exported symbol_table;
						let stmts =[
							Declaration(Name("a"), FunctionCall(CompoundName(["Float";"round"]),[Value(FloatValue(12.34))]),("",0));
							Declaration(Name("b"), FunctionCall(CompoundName(["Float";"round"]),[Value(FloatValue(12.64))]),("",0));
							] in
						Interpreter.interpret_statements stmts symbol_table;
						IntegerValue(12) = SymbolTable.get_value (Name("a")) symbol_table &&
						IntegerValue(13) = SymbolTable.get_value (Name("b")) symbol_table
			);
			])
end