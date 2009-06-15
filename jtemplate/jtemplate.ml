open Lexer
open Parser
open Lexing
open Ast_info
open Symbol_table
open Interpreter
open Library_builtin
open Library_string
open Library_io
open Library_helper
open Ast
open Filename_util
open RuntimeError

let rec register_args ind len symbol_table =
	if ind = 1 then
		SymbolTable.declare (Name("args")) (MapValue(Hashtbl.create (len - 1), ArraySubtype)) symbol_table
	else ();
	if ind = len then
		SymbolTable.declare (CompoundName(["args";"length"])) (IntegerValue(len - 1)) symbol_table
	else
		(
			SymbolTable.declare (CompoundName(["args"; string_of_int (ind - 1)])) (StringValue(Sys.argv.(ind))) symbol_table;
			register_args (ind + 1) len symbol_table
		)

let _ =
	let symbol_table = SymbolTable.initialize_environment
			{ parse_callback = (Parser_util.parse_filename);
				loaded_imports =[] ; current_stmt = ("", 0);
				stack_trace = []; } in
	register_library BuiltinLibrary.exported symbol_table;
	register_library StringLibrary.exported symbol_table;
	register_library IOLibrary.exported symbol_table;
	let _ = Parsing.set_trace false in
	let argl = Array.length Sys.argv in
	if argl < 2 then
		prerr_string ("Usage: "^(Filename.basename Sys.argv.(0))^" scriptfile [args...]\n")
	else
		(
			register_args 1 argl symbol_table;
			let filename = Sys.argv.(1) in
			let ast = if filename ="-" then Parser_util.parse stdin "stdin" else Parser_util.parse_filename (resolve_filename (Unix.getcwd()) filename)
			in
			try
			 Interpreter.interpret_statement ast symbol_table
			with
				| FatalExit _ -> exit(-1)
		)
