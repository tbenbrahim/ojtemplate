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

let _ =
	let symbol_table = SymbolTable.initialize_environment
			{ parse_callback = (Parser_util.parse_filename);
				loaded_imports =[] ;current_stmt=("",0);
				stack_trace= []; } in
	register_library BuiltinLibrary.exported symbol_table;
	register_library StringLibrary.exported symbol_table;
	register_library IOLibrary.exported symbol_table;
	let _ = Parsing.set_trace false in
	let ast = Parser_util.parse_filename
			(resolve_filename (Unix.getcwd()) "..\\samples\\import_demo.jtp")
	in
	Interpreter.interpret_statement ast symbol_table
