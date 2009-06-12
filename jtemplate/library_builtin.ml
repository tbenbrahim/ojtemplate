open Interpreter
open RuntimeError
open Symbol_table
open Ast

module BuiltinLibrary =
struct
	let exported =[
		("print",[Name("value")], fun (stbl) ->
					let value = SymbolTable.get_value (Name("value")) stbl in
					print_string (Interpreter.cast_to_string value)
		);
		("println",[Name("value")], fun (stbl) ->
					let value = SymbolTable.get_value (Name("value")) stbl in
					print_string (Interpreter.cast_to_string value); print_newline()
		);
		]
end