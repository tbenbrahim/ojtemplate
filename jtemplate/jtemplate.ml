open Lexer
open Parser
open Ast
open Lexing
open Ast_info
open Symbol_table
open Interpreter

let get_ast lexbuf =
	try Parser.program Lexer.main lexbuf
	with
	| LexerException (msg, line, col) ->
			(print_string
					(msg ^
						(" at line " ^
							((string_of_int line) ^
								(" col " ^ ((string_of_int col) ^ "\n")))));
				exit 0)

let parse filename =
	let lexbuf = Lexing.from_channel (open_in filename) in
	let pos = lexbuf.Lexing.lex_curr_p in
	let _ =
		lexbuf.Lexing.lex_curr_p <-
		{ (pos) with Lexing.pos_lnum = 1; Lexing.pos_fname = filename; }
	in get_ast lexbuf

let _ =
	let _ = Parsing.set_trace false in
	let ast = parse "c:\\test.jtp"
	in List.fold_left (fun acc el -> AstInfo.print_ast el) () ast
