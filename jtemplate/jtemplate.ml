open Lexer
open Parser
open Lexing
open Ast
open Filename_util
open RuntimeError
open Environment
open Analysis
open Ast_info

let register_args len env =
	let h = Hashtbl.create (len - 1)
	in let rec loop = function
		| 0 -> ()
		| ind -> Hashtbl.add h (string_of_int (ind - 1)) (RStringValue(Sys.argv.(ind))); loop (ind - 1)
	in loop (len - 1);
	Hashtbl.add h "length" (RIntegerValue(len - 1));
	Environment.set_value env (RMapValue(h, ArraySubtype)) (GlobalVar(0, 0))

let _ =
	let _ = Parsing.set_trace false in
	let argl = Array.length Sys.argv in
	if argl < 2 then
		prerr_string ("Usage: "^(Filename.basename Sys.argv.(0))^" scriptfile [args...]\n")
	else
		(
			let filename = Sys.argv.(1) in
			let ast = if filename ="-" then Parser_util.parse stdin "stdin" else Parser_util.parse_filename (resolve_filename (Unix.getcwd()) filename)
			in let (ast, env) = try
					Analysis.analyze ast
				with RuntimeError.FatalExit(_) -> exit(- 2)
			in 
			AstInfo.print_ast ast;
			print_name_info env;
			let renv ={
				heap = Array.make env.num_globals (- 1, RUndefined);
				stackframes = Array.make (env.max_depth + 1) [||];
				closure_vars = None;
				gnames = Array.of_list env.names;
				current_line = ("", 0);
				callstack =[];
			}
			in let _ = Library.register_for_runtime env renv
			in let _ = register_args argl renv
			in try
				Interpreter.interpret renv ast
			with
			| RuntimeError.FatalExit _ -> exit(- 1)
			| ex -> RuntimeError.display_error ex renv.current_line; raise ex
		)
