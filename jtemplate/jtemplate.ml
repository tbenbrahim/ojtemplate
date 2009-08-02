(**
This program is free software; you can redistribute it and / or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

Jtemplate initialization and launching of program

@author Tony BenBrahim < tony.benbrahim at gmail.com >

*)

open Lexer
open Parser
open Lexing
open Ast
open Filename_util
open RuntimeError
open Environment
open Analysis
(**open Ast_info*)

let _ =
	(**
	Registers arguments in the runtime environment
	*)
	let register_args len env =
		let h = Hashtbl.create (len - 1)
		in let rec loop = function
			| 0 -> ()
			| ind -> Hashtbl.add h (string_of_int (ind - 1)) (RStringValue(Sys.argv.(ind))); loop (ind - 1)
		in loop (len - 1);
		Hashtbl.add h "length" (RIntegerValue(len - 1));
		Environment.set_value env (RMapValue(h, ArraySubtype)) (GlobalVar(0, 0))
	in let argl = Array.length Sys.argv in
	if argl < 2 then
		prerr_string ("Usage: "^(Filename.basename Sys.argv.(0))^" scriptfile [args...]\n")
	else
		(
			let filename = Sys.argv.(1) in
			let ast = try
					if filename ="-" then Parser_util.parse stdin "stdin" else Parser_util.parse_filename (resolve_filename (Unix.getcwd()) filename)
				with ParseException(_) as ex ->
						RuntimeError.display_error ex ("", 0);
						exit(- 2)
			in let (ast, env) =
				try Analysis.analyze ast
				with
				| RuntimeError.FatalExit(_) -> exit(- 2)
				| ex ->
						RuntimeError.display_error ex ("", 0);
						exit(- 2)
			in
			(**AstInfo.print_ast ast;
			print_name_info env;*)
			let renv ={
				heap = Array.make env.num_globals (- 1, RUndefined);
				stackframes = Array.make (env.max_depth + 1) [||];
				closure_vars = None;
				gnames = Array.of_list env.names;
				current_line = ("", 0);
				callstack = Stack.create ();
				skip_callstack_pop = false;
			}
			in let _ = Library.register_for_runtime env renv
			in let _ = register_args argl renv
			in try
				Interpreter.interpret renv ast
			with
			| RuntimeError.FatalExit _ -> exit(- 1)
			| ex ->
					RuntimeError.display_error ex renv.current_line;
					Stack.iter (fun loc -> let (file, line) = loc in
									print_string ("\tCalled from " ^ file ^ " line " ^ (string_of_int line))) renv.callstack;
					exit(- 1)
			
		)
