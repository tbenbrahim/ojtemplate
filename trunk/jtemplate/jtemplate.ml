(**
Jtemplate initialization and launching of program

@author Tony BenBrahim < tony.benbrahim at gmail.com >
*)
(* This program is free software; you can redistribute it and / or modify  *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation; version 3 of the License. This program is *)
(* distributed in the hope that it will be useful, but WITHOUT ANY         *)
(* WARRANTY; without even the implied warranty of MERCHANTABILITY or       *)
(* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details.                                                       *)

open Lexer
open Parser
open Lexing
open Ast
open Filename_util
open RuntimeError
open Environment
open Analysis
open Ast_info

(**
Program entry point
*)
let _ =
	(**
	Registers script arguments (arguments following the script file)
	@param args a list of string arguments
	@param env runtime environment
	@return unit
	*)
	let register_args args env =
		let h = Hashtbl.create 1
		in let rec loop = function
			| (n,[]) ->
					Hashtbl.add h "length" (RIntegerValue(n));
					Environment.set_value env (RMapValue(h, ArraySubtype)) (GlobalVar(0, 0))
			| (ind, arg:: tl) -> Hashtbl.add h (string_of_int ind) (RStringValue(arg)); loop (ind + 1, tl)
		in loop (0, args);
	(* parse command line arguments *)
	in let show_parse_tree = ref false and args_list = ref [] and print_version = ref false
	in let _ = Arg.parse
			[("-parsetree", Arg.Unit (fun () -> show_parse_tree := true), "print the parse tree and symbols before executing the program");
			("-version", Arg.Unit (fun () -> print_version := true), "print the version and exit")
			] (fun arg -> args_list:= (arg::!args_list)) ("jtemplate [-p] scriptfile [scriptargs...]" ^
				"scriptfile the script file to execute, read from stdin if missing\n"^"scriptargs... optional script arguments, separated by a space")
	in let _ = (if !print_version then (print_string "Jtemplate 0.8\n"; exit(0)) else ())
	in let args = List.rev !args_list
	in let filename = match args with
		| [] -> "-"
		| name:: tl -> name
	(* generate parsing AST *)
	in let ast = try
			if filename ="-" then Parser_util.parse stdin "stdin"
			else (
				let resolved_filename = resolve_filename (Unix.getcwd()) filename
				in Unix.chdir (Filename.dirname resolved_filename);
				Parser_util.parse_filename (resolved_filename)
			)
		with ParseException(_) as ex ->
				RuntimeError.display_error ex ("", 0);
				exit(2)
	(* analyze AST and create optimized runtime AST *)
	in let (ast, env) =
		try Analysis.analyze ast
		with
		| RuntimeError.FatalExit(_) -> exit(2)
		| ex ->
				RuntimeError.display_error ex ("", 0);
				exit(2)
	(* show parse tree if dictated by command line switch *)
	in let _ = (if !show_parse_tree then
				(Ast_info.print_ast ast; print_name_info env)
			else
				())
	in
	(* create a runtime environment *)
	let renv ={
		heap = Array.make env.num_globals (- 1, RUndefined);
		stackframes = Array.make (env.max_depth + 1) [||];
		closure_vars = None;
		gnames = Array.of_list env.names;
		current_line = ("", 0);
		callstack = Stack.create ();
		skip_callstack_pop = false;
	}
	(* copy library definitions to runtime environment *)
	in let _ = Library.register_for_runtime env renv
	in let _ = register_args args renv
	(* interpret runtime AST *)
	in try
		Interpreter.interpret renv ast
	with
	| RuntimeError.FatalExit _ -> exit(1)
	| ex ->
			RuntimeError.display_error ex renv.current_line;
			Stack.iter (fun loc -> let (file, line) = loc in
							print_string ("\tCalled from " ^ file ^ " line " ^ (string_of_int line))) renv.callstack;
			exit(1)
