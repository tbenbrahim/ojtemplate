open Environment
open Ast
open Expression
open RuntimeError

(**
Returns the value of this as a string
@param env the runtime environment
@return the string value of this
@throws InternalError is this is not a string
*)
let get_this env =
	match env.stackframes.(0).(0) with
	| RStringValue(s) -> s
	| v -> raise (RuntimeError.InternalError "mismatched this in call to String.prototype.length")

(**
Returns the positing of a substring within a string
@param str the string to search
@param substr the substring to find
@return the position of the substring in the string, or - 1 if not found
*)
let indexOf str substr =
	let ssl = String.length substr in
	let max = String.length str - ssl in
	let rec loop i =
		(if i > max then - 1
			else(
				if String.sub str i ssl = substr then i
				else loop (i + 1))
		)
	in loop 0

(**
Entry point for library initialization
@return a list of exported functions
*)
let initialize env =
	([{
			name = ["String";"prototype";"length"];
			args = [];
			num_args = 0;
			vararg = false;
			code = fun env -> raise (CFReturn (RIntegerValue(String.length (get_this env))))
		};
		{
			name =["String";"prototype";"toUppercase"];
			args = [];
			num_args = 0;
			vararg = false;
			code = fun env -> raise (CFReturn (RStringValue(String.uppercase (get_this env))))
		};
		{
			name =["String";"prototype";"toLowercase"];
			args = [];
			num_args = 0;
			vararg = false;
			code = fun env -> raise (CFReturn (RStringValue(String.lowercase (get_this env))))
		};
		{
			name =["String";"prototype";"toFirstUpper"];
			args = [];
			num_args = 0;
			vararg = false;
			code = fun env -> raise (CFReturn (RStringValue(String.capitalize (get_this env))))
		};
		{
			name =["String";"prototype";"toFirstLower"];
			args = [];
			num_args = 0;
			vararg = false;
			code = fun env -> raise (CFReturn (RStringValue(String.uncapitalize (get_this env))))
		};
		{
			name =["String";"prototype";"charAt"];
			args = ["index"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						match env.stackframes.(0).(1) with
							RIntegerValue(index) ->
								(try
									raise (CFReturn (RStringValue (String.make 1 (String.get (get_this env) index))))
								with
								| Invalid_argument _ -> raise (LibraryError ("invalid index "^(string_of_int index)^" in call to charAt")))
						| _ -> raise (LibraryError "argument index in charAt should be an integer")
		};
		{
			name =["String";"prototype";"indexOf"];
			args = ["substring"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let substring = cast_to_string (env.stackframes.(0).(1))
						in raise (CFReturn (RIntegerValue(indexOf (get_this env) substring)))
		};
		{
			name =["String";"prototype";"substr"];
			args = ["start";"length"];
			num_args = 2;
			vararg = false;
			code = fun env ->
						match env.stackframes.(0).(1) with
						| RIntegerValue(start) ->
								(match env.stackframes.(0).(2) with
									| RIntegerValue(length) -> raise (CFReturn (RStringValue(String.sub (get_this env) start length)))
									| _ -> raise (LibraryError "length in substr should be an integer"))
						| _ -> raise (LibraryError "start in substr should be an integer")
		};
		{
			name =["String";"prototype";"startsWith"];
			args = ["substring"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let substring = cast_to_string (env.stackframes.(0).(1))
						in raise (CFReturn (RBooleanValue(substring = String.sub (get_this env) 0 (String.length substring))))
		};
		{
			name =["String";"prototype";"endsWith"];
			args = ["substring"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let ss = cast_to_string (env.stackframes.(0).(1))
						in let s = get_this env
						in raise (CFReturn (RBooleanValue(ss = String.sub s (String.length s - String.length ss) (String.length ss))))
		};
		{
			name =["String";"prototype";"replaceAll"];
			args = ["substring";"replacement"];
			num_args = 2;
			vararg = false;
			code = fun env ->
						let substr = cast_to_string (env.stackframes.(0).(1))
						in let repl = cast_to_string (env.stackframes.(0).(2))
						in let s = get_this env
						in let ssl = String.length substr in
						let rec loop str =
							match indexOf str substr with
							| - 1 -> raise (CFReturn(RStringValue str))
							| i -> loop ((String.sub str 0 i)^ repl ^
											(String.sub str (i + ssl)
													(String.length str - ssl - i)))
						in loop s
		};
		{
			name =["String";"prototype";"split"];
			args = ["delim"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let str = get_this env
						in let substr = cast_to_string (env.stackframes.(0).(1))
						in let result = Hashtbl.create 10
						in let rec loop s ind =
							match indexOf s substr with
							| - 1 -> Hashtbl.add result (string_of_int ind) (RStringValue(s));
									Hashtbl.add result "length" (RIntegerValue(ind + 1));
									raise (CFReturn (RMapValue(result, ArraySubtype)))
							| i -> let offset = i + String.length substr in
									Hashtbl.add result (string_of_int ind) (RStringValue(String.sub s 0 i));
									loop (String.sub s offset (String.length s - offset)) (ind + 1) in
						loop str 0
		};
		{
			name =["String";"prototype";"parseInt"];
			args = [];
			num_args = 0;
			vararg = false;
			code = fun env ->
						raise (CFReturn( try RIntegerValue(int_of_string (get_this env)) with Failure _ -> RVoid ))
		};
		{
			name =["String";"prototype";"parseFloat"];
			args = [];
			num_args = 0;
			vararg = false;
			code = fun env ->
						raise (CFReturn( try RFloatValue(float_of_string (get_this env)) with Failure _ -> RVoid ))
		};
		{
			name =["String";"prototype";"mreplace"];
			args =["substrings";"values"];
			num_args = 2;
			vararg = false;
			code = fun env ->
						let rec make_array result = function
							| [] -> Array.of_list(List.rev result)
							| v:: tl -> make_array ((cast_to_string v):: result) tl
						in let string = cast_to_string env.stackframes.(0).(0)
						in let substrings = make_array [] ( list_of_array env.stackframes.(0).(1))
						in let values = make_array [] (list_of_array env.stackframes.(0).(2))
						in let len = Array.length substrings
						in if len!= Array.length values then
							raise (LibraryError "substrings and values arrays should have the same length in String.prototype.mreplace")
						else
							let rec loop_replacements result ind =
								if ind = len then
									result
								else
									let substring = substrings.(ind)
									in let len = String.length substring
									in let rec loop_string string result offset =
										match indexOf string substring with
										| - 1 -> result
										| pos ->
												if pos + len!= String.length string then
													loop_string (String.sub string (pos + len) ((String.length string) - pos - len) ) (pos + offset:: result) (offset + pos + len)
												else
													loop_string "" (pos + offset:: result) (offset + pos + len)
									in let positions = loop_string string [] 0
									in loop_replacements ((values.(ind), len, positions) :: result) (ind + 1)
							in let replacements = loop_replacements [] 0
							in let replace str i ssl repl =
								(String.sub str 0 i)^ repl ^ (String.sub str (i + ssl) (String.length str - ssl - i))
							in let rec loop_values result_string offset = function
								| [] -> result_string
								| repl:: tl ->
										let (value, len, positions) = repl
										in let delta = String.length value - len
										in let rec loop_repl result_string offset = function
											| [] -> (offset, result_string)
											| pos:: tl ->
													loop_repl (replace result_string pos len value) (offset+delta) tl
										in let (offset, result_string) = loop_repl result_string offset positions
										in loop_values result_string offset tl
							in let result = loop_values string 0 replacements
							in raise (CFReturn(RStringValue(result)))
		};
		], env)