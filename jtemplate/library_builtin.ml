(**
Built in library implementation

@author Tony BenBrahim < tony.benbrahim at gmail.com >
*)
(* This program is free software; you can redistribute it and / or modify  *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation; version 3 of the License. This program is *)
(* distributed in the hope that it will be useful, but WITHOUT ANY         *)
(* WARRANTY; without even the implied warranty of MERCHANTABILITY or       *)
(* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details.                                                       *)

open Environment
open Ast
open Expression
open RuntimeError
open Unix

(**
Entry point for library initialization
@return a list of exported functions
*)
let initialize env =
	let this_array_map env =
		match env.stackframes.(0).(0) with
		| RMapValue(h, ArraySubtype) -> h
		| _ -> raise (InternalError "expected array for this")
	in let this_map_map env =
		match env.stackframes.(0).(0) with
		| RMapValue(h, MapSubtype) -> h
		| _ -> raise (InternalError "expected map for this")
	in let this_float env =
		match env.stackframes.(0).(0) with
		| RFloatValue(f) -> f
		| _ -> raise (InternalError "expected float for this")
	in let _ = Random.self_init()
	in let date_map = Hashtbl.create 2
	in let date_proto_map = Hashtbl.create 1
	in let _ = Hashtbl.replace date_map "prototype" (RMapValue(date_proto_map, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Date" (RMapValue(date_map, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Debug" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "System" (RMapValue(Hashtbl.create 10, MapSubtype))
	in ([
		{
			name = ["Array";"prototype";"push"];
			args = ["value"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let value = env.stackframes.(0).(1)
						in let array = this_array_map env
						in let len = string_of_value (Hashtbl.find array "length") in
						Hashtbl.replace array len value;
						Hashtbl.replace array "length" (RIntegerValue((int_of_string len) + 1))
		};
		{
			name =["Array";"prototype";"pop"];
			args =[];
			num_args = 0;
			vararg = false;
			code = fun env ->
						let hashtbl = this_array_map env
						in let len = int_of_string (string_of_value (Hashtbl.find hashtbl "length")) in
						if len = 0 then
							raise (LibraryError "Error while attempting to pop an empty array in Array.pop")
						else
							let result = Hashtbl.find hashtbl (string_of_int (len - 1)) in
							Hashtbl.remove hashtbl (string_of_int (len - 1));
							Hashtbl.replace hashtbl "length" (RIntegerValue(len - 1));
							raise (CFReturn result)
		};
		{
			name = ["Array";"prototype";"length"];
			args =[];
			num_args = 0;
			vararg = false;
			code = fun env ->
						let hashtbl = this_array_map env
						in try (match Hashtbl.find hashtbl "length" with
								| RIntegerValue(len) -> raise (CFReturn(RIntegerValue(len)))
								| _ -> raise (LibraryError "First parameter is not an array in call to Array.length"))
						with Not_found ->
								raise (LibraryError "First parameter is not an array in call to Array.length")
		};
		{
			name =["Map";"prototype";"remove"];
			args =["key"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let hashtbl = this_map_map env
						in let key = string_of_value (env.stackframes.(0).(1))
						in let _ = Hashtbl.remove hashtbl key
						in ()
		};
		{
			name =["Map";"prototype";"contains"];
			args =["key"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let hashtbl = this_map_map env
						in let key = string_of_value (env.stackframes.(0).(1))
						in raise (CFReturn(RBooleanValue(Hashtbl.mem hashtbl key)))
		};
		{
			name =["Map";"prototype";"keys"];
			args =[];
			num_args = 0;
			vararg = false;
			code = fun env ->
						let hashtbl = this_map_map env
						in let result = Hashtbl.create 10
						in let (_, cnt) = Hashtbl.fold (fun k _ (h, cnt) -> (Hashtbl.add h (string_of_int cnt) (RStringValue k); h, cnt + 1 )) hashtbl (result, 0)
						in Hashtbl.replace result "length" (RIntegerValue cnt );
						raise (CFReturn (RMapValue (result, ArraySubtype)))
		};
		{
			name =["Integer";"random"];
			args =["upperBound"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let upperBound =
							(try
								cast_to_integer(env.stackframes.(0).(1))
							with
							| _ -> raise (LibraryError("upperBound must an integer in call to Integer.random")))
						in
						raise (CFReturn(RIntegerValue(Random.int upperBound)))
		};
		{
			name =["Float";"prototype";"round"];
			args =[];
			num_args = 0;
			vararg = false;
			code = fun env ->
						let f = this_float env
						in raise (CFReturn (RIntegerValue(int_of_float(
												let (frac, _) = modf f in (if frac >= 0.5 then ceil f else floor f)))))
		};
		{
			name =["Date";"now"];
			args =[];
			num_args = 0;
			vararg = false;
			code = fun env ->
						let t = (localtime (time())) in
						let gmt_offset = (localtime (time())).tm_hour - (gmtime (time())).tm_hour in
						let h = Hashtbl.create 10 in
						Hashtbl.add h "prototype" (RMapValue(date_proto_map, MapSubtype));
						Hashtbl.add h "second" (RIntegerValue(t.tm_sec));
						Hashtbl.add h "minute" (RIntegerValue(t.tm_min));
						Hashtbl.add h "hour" (RIntegerValue(t.tm_hour));
						Hashtbl.add h "dayOfMonth" (RIntegerValue(t.tm_mday));
						Hashtbl.add h "month" (RIntegerValue(t.tm_mon + 1));
						Hashtbl.add h "year" (RIntegerValue(1900 + t.tm_year));
						Hashtbl.add h "dayOfWeek" (RIntegerValue(t.tm_wday)); (* Sunday 0 *)
						Hashtbl.add h "dayOfYear" (RIntegerValue(t.tm_yday));
						Hashtbl.add h "dst" (RBooleanValue(t.tm_isdst));
						Hashtbl.add h "gmtOffset" (RIntegerValue(gmt_offset));
						raise(CFReturn(RMapValue(h, MapSubtype)))
		};
		{
			name =["System";"command"];
			args = ["command"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let command = string_of_value (env.stackframes.(0).(1)) in
						raise (CFReturn (RIntegerValue(Sys.command command)))
		};
		{
			name =["exit"];
			args =["exitcode"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						match env.stackframes.(0).(1) with
						| RIntegerValue(c) -> if c >= 0 && c <= 255 then exit c else
									raise (LibraryError("exitcode must be an integer between 0 and 255 in call to exit"))
						| _ -> raise (LibraryError("exitcode must be an integer in call to exit"))
		};
		{
			name =["Debug";"dumpSymbolTable"];
			args =[];
			num_args = 0;
			vararg = false;
			code = fun env ->
						let rec loop = function
							| 0 -> ()
							| n ->
									let (uid, value) = env.heap.(n - 1)
									in let _ = print_string (env.gnames.(uid)^" = " ^(string_of_value value) ^ "\n")
									in loop (n - 1)
						in loop (Array.length env.heap)
		};
		{
			name =["Function";"prototype";"apply"];
			args =["args..."];
			num_args = 1;
			vararg = true;
			code = fun env ->
						let func = env.stackframes.(0).(0)
						in match func with
						| RFunctionValue(_, _, _, _, _, _, _) | RLibraryFunction(_) ->
								let args = list_of_array env.stackframes.(0).(1)
								in let this = List.hd args
								in let args = List.tl args
								in let (_, v) = Interpreter.run_function env args this func
								in raise (CFReturn v)
						| _ -> raise (LibraryError "expected a function in first parameter of call to apply")
		};
		{
			name =["Debug";"dumpStackTrace"];
			args =[];
			num_args = 0;
			vararg = false;
			code = fun env ->
						Stack.iter (fun loc ->
										let (file, line) = loc
										in print_string ("Called from line "^(string_of_int line)^" in file "^ (Filename.basename file)^":\n"))
							env.callstack
		};
		{
			name = ["typeof"];
			args =["value"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let s = match env.stackframes.(0).(1) with
							| RStringValue(_) -> "string"
							| RIntegerValue(_) ->"integer"
							| RFloatValue(_) -> "float"
							| RBooleanValue(_) -> "boolean"
							| RFunctionValue(_) | RLibraryFunction(_) -> "function"
							| RMapValue(_, ArraySubtype) -> "array"
							| RMapValue(_, MapSubtype) ->"map"
							| RVoid ->"void"
							| RUndefined -> "undefined"
						in raise (CFReturn (RStringValue s))
		};
		], env)