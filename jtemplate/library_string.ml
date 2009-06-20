open Interpreter
open RuntimeError
open Symbol_table
open Ast

module StringLibrary =
struct
	
	let indexOf str substr =
		let ssl = String.length substr in
		let max = String.length str - ssl in
		let rec loop i =
			(if i > max then - 1
				else(
					if String.sub str i ssl = substr then i
					else loop (i + 1))
			) in loop 0
	
	let exported =[
		(["String";"prototype";"toUppercase"],[], fun (stbl) ->
					let value = SymbolTable.get_value (Name("this")) stbl in
					raise (Interpreter.CFReturn (StringValue(String.uppercase (Interpreter.cast_to_string value))))
		);
		(["String";"prototype";"toLowercase"],[], fun (stbl) ->
					let value = SymbolTable.get_value (Name("this")) stbl in
					raise (Interpreter.CFReturn (StringValue(String.lowercase (Interpreter.cast_to_string value))))
		);
		(["String";"prototype";"toFirstUpper"],[], fun (stbl) ->
					let value = SymbolTable.get_value (Name("this")) stbl in
					raise (Interpreter.CFReturn (StringValue(String.capitalize (Interpreter.cast_to_string value))))
		);
		(["String";"prototype";"toFirstLower"],[], fun (stbl) ->
					let value = SymbolTable.get_value (Name("this")) stbl in
					raise (Interpreter.CFReturn (StringValue(String.uncapitalize (Interpreter.cast_to_string value))))
		);
		(["String";"prototype";"length"],[], fun (stbl) ->
					let value = SymbolTable.get_value (Name("this")) stbl in
					raise (Interpreter.CFReturn (IntegerValue(String.length (Interpreter.cast_to_string value))))
		);
		(["String";"prototype";"charAt"],["position"], fun (stbl) ->
					let value = SymbolTable.get_value (Name("this")) stbl in
					let position = SymbolTable.get_value(Name("position")) stbl in
					match position with
					| IntegerValue(intPos) ->
							(try
								raise (Interpreter.CFReturn (StringValue (String.make 1 (String.get (Interpreter.cast_to_string value) intPos))))
							with
							| Invalid_argument _ -> raise (LibraryError ("invalid index "^(string_of_int intPos)^" in call to charAt")))
					| _ -> raise (LibraryError "second argument to charAt should be an integer")
		);
		(["String";"prototype";"indexOf"],["substring"], fun (stbl) ->
					let s = Interpreter.cast_to_string (SymbolTable.get_value (Name("this")) stbl) in
					let ss = Interpreter.cast_to_string (SymbolTable.get_value (Name("substring")) stbl) in
					raise (Interpreter.CFReturn (IntegerValue(indexOf s ss)))
		);
		(["String";"prototype";"substr"],["start"; "length"], fun(stbl) ->
					let s = Interpreter.cast_to_string (SymbolTable.get_value (Name("this")) stbl) in
					let start = SymbolTable.get_value(Name("start")) stbl in
					let length = SymbolTable.get_value(Name("length")) stbl in
					try(
							match start with
							| IntegerValue(iStart) ->
									(match length with
										| IntegerValue(iLength) -> raise (Interpreter.CFReturn (StringValue(String.sub s iStart iLength)))
										| _ -> raise (LibraryError "third parameter (length) in substr must be an integer"))
							| _ -> raise (LibraryError "second parameter (start) in substr must be an integer"))
					with
					| Invalid_argument _ -> raise (LibraryError "Invalid start/length arguments in substr")
		);
		(["String";"prototype";"startsWith"],["substring"], fun (stbl) ->
					let s = Interpreter.cast_to_string (SymbolTable.get_value (Name("this")) stbl) in
					let ss = Interpreter.cast_to_string (SymbolTable.get_value (Name("substring")) stbl) in
					raise (Interpreter.CFReturn (BooleanValue(ss = String.sub s 0 (String.length ss))))
		);
		(["String";"prototype";"endsWith"],["substring"], fun (stbl) ->
					let s = Interpreter.cast_to_string (SymbolTable.get_value (Name("this")) stbl) in
					let ss = Interpreter.cast_to_string (SymbolTable.get_value (Name("substring")) stbl) in
					raise (Interpreter.CFReturn (BooleanValue(ss = String.sub s (String.length s - String.length ss) (String.length ss))))
		);
		(["String";"prototype";"replaceAll"],["substring";"replacement"], fun(stbl) ->
					let s = Interpreter.cast_to_string (SymbolTable.get_value (Name("this")) stbl) in
					let substr = Interpreter.cast_to_string (SymbolTable.get_value (Name("substring")) stbl) in
					let repl = Interpreter.cast_to_string (SymbolTable.get_value (Name("replacement")) stbl) in
					let ssl = String.length substr in
					let rec loop str =
						match indexOf str substr with
						| - 1 -> raise (Interpreter.CFReturn(StringValue str))
						| i -> loop ((String.sub str 0 i)^ repl ^
										(String.sub str (i + ssl)
												(String.length str - ssl - i))) in
					loop s
		);
		(["String";"prototype";"split"],["delim"], fun(stbl) ->
					let str = Interpreter.cast_to_string (SymbolTable.get_value (Name("this")) stbl) in
					let substr = Interpreter.cast_to_string (SymbolTable.get_value (Name("delim")) stbl) in
					let result = Hashtbl.create 10 in
					let rec loop s ind =
						match indexOf s substr with
						| - 1 -> Hashtbl.add result (string_of_int ind) (StringValue(s));
								Hashtbl.add result "length" (IntegerValue(ind + 1));
								raise (Interpreter.CFReturn (MapValue(result, ArraySubtype)))
						| i -> let offset = i + String.length substr in
								Hashtbl.add result (string_of_int ind) (StringValue(String.sub s 0 i));
								loop (String.sub s offset (String.length s - offset)) (ind + 1) in
					loop str 0
		);
		(["String";"prototype";"parseInt"],[], fun stbl ->
					let value = Interpreter.cast_to_string (SymbolTable.get_value (Name("this")) stbl) in
					let result = try IntegerValue(int_of_string value) with Failure _ -> Void in
					raise (Interpreter.CFReturn result)
		);
		(["String";"prototype";"parseFloat"],[], fun stbl ->
					let value = Interpreter.cast_to_string (SymbolTable.get_value (Name("this")) stbl) in
					let result = try FloatValue(float_of_string value) with Failure _ -> Void in
					raise (Interpreter.CFReturn result)
		);
		
		]
end