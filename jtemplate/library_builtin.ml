open Interpreter
open RuntimeError
open Symbol_table
open Ast

module BuiltinLibrary =
struct
	let exported =[
		(["Array";"push"],["array"; "value"], fun stbl ->
					let array = SymbolTable.get_value (Name("array")) stbl in
					let value = SymbolTable.get_value (Name("value")) stbl in
					try
						match array with
						| MapValue(hashtbl, ArraySubtype) ->
								let len = Hashtbl.find hashtbl "length" in
								Hashtbl.replace hashtbl (Interpreter.cast_to_string len) value;
								Hashtbl.replace hashtbl "length" (IntegerValue((Interpreter.cast_to_integer len) + 1))
						| _ -> raise (LibraryError "First parameter is not an array in call to Array.push")
					with _ -> raise (LibraryError "First parameter is not an array in call to Array.push")
		);
		(["Array";"pop"],["array"], fun stbl ->
					let array = SymbolTable.get_value (Name("array")) stbl in
					match array with
					| MapValue(hashtbl, ArraySubtype) ->
							let len = Interpreter.cast_to_integer(Hashtbl.find hashtbl "length") in
							if len = 0 then
								raise (LibraryError "Error while attempting to pop an empty array in Array.pop")
							else
								let result = Hashtbl.find hashtbl (string_of_int (len - 1)) in
								Hashtbl.remove hashtbl (string_of_int (len - 1));
								Hashtbl.replace hashtbl "length" (IntegerValue(len - 1));
								raise (Interpreter.CFReturn result)
					| _ -> raise (LibraryError "First parameter is not an array in call to Array.pop")
		);
		(["Array";"length"],["array"], fun stbl ->
					let array = SymbolTable.get_value (Name("array")) stbl in
					match array with
					| MapValue(hashtbl, ArraySubtype) ->
							(match Hashtbl.find hashtbl "length" with
								| IntegerValue(len) -> raise (Interpreter.CFReturn(IntegerValue(len)))
								| _ -> raise (LibraryError "First parameter is not an array in call to Array.pop"))
					| _ -> raise (LibraryError "First parameter is not an array in call to Array.pop")
		);
		(["Map";"remove"],["map";"key"], fun stbl ->
					let map = SymbolTable.get_value (Name("map")) stbl in
					let key = Interpreter.cast_to_string(SymbolTable.get_value (Name("key")) stbl) in
					match map with
					| MapValue(hashtbl, MapSubtype) -> Hashtbl.remove hashtbl key
					| _ -> raise (LibraryError "First parameter is not a map in call to Map.keys")
		);
		(["Map";"contains"],["map";"key"], fun stbl ->
					let map = SymbolTable.get_value (Name("map")) stbl in
					let key = Interpreter.cast_to_string(SymbolTable.get_value (Name("key")) stbl) in
					match map with
					| MapValue(hashtbl, MapSubtype) -> raise (Interpreter.CFReturn(BooleanValue(Hashtbl.mem hashtbl key)))
					| _ -> raise (LibraryError "First parameter is not a map in call to Map.keys")
		);
		(["Map";"keys"],["map"], fun stbl ->
					let map = SymbolTable.get_value (Name("map")) stbl in
					let result = Hashtbl.create 10 in
					match map with
					| MapValue(hashtbl, MapSubtype) ->
							let (_, cnt) = Hashtbl.fold (fun k _ (h, cnt) -> (Hashtbl.add h (string_of_int cnt) (StringValue k); h, cnt + 1 )) hashtbl (result, 0) in
							Hashtbl.replace result "length" (IntegerValue cnt );
							raise (Interpreter.CFReturn (MapValue (result, ArraySubtype)))
					| _ -> raise (LibraryError "First parameter is not a map in call to Map.keys")
		);
		(["Integer";"parse"],["value"], fun stbl ->
					let value = Interpreter.cast_to_string (SymbolTable.get_value (Name("value")) stbl) in
					let result = try IntegerValue(int_of_string value) with Failure _ -> Void in
					raise (Interpreter.CFReturn result)
		);
		(["Float";"parse"],["value"], fun stbl ->
					let value = Interpreter.cast_to_string (SymbolTable.get_value (Name("value")) stbl) in
					let result = try FloatValue(float_of_string value) with Failure _ -> Void in
					raise (Interpreter.CFReturn result)
		);
		(["Float";"round"],["value"], fun stbl ->
					match SymbolTable.get_value (Name("value")) stbl with
					| FloatValue(f) -> raise (Interpreter.CFReturn (IntegerValue(int_of_float(
													let (frac, _) = modf f in (if frac >= 0.5 then ceil f else floor f)))))
					| _ -> raise (LibraryError("parameter is not a float in call to Float.round"))
		);
		(["typeof"],["value"], fun stbl ->
					raise (Interpreter.CFReturn(StringValue(
									match SymbolTable.get_value (Name("value")) stbl with
									| StringValue(_) -> "string"
									| IntegerValue(_) -> "integer"
									| BooleanValue(_) -> "boolean"
									| FloatValue(_) -> "float"
									| LibraryFunction(_, _, _) | FunctionValue(_, _, _) -> "function"
									| MapValue(_, MapSubtype) -> "map"
									| MapValue(_, ArraySubtype) -> "array"
									| NaN -> "NaN"
									| Void -> "void"
								)))
		);
		(["exit"],["exitcode"], fun stbl ->
					match SymbolTable.get_value (Name("code")) stbl with
					| IntegerValue(c) -> if c >= - 128 && c <= 127 then exit c else
								raise (LibraryError("exit code must be an integer between -128 and 127 in call to exit"))
					| _ -> raise (LibraryError("exit code must be an integer in call to exit"))
		);
		]
end