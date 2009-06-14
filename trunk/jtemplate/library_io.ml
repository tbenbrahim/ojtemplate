open Ast
open Interpreter
open Symbol_table
open RuntimeError
open Unix

module IOLibrary =
struct
	
	type channelType = OutChannel of out_channel | InChannel of in_channel * (string * bool)
	
	let exported =
		let descriptors = Hashtbl.create 10 in
		let get_descriptor handle command =
			(try
				Hashtbl.find descriptors handle
			with
			| Not_found -> raise (LibraryError ("invalid handle for "^command))) in
		let try_read ch =
			(
				try
					(input_line ch, false)
				with
				| End_of_file -> ("", true)
			) in
		[
		(["print"],["[value"], fun (stbl) ->
					let _ = List.map (fun el -> print_string (Interpreter.cast_to_string el))
							(SymbolTable.list_of_array (SymbolTable.get_value (Name("value")) stbl)) in ()
		);
		(["println"],["[value"], fun (stbl) ->
					let _ = List.map (fun el -> print_string (Interpreter.cast_to_string el))
							(SymbolTable.list_of_array (SymbolTable.get_value (Name("value")) stbl)) in
					print_newline()
		);
		(["readln"],[], fun (stbl) ->
					raise (Interpreter.CFReturn(StringValue(read_line())))
		);
		(["File";"openForWriting"],["handle"; "filename"], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let filename = Interpreter.cast_to_string (SymbolTable.get_value (Name("filename")) stbl) in
					try
						if Hashtbl.mem descriptors handle then
							raise (LibraryError("handle "^handle^" is already opened"))
						else
							let ch = open_out filename in
							Hashtbl.add descriptors handle (OutChannel(ch), filename)
					with
					| _ -> raise (LibraryError ("error opening file "^filename^" in openFileForWriting"))
		);
		(["File";"openForReading"],["handle"; "filename"], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let filename = Interpreter.cast_to_string (SymbolTable.get_value (Name("filename")) stbl) in
					try
						if Hashtbl.mem descriptors handle then
							raise (LibraryError("handle "^handle^" is already opened"))
						else
							let ch = open_in filename in
							Hashtbl.add descriptors handle (InChannel(ch, (try_read ch)), filename)
					with
					| _ -> raise (LibraryError ("error opening file "^filename^" in openFileForReading"))
		);
		(["File";"close"],["handle"], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let (c, _ ) = get_descriptor handle "closeFile" in
					try
						(match c with
							| OutChannel(ch) -> close_out ch
							| InChannel(ch, _) -> close_in ch);
						Hashtbl.remove descriptors handle
					with
					| Sys_error msg -> raise (LibraryError ("System error on closeFile:" ^ msg ))
		);
		(["File";"write"],["handle"; "[value"], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let (c, filename) = get_descriptor handle "write" in
					match c with
					| OutChannel(ch) ->
							( try
								let _ = List.map (fun el -> output_string ch (Interpreter.cast_to_string el))
										(SymbolTable.list_of_array (SymbolTable.get_value (Name("value")) stbl)) in ()
							with
							| _ -> raise (LibraryError ("error writing file "^filename^" in write")))
					| InChannel(ch, _) -> raise (LibraryError ("invalid handle in call to write. Handle "^handle^" was opened for reading "^filename))
		);
		(["File";"writeln"],["handle"; "[value"], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let (c, filename) = get_descriptor handle "writeln" in
					match c with
					| OutChannel(ch) ->
							(try
								let _ = List.map (fun el -> output_string ch (Interpreter.cast_to_string el))
										(SymbolTable.list_of_array (SymbolTable.get_value (Name("value")) stbl)) in
								output_string ch ( "\n")
							with
							| _ -> raise (LibraryError ("error writing file "^filename^" in writeln")))
					| InChannel(ch, _) -> raise (LibraryError ("invalid handle in call to write. Handle "^handle^" was opened for reading "^filename))
		);
		(["File";"readln"],["handle"], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let (c, filename) = get_descriptor handle "readln" in
					match c with
					| OutChannel(ch) -> raise (LibraryError ("invalid handle in call to readln. Handle "^handle^" was opened for writing "^filename))
					| InChannel(ch, (_, true)) -> raise (LibraryError ("End of file reached for handle "^handle^" in call to readln"))
					| InChannel(ch, (data, false)) ->
							( try
								Hashtbl.replace descriptors handle (InChannel(ch, (try_read ch)), filename)
							with
							| _ -> raise (LibraryError ("error reading file "^filename^" in readln")));
							raise (Interpreter.CFReturn(StringValue(data)))
		);
		(["File";"eof"],["handle"], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let (c, filename) = get_descriptor handle "eof" in
					match c with
					| OutChannel(ch) -> raise (LibraryError ("invalid handle in call to eof. Handle "^handle^" was opened for writing "^filename))
					| InChannel(ch, (_, eof)) -> raise (Interpreter.CFReturn(BooleanValue(eof)))
		);
		(["File";"exists"],["filename"], fun stbl ->
					let filename = Interpreter.cast_to_string (SymbolTable.get_value (Name("filename")) stbl) in
					(try
						close_in (open_in filename)
					with
					| _ -> raise (Interpreter.CFReturn(BooleanValue(false))));
					raise (Interpreter.CFReturn(BooleanValue(true)))
		);
		(["File";"delete"],["name"], fun stbl ->
					let name = Interpreter.cast_to_string (SymbolTable.get_value (Name("name")) stbl) in
					(try
						unlink name
					with
					| _ -> raise (Interpreter.CFReturn(BooleanValue(false))));
					raise (Interpreter.CFReturn(BooleanValue(true)))
		);
		(["File";"rename"],["fromname";"toname"], fun stbl ->
					let fromname = Interpreter.cast_to_string (SymbolTable.get_value (Name("fromname")) stbl) in
					let toname = Interpreter.cast_to_string (SymbolTable.get_value (Name("toname")) stbl) in
					(try
						rename fromname toname
					with
					| _ -> raise (Interpreter.CFReturn(BooleanValue(false))));
					raise (Interpreter.CFReturn(BooleanValue(true)))
		);
		(["Directory";"create"],["name"], fun stbl ->
					let name = Interpreter.cast_to_string (SymbolTable.get_value (Name("name")) stbl) in
					(try
						mkdir name 0o640
					with
					| _ -> raise (Interpreter.CFReturn(BooleanValue(false))));
					raise (Interpreter.CFReturn(BooleanValue(true)))
		);
		(["Directory";"delete"],["name"], fun stbl ->
					let name = Interpreter.cast_to_string (SymbolTable.get_value (Name("name")) stbl) in
					(try
						rmdir name
					with
					| _ -> raise (Interpreter.CFReturn(BooleanValue(false))));
					raise (Interpreter.CFReturn(BooleanValue(true)))
		);
		(["Directory";"list"],["name"], fun stbl ->
					let name = Interpreter.cast_to_string (SymbolTable.get_value (Name("name")) stbl) in
					let arr = (try
							let handle = opendir name in
							let h = Hashtbl.create 10
							in let rec loop cnt =
								try
									Hashtbl.add h (string_of_int cnt) (StringValue(readdir handle));
									loop (cnt + 1)
								with
								| End_of_file -> closedir handle; cnt
								| _ -> closedir handle; raise (Interpreter.CFReturn(Void))
							in Hashtbl.add h "length" (IntegerValue(loop 0));
							h
						with
						| _ -> raise (Interpreter.CFReturn(Void)))
					in raise (Interpreter.CFReturn(MapValue(arr, ArraySubtype)));
		);
		(["Directory";"exists"],["name"], fun stbl ->
					let name = Interpreter.cast_to_string (SymbolTable.get_value (Name("name")) stbl) in
					raise (Interpreter.CFReturn(BooleanValue((try
										Sys.is_directory name
									with
									| _ -> raise (Interpreter.CFReturn(Void))))))
		);
		]
	
end