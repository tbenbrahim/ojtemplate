open Ast
open Interpreter
open Symbol_table
open RuntimeError

module IOLibrary =
struct
	
	type channelType = OutChannel of out_channel | InChannel of in_channel * string
	
	let exported =
		let descriptors = Hashtbl.create 10 in
		let get_descriptor handle =
			(try
				Hashtbl.find descriptors handle
			with
			| Not_found -> raise (LibraryError "invalid handle for closeFile")) in
		[
		("openFileForWriting",[Name("handle"); Name("filename")], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let filename = Interpreter.cast_to_string (SymbolTable.get_value (Name("filename")) stbl) in
					try
						let ch = open_out filename in
						Hashtbl.add descriptors handle (OutChannel(ch), filename)
					with
					| _ -> raise (LibraryError ("error opening file "^filename^" in openFileForWriting"))
		);
		("openFileForReading",[Name("handle"); Name("filename")], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let filename = Interpreter.cast_to_string (SymbolTable.get_value (Name("filename")) stbl) in
					try
						let ch = open_in filename in
						Hashtbl.add descriptors handle (InChannel(ch,""), filename)
					with
					| _ -> raise (LibraryError ("error opening file "^filename^" in openFileForReading"))
		);
		("closeFile",[Name("handle")], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let (c, _ ) = get_descriptor handle in
					try
						(match c with
							| OutChannel(ch) -> close_out ch
							| InChannel(ch, _) -> close_in ch);
						Hashtbl.remove descriptors handle
					with
					| Sys_error msg -> raise (LibraryError ("System error on closeFile:" ^ msg ))
		);
		("write",[Name("handle"); Name("string")], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let data = Interpreter.cast_to_string (SymbolTable.get_value (Name("string")) stbl) in
					let (c, filename) = get_descriptor handle in
					try
						match c with
						| OutChannel(ch) -> output_string ch data
						| InChannel(ch, _) -> raise (LibraryError ("invalid handle in call to write. Handle "^handle^" was opened for reading "^filename))
					with
					| _ -> raise (LibraryError ("error writing file "^filename^" in write"))
		);
		("writeln",[Name("handle"); Name("data")], fun stbl ->
					let handle = Interpreter.cast_to_string (SymbolTable.get_value (Name("handle")) stbl) in
					let data = Interpreter.cast_to_string (SymbolTable.get_value (Name("data")) stbl) in
					let (c, filename) = get_descriptor handle in
					try
						match c with
						| OutChannel(ch) -> output_string ch (data ^ "\n")
						| InChannel(ch, _) -> raise (LibraryError ("invalid handle in call to write. Handle "^handle^" was opened for reading "^filename))
					with
					| _ -> raise (LibraryError ("error writing file "^filename^" in writeln"))
		);
		("writeFile",[Name("filename"); Name("data")], fun stbl ->
					let filename = Interpreter.cast_to_string (SymbolTable.get_value (Name("filename")) stbl) in
					let data = Interpreter.cast_to_string (SymbolTable.get_value (Name("data")) stbl) in
					try
						let ch = open_out filename in
						output_string ch data;
						close_out ch
					with
					| _ -> raise (LibraryError ("error writing file "^filename^" in writeFile"))
		);
		("readFile",[Name("filename")], fun stbl ->
					let filename = Interpreter.cast_to_string (SymbolTable.get_value (Name("filename")) stbl) in
					let data =
						try
							let ch = open_in filename in
							let rec loop data =
								(try
									loop (data ^ (input_line ch) ^ "\n")
								with
								| End_of_file -> close_in ch; data) in
							loop ""
						with
						| _ -> raise (LibraryError ("error reading file "^filename^" in readFile")) in
					raise (Interpreter.CFReturn (StringValue(data)))
		);
		]
	
end