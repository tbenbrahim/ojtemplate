open Environment
open Ast
open Expression
open RuntimeError
open Unix

exception EIOPassthrough of string

type channelType = OutChannel of out_channel | InChannel of in_channel * (string * bool)

(**
Entry point for library initialization
@return a list of exported functions
*)
let initialize env =
	let env = Environment.declare_variable_and_value env "File" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let env = Environment.declare_variable_and_value env "Directory" (RMapValue(Hashtbl.create 10, MapSubtype))
	in let descriptors = Hashtbl.create 10 in
	let get_descriptor handle command =
		(try
			Hashtbl.find descriptors handle
		with
		| Not_found -> raise (EIOPassthrough("invalid handle for "^command)))
	in let try_read ch =
		(
			try
				(input_line ch, false)
			with
			| End_of_file -> ("", true)
		)
	in ([
		{
			name = ["print"];
			args = ["values..."];
			num_args = 1;
			vararg = true;
			code = fun env ->
						match env.stackframes.(0).(1) with
						| RMapValue(t, ArraySubtype) ->
								List.iter (fun value ->
												print_string (string_of_value value)) (list_of_array (RMapValue(t, ArraySubtype)))
						| _ -> raise (RuntimeError.LibraryError "expected vararg for first parameter of print")
		};
		{
			name = ["println"];
			args = ["values..."];
			num_args = 1;
			vararg = true;
			code = fun env ->
						match env.stackframes.(0).(1) with
						| RMapValue(t, ArraySubtype) ->
								List.iter (fun value ->
												print_string (string_of_value value)) (list_of_array (RMapValue(t, ArraySubtype)));
								print_newline()
						| _ -> raise (RuntimeError.LibraryError "expected vararg for first parameter of println")
		};
		{
			name =["readln"];
			args =[];
			num_args = 0;
			vararg = false;
			code = fun env ->
						raise (CFReturn(RStringValue(read_line())))
		};
		{
			name =["File";"openForWriting"];
			args =["handle"; "filename"];
			num_args = 2;
			vararg = false;
			code = fun env ->
						let handle = cast_to_string (env.stackframes.(0).(1)) in
						let filename = cast_to_string (env.stackframes.(0).(2)) in
						try
							if Hashtbl.mem descriptors handle then
								raise (EIOPassthrough("handle "^handle^" is already opened in call to openForWriting"))
							else
								let ch = open_out filename in
								Hashtbl.add descriptors handle (OutChannel(ch), filename)
						with
						| EIOPassthrough(msg) -> raise (LibraryError msg)
						| _ -> raise (LibraryError ("error opening file "^filename^" in openFileForWriting"))
		};
		{
			name =["File";"openForReading"];
			args =["handle"; "filename"];
			num_args = 2;
			vararg = false;
			code = fun env ->
						let handle = cast_to_string (env.stackframes.(0).(1)) in
						let filename = cast_to_string (env.stackframes.(0).(2)) in
						try
							if Hashtbl.mem descriptors handle then
								raise (EIOPassthrough("handle "^handle^" is already opened in call to openForReading"))
							else
								let ch = open_in filename in
								Hashtbl.add descriptors handle (InChannel(ch, (try_read ch)), filename)
						with
						| EIOPassthrough(msg) -> raise (LibraryError msg)
						| _ -> raise (LibraryError ("error opening file "^filename^" in openFileForReading"))
		};
		{
			name =["File";"close"];
			args =["handle"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let handle = cast_to_string (env.stackframes.(0).(1)) in
						let (c, _ ) = get_descriptor handle "closeFile" in
						try
							(match c with
								| OutChannel(ch) -> close_out ch
								| InChannel(ch, _) -> close_in ch);
							Hashtbl.remove descriptors handle
						with
						| EIOPassthrough(msg) -> raise (LibraryError msg)
						| Sys_error msg -> raise (LibraryError ("System error on closeFile:" ^ msg ))
		};
		{
			name =["File";"write"];
			args =["handle"; "values..."];
			num_args = 2;
			vararg = true;
			code = fun env ->
						let handle = cast_to_string (env.stackframes.(0).(1)) in
						let (c, filename) = get_descriptor handle "write" in
						match c with
						| OutChannel(ch) ->
								( try
									let _ = List.map (fun el -> output_string ch (cast_to_string el))
											(list_of_array (env.stackframes.(0).(2))) in ()
								with
								| EIOPassthrough(msg) -> raise (LibraryError msg)
								| _ -> raise (LibraryError ("error writing file "^filename^" in write")))
						| InChannel(ch, _) -> raise (LibraryError ("invalid handle in call to write. Handle "^handle^" was opened for reading "^filename))
		};
		{
			name =["File";"writeln"];
			args =["handle"; "values..."];
			num_args = 2;
			vararg = true;
			code = fun env ->
						let handle = cast_to_string (env.stackframes.(0).(1)) in
						let (c, filename) = get_descriptor handle "writeln" in
						match c with
						| OutChannel(ch) ->
								(try
									let _ = List.map (fun el -> output_string ch (cast_to_string el))
											(list_of_array (env.stackframes.(0).(2))) in
									output_string ch ( "\n")
								with
								| EIOPassthrough(msg) -> raise (LibraryError msg)
								| _ -> raise (LibraryError ("error writing file "^filename^" in writeln")))
						| InChannel(ch, _) -> raise (LibraryError ("invalid handle in call to write. Handle "^handle^" was opened for reading "^filename))
		};
		{
			name=["File";"readln"];
			args=["handle"];
			num_args=1;
			vararg=false;
			code= fun env ->
					let handle = cast_to_string (env.stackframes.(0).(1)) in
					let (c, filename) = get_descriptor handle "readln" in
					match c with
					| OutChannel(ch) -> raise (EIOPassthrough ("invalid handle in call to readln. Handle "^handle^" was opened for writing "^filename))
					| InChannel(ch, (_, true)) -> raise (EIOPassthrough ("End of file reached for handle "^handle^" in call to readln"))
					| InChannel(ch, (data, false)) ->
							( try
								Hashtbl.replace descriptors handle (InChannel(ch, (try_read ch)), filename)
							with
							| EIOPassthrough(msg) -> raise (LibraryError msg)
							| _ -> raise (LibraryError ("error reading file "^filename^" in readln")));
							raise (CFReturn(RStringValue(data)))
		};
		{
			name=["File";"eof"];
			args=["handle"];
			num_args=1;
			vararg=false;
			code= fun env ->
					let handle = cast_to_string (env.stackframes.(0).(1)) in
					let (c, filename) = get_descriptor handle "eof" in
					match c with
					| OutChannel(ch) -> raise (EIOPassthrough("invalid handle in call to eof. Handle "^handle^" was opened for writing "^filename))
					| InChannel(ch, (_, eof)) -> raise (CFReturn(RBooleanValue(eof)))
		};
		{
			name =["File";"exists"];
			args =["filename"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let filename = cast_to_string (env.stackframes.(0).(1)) in
						raise (CFReturn(RBooleanValue (Sys.file_exists filename)))
		};
		{
			name =["File";"delete"];
			args =["name"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let name = cast_to_string (env.stackframes.(0).(1)) in
						try
							unlink name;
							raise (CFReturn(RBooleanValue(true)))
						with
						| _ -> raise (CFReturn(RBooleanValue(false)))
		
		};
		{
			name =["File";"rename"];
			args =["fromname";"toname"];
			num_args = 2;
			vararg = false;
			code = fun env ->
						let fromname = cast_to_string (env.stackframes.(0).(1)) in
						let toname = cast_to_string (env.stackframes.(0).(2)) in
						try
							rename fromname toname;
							raise (CFReturn(RBooleanValue(true)))
						with
						| _ -> raise (CFReturn(RBooleanValue(false)))
		};
		{
			name =["Directory";"create"];
			args =["name"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let name = cast_to_string (env.stackframes.(0).(1)) in
						try
							mkdir name 0o640;
							raise (CFReturn(RBooleanValue(true)))
						with
						| _ -> raise (CFReturn(RBooleanValue(false)))
		};
		{
			name =["Directory";"delete"];
			args =["name"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let name = cast_to_string (env.stackframes.(0).(1)) in
						try
							rmdir name;
							raise (CFReturn(RBooleanValue(true)))
						with
						| _ -> raise (CFReturn(RBooleanValue(false)))
		};
		{
			name =["Directory";"list"];
			args =["name"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let name = cast_to_string (env.stackframes.(0).(1)) in
						let arr = (try
								let handle = opendir name in
								let h = Hashtbl.create 10
								in let rec loop cnt =
									try
										Hashtbl.add h (string_of_int cnt) (RStringValue(readdir handle));
										loop (cnt + 1)
									with
									| End_of_file -> closedir handle; cnt
									| _ -> closedir handle; raise (CFReturn(RVoid))
								in Hashtbl.add h "length" (RIntegerValue(loop 0));
								h
							with
							| _ -> raise (CFReturn(RVoid)))
						in raise (CFReturn(RMapValue(arr, ArraySubtype)));
		};
		{
			name =["Directory";"exists"];
			args =["name"];
			num_args = 1;
			vararg = false;
			code = fun env ->
						let name = cast_to_string (env.stackframes.(0).(1)) in
						raise (CFReturn(RBooleanValue((try
											Sys.is_directory name
										with
										| _ -> raise (CFReturn(RVoid))))))
		};
		], env)