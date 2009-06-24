open Symbol_table
open Ast

let create_namespace namelist symbol_table =
	let rec create_member list h =
		match list with
		| [] -> raise (RuntimeError.InternalError "expected mapvalue in library namespace")
		| hd:: [] -> (h, hd)
		| hd:: tl ->
				let nh =
					(try
						(match Hashtbl.find h hd with
							| MapValue(nh, MapSubtype) -> nh
							| _ -> raise (RuntimeError.InternalError "expected mapvalue in library namespace"))
					with
					| Not_found ->
							let nh = Hashtbl.create 10 in
							Hashtbl.add h hd (MapValue(nh, MapSubtype));
							nh)
				in create_member tl nh
	in create_member namelist symbol_table.values

let rec make_name_list strlist namelist=
    match strlist with
        | [] -> List.rev namelist
        | hd::tl -> make_name_list tl (hd::namelist)

let register_library defs symbol_table =
	let _ = List.map (fun def -> let (names, formal_args, code) = def in
						let (h, name) = create_namespace names symbol_table
						in Hashtbl.replace h name (Ast.LibraryFunction(make_name_list formal_args [], code, symbol_table))
			) defs
	in ()
