open Symbol_table
open Ast


let get_name = function 
	| name::[] -> Name(name)
	| list -> CompoundName(list)
	
let create_namespace list symbol_table =
	if SymbolTable.is_undefined (get_name list) symbol_table then
		SymbolTable.declare (get_name list) (MapValue((Hashtbl.create 10), MapSubtype)) symbol_table
	else
		()

let rec check_namespace list prev symbol_table =
	match list with
	| _::[] | [] -> ()
	| name:: tl -> let prev = List.append prev [name]  in
			create_namespace prev symbol_table;
			check_namespace tl prev symbol_table

let register_library defs symbol_table =
	let _ = List.map (fun def -> let (names, formal_args, code) = def in
						(match names with
							| name::[]-> SymbolTable.declare (Ast.Name(name)) (Ast.LibraryFunction(formal_args, code, symbol_table)) symbol_table
							| namelist -> check_namespace namelist [] symbol_table;
									SymbolTable.declare (Ast.CompoundName(namelist)) (Ast.LibraryFunction(formal_args, code, symbol_table)) symbol_table
						); ()) defs
	in ()
