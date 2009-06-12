open Symbol_table

let register_library defs symbol_table =
	let _ = List.map (fun def -> let (name, formal_args, code) = def in
						SymbolTable.declare (Ast.Name(name)) (Ast.LibraryFunction(formal_args, code, symbol_table)) symbol_table) defs
	in ()
