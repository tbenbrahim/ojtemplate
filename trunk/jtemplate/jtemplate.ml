open Lexer
open Parser
open Lexing
open Symbol_table
open Interpreter
open Library_builtin
open Library_string
open Library_io
open Library_helper
open Ast
open Filename_util
open RuntimeError

let register_args len symbol_table =
  let h = Hashtbl.create (len - 1) in
  SymbolTable.declare "args" (MapValue(h, ArraySubtype)) symbol_table;
  Hashtbl.add h "length" (IntegerValue(len - 1));
  let rec loop ind =
    Hashtbl.add h (string_of_int (ind - 1)) (StringValue(Sys.argv.(ind)));
    if ind > 1 then loop (ind - 1) else ()
  in
  loop (len - 1)

let _ =
  let symbol_table = SymbolTable.initialize_environment
      { parse_callback = (Parser_util.parse_filename);
        loaded_imports =[] ; current_stmt = ("", 0);
        stack_trace = []; } in
  register_library BuiltinLibrary.exported symbol_table;
  register_library StringLibrary.exported symbol_table;
  register_library IOLibrary.exported symbol_table;
  let _ = Parsing.set_trace false in
  let argl = Array.length Sys.argv in
  if argl < 2 then
    prerr_string ("Usage: "^(Filename.basename Sys.argv.(0))^" scriptfile [args...]\n")
  else
    (
      register_args argl symbol_table;
      let filename = Sys.argv.(1) in
      let ast = if filename ="-" then Parser_util.parse stdin "stdin" else Parser_util.parse_filename (resolve_filename (Unix.getcwd()) filename)
      in
      try
        Interpreter.interpret_program ast symbol_table
      with
      | FatalExit _ -> exit(- 1)
    )
