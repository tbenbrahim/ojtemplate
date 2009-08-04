(**
This program is free software; you can redistribute it and / or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

Routines to parse a file

@author Tony BenBrahim < tony.benbrahim at gmail.com >

*)

open RuntimeError

let get_ast lexbuf =
  let _ = Parsing.set_trace false in
  try Parser.program Lexer.main lexbuf
  with
  | RuntimeError.LexerException (msg, line, col) ->
      (print_string
          (msg ^
            (" at line " ^
              ((string_of_int line) ^
                (" col " ^ ((string_of_int col) ^ "\n")))));
        exit 2)

let parse channel name =
  let lexbuf = Lexing.from_channel channel in
  let pos = lexbuf.Lexing.lex_curr_p in
  let _ =
    lexbuf.Lexing.lex_curr_p <-
    { (pos) with Lexing.pos_lnum = 1; Lexing.pos_fname = name; }
  in get_ast lexbuf

let parse_filename filename =
  let channel = open_in filename in
  let ast = parse channel filename in
  close_in channel;
  ast

