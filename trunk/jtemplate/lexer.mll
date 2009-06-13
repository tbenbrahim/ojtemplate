{
open Ast
open Parser

(* from http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamllex-tutorial.pdf , p.9 *)
let incr_linenum lexbuf =
	let pos = lexbuf.Lexing.lex_curr_p in
	lexbuf.Lexing.lex_curr_p <- { pos with
		Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
		Lexing.pos_bol = pos.Lexing.pos_cnum;
		}

exception LexerException of string * int * int

let syntax_exception msg lexbuf=
	raise (LexerException (msg, lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum,
					   lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol))

let map_id name=
	match name with
	  "foreach" -> FOREACH
	| "in" -> IN
	|	"while" -> WHILE
	| "function" -> FUNCTION
	|	"if" -> IF
	| "else" -> ELSE
	|	"template" -> TEMPLATE
	|	"instructions" ->INSTRUCTIONS
	| "continue" -> CONTINUE
	| "break" -> BREAK
	| "return" -> RETURN
	| "for" -> FOR
	| "once" -> ONCE
	| "when" -> WHEN
	| "var" -> VAR
	| "true" -> BOOLEAN(true)
	| "false" -> BOOLEAN(false)
	|	_ ->  ID(name)
		
}
		
let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_' '$']['A'-'Z' 'a'-'z' '0'-'9' '_' '$' ]*
let whitespace = ['\r' '\t' ' ']
let text = '#'[^'\n']*

rule main =	parse 
| whitespace { main lexbuf }
| text as token { TEXT(String.sub token 1 ((String.length token) - 1))}
| digit+ as token { INT( int_of_string token)}
| digit+ '.' digit* | digit* '.' digit+ as token { REAL(float_of_string token)}
| id as token { (map_id token )}
| '\'' { single_quote_string "" lexbuf } 
| "//" [^'\n']* { main lexbuf}
| "/*" {multiline_comment lexbuf}
| '\n' { incr_linenum lexbuf;main lexbuf }
| "&&" {AND}
| "||" {OR}
| "<" {COMPOP(LessThan)}
| ">" {COMPOP(GreaterThan)}
| "<=" {COMPOP(LessThanEqual)}
| ">=" {COMPOP(GreaterThanEqual)}
| "==" {COMPOP(Equal)}
| "!=" {COMPOP(NotEqual)}
| "..." {DOTDOTDOT}
| '=' {EQUALS} 
| '.' {DOT}
| '{' {LBRACE}
| '}' {RBRACE}
| '(' {LPAREN}
| ')' {RPAREN}
| '[' {LBRACKET}
| ']' {RBRACKET}
| ',' {COMMA} 
| ';' {SEMICOLON}
| ':' {COLON}
| '!' {NOT}
| '?' {QUESTION}
| '+' {PLUS}
| '-' {MINUS}
| '*' {TIMES}
| '/' {DIVIDE}
| '%' {MODULO}
| _ as c { syntax_exception ("Invalid character "^String.make 1 c) lexbuf} 
| eof { EOF } 
and single_quote_string s = parse
| '\n'  { incr_linenum lexbuf; single_quote_string (s ^ "\n") lexbuf }
| "\\'"  { single_quote_string (s ^ "\'") lexbuf }
| "'"  { STRING(s) }
| [^ '\''] as c  { single_quote_string (s ^ String.make 1 c) lexbuf }
| eof  { syntax_exception "Unterminated string constant" lexbuf }
and multiline_comment = parse
| '\n' { incr_linenum lexbuf; multiline_comment lexbuf }
| "*/" { main lexbuf }
| [^ '\n'] { multiline_comment lexbuf}
| eof { syntax_exception "Unterminated multiline comment" lexbuf } 