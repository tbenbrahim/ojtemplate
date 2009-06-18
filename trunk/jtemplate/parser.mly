%{
	
open Ast
open Symbol_table

let parse_error s =
    let pos = Parsing.symbol_start_pos() in
    print_string ("in file " ^ (Filename.basename pos.Lexing.pos_fname) ^ ": "^ s^" at line ");
    print_int pos.Lexing.pos_lnum;
    print_string " at columns ";
    print_int (Parsing.symbol_start() - pos.Lexing.pos_bol);
    print_string("-");
    print_int (Parsing.symbol_end() - pos.Lexing.pos_bol);
    print_string "\n";
    flush stdout
		
let get_env ()= 
	let pos=Parsing.symbol_start_pos() in
	(pos.Lexing.pos_fname,pos.Lexing.pos_lnum)

		
let resolve_import (filename, library, (inp_file, _))=
	let fullname=Filename_util.resolve_filename (Filename.dirname inp_file) filename
	in
	   (fullname, {loaded=false})
		
let extract_stmt_list=function
	| StatementBlock(lst) -> lst
	| _ -> raise ( RuntimeError.InternalError "expected statement block" )
%}

%token<string> ID
%token <int> INT
%token <string> STRING
%token <float> REAL
%token <bool> BOOLEAN
%token <string> TEXT
%token <Ast.comparator> COMPOP
%token <bool> IMPORT

%token FOREACH WHILE IF  FOR ELSE TEMPLATE INSTRUCTIONS FUNCTION CONTINUE BREAK
%token RETURN IN ONCE WHEN VAR EOF LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token COMMA SEMICOLON COLON DOTDOTDOT DOT EQUALS NOT QUESTION PLUS MINUS TIMES
%token DIVIDE MODULO AND OR VOID NAN SWITCH CASE DEFAULT PLUSEQUALS MINUSEQUALS
%token TIMESEQUALS DIVEQUALS MODEQUALS PLUSPLUS MINUSMINUS AT


%start program
%type <Ast.statement> program

/* resolve shift/reduce conflict for ELSE */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* resolve shift/reduce conflict for {} (map) and {} (empty block) */
%nonassoc MAP
%nonassoc EMPTYBLOCK

%right EQUALS 

%left NOT
%left AND OR
%left COMPOP

%left PLUSEQUALS MINUSEQUALS 
%left TIMESEQUALS DIVEQUALS MODEQUALS
%left PLUS MINUS
%left TIMES DIVIDE MODULO


%%
program:
    | opt_statements EOF { Program($1) }
;
statements:                           
    | statement   { [$1] }
    | statement statements  { $1::$2 }
;
opt_statements:
    | statements  { $1 }
		| /*nothing*/ { [] }
;
statement_block:                      
    | LBRACE opt_statements RBRACE  { StatementBlock($2) } 
;
else_clause:                         
    | ELSE statement { $2 }
    | %prec LOWER_THAN_ELSE { Noop }
;
statement:                                    
    | IF LPAREN expression RPAREN statement else_clause { If($3,$5,$6,get_env()) }
		| expression_statement SEMICOLON   { ExpressionStatement($1, get_env()) }
    | statement_block { $1 }
    | FOREACH LPAREN ID IN expression RPAREN statement { ForEach(Name($3),$5,$7,get_env()) }
		| WHILE LPAREN expression RPAREN statement { For(Value(Void),$3,Value(Void),$5,get_env()) }
		| CONTINUE SEMICOLON                      { Continue(get_env())}
    | BREAK SEMICOLON                         { Break(get_env())}
    | RETURN opt_expression SEMICOLON         { Return($2,get_env()) }
    | IMPORT STRING SEMICOLON                 { Import(resolve_import($2,$1,get_env()),get_env()) }
		| TEMPLATE ID LBRACE template_specs RBRACE { TemplateDef(Name($2), $4,get_env()) }
    | INSTRUCTIONS FOR ID LPAREN arglist RPAREN LBRACE instruction_specs RBRACE
                                              { Instructions(Name($3),$5,$8,get_env()) }
    | SWITCH LPAREN expression RPAREN LBRACE switch_statements RBRACE
                                              { Switch($3,$6, get_env()) }		
    | FOR LPAREN opt_expression SEMICOLON 
                 opt_expression SEMICOLON 
                 opt_expression RPAREN statement { For($3,$5,$7,$9,get_env()) }																					
;
switch_statement:
    | CASE expression COLON                   { Case(Some $2,get_env()) }
    | DEFAULT COLON                           { Case(None,get_env()) }
		| statement                               { $1 }
;
switch_statements:
    | switch_statement                        { [$1] }
		| switch_statement switch_statements      { $1::$2 }
;
value:                                
    | INT                                     { Value(IntegerValue($1)) }
    | REAL                                    { Value(FloatValue($1)) }
    | STRING                                  { Value(StringValue($1)) }
    | BOOLEAN                                 { Value(BooleanValue($1)) }
    | VOID                                    { Value(Void) }
    | NAN                                     { Value(NaN) }
    | function_def                            { $1 }
    | LBRACKET expr_list RBRACKET             { ArrayExpr($2) }
    | LBRACE prop_list RBRACE                 { MapExpr($2) }
    | variable                                { VariableExpr($1) }
;
opt_expression:
    | expression                              { $1 }
		| empty_expression                        { Value(Void) }
;
empty_expression:
    | /*nothing */                            { Value(Void) }
;
expression_statement: //expressions that can be statements when followed by semicolon
    | function_call                           { $1 }
		| empty_expression                        { Value(Void) }
    | variable EQUALS expression              { Assignment($1,$3) } 
    | VAR variable EQUALS expression          { Declaration($2,$4) }
    | variable PLUSEQUALS expression          { Assignment($1,BinaryOp(VariableExpr($1),Plus,$3)) }
    | variable MINUSEQUALS expression         { Assignment($1,BinaryOp(VariableExpr($1),Minus,$3)) }
    | variable TIMESEQUALS expression         { Assignment($1,BinaryOp(VariableExpr($1),Times,$3)) }
    | variable DIVEQUALS expression           { Assignment($1,BinaryOp(VariableExpr($1),Divide,$3)) }
    | variable MODEQUALS expression           { Assignment($1,BinaryOp(VariableExpr($1),Modulo,$3)) }
    | PLUSPLUS variable                       { Assignment($2,BinaryOp(VariableExpr($2),Plus,Value(IntegerValue(1)))) }
    | MINUSMINUS variable                     { Assignment($2,BinaryOp(VariableExpr($2),Minus,Value(IntegerValue(1)))) }
    | variable PLUSPLUS                       { DirectFunctionCall(Value(FunctionValue([],[StatementBlock([
                                                  ExpressionStatement(Declaration(Name("x"),VariableExpr($1)),get_env());
                                                  ExpressionStatement(Assignment($1,BinaryOp(VariableExpr($1),Plus,Value(IntegerValue(1)))),get_env());
                                                  Return(VariableExpr(Name("x")),get_env()); ])])),[]) }                                                                                                    
    | variable MINUSMINUS                     { DirectFunctionCall(Value(FunctionValue([],[StatementBlock([
                                                  ExpressionStatement(Declaration(Name("x"),VariableExpr($1)),get_env());
                                                  ExpressionStatement(Assignment($1,BinaryOp(VariableExpr($1),Minus,Value(IntegerValue(1)))),get_env());
                                                  Return(VariableExpr(Name("x")),get_env()); ])])),[]) }                                                                                                    
;
expression:
    | function_call                           { $1 }           
    | value                                   { $1 }
    | unbound_variable                        { $1 }
    | LPAREN expression RPAREN                { $2 }
    | expression PLUS expression              { BinaryOp($1,Plus,$3) }
    | expression MINUS expression             { BinaryOp($1,Minus,$3) }
    | expression TIMES expression             { BinaryOp($1,Times,$3) }
    | expression DIVIDE expression            { BinaryOp($1,Divide,$3) }
    | expression MODULO expression            { BinaryOp($1,Modulo,$3) }
		| NOT expression                          { Not($2) }
    | expression AND expression               { BinaryOp($1,And,$3) }
    | expression OR expression                { BinaryOp($1,Or,$3) }
    | expression COMPOP expression            { CompOp($1,$2,$3) }
		| variable EQUALS expression              { Assignment($1,$3) } 
    | VAR variable EQUALS expression          { Declaration($2,$4) }
    | variable PLUSEQUALS expression          { Assignment($1,BinaryOp(VariableExpr($1),Plus,$3)) }
    | variable MINUSEQUALS expression         { Assignment($1,BinaryOp(VariableExpr($1),Minus,$3)) }
    | variable TIMESEQUALS expression         { Assignment($1,BinaryOp(VariableExpr($1),Times,$3)) }
    | variable DIVEQUALS expression           { Assignment($1,BinaryOp(VariableExpr($1),Divide,$3)) }
    | variable MODEQUALS expression           { Assignment($1,BinaryOp(VariableExpr($1),Modulo,$3)) }
		| PLUSPLUS variable                       { Assignment($2,BinaryOp(VariableExpr($2),Plus,Value(IntegerValue(1)))) }
    | MINUSMINUS variable                     { Assignment($2,BinaryOp(VariableExpr($2),Minus,Value(IntegerValue(1)))) }
		| variable PLUSPLUS                       { DirectFunctionCall(Value(FunctionValue([],[StatementBlock([
			                                            ExpressionStatement(Declaration(Name("x"),VariableExpr($1)),get_env());
																									ExpressionStatement(Assignment($1,BinaryOp(VariableExpr($1),Plus,Value(IntegerValue(1)))),get_env());
                                                  Return(VariableExpr(Name("x")),get_env()); ])])),[]) }																									
    | variable MINUSMINUS                     { DirectFunctionCall(Value(FunctionValue([],[StatementBlock([
                                                  ExpressionStatement(Declaration(Name("x"),VariableExpr($1)),get_env());
                                                  ExpressionStatement(Assignment($1,BinaryOp(VariableExpr($1),Minus,Value(IntegerValue(1)))),get_env());
                                                  Return(VariableExpr(Name("x")),get_env()); ])])),[]) } 
		| LPAREN expression QUESTION expression COLON expression RPAREN 
		                                          {DirectFunctionCall(Value(FunctionValue([],[
																								  If($2,Return($4,get_env()),Return($6,get_env()),get_env()) ])),[]) }                                                                                                   
;                           
function_call:                        
    | variable LPAREN expr_list RPAREN        { FunctionCall($1,$3) }
    | function_def LPAREN expr_list RPAREN    { DirectFunctionCall($1,$3) }
;
function_def:
    | FUNCTION LPAREN arglist RPAREN statement_block { Value(FunctionValue($3,extract_stmt_list($5))) }
;                                                                                                                                                           
arglist:                              
    | ID                                      { [Name($1)] }
    | ID DOTDOTDOT                            { [Name("["^$1)] }
    | ID COMMA arglist                        { Name($1)::$3 }
    | /* nothing */                            { [] }
;
variable:                             
    | ids                                     {match $1 with
                                                 Name(id) :: [] -> Name(id)
                                                 | _ -> EvaluatedName($1) }
;
ids:                                  
    | ID                                      { [Name($1)] (*TODO array ref*)}
    | array_index                             { [$1] }
    | ID DOT ids                              { Name($1)::$3 }
;
array_index:                          
    | ID LBRACKET expression RBRACKET         { ArrayIndex($1,$3) }
;
unbound_variable:                    
    | AT ID                                   { UnboundVar(Name($2)) }
    | AT ID DOTDOTDOT                         { UnboundVar(Name("["^$2)) }
;
expr_list:
    | expression                              { [$1] }
    | expression COMMA expr_list              { $1::$3 }
    | /*nothing*/                             { [] }
;
property:                             
    | ID COLON expression                     { ($1,$3) }
;
prop_list:                            
    | property                                { [$1] }
    | property COMMA prop_list                { $1::$3 }
		| /*empty*/                               { [] }
;
template_spec:                        
    | label TEXT                              { (Some $1,$2) }
    | TEXT                                    { (None, $1) }
;
template_specs:                       
    | template_spec                           { [$1] }
    | template_spec template_specs            { $1::$2 }
;   
instruction_spec:                     
    | label repl_condition COLON replacement_list SEMICOLON { ($1,$2,$4) }
    | label repl_condition SEMICOLON          { ($1,$2,[]) }
;
repl_condition:                       
    | ONCE                                    { Once }
    | WHEN LPAREN expression RPAREN           { When($3) }
    | FOREACH LPAREN ID IN expression RPAREN  { Loop(Name($3),$5) }
    | FOREACH LPAREN ID IN expression RPAREN WHEN LPAREN expression RPAREN
                                              { CondLoop($9,Name($3),$5) }  
;
replacement:                          
    | ID EQUALS expression                    { ($1,$3) }
;
replacement_list:                     
    | replacement                             { [$1] }
    | replacement COMMA replacement_list      { $1::$3 }
;
instruction_specs:                    
    | instruction_spec                        { [$1] }
    | instruction_spec instruction_specs      { $1::$2 }
; 
label:
    | ID                                      { Label($1) }
    | INT                                     { Label(string_of_int($1)) }
;
%%
