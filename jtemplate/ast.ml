type operator = | Plus | Minus | Times | Divide | Modulo | And | Or

type comparator =
  | LessThan
  | LessThanEqual
  | Equal
  | GreaterThanEqual
  | GreaterThan
  | NotEqual

(**
location for a variable in the runtime AST
for globals, unique id * an index into the global variables array
for locals, unique id * an index into the current stackframe * an index into the stack
*)
type variable_location =
  | GlobalVar of int * int | LocalVar of int * int * int

type replacement =
  (string * expression)

  and replacement_list =
  replacement list

  and conditional_spec =
  | Once
  | When of expression
  | Loop of string * expression
  | CondLoop of expression * string * expression

  and replacement_spec =
  (string * conditional_spec * replacement_list)

  and template_spec =
  ((string option) * string)

  and map_subtype =
  | MapSubtype | ArraySubtype

  and (** variable values used in parsing AST *)
  variable_value =
  | IntegerValue of int
  | FloatValue of float
  | StringValue of string
  | BooleanValue of bool
  | FunctionValue of string list * statement list
  | MapValue of (string, variable_value) Hashtbl.t * map_subtype
  | Void
  | NaN

  and (** variable values used in runtime AST *)
  runtime_variable_value =
  | RIntegerValue of int
  | RFloatValue of float
  | RStringValue of string
  | RBooleanValue of bool
  | RFunctionValue of int * int * int * bool * runtime_statement list
                      * ((int, runtime_variable_value) Hashtbl.t) option
  | (* stack size, depth, argslen,varargs?, statements, closure vars *)
  RLibraryFunction of lib_function_def
  | RMapValue of (string, runtime_variable_value) Hashtbl.t * map_subtype
  | RVoid
  | RNaN
  | RUndefined

  and (**
The runtime environment.
consists of a heap for globals and an array of stackframes to support nested functions
*)
  runtime_env =
  { heap : (int * runtime_variable_value) array;
    stackframes : (runtime_variable_value array) array;
    mutable closure_vars : ((int, runtime_variable_value) Hashtbl.t) option;
    gnames : string array; mutable current_line : (string * int);
    mutable callstack : (string * int) list
  }

  and (**
Definition for a library function
*)
  lib_function_def =
  { name : string list; args : string list; num_args : int; vararg : bool;
    code : runtime_env -> unit
  }

  and (** expressions used in parsing AST *)
  expression =
  | Id of string
  | VarArg of string
  | BinaryOp of expression * operator * expression
  | CompOp of expression * comparator * expression
  | Not of expression
  | FunctionCall of expression * expression list
  | MapExpr of (string * expression) list
  | ArrayExpr of expression list
  | Value of variable_value
  | UnboundVar of string
  | Assignment of expression * expression
  | Declaration of expression * expression
  | MemberExpr of expression * expression
  | PostFixSum of expression * int
  | TernaryCond of expression * expression * expression

  and (** expressions used in runtime AST *)
  runtime_expression =
  | RVariable of variable_location
  | RVarArg of variable_location
  | RBinaryOp of runtime_expression * operator * runtime_expression
  | RCompOp of runtime_expression * comparator * runtime_expression
  | RNot of runtime_expression
  | RFunctionCall of runtime_expression * runtime_expression list
  | (*| RFastFunctionCall of int * int * int * bool * runtime_statement list * (int, runtime_variable_value) Hashtbl.t option * runtime_expression list*)
  RMapExpr of (string * runtime_expression) list
  | RArrayExpr of runtime_expression list
  | RValue of runtime_variable_value
  | RAssignment of runtime_expression * runtime_expression
  | RDeclaration of runtime_expression * runtime_expression
  | RMemberExpr of runtime_expression * runtime_expression
  | RPostFixSum of runtime_expression * int
  | RTernaryCond of runtime_expression * runtime_expression
                    * runtime_expression

  and (** statements used in parsing AST *)
  statement =
  | ForEach of string * expression * statement * (string * int)
  | For of expression * expression * expression * statement * (string * int)
  | ExpressionStatement of expression * (string * int)
  | Break of (string * int)
  | Continue of (string * int)
  | Noop
  | Return of expression * (string * int)
  | If of expression * statement * statement * (string * int)
  | TemplateDef of string * template_spec list * (string * int)
  | Instructions of string * string list * replacement_spec list
                    * (string * int)
  | StatementBlock of statement list
  | Program of statement list
  | (* like a statement block but does not push scope*)
  Import of string * (string * int)
  | Switch of expression * statement list * (string * int)
  | Case of expression option * (string * int)
  | TryCatch of statement * string * statement * (string * int)
  | TryFinally of statement * statement * (string * int)
  | Throw of expression * (string * int)

  and (** statements used in runtime AST *)
  runtime_statement =
  | RForEach of variable_location * runtime_expression * runtime_statement
                * (string * int)
  | RFor of runtime_expression * runtime_expression * runtime_expression
            * runtime_statement * (string * int)
  | RExpressionStatement of runtime_expression * (string * int)
  | RBreak of (string * int)
  | RContinue of (string * int)
  | RNoop
  | RReturn of runtime_expression * (string * int)
  | RIf of runtime_expression * runtime_statement * runtime_statement
           * (string * int)
  | RStatementBlock of runtime_statement list
  | RProgram of runtime_statement list
  | (* like a runtime_statement block but does not push scope*)
  RSwitch of runtime_expression * runtime_statement list * (string * int)
  | RCase of runtime_expression option * (string * int)
  | RTryCatch of runtime_statement * variable_location * runtime_statement
                 * (string * int)
  | RTryFinally of runtime_statement * runtime_statement * (string * int)
  | RThrow of runtime_expression * (string * int)

let is_vararg varname = varname.[0] = '['
  
let vararg_formalname varname =
  String.sub varname 1 ((String.length varname) - 1)
  
exception CFReturn of runtime_variable_value
  
exception CFBreak
  
exception CFContinue
  
exception CFUserException of runtime_variable_value
  
