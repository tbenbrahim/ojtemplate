(**
Definition of the parser generated AST and the runtime AST

@author Tony BenBrahim < tony.benbrahim at gmail.com >
*)
(* This program is free software; you can redistribute it and / or modify  *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation; version 3 of the License. This program is *)
(* distributed in the hope that it will be useful, but WITHOUT ANY         *)
(* WARRANTY; without even the implied warranty of MERCHANTABILITY or       *)
(* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details. s                                                     *)

(** binary operation operators *)
type operator = | Plus | Minus | Times | Divide | Modulo | And | Or

(** binary comparaison operators *)
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

(** string replacement specification in a template instruction *)
type replacement =
	(string * expression)

(** list of replacements for a template instructions *)
and replacement_list =
	replacement list

(** conditional replacement criteria for a template instruction *)
and conditional_spec =
	| Once
	| When of expression
	| Loop of string * expression
	| CondLoop of expression * string * expression

(** a single instruction in a set of template instructions *)
and replacement_spec =
	(string * conditional_spec * replacement_list)

(** definition for a line in a template definition *)
and template_spec =
	((string option) * string)

(** type of map variable, either a dictionary or an array *)
and map_subtype =
	| MapSubtype | ArraySubtype

(** variable values used in parsing AST *)
and variable_value =
	| IntegerValue of int
	| FloatValue of float
	| StringValue of string
	| BooleanValue of bool
	| FunctionValue of string list * statement list
	| MapValue of (string, variable_value) Hashtbl.t * map_subtype
	| Void

(** variable values used in runtime AST *)
and runtime_variable_value =
	| RIntegerValue of int
	| RFloatValue of float
	| RStringValue of string
	| RBooleanValue of bool
	| RFunctionValue of int * int * int * bool * runtime_statement list	* (((int * int), runtime_variable_value) Hashtbl.t)	option * runtime_expression option
	|	RLibraryFunction of lib_function_def
	| RMapValue of (string, runtime_variable_value) Hashtbl.t * map_subtype
	| RVoid
	| RUndefined

(**
The runtime environment.
consists of a heap for globals and an array of stackframes to support nested functions
*)
and runtime_env =
	{ heap : (int * runtime_variable_value) array; (** heap, arary of tuple of uid and value *)
		stackframes : (runtime_variable_value array) array; (** array of stackframes *)
		mutable closure_vars :
		(((int * int), runtime_variable_value) Hashtbl.t) option; (** map of closure variables *)
		gnames : string array; (** array of global names, indexed by uid *)
		mutable current_line : (string * int);  (** file and line currently interpreted *)
		callstack : (string * int) Stack.t; (** callstack *)
		mutable skip_callstack_pop: bool; (** to indicate whether the call stack entry was skipped in a recursive call *)
	}

(**
Definition for a library function
*)
and lib_function_def =
	{ name : string list; (** namespace and name of function *)
		args : string list; (** list of arguments *)
		num_args : int; (** number of arguments *)
		vararg : bool; (** flag indicating whether the last argument is vararg *)
		code : runtime_env -> unit (** function call to invoked the function *)
	}

(** expressions used in parsing AST *)
and expression =
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

(** expressions used in runtime AST *)
and runtime_expression =
	| RVariable of variable_location
	| RVarArg of variable_location
	| RBinaryOp of runtime_expression * operator * runtime_expression
	| RCompOp of runtime_expression * comparator * runtime_expression
	| RNot of runtime_expression
	| RFunctionCall of runtime_expression * runtime_expression list
	| RMapExpr of (string * runtime_expression) list
	| RArrayExpr of runtime_expression list
	| RValue of runtime_variable_value
	| RAssignment of runtime_expression * runtime_expression
	| RDeclaration of runtime_expression * runtime_expression
	| RMemberExpr of runtime_expression * runtime_expression
	| RPostFixSum of runtime_expression * int
	| RTernaryCond of runtime_expression * runtime_expression	* runtime_expression

(** statements used in parsing AST *)
and statement =
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
	| Import of string * (string * int)
	| Switch of expression * statement list * (string * int)
	| Case of expression option * (string * int)
	| TryCatch of statement * string * statement * (string * int)
	| TryFinally of statement * statement * (string * int)
	| Throw of expression * (string * int)

(** statements used in runtime AST *)
and runtime_statement =
	| RForEach of variable_location * runtime_expression * runtime_statement	* (string * int)
	| RFor of runtime_expression * runtime_expression * runtime_expression	* runtime_statement * (string * int)
	| RExpressionStatement of runtime_expression * (string * int)
	| RBreak of (string * int)
	| RContinue of (string * int)
	| RNoop
	| RReturn of runtime_expression * (string * int)
	| RIf of runtime_expression * runtime_statement * runtime_statement	* (string * int)
	| RStatementBlock of runtime_statement list
	| RProgram of runtime_statement list
	| RSwitch of runtime_expression * runtime_statement list * (string * int)
	| RCase of runtime_expression option * (string * int)
	| RTryCatch of runtime_statement * variable_location * runtime_statement	* (string * int)
	| RTryFinally of runtime_statement * runtime_statement * (string * int)
	| RThrow of runtime_expression * (string * int)
	| RFastIterator of variable_location * int * int * int * runtime_statement * (string * int)

(**
determines if a variable is a varag
@param varname the variable name
@return true if the variable is a vararg, false otherwise
*)
let is_vararg varname = varname.[0] = '['

(** retuns the name for a vararg *)
let vararg_formalname varname =
	String.sub varname 1 ((String.length varname) - 1)

(** control flow exception for return instruction *)
exception CFReturn of runtime_variable_value

(** control flow exception for break instruction *)
exception CFBreak

(** control flow exception for continue instruction *)
exception CFContinue

(** exception generated by interpreted throw exception *)
exception CFUserException of runtime_variable_value * string

