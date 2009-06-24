type operator = | Plus | Minus | Times | Divide | Modulo | And | Or

type comparator =
	| LessThan
	| LessThanEqual
	| Equal
	| GreaterThanEqual
	| GreaterThan
	| NotEqual

type label = | Label of string

type text = string

type replacement =
	(text * expression)

and replacement_list =
	replacement list

and conditional_spec =
	| Once
	| When of expression
	| Loop of string * expression
	| CondLoop of expression * string * expression

and replacement_spec =
	(label * conditional_spec * replacement_list)

and template_spec =
	((label option) * text)

and map_subtype = | MapSubtype | ArraySubtype

(** type definition for the four scalar values (int, float, string, bool)
for a function definition, and for maps. Arrays are syntactic sugar
for maps, for example foo.bar[1] is equivalent to foo.bar.1
Map values are implemented as a recursive structure of Maps. To
resolved foo.bar.1, foo is looked up in the symbol table. It must be
a MapValue. In the MapValue hashtable, bar is looked up. It must be also
be a MapValue. Finally, the last component is looked up in the hashtable
and it can be of any type. *)
and variable_value =
		IntegerValue of int
	| FloatValue of float
	| StringValue of string
	| BooleanValue of bool
	| FunctionValue of string list * statement list
	| ScopedFunctionValue of string list * statement list * symbol_table
	| LibraryFunction of string list * (symbol_table -> unit) * symbol_table
	| MapValue of (string, variable_value) Hashtbl.t * map_subtype
	| Void
	| NaN
and
(**
Definition for a symbol table.
*)
environment ={
	parse_callback: string -> statement;
	mutable loaded_imports: string list;
	mutable current_stmt: string * int;
	mutable stack_trace: (string * int) list; (* filename, line, repeat *)
}
and
symbol_table ={
	values: (string, variable_value) Hashtbl.t; (* variable values for this scope *)
	parent_table: symbol_table option;
	env: environment;
}
and expression =
	| Id of string
	| BinaryOp of expression * operator * expression
	| CompOp of expression * comparator * expression
	| Not of expression
	| FunctionCall of expression * expression list
	| MapExpr of (string * expression) list
	| ArrayExpr of expression list
	| VariableExpr of string
	| Value of variable_value
	| UnboundVar of string
	| Assignment of expression * expression
	| Declaration of expression * expression
	| MemberExpr of expression * expression
	| IndexExpr of expression
	| PostFixSum of expression * int
	(* used internally for expansion of vararg in partially applied          *)
	(* functions                                                             *)
	| FunctionCallExpandVarArg of expression * expression list * string

and imported_statements ={ mutable loaded: bool }

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
	| Instructions of string * string list * replacement_spec list * (string * int)
	| StatementBlock of statement list
	| Program of statement list (* like a statement block but does not push scope*)
	| Import of (string * imported_statements) * (string * int)
	| Switch of expression * statement list * (string * int)
	| Case of expression option * (string * int)
	| TryCatch of statement * string * statement * (string * int)
	| TryFinally of statement * statement * (string * int)
	| Throw of expression * (string * int)

let is_vararg varname =
	varname.[0]='['

let vararg_formalname varname =
	String.sub varname 1 ((String.length varname) - 1)
