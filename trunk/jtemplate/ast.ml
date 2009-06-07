open Stringmap

type operator = | Plus | Minus | Times | Divide | Modulo

type comparator =
	| LessThan
	| LessThanEqual
	| Equal
	| GreaterThanEqual
	| GreaterThan
	| NotEqual

(* type value_type= StringType | IntegerType | FloatType | BooleanType |   *)
(* FunctionType | ArrayType | MapType                                      *)
type label = | Label of string

type text = string 

type replacement =
	(text * expression)

and replacement_list =
	replacement list

and conditional_spec =
	| Once
	| When of expression
	| Loop of variable_name * expression
	| CondLoop of expression * variable_name * expression

and replacement_spec =
	(label * conditional_spec * replacement_list)

and template_spec =
	((label option) * text)

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
	| FunctionValue of variable_name list * statement list * symbol_table option
	| MapValue of variable_value StringMap.t
	| Void
	| NaN
and
(** Definition for a symbol table. *)
symbol_table ={
	values: variable_value StringMap.t; (* variable values for this scope *)
	parent_table: symbol_table option
}

and variable_name =
	| Name of string | CompoundName of string list

and expression =
	| BinaryOp of expression * operator * expression
	| CompOp of expression * comparator * expression
	| FunctionCall of variable_name * expression list
	| MapExpr of (string * expression) list
	| ArrayExpr of expression list
	| VariableExpr of variable_name
	| Value of variable_value

and statement =
	| Assignment of variable_name * expression
	| Declaration of variable_name * expression
	| ForEach of variable_name * expression * statement list
	| For of statement * statement * statement * statement list
	| ExpressionStatement of expression
	| Break
	| Continue
	| Noop
	| Return of expression
	| If of expression * statement list * statement list
	| TemplateDef of variable_name * template_spec list
	| Instructions of variable_name * variable_name list
	* replacement_spec list
