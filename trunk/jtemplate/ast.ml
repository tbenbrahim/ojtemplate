type operator = | Plus | Minus | Times | Divide | Modulo

type comparator =
  | LessThan
  | LessThanEqual
  | Equal
  | GreaterThanEqual
  | GreaterThan
  | NotEqual

(*type value_type= StringType | IntegerType | FloatType | BooleanType | FunctionType | ArrayType | MapType*)
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

  and value =
  | Integer of int
  | Float of float
  | String of string
  | Boolean of bool
  | Function of variable_name list * statement list
  | Array of expression list
  | Map of (string * expression) list
  | Expression of expression
  | Variable of variable_name
	| Void
	| NaN

  and variable_name =
  | Name of string | CompoundName of string list

  and expression =
  | BinaryOp of expression * operator * expression
  | CompOp of expression * comparator * expression
  | FunctionCall of variable_name * expression list
  | Value of value

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
