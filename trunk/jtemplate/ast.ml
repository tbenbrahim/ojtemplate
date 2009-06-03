
type operator = Plus | Minus | Times | Divide | Modulo

type comparator = LessThan | LessThanEqual | Equal | GreaterThanEqual | GreaterThan | NotEqual

(*type value_type= StringType | IntegerType | FloatType | BooleanType | FunctionType | ArrayType | MapType*) 

type label=
	Label of string
type text=string

type replacement=text * expression
and replacement_list= replacement list 
and conditional_spec=
		Once
	| When of expression
	| Loop of variable*expression
and replacement_spec=label*conditional_spec*replacement_list
and template_spec=label option*text 
and value = 
		Integer of int
	| Float of float
	| String of string
	| Boolean of bool
	| Function of variable list*statement list
	| Array of expression list
	| Map of (string*expression) list
	| Expression of expression
	| Variable of variable
and variable=  
	  Name of string
	|	CompoundName of string list
and expression =
		BinaryOp of expression*operator*expression
	| CompOp of expression*comparator*expression
	| FunctionCall of variable*expression list
	| Value of value
and statement =
	  StatementBlock of statement list
	| Assignment of variable*expression
	| Declaration of variable*expression
	| ForEach of variable*expression*statement
	| While of expression*statement
	| ExpressionStatement of expression
	| Break
	| Continue
	| Return of expression
	| If of expression*statement*statement
	| TemplateDef of variable*(template_spec list)
	| Instructions of variable*(variable list)*(replacement_spec list)