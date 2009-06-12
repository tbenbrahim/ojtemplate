(**
this module defines runtime errors that are reported to the user
@author tbenbrahim
*)

open Ast

(** This exception indicates that an access was made to an undefined variable
@param field1 the variable name
*)
exception ReferenceToUndefinedVariable of string

(** This exception indicates that an access was made to an undefined map field
@param field1 the field name
@param field2 the variable name
*)
exception ReferenceToUndefinedMapVariable of string * string

(** This exception indicates that a field that was accessed as a map is not a map
@param field1 the field name
@param field2 the variable name
*)
exception NotAMap of string * string

(** This exception indicates that an assignment that would change a variable's type
was attempted
@param field1 the variable name
@param field2 the type of the variable
@param field3 the type of the value that was to replace the value
*)
exception TypeMismatchInAssignment of string * string * string

(** This exception indicates that an assignment that would change a map field's type
was attempted
@param field1 the field name
@param field2 the variable name
@param field3 the type of the variable
@param field4 the type of the value that was to replace the value
*)
exception TypeMismatchInMapAssignment of string * string * string * string

(** This exception indicates that the number of arguments passed in
to a function call do not match with number of arguments in the formal arguments
@param field1 the name of the function
@param field2 the number of formal arguments
@param field3 the number of arguments passed in during the call
*)
exception MismatchedCallArgs of string * int * int

(** Generic library call error *)
exception LibraryError of string

let string_of_error ex =
	match ex with
	| ReferenceToUndefinedVariable(varname) -> "Reference to undefined variable " ^ varname
	| ReferenceToUndefinedMapVariable(comp, varname) ->
			"Reference to undefined field " ^ comp ^ "variable " ^ varname
	| NotAMap(comp, varname) -> comp ^ " is not a map in access of map " ^ varname
	| TypeMismatchInAssignment(name, old_type, new_type) ->
			"Cannot assign a "^new_type^" to variable "^name^" of type "^old_type
	| TypeMismatchInMapAssignment(comp, name, old_type, new_type) ->
			"Cannot assign a "^new_type^" to in "^comp^" of type "^old_type^" in map "^name
	| MismatchedCallArgs(name, num_formal, num_call) ->
			"In function call to " ^ name ^ ", expected " ^ (string_of_int num_formal) ^
			" arguments, got "^ (string_of_int num_call) ^ " instead."
	| LibraryError msg -> msg
	| e -> "uncaught exception "^(Printexc.to_string e)

