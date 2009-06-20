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
*)
exception MismatchedCallArgs

exception TypeMismatchInMapAssignment of string * string * string * string

(** This exception indicates that a vararg is not last in function definition
@param field1 the name of the function
*)
exception VarArgsMustbeLast 

exception LibraryError of string

exception InvalidArrayIndex of string * string (* type value *)
exception ArrayIndexOutOfBounds of string (*index*)

(** indicates that a collection type was expected but another
type was seen instead
@param field1 the location of the error
@param field2 the name of the offending type
*)
exception NotACollectionType of string * string

exception LexerException of string * int * int

exception InternalError of string

exception DefaultCaseShouldBeLast

exception UnexpectedUnboundVar of string (*name*)

exception FatalExit of exn

exception UserException of variable_value

exception InvalidMember of string * string (* typename,value *)
exception UndefinedMapMember of string (* value *)
exception LeftSideIsNotAMap of string * string (* typename value *)
exception LeftSideCannotBeAssigned
exception NotAFunction
exception NotAnInteger of string

let string_of_error ex =
	match ex with
	| ReferenceToUndefinedVariable(varname) -> "Reference to undefined variable " ^ varname
	| ReferenceToUndefinedMapVariable(comp, varname) ->
			"Reference to undefined field " ^ comp ^ " variable in " ^ varname
	| NotAMap(comp, varname) -> comp ^ " is not a map in access of map " ^ varname
	| TypeMismatchInAssignment(name, old_type, new_type) ->
			"Cannot assign a "^new_type^" to variable "^name^" of type "^old_type
	| TypeMismatchInMapAssignment(comp, name, old_type, new_type) ->
			"Cannot assign a "^new_type^" to in "^comp^" of type "^old_type^" in map "^name
	| MismatchedCallArgs ->
			"The number of arguments do not match the number of formal parameters in function call "
	| VarArgsMustbeLast->
			"In the definition of a function, a vararg parameter must be the last formal parameter"
	| LibraryError msg | InternalError msg | NotAnInteger msg -> msg
	| InvalidArrayIndex(indtype, value) -> "Invalid array index of type "^indtype^" with value "^value
	| ArrayIndexOutOfBounds index -> "Array index out of bounds ("^index^")"
	| NotACollectionType(where, typename) -> "Expected a collection type for "^
			where^", received a "^typename^" instead"
	| DefaultCaseShouldBeLast -> "Unexpected case statement found after a default statement"
	| UnexpectedUnboundVar name -> "Unexpected unbound variable "^name^
			". Unbound variables can only be used in function calls."
	| UserException(value) -> "Unhandled user exception"
	| InvalidMember(typename, value) -> "Invalid member of type "^typename^" with value "^value
	| UndefinedMapMember(value) -> "Undefined map member "^value
	| LeftSideIsNotAMap(typename,value) -> "Expected a collection type but found type "^typename^" with value "^value
	| LeftSideCannotBeAssigned -> "Left side cannot be assigned"
	| e -> "uncaught exception "^(Printexc.to_string e)

