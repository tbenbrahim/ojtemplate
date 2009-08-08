(**
This module defines runtime errors that are reported to the user

@author Tony BenBrahim < tony.benbrahim at gmail.com >
*)
(*
This program is free software; you can redistribute it and / or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.
*)

open Ast

(** this error represents an unexpected condition, caused by a programming
error in the interpreter implementation *)
exception InternalError of string

(** this error is a generic error thrown by library routines to indicate an
error condition, such as the incorrect type of a passed in argument *)
exception LibraryError of string

(** this error is caused by an abnormal error caused by the lexer, such as an
unterminated string **)
exception LexerException of string * int * int

(** marker exception to note that the program should exit, error has already
been reported during analysis *)
exception FatalExit

(** indicates that an assignment was attempted on two incompatible types*)
exception EIncompatibleTypes of string * string (* type1, type2 *)

(** indicates that the value is not of the expected type *)
exception EInvalidCast of string * string (* value type name, typename *)

(** indicates that an invalid operation was attempted on the specified types *)
exception EInvalidOperation of string * string  (* operator, typename *)

(** indicates that an invalid comparaison was attempted on the given types *)
exception EInvalidComparaison of string * string * string (* comparator, typename *)

(** indicates that a member expression is not applied to a map *)
exception ELeftSideIsNotAMap of string * string (* typename value *)

(** indicates that a member expression is not applied to a map *)
exception ELeftSideIsNotAMap of string * string (* typename value *)

(** indicates an attempt at an assignment to something that is a not a variable or map *)
exception ELeftSideCannotBeAssigned

(** indicates that the map member did not evaluate to a string or integer *)
exception EInvalidMember of string * string (* typename,value *)

(** indicates that a reference was made to a map member that does not exist *)
exception EUndefinedMapMember of string (* value *)

(** indicates a non integer array index *)
exception EInvalidArrayIndex of string * string (* type value *)

(** indicates an out of bounds index *)
exception EArrayIndexOutOfBounds of string (*index*)

(** indicates that the type in the assignment does not match the declare type *)
exception ETypeMismatchInAssignment of string * string * string (* name oldtype new type *)

(** indicates that an incorrect number of arguments were passed to a function *)
exception EMismatchedFunctionArgs of int * int (* expected actual *)

(** indicates an attempt to apply a function to a non function *)
exception ENotAFunction

(** indicates applying for each on a non collection type *)
exception ENotACollectionType of string * string (* message, value *)

(** indicates that the default case should be last *)
exception EDefaultCaseShouldBeLast

(** indicates a parsing error *)
exception ParseException of string

(**
Returns an error message for an exception
@param ex exception
@return error message
*)
let string_of_error ex =
	match ex with
	| InternalError msg -> "INT-00 internal error, interpreter is in inconsistent state: "^msg
	| LibraryError msg -> "LIB-00 library error: "^msg
	| LexerException (msg, line, col) -> "PRS-00 parsing error: " ^ msg ^ " at line " ^
			(string_of_int line) ^ " column " ^ (string_of_int col)
	| ParseException msg -> "PRS-01 parsing error: " ^ msg
	| EIncompatibleTypes(type1, type2) -> "EXP-00 incompatible types " ^ type1 ^ " and " ^ type2
	| EInvalidCast(type1, type2) ->"EXP-01 cannot cast a " ^ type1 ^ " to a " ^ type2
	| EInvalidOperation(operator, type1) -> "EXP-02 invalid operation " ^ operator ^ " for " ^ type1 ^ "s"
	| EInvalidComparaison(operator, type1, type2) -> "EXP-03 invalid comparaison " ^ operator ^ " for " ^
			type1 ^ " and " ^ type2
	| ELeftSideIsNotAMap (typename, value) -> "EXP-04 left side of member expression is not a map or array, but a " ^
			typename ^ " with value " ^ value
	| ELeftSideCannotBeAssigned -> "EXP-05 left side of assignment expression cannot be assigned"
	| EInvalidMember (typename, value) -> "EXP-06 member expression did not evaluate to a string or integer, but to a " ^
			typename ^ " with value " ^ value
	| EUndefinedMapMember(name) -> "EXP-07 member expression " ^ name ^ " is undefined"
	| EInvalidArrayIndex(typename, value) -> "EXP-08 invalid array index of type " ^ typename ^ " with value " ^ value
	| EArrayIndexOutOfBounds(value) -> "EXP-09 array index out of bounds: " ^ value
	| ETypeMismatchInAssignment(name, shouldbe, is) -> "EXP-10 type mismatch in assignment of " ^ name ^
			" declared as " ^ shouldbe ^ ", attempting to assign "^is
	| EMismatchedFunctionArgs(expected, actual) -> "EXP-11 wrong number of arguments in function call, expected " ^
			(string_of_int expected) ^ ", got " ^ (string_of_int actual)
	| ENotAFunction -> "EXP-12 invalid function call on a non-function variable"
	| ENotACollectionType (msg, typename) -> "EXP-13 expected a collection type for " ^ msg ^ ", but got a " ^
			typename
	| Division_by_zero -> "EXP-14 Division by zero"
	| EDefaultCaseShouldBeLast -> "STM-00 the default case in a switch statement should be the last case"
	| CFReturn _ -> "STM-01 unexpected return statement outside of a function definition"
	| CFBreak -> "STM-02 unexpected break statement outside of a loop"
	| CFContinue -> "STM-03 unexpected continue statement outside of a loop"
	| CFUserException (_, value) -> "USR-00 unhandled user exception " ^ value
	| Parsing.Parse_error -> ""
	| e -> "uncaught exception "^(Printexc.to_string e)

(**
Displays an error to stdout
@param err exception
@param cloc tuple of file, line where error occured
*)
let display_error err cloc =
	let (file, line) = cloc
	in match string_of_error err with
	| "" -> ()
	| msg -> print_string ("\nAt line " ^ (string_of_int line) ^ " in file " ^ file ^ ":\n\t" ^
					msg ^ "\n" )