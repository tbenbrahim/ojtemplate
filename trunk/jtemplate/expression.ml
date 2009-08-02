(**
This program is free software; you can redistribute it and / or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

Evaluation of binary operations and comparaison of values
Various helper functions for expression evaluation

@author Tony BenBrahim < tony.benbrahim at gmail.com >

*)

open Ast

(**
Converts a MapValue array to a list of values
@param arr array
@return a list of values
*)
let list_of_array arr =
	match arr with
	| RMapValue(h, ArraySubtype) -> (
				match Hashtbl.find h "length" with
				| RIntegerValue(0) ->[]
				| RIntegerValue(len) ->
						let rec loop lst ind =
							(let lst = (Hashtbl.find h (string_of_int ind)):: lst in
								if ind = 0 then lst else (loop lst (ind - 1)))
						in loop [] (len - 1)
				| _ -> raise (RuntimeError.InternalError "inconsistent array/length not found")
			)
	| _ -> raise (RuntimeError.InternalError "inconsistent array/not an array")

(**
Converts a value to a string
@param value the value to convert
@return a string representing the value
*)
let rec string_of_value = function
	| RStringValue(s) -> s
	| RIntegerValue(i) -> string_of_int i
	| RFloatValue(f) -> string_of_float f
	| RBooleanValue(b) -> string_of_bool b
	| RMapValue(t, ArraySubtype) as v ->
			let lst = list_of_array v
			in let rec loop s = function
				| [] -> s^"]"
				| v::[] -> loop (s^(string_of_value v)) []
				| v:: tl -> loop (s^(string_of_value v)^", ") tl
			in loop "[" lst
	| RMapValue(t, MapSubtype) ->
			(Hashtbl.fold (fun prop v s ->
								s^prop^": "^(string_of_value v)^";") t "{")^"}"
	| RFunctionValue(_, _, _, _, _, _)	| RLibraryFunction(_) -> "function"
	| RVoid -> "void"
	| RNaN -> "NaN"
	| RUndefined -> "undefined"

(**
enumeration of a value's possible types
*)
type valuetype =
	| IntegerType
	| FloatType
	| BooleanType
	| StringType
	| FunctionType
	| LibraryCallType
	| MapType
	| ArrayType
	| VoidType
	| NaNType
	| UndefinedType

(**
Returns a value's type
@param a value
@return the value's type
*)
let value_type = function
	| RIntegerValue(_) -> IntegerType
	| RFloatValue(_) -> FloatType
	| RBooleanValue(_) -> BooleanType
	| RStringValue(_) -> StringType
	| RFunctionValue(_, _, _, _, _, _) -> FunctionType
	| RLibraryFunction(_) -> LibraryCallType
	| RMapValue(_, MapSubtype) -> MapType
	| RMapValue(_, ArraySubtype _) -> ArrayType
	| RVoid -> VoidType
	| RNaN -> NaNType
	| RUndefined -> UndefinedType

(**
returns a string name for a value's type
@param value a value
@return string name for the value's type
*)
let string_of_value_type = function
	| RStringValue(s) -> "string"
	| RIntegerValue(i) -> "integer"
	| RFloatValue(f) -> "float"
	| RBooleanValue(b) -> "boolean"
	| RMapValue(_, ArraySubtype) ->"map"
	| RMapValue(_, MapSubtype) -> "array"
	| RFunctionValue(_, _, _, _, _, _)	| RLibraryFunction(_) -> "function"
	| RVoid -> "void"
	| RNaN -> "NaN"
	| RUndefined -> "undefined"

exception EIncompatibleTypes of string * string (* type1, type2 *)

exception EInvalidCast of string * string (* value type name, typename *)

exception EInvalidOperation of Ast.operator * string  (* operator, typename *)

exception EInvalidComparaison of Ast.comparator * string * string (* comparator, typename *)

exception ELeftSideIsNotAMap of string * string (* typename value *)

exception ELeftSideCannotBeAssigned

exception EInvalidMember of string * string (* typename,value *)

exception EUndefinedMapMember of string (* value *)

exception EInvalidArrayIndex of string * string (* type value *)

exception EArrayIndexOutOfBounds of string (*index*)

exception ETypeMismatchInAssignment of string * string * string (* name oldtype new type *)

exception EMismatchedFunctionArgs of int * int (* expected actual *)

exception ENotAFunction

exception ENotACollectionType of string * string (* message, value *)

exception EDefaultCaseShouldBeLast

(** type to hold the result of casting two values to the same type *)
type cast_type =
	| IntegerCast of int * int
	| FloatCast of float * float
	| StringCast of string * string
	| BoolCast of bool * bool

(**
cast to boolean
@param value the value to cast to a boolean
@return a boolean
@throws EInvalidCast in value is not a boolean
*)
let cast_to_bool value =
	match value with
	| RBooleanValue(b) -> b
	| _ -> raise (EInvalidCast (string_of_value_type value,"boolean"))


let cast_to_integer value =
	match value with
	| RIntegerValue(i) -> i
	| _ -> raise (EInvalidCast (string_of_value_type value,"integer"))

let cast_to_float value =
	match value with
	| RFloatValue(f) -> f
	| RIntegerValue(i) -> float_of_int i
	| _ -> raise (EInvalidCast (string_of_value_type value,"float"))


(**
Evaluate the operation
@param value1 the first value
@param value2 the second value
@param operator the operator
@return the value that results from the operation
*)
let evaluate_op value1 value2 operator =
	let string_op s1 s2 =
		(match operator with
			| Plus -> RStringValue(s1 ^ s2)
			| _ -> raise (EInvalidOperation (operator,"string"))
		)
	in let float_op f1 f2 = (let f = (match operator with
					| Plus -> f1 +. f2
					| Minus -> f1 -. f2
					| Times -> f1 *. f2
					| Divide -> f1 /. f2
					| _ -> raise (EInvalidOperation (operator,"float"))) in
			if f = infinity || f = neg_infinity || f = nan then RNaN
			else RFloatValue(f)
		)
	in match (value1, value2) with
	| (RIntegerValue(i1), RIntegerValue(i2)) ->
			(match operator with
				| Plus -> RIntegerValue( i1 + i2 )
				| Minus -> RIntegerValue( i1 - i2)
				| Times -> RIntegerValue( i1 * i2)
				| Divide -> if i2 <> 0 then RIntegerValue( i1 / i2) else RNaN
				| Modulo -> if i2 <> 0 then RIntegerValue( i1 mod i2) else RNaN
				| _ -> raise (EInvalidOperation (operator,"integer"))
			)
	| (RBooleanValue(b1), RBooleanValue(b2)) ->
			(match operator with
				| And -> RBooleanValue(b1 && b2)
				| Or -> RBooleanValue(b1 || b2)
				| _ -> raise (EInvalidOperation (operator,"boolean"))
			)
	| (RStringValue(s1), RStringValue(s2)) -> string_op s1 s2
	| (RStringValue(s1), v2) -> string_op s1 (string_of_value v2)
	| (v1, RStringValue(s2)) -> string_op (string_of_value v1) s2
	| (RFloatValue(f1), RFloatValue(f2)) -> float_op f1 f2
	| (RFloatValue(f1), RIntegerValue(i2)) -> float_op f1 (float_of_int i2)
	| (RIntegerValue(i1), RFloatValue(f2)) -> float_op (float_of_int i1) f2
	| (value1, value2) -> raise (EIncompatibleTypes(string_of_value_type value1, string_of_value_type value2))

(**
Implements comparaison of two values, according to the following semantics:

Value 1 type Value 2 type Operator Result
-------------- -------------- ----------- -----------------------------------------------------------------------
Integer Integer Any Comparison of integer values
Float Float Any Comparison of float values
Float Integer Any Comparison of float values
String any type Float comparison of first value to second value,
Integer with non string values converted to strings
String
Both types are Booleans,
maps, arrays, functions,
NaN or void == and != comparison of first value to second value,
observing the semantics of equality described below.
Different types not listed
above == and != == always returns false != always returns true

@param v1 the first value to compare
@param op the comparaison operator
@param v2 the second value to compare
@return a boolean value type
*)
let rec compare v1 op v2 =
	match v1 with
	| RIntegerValue(i1) ->
			(match v2 with
				| RIntegerValue(i2) ->
						(match op with
							| Equal -> RBooleanValue(i1 = i2)
							| NotEqual -> RBooleanValue(i1 <> i2)
							| LessThan -> RBooleanValue(i1 < i2)
							| LessThanEqual -> RBooleanValue(i1 <= i2)
							| GreaterThan -> RBooleanValue(i1 > i2)
							| GreaterThanEqual -> RBooleanValue(i1 >= i2) )
				| RFloatValue(f2) -> compare (RFloatValue (float_of_int i1)) op v2
				| RStringValue(s2) -> compare (RStringValue (string_of_int i1)) op v2
				| _ -> mismatched_compare v1 op v2 )
	| RFloatValue(f1) ->
			(match v2 with
				| RFloatValue(f2) ->
						(match op with
							| Equal -> RBooleanValue(f1 = f2)
							| NotEqual -> RBooleanValue(f1 <> f2)
							| LessThan -> RBooleanValue(f1 < f2)
							| LessThanEqual -> RBooleanValue(f1 <= f2)
							| GreaterThan -> RBooleanValue(f1 > f2)
							| GreaterThanEqual -> RBooleanValue(f1 >= f2) )
				| RIntegerValue(i2) -> compare v1 op (RFloatValue (float_of_int i2))
				| RStringValue(s2) -> compare (RStringValue(string_of_float f1)) op v2
				| _ -> mismatched_compare v1 op v2 )
	| RStringValue(s1) ->
			(match v2 with
				| RStringValue(s2) ->
						(match op with
							| Equal -> RBooleanValue(s1 = s2)
							| NotEqual -> RBooleanValue(s1 <> s2)
							| LessThan -> RBooleanValue(s1 < s2)
							| LessThanEqual -> RBooleanValue(s1 <= s2)
							| GreaterThan -> RBooleanValue(s1 > s2)
							| GreaterThanEqual -> RBooleanValue(s1 >= s2) )
				| RIntegerValue(i2) -> compare v1 op (RStringValue(string_of_int i2))
				| RFloatValue(f2) -> compare v1 op (RStringValue(string_of_float f2))
				| _ -> mismatched_compare v1 op v2 )
	| RBooleanValue(b1) ->
			(match v2 with
				| RBooleanValue(b2) -> (
							match op with
							| Equal -> RBooleanValue(b1 = b2)
							| NotEqual -> RBooleanValue(b1 <> b2)
							| _ -> raise (EInvalidComparaison(op, string_of_value_type v1, string_of_value_type v2)) )
				| _ -> mismatched_compare v1 op v2 )
	| RVoid ->
			(match v2 with
				| RVoid -> (
							match op with
							| Equal -> RBooleanValue(true)
							| NotEqual -> RBooleanValue(false)
							| _ -> raise (EInvalidComparaison(op, string_of_value_type v1, string_of_value_type v2)) )
				| _ -> mismatched_compare v1 op v2 )
	| RNaN ->
			(match v2 with
				| RNaN -> (
							match op with
							| Equal -> RBooleanValue(true)
							| NotEqual -> RBooleanValue(false)
							| _ -> raise (EInvalidComparaison(op,
												string_of_value_type v1,
												string_of_value_type v2)) )
				| _ -> mismatched_compare v1 op v2 )
	| RMapValue(h1, ArraySubtype) ->
			(match v2 with
				| RMapValue(h2, ArraySubtype) -> (
							match op with
							| Equal -> RBooleanValue(hashtbl_equal h1 h2)
							| NotEqual -> RBooleanValue(not (hashtbl_equal h1 h2))
							| _ -> raise (EInvalidComparaison(op,
												string_of_value_type v1,
												string_of_value_type v2)) )
				| _ -> mismatched_compare v1 op v2 )
	| RMapValue(h1, MapSubtype) ->
			(match v2 with
				| RMapValue(h2, MapSubtype) -> (
							match op with
							| Equal -> RBooleanValue(hashtbl_equal h1 h2)
							| NotEqual -> RBooleanValue(not (hashtbl_equal h1 h2))
							| _ -> raise (EInvalidComparaison(op,
												string_of_value_type v1,
												string_of_value_type v2)) )
				| _ -> mismatched_compare v1 op v2 )
	| RFunctionValue(size1, depth1, len1, varargs1, stmts1, clos1) ->
			(match v2 with
				| RFunctionValue(size2, depth2, len2, varargs2, stmts2, clos2) -> (
							match op with
							| Equal -> RBooleanValue(size1 = size2 && stmts1 = stmts2)
							| NotEqual -> RBooleanValue(not (size1 = size2 && stmts1 = stmts2))
							| _ -> raise (EInvalidComparaison(op,
												string_of_value_type v1,
												string_of_value_type v2)) )
				| _ -> mismatched_compare v1 op v2 )
	| RLibraryFunction(def1) ->
			(match v2 with
				| RLibraryFunction(def2) ->
						( match op with
							| Equal -> RBooleanValue(def1 == def2)
							| NotEqual -> RBooleanValue(not (def1 == def2))
							| _ -> raise (EInvalidComparaison(op,
												string_of_value_type v1,
												string_of_value_type v2)) )
				| _ -> mismatched_compare v1 op v2 )
	| RUndefined -> raise (RuntimeError.InternalError "unexpected value in compare")
and hashtbl_equal h1 h2 =
	(Hashtbl.length h1) = (Hashtbl.length h2) &&
	try
		Hashtbl.fold (fun k v init -> init && (compare (Hashtbl.find h2 k) Equal v) = RBooleanValue(true) ) h1 true
	with
	| Not_found -> false
and mismatched_compare v1 op v2 =
	match op with
	| Equal -> RBooleanValue(false)
	| NotEqual -> RBooleanValue(true)
	| _ -> raise (EInvalidComparaison(op, string_of_value_type v1, string_of_value_type v2))
(**
Makes a stack frame from the supplied value list
@param size size of stack frame
@param vararg true if the last argument is a vararg, false otherwise
@param value_list list of values to add to the stack frame
@param this the value of this
@return a stack frame (an array of values)
*)
and make_stackframe size numargs vararg value_list this =
	let stackframe = Array.make (size + 1) RUndefined
	in let rec loop_single_values = function
		| (0, _, rest) -> rest
		| (num_left, ind, value:: rest) ->
				stackframe.(ind) <- value;
				loop_single_values (num_left - 1, ind + 1, rest)
		| (num_left, ind,[]) -> []
	in let rest = loop_single_values ((if vararg then numargs - 1 else numargs), 1, value_list)
	in ((match (rest, vararg) with
			| (list, true) ->	stackframe.(numargs) <- array_of_value_list(list)
			| ([] , false) -> ()
			| (_, false) ->
					raise (EMismatchedFunctionArgs (size, List.length value_list))));
	stackframe.(0) <- this;
	stackframe
(**
Creates an Array from a list of values
@param value_list a list of values
@return a MapValue with the array
*)
and array_of_value_list value_list =
	let rec loop = function
		| (_,[], h) -> h
		| (ind, value:: rest, h) ->
				Hashtbl.replace h (string_of_int ind) value;
				loop (ind + 1, rest, h)
	in let length = List.length value_list
	in let h = Hashtbl.create (length + 1)
	in Hashtbl.replace h "length" (RIntegerValue(length));
	RMapValue(loop (0, value_list, h), ArraySubtype)