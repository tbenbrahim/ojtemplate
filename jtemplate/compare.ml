open Ast
open Symbol_table
open RuntimeError

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

@author Abed Tony Benbrahim

*)

let rec compare v1 op v2 =
  match v1 with
  | IntegerValue(i1) ->
      (match v2 with
        | IntegerValue(i2) ->
            (match op with
              | Equal -> BooleanValue(i1 = i2)
              | NotEqual -> BooleanValue(i1 <> i2)
              | LessThan -> BooleanValue(i1 < i2)
              | LessThanEqual -> BooleanValue(i1 <= i2)
              | GreaterThan -> BooleanValue(i1 > i2)
              | GreaterThanEqual -> BooleanValue(i1 >= i2)
            )
        | FloatValue(f2) -> compare (FloatValue (float_of_int i1)) op v2
        | StringValue(s2) -> compare (StringValue (string_of_int i1)) op v2
        | _ -> mismatched_compare v1 op v2
      )
  | FloatValue(f1) ->
      (match v2 with
        | FloatValue(f2) ->
            (match op with
              | Equal -> BooleanValue(f1 = f2)
              | NotEqual -> BooleanValue(f1 <> f2)
              | LessThan -> BooleanValue(f1 < f2)
              | LessThanEqual -> BooleanValue(f1 <= f2)
              | GreaterThan -> BooleanValue(f1 > f2)
              | GreaterThanEqual -> BooleanValue(f1 >= f2)
            )
        | IntegerValue(i2) -> compare v1 op (FloatValue (float_of_int i2))
        | StringValue(s2) -> compare (StringValue(string_of_float f1)) op v2
        | _ -> mismatched_compare v1 op v2
      )
  | StringValue(s1) ->
      (match v2 with
        | StringValue(s2) ->
            (match op with
              | Equal -> BooleanValue(s1 = s2)
              | NotEqual -> BooleanValue(s1 <> s2)
              | LessThan -> BooleanValue(s1 < s2)
              | LessThanEqual -> BooleanValue(s1 <= s2)
              | GreaterThan -> BooleanValue(s1 > s2)
              | GreaterThanEqual -> BooleanValue(s1 >= s2)
            )
        | IntegerValue(i2) -> compare v1 op (StringValue(string_of_int i2))
        | FloatValue(f2) -> compare v1 op (StringValue(string_of_float f2))
        | _ -> mismatched_compare v1 op v2
      )
  | BooleanValue(b1) ->
      (match v2 with
        | BooleanValue(b2) -> (
              match op with
              | Equal -> BooleanValue(b1 = b2)
              | NotEqual -> BooleanValue(b1 <> b2)
              | _ -> raise (InvalidComparaison(op,
                        SymbolTable.string_of_symbol_type v1,
                        SymbolTable.string_of_symbol_type v2))
            )
        | _ -> mismatched_compare v1 op v2
      )
  | Void ->
      (match v2 with
        | Void -> (
              match op with
              | Equal -> BooleanValue(true)
              | NotEqual -> BooleanValue(false)
              | _ -> raise (InvalidComparaison(op,
                        SymbolTable.string_of_symbol_type v1,
                        SymbolTable.string_of_symbol_type v2))
            )
        | _ -> mismatched_compare v1 op v2
      )
  | NaN ->
      (match v2 with
        | NaN -> (
              match op with
              | Equal -> BooleanValue(true)
              | NotEqual -> BooleanValue(false)
              | _ -> raise (InvalidComparaison(op,
                        SymbolTable.string_of_symbol_type v1,
                        SymbolTable.string_of_symbol_type v2))
            )
        | _ -> mismatched_compare v1 op v2
      )
  | MapValue(h1, ArraySubtype) ->
      (match v2 with
        | MapValue(h2, ArraySubtype) -> (
              match op with
              | Equal -> BooleanValue(hashtbl_equal h1 h2)
              | NotEqual -> BooleanValue(not (hashtbl_equal h1 h2))
              | _ -> raise (InvalidComparaison(op,
                        SymbolTable.string_of_symbol_type v1,
                        SymbolTable.string_of_symbol_type v2))
            )
        | _ -> mismatched_compare v1 op v2
      )
  | MapValue(h1, MapSubtype) ->
      (match v2 with
        | MapValue(h2, MapSubtype) -> (
              match op with
              | Equal -> BooleanValue(hashtbl_equal h1 h2)
              | NotEqual -> BooleanValue(not (hashtbl_equal h1 h2))
              | _ -> raise (InvalidComparaison(op,
                        SymbolTable.string_of_symbol_type v1,
                        SymbolTable.string_of_symbol_type v2))
            )
        | _ -> mismatched_compare v1 op v2
      )
  | FunctionValue(args1, stmts1) | ScopedFunctionValue(args1, stmts1, _) ->
      (match v2 with
        | FunctionValue(args2, stmts2) | ScopedFunctionValue(args2, stmts2, _) -> (
              match op with
              | Equal -> BooleanValue(args1 = args2 && stmts1 = stmts2)
              | NotEqual -> BooleanValue(not (args1 = args2 && stmts1 = stmts2))
              | _ -> raise (InvalidComparaison(op,
                        SymbolTable.string_of_symbol_type v1,
                        SymbolTable.string_of_symbol_type v2))
            )
        | _ -> mismatched_compare v1 op v2
      )
  | LibraryFunction(args1, stmts1, _) ->
      (match v2 with
        | LibraryFunction(args2, stmts2, _) ->
            ( match op with
              | Equal -> BooleanValue(args1 = args2 && stmts1 == stmts2)
              | NotEqual -> BooleanValue(not (args1 = args2 && stmts1 == stmts2))
              | _ -> raise (InvalidComparaison(op,
                        SymbolTable.string_of_symbol_type v1,
                        SymbolTable.string_of_symbol_type v2))
            )
        | _ -> mismatched_compare v1 op v2
      )
and hashtbl_equal h1 h2 =
  (Hashtbl.length h1) = (Hashtbl.length h2) &&
  try
    Hashtbl.fold (fun k v init -> init && (compare (Hashtbl.find h2 k) Equal v) = BooleanValue(true) ) h1 true
  with
  | Not_found -> false
and mismatched_compare v1 op v2 =
  match op with
  | Equal -> BooleanValue(false)
  | NotEqual -> BooleanValue(true)
  | _ -> raise (InvalidComparaison(op, SymbolTable.string_of_symbol_type v1, SymbolTable.string_of_symbol_type v2))

