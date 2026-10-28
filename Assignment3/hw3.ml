(* NOTE: Uncomment the following line if you want to #use this file in utop
 * (optionally, call it from utop directly): *)
(* #mod_use "hw3types.ml";; *)


(* NOTE: to get rid off the red-wiggles in VSCode, first compile the
 * the hw3types module running this 
 * from the command line: 
       ocamlopt hw3types.ml
 *)
open Hw3types

(**** Implement the following functions, remembering the "important note on function bindings" in the assignment write-up ****)

(* #1 *)
let only_lowercase strs =
  List.filter
    (fun str -> str.[0] >= 'a' && str.[0] <= 'z')
    strs

(* #2 *)
let longest_string1 =
  List.fold_left
    (fun best current ->
      if String.length current > String.length best
      then current
      else best)
    ""

(* #3 *)
let longest_string2 =
  List.fold_left
    (fun best current ->
      if String.length current >= String.length best
      then current
      else best)
    ""

(* #4 *)
let longest_string_helper f =
  List.fold_left
    (fun best current ->
      if f (String.length current) (String.length best)
      then current
      else best)
    ""

let longest_string3 =
  longest_string_helper (>)

let longest_string4 =
    longest_string_helper (>=)
  
(* #5 *)
let longest_lowercase =
  longest_string1 % only_lowercase

(* #6 *)
let caps_no_X_string =
  String.concat "" % String.split_on_char 'X' % String.uppercase_ascii


(* #7 *)
let rec first_answer f xs = 
  match xs with
  | [] -> raise NoAnswer
  | x :: rest ->
    match f x with
    | Some value -> value
    | None -> first_answer f rest

(* #8 *)
let all_answers f xs = 
  let rec helper remaining accumulated =
    match remaining with
    | [] -> Some accumulated
    | x :: rest ->
      match f x with
      | None -> None
      | Some values -> helper rest (accumulated @ values)
  in
  helper xs []

(* #9 *)
(* the arguments that g takes is f1 and f2 (two functions) and a pattern.  g computes a number based on a pattern*)

let count_wildcards =
  g (fun () -> 1) (fun x -> 0)

let count_wild_and_variable_lengths =
  g (fun () -> 1) (fun x -> String.length x)

let count_a_var s = 
  g (fun () -> 0) (fun x -> if x = s then 1 else 0)

(* #10 *)
let rec get_vars pat =
  match pat with
  | WildcardP -> []
  | VariableP x -> [x]
  | UnitP -> []
  | ConstantP _ -> []
  | ConstructorP (_, p) -> get_vars p
  | TupleP ps ->
    List.fold_left (fun acc p -> acc @ get_vars p) [] ps

let rec no_repeats strs =
  match strs with
  | [] -> true
  | x :: xs ->
    if List.mem x xs then false else no_repeats xs

let check_pat pat = 
  (no_repeats % get_vars) pat

(* #11 *)
let rec matches v pat = 
  match (v, pat) with
  | (_, WildcardP) -> Some []
  | (v, VariableP s) -> Some [(s, v)]
  | (Unit, UnitP) -> Some []
  | (Constant i, ConstantP j) ->
    if i = j then Some [] else None
  | (Constructor (s1, v), ConstructorP (s2, p)) ->
    if s1 = s2 then matches v p else None
  | (Tuple vs, TupleP ps) ->
    if List.length vs = List.length ps
    then
      all_answers
        (fun (v, p) -> matches v p)
        (List.combine vs ps)
    else None
  | _ -> None

(* #12 *)
let first_match v pats = 
  try Some (first_answer (matches v) pats)
  with NoAnswer -> None

(* optional challenge problem  *)

let typecheck_patterns cons pats = 
  failwith "Need to implement typecheck_patterns"
