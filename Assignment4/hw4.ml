(* CS 314, Fall 2026, Homework 4: Property-Based Testing *)

(* Keep the provided definitions, input recipes, and test names. Fill in the
   HW1/HW3 stubs and the required TODOs; see the handout for full requirements.
   From this directory, use dune build to compile and dune test to run tests.
   Use dune test --force to rerun unchanged tests. The runner is hw4test.ml. *)

(* Replace these three stubs with your HW1 functions and needed helpers. *)
let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let thd3 (_, _, z) = z

let is_older ((d1 : int * int * int), (d2 : int * int * int)) =
  if thd3 d1 < thd3 d2 then
		true
	else if thd3 d1 > thd3 d2 then
		false
	else if snd3 d1 < snd3 d2 then
		true
	else if snd3 d1 > snd3 d2 then
		false
	else
		fst3 d1 < fst3 d2

let cumulative_sum (numbers : int list) = 
	let rec helper numbers sum =
		if  numbers = [] then
			[]
		else 
			(sum + List.hd numbers) :: 
			helper (List.tl numbers)(sum + List.hd numbers)
	in 
	helper numbers 0

let rec oldest (dates : (int * int * int) list) = 
	if dates = [] then
		None
	else
		let rest_older = oldest(List.tl dates) in 
		if Option.is_none rest_older then
			Some (List.hd dates)
		else if is_older (List.hd dates, Option.get rest_older) then
			Some (List.hd dates)
		else 
			rest_older


  

open QCheck

(* Provided date input recipes; use them unchanged for the required work.
   Gen.(...) opens QCheck.Gen only inside the parentheses. Gen functions build
   generators; make packages a generator as an arbitrary (an input recipe for
   Test.make). ~print supplies a readable counterexample printer, not a shrinker.
   Dates are (day, month, year); days 1..28 are valid in every month. *)
let date_gen = Gen.(triple (int_range 1 28) (int_range 1 12) (int_range 1 2100))
let date_arb = make ~print:(fun (d,m,y) -> Printf.sprintf "(%d/%d/%d)" d m y) date_gen
(* Same-year pairs make the provided bug observable in a short run. *)
let date_pair_arb =
  let same_year = Gen.(map2
    (fun (d1,m1) (d2,m2) -> ((d1,m1,2000), (d2,m2,2000)))
    (pair (int_range 1 28) (int_range 1 12))
    (pair (int_range 1 28) (int_range 1 12))) in
  make ~print:(fun ((d1,m1,y1),(d2,m2,y2)) ->
      Printf.sprintf "((%d,%d,%d),(%d,%d,%d))" d1 m1 y1 d2 m2 y2)
    Gen.(oneof [pair date_gen date_gen; same_year])

(* Problem 1: cumulative_sum postconditions. *)

(* Reading the test skeletons:
   Test.make combines an input recipe with a Boolean-valued property; the
   supplied runner executes it. ~name labels the output and ~count:200 requests
   200 random checks, which may repeat inputs. A failure can stop a test early.
   Replace each fun _ -> false TODO with a function that names or destructures
   ONE generated input and checks the stated requirement. true passes that case;
   false reports a counterexample. The initial false bodies deliberately fail.
   A pair recipe supplies one tuple, so use fun (x, y) -> ..., not fun x y -> ... . *)

(* Input xs : int list. Check that cumulative_sum preserves the input length. *)
let test_cumsum_length =
  Test.make ~name:"cumulative_sum: length" ~count:200
    (list small_int)
    (fun xs ->  List.length (cumulative_sum xs) = List.length xs)

(* Input xs : int list. For nonempty xs, require a nonempty result whose last
   element is the sum of the input numbers. Handle [] separately: this property
   imposes no last-element requirement for it. Do not take the last element of
   an empty list; an empty result on a nonempty input should fail the property. *)
let test_cumsum_last =
  Test.make ~name:"cumulative_sum: last element" ~count:200
    (list small_int)
    (fun xs -> 
      match xs with
      | [] -> true
      | _ ->
          match cumulative_sum xs with
          | [] -> false
          | result ->
              List.hd (List.rev result) =
              List.fold_left (+) 0 xs)

(* Problem 2: target the provided oldest_buggy without repairing its code.
   A sound property should find a counterexample here, so this test is EXPECTED
   to report a failure after you complete it. It must also pass for correct
   oldest: temporarily target oldest to check this, then restore oldest_buggy
   before submitting. Merely demanding disagreement with oldest is not a
   property of correct oldest-date behavior. *)

let rec oldest_buggy (dates : (int * int * int) list) =
  match dates with
  | [] -> None
  | [d] -> Some d
  | d :: rest ->
    (match oldest_buggy rest with
     | None -> Some d
     | Some best ->
       let (dd, dm, dy) = d in
       let (bd, bm, by) = best in
       if dy < by
          || (dy = by && dd < bd)
          || (dy = by && dd = bd && dm < bm)
       then Some d
       else Some best)

(* Input (d1, d2): each component is a whole (day, month, year) date.
   Apply oldest_buggy to the list containing these two dates and use is_older
   to check the oldest-date behavior described in the handout. *)
let test_oldest =
  Test.make ~name:"oldest: bug hunt" ~count:200
    (date_pair_arb)
    (fun (d1, d2) ->
      oldest_buggy [d1; d2] =
      (if is_older (d1, d2) then Some d1 else Some d2))

(* Problem 3: matching properties with provided input recipes. *)

type pattern = WildcardP | VariableP of string | UnitP | ConstantP of int
             | ConstructorP of string * pattern | TupleP of pattern list
type valu = Constant of int | Unit | Constructor of string * valu | Tuple of valu list
exception NoAnswer

(* Replace these stubs with your HW3 functions. *)
let rec first_answer f xs = 
  match xs with
  | [] -> raise NoAnswer
  | x :: rest ->
    match f x with
    | Some value -> value
    | None -> first_answer f rest

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

let first_match v pats = 
  try Some (first_answer (matches v) pats)
  with NoAnswer -> None

(* Provided: an exact pattern with no variable bindings. Do not modify. *)
let rec pattern_of_value value =
  match value with
  | Unit -> UnitP
  | Constant i -> ConstantP i
  | Constructor (name, v) -> ConstructorP (name, pattern_of_value v)
  | Tuple vs -> TupleP (List.map pattern_of_value vs)

let base_valu_gen = Gen.(oneof [
  return Unit; map (fun i -> Constant i) small_int
])
let base_pat_gen = Gen.(oneof [
  return WildcardP; return UnitP;
  map (fun s -> VariableP s) (oneofl ["x"; "y"; "z"]);
  map (fun i -> ConstantP i) small_int
])
(* Small shared label vocabulary makes related inputs less rare. *)
let label_gen = Gen.oneofl ["A"; "B"; "C"]

(* Provided input recipes: base cases, one constructor layer, and tuples of
   base cases. These support all required tests without the optional challenge.
   Keep these recipes and the required arbitraries unchanged. *)
let supplied_valu_gen = Gen.(oneof [
  base_valu_gen;
  map2 (fun s v -> Constructor (s, v)) label_gen base_valu_gen;
  map (fun vs -> Tuple vs) (list_size (int_range 0 3) base_valu_gen)
])
let supplied_pat_gen = Gen.(oneof [
  base_pat_gen;
  map2 (fun s p -> ConstructorP (s, p)) label_gen base_pat_gen;
  map (fun ps -> TupleP ps) (list_size (int_range 0 3) base_pat_gen)
])

(* Provided printers make nested counterexamples readable. *)
let rec print_valu value =
  match value with
  | Unit -> "Unit"
  | Constant i -> Printf.sprintf "Constant %d" i
  | Constructor (s, v) -> Printf.sprintf "Constructor (%S, %s)" s (print_valu v)
  | Tuple vs -> "Tuple [" ^ String.concat "; " (List.map print_valu vs) ^ "]"
let rec print_pat pattern =
  match pattern with
  | WildcardP -> "WildcardP"
  | VariableP s -> Printf.sprintf "VariableP %S" s
  | UnitP -> "UnitP"
  | ConstantP i -> Printf.sprintf "ConstantP %d" i
  | ConstructorP (s, p) -> Printf.sprintf "ConstructorP (%S, %s)" s (print_pat p)
  | TupleP ps -> "TupleP [" ^ String.concat "; " (List.map print_pat ps) ^ "]"
let valu_arb = make ~print:print_valu supplied_valu_gen
let pat_arb = make ~print:print_pat supplied_pat_gen

(* Input (v, p) : valu * pattern. Check that trying a singleton pattern list
   agrees with matching its only pattern directly, including the bindings.
   The two inputs are generated independently; both calls may return None. *)
let test_first_match =
  Test.make ~name:"first_match: singleton" ~count:200
    (pair valu_arb pat_arb)
    (fun (v, p) -> first_match v [p] = matches v p)

(* Input v : valu. Use pattern_of_value to obtain an exact pattern for v.
   Check that matching succeeds without producing any variable bindings. *)
let test_match_exact =
  Test.make ~name:"matches: exact generated pattern" ~count:200
    (valu_arb)
    (fun v -> matches v (pattern_of_value v) = Some [])

(* Problem 4: tests inside an abstract module. *)
module type INT_SET = sig
  type t
  val empty : t
  val insert : int -> t -> t
  val remove : int -> t -> t
  val mem : int -> t -> bool
  (* Tests are constructed inside the module; the runner only executes them. *)
  val tests : QCheck.Test.t list
end
module IntSet : INT_SET = struct

type t = int list
let empty = []
let rec insert x elements =
  match elements with
  | [] -> [x]
  | y :: ys as s ->
      if x < y then x :: s
      else if x = y then s
      else y :: insert x ys
let rec remove x elements =
  match elements with
  | [] -> []
  | y :: ys as s ->
      if x < y then s
      else if x = y then ys
      else y :: remove x ys
let rec mem x elements =
  match elements with
  | [] -> false
  | y :: ys -> x = y || (x > y && mem x ys)

(* Check the representation as given: it must be sorted with no duplicates.
   Empty and singleton lists are valid; negative integers are allowed.
   Do not sort or remove duplicates before checking, since that hides errors.
   TODO: state the invariant in your own words; give three valid and three
   invalid examples. *)
let rec rep_ok (s : t) =
  match s with
  | [] -> true
  | [_] -> true
  | x :: (y :: _ as rest) -> x < y && rep_ok rest

(* Provided, independent of insert/remove: every generated s is valid.
   Never discard a failing operation result with assume. Check it with rep_ok. *)
let set_gen = Gen.(map (List.sort_uniq Stdlib.compare)
  (list_size (int_range 0 20) (int_range (-10) 10)))
let set_arb = make ~print:(Print.list Print.int) set_gen
let element_arb = int_range (-10) 10
(* Half the time y = x, so inserted-element queries are common. *)
let query_arb = make ~print:(Print.triple Print.int Print.int (Print.list Print.int))
  Gen.(map2 (fun (x,y) s -> (x,y,s))
    (oneof [map (fun x -> (x,x)) (int_range (-10) 10);
            pair (int_range (-10) 10) (int_range (-10) 10)]) set_gen)

(* Input () : unit. There is no randomly generated set argument for this test;
   check that the module's provided empty set satisfies rep_ok. *)
let test_empty =
  Test.make ~name:"set: empty is valid" ~count:200
    (unit)
    (fun _ -> rep_ok empty)

(* Input (x, s): an integer and an already-valid set. Check that inserting x
   produces a representation satisfying rep_ok. *)
let test_insert =
  Test.make ~name:"set: insert preserves invariant" ~count:200
    (pair element_arb set_arb)
    (fun (x, s) -> rep_ok (insert x s))

(* Input (x, s): an integer and an already-valid set. Check that removing x
   preserves rep_ok, whether or not x was present. *)
let test_remove =
  Test.make ~name:"set: remove preserves invariant" ~count:200
    (pair element_arb set_arb)
    (fun (x, s) -> rep_ok (remove x s))

(* Input (x, y, s) : int * int * t, a flat triple. Insert x into valid s and
   check the expected membership of query value y. Consider both y = x and
   y <> x. Valid representation alone does not establish correct membership. *)
let test_insert_mem =
  Test.make ~name:"set: membership after insert" ~count:200
    (query_arb)
    (fun (x, y, s) -> mem y (insert x s) = (y = x || mem y s))

let tests = [test_empty; test_insert; test_remove; test_insert_mem]
end

(* TODO: In 3-5 sentences, explain:
   (1) Why can internal tests inspect t although the signature makes it abstract?
      The tests are inside the 'IntSet' module, so they can see how the set is stored. 
      Code outside the module cannot see that because the signature hides it.

   (2) What does passing these tests establish, and what does it not prove?
        They show that the code works for the cases tested, 
        they don't guarantee that it works for every input.

   (3) Why does insert x s = [] pass preservation but fail the behavioral test? 
        The preservation test only checks whether the result is a valid set. An empty set [] is valid, so it passes but 
        the behavioral test checks whether the inserted number is actually in the set. [] contains nothing so it fails.
   *)

(* Optional challenge: recursive generators (1 bonus point).
   Complete these only after the required properties and IntSet work.
   Correct implementations of BOTH generators earn the bonus. Required work
   is worth 5 points, so completing it and the challenge earns 6/5.
   Leaving these stubs unchanged does not reduce the required-work score.
   For n <= 0 use the base generators. For n > 0 cover every constructor,
   decrease each child's nonnegative depth budget, and use tuple widths 0..3.
   The supplied required tests above do not depend on these definitions.
   Sample in dune utop with QCheck.Gen.generate ~n:10 (valu_gen 3), or make
   a new arbitrary with QCheck.make ~print:print_valu (valu_gen 3) and use it
   to rerun the matching properties. Use pat_gen similarly. *)


let rec valu_gen n = 
  if n <= 0 then
    base_valu_gen
  else
    let child_depth = n / 2 in
    Gen.oneof [ 
      base_valu_gen;
      Gen.map2
        (fun s v -> Constructor (s, v))
        label_gen (valu_gen child_depth);
      Gen.map
        (fun vs -> Tuple vs)
        (Gen.list_size
          (Gen.int_range 0 3)
          (valu_gen child_depth))
    ]


let rec pat_gen n = 
  if n <= 0 then
    base_pat_gen
  else
    Gen.oneof [
      base_pat_gen;
      Gen.map2
        (fun s p -> ConstructorP (s, p))
        label_gen (pat_gen (n / 2));
      Gen.map
        (fun ps -> TupleP ps)
        (Gen.list_size
          (Gen.int_range 0 3)
          (pat_gen (n / 2))
        )
    ]