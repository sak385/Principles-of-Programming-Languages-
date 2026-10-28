(* CS 314, HW2 Provided Code *)

(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json

(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (357 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* provided helper function to convert a float to a string *)
(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
let json_string_of_float f =
  Printf.sprintf "%g" f
  
(* 1 *)
let make_silly_json i =
  let rec helper n =
    if n = 0 then
      []
    else
      Object [("n", Num (float_of_int n)); ("b", True)] ::
      helper (n - 1)
  in
  Array (helper i)



(* 2 *)
let rec concat_with (sep, ss) =
  match ss with
  | [] -> ""
  | [s] -> s
  | s :: rest -> s ^ sep ^ concat_with (sep, rest)

(* 3 *)
let quote_string s =
   "\"" ^ s ^ "\""


(* 4 *)
let rec string_of_json j =
  match j with
  |True -> "true"
  |False -> "false"
  |Null -> "null"
  |String s -> quote_string s
  |Num n -> json_string_of_float n
  |Array items -> "[" ^ concat_with (",", List.map string_of_json items) ^ "]"
  | Object pairs -> "{" ^ concat_with (",", List.map (fun (key, value) -> quote_string key ^ ":" ^ string_of_json value) pairs)^ "}"

(* 5 *)
let rec take (n,xs) = 
  match (n, xs) with
  | (0, _) -> []
  | (_, []) -> []
  | (n, x :: rest) -> x :: take (n - 1, rest)

(* 6 *)
let rec firsts xs = 
  match xs with
  |[] -> []
  |(x,y) :: rest -> x :: firsts rest

(* 7 *)
(* firsts is the function that takes the first element in a pair.  take puts the element in a list*)
(* firsts (take (n, xs)) takes takes the first n pairs and extract their value, while take (n, firsts xs) takes the first element of every pair before takeing the first n*)
(*the only difference is effiency*)

(* 8 *)
let rec assoc (k, xs) =
  match xs with
  | [] -> None
  | (key, value) :: rest ->
      if k = key then
        Some value
      else
        assoc (k, rest)

(* 9 *)
let dot (j, f) = 
  match j with
  | Object fields -> assoc (f, fields)
  | _ -> None

(* 10 *)
let rec dots (j, fs) =
  match fs with
  | [] -> Some j
  | f :: rest ->
      match dot (j, f) with
      | None -> None
      | Some value -> dots (value, rest)

(* 11 *)
let one_fields j =
  let rec helper (fields, acc) =
    match fields with
    | [] -> acc
    | (name, _) :: rest -> helper (rest, name :: acc)
  in
  match j with
  | Object fields -> helper (fields, [])
  | _ -> []

(* 12 *)
let no_repeats xs = 
  List.length (dedup xs) = List.length xs

(* 13 *)
let rec recursive_no_field_repeats j = 
  let rec check_array items =
    match items with
    | [] -> true
    | x :: rest ->
        recursive_no_field_repeats x && check_array rest
  in
  let rec check_fields fields =
    match fields with
    | [] -> true
    | (_, value) :: rest ->
        recursive_no_field_repeats value && check_fields rest
  in
  match j with
  | Object fields ->
      no_repeats (one_fields j) && check_fields fields
  | Array items -> check_array items
  | _ -> true

(* 14 *)
let count_occurrences (xs, e) =
  match xs with
  | [] -> []
  | first :: rest ->
      let rec helper (current, count, remaining, acc) =
        match remaining with
        | [] -> (current, count) :: acc
        | x :: tail ->
            if x < current then raise e 
            else if x = current then
              helper (current, count + 1, tail, acc)
            else
              helper (x, 1, tail, (current, count) :: acc)
      in
      helper (first, 1, rest, [])

(* 15 *)
let rec string_values_for_access_path (fs, js) = 
  let rec helper js =
    match js with
    | [] -> []
    | j :: rest ->
        match dots (j, fs) with
        | Some (String s) -> s :: helper rest
        | _ -> helper rest
  in
  helper js

(* 16 *)
let rec filter_access_path_value (fs, v, js) = 
  let rec helper js =
    match js with
    | [] -> []
    | j :: rest ->
        match dots (j, fs) with
        | Some (String s) ->
            if s = v then j :: helper rest
            else helper rest
        | _ -> helper rest
  in
  helper js

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(* 17 *)
let in_rect (r, p) = 
  r.min_latitude <= p.latitude &&
  p.latitude <= r.max_latitude &&
  r.min_longitude <= p.longitude &&
  p.longitude <= r.max_longitude

(* 18 *)
let point_of_json j = 
  match dot (j, "latitude") with
  | Some (Num lat) ->
      (match dot (j, "longitude") with
       | Some (Num lon) ->
           Some { latitude = lat; longitude = lon }
       | _ -> None)
  | _ -> None

(* 19 *)
let rec filter_access_path_in_rect (fs, r, js) = 
  let rec helper js =
    match js with
    | [] -> []
    | j :: rest ->
        match dots (j, fs) with
        | Some obj ->
            (match point_of_json obj with
             | Some p ->
                 if in_rect (r, p) then j :: helper rest
                 else helper rest
             | None -> helper rest)
        | None -> helper rest
  in
  helper js
(* 20 *)
(* both functions use a path to find values in JSON objects, then filter a list based on a condision.
you could refactor the two functions if you but the condision your sorting the list on, in the argument of the function.  on a scale of 
1 - 10, My anoyance is a 7*)

(* For this section, we provide the definition of the College Avenue area and the functions
 * to calculate a histogram. Use these to create the bindings as requested. 
 * But notice our implementation of histogram uses *your* definition of count_occurrences
 *)
 (* We provide this code commented out because it uses some of your functions 
    that you haven't implemented yet *)


exception SortIsBroken

(* The definition of the College Avenue Campus area for this assignment. *)
let college_ave_area =
  { min_latitude  =  40.496000;
    min_longitude = -74.452500;
    max_latitude  =  40.505500;
    max_longitude = -74.443000
  }

(* Creates a histogram for the given list of strings. 
 * Returns a tuple in which the first element is
 * a string, and the second is the number of times that string
 * is found. *)
let histogram xs = 
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences (sorted_xs, SortIsBroken) in
  let compare_counts (s1, n1) (s2, n2) =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs, js) = 
  histogram (string_values_for_access_path (fs,js))

(* notice we use *your* definition of dot *)
let complete_bus_positions_list =
  match (dot (complete_bus_positions, "entity")) with
  | Some (Array xs) -> xs
  | _ -> failwith "complete_bus_positions_list"


exception Unimplemented
let route_histogram =
  histogram_for_access_path
    (["vehicle"; "trip"; "route_name"], complete_bus_positions_list)


let top_three_routes = 
  firsts (take (3, route_histogram))

let buses_in_college_ave =
  filter_access_path_in_rect
    (["vehicle"; "position"], college_ave_area, complete_bus_positions_list)


let college_ave_route_histogram = 
  histogram_for_access_path
    (["vehicle"; "trip"; "route_name"], buses_in_college_ave)

let top_three_college_ave_routes =
  firsts (take (3, college_ave_route_histogram))

let all_lx_buses =
  filter_access_path_value (["vehicle"; "trip"; "route_name"], "LX", complete_bus_positions_list)
