(* Your comment *)

(* test function one *)
only_lowercase ["hello"; "World"; "OCaml"];;
only_lowercase ["ABC"; "abc"; "123"; "!wow"];;
only_lowercase [];;

(* test function two *)
longest_string1 ["cat"; "elephant"; "dog"];;
longest_string1 ["apple"; "grape"];;
longest_string1 [];;

(* test function three *)
longest_string2 ["apple"; "grape"; "banana"];;

(* test function four *)
longest_string_helper (>) ["apple"; "grape"];;
longest_string3 ["apple"; "grape"];;
longest_string4 ["apple"; "grape"];;
(* expected results: apple, apple, grape *)

(* test function five *)
longest_lowercase ["Apple"; "cat"; "grape"; "Dog"; "pear"];;

(* test function six *)
caps_no_X_string "aBxXXxDdx";;

(* test function seven *)
first_answer check_number [2; 4; 8; 10];;

(* test function eight *)
all_answers (fun x -> Some [x]) [1; 2; 3];;
all_answers (fun x -> if x = 2 then None else Some [x]) [1; 2; 3];;
all_answers (fun x -> Some [x]) [];;

(* test function nine *)
count_wildcards (TupleP [WildcardP; VariableP "x"; WildcardP]);;
count_wild_and_variable_lengths (TupleP [VariableP "hi"; WildcardP; VariableP "abc"]);;
count_a_var "x" (TupleP [VariableP "x"; VariableP "y"; VariableP "x"]);;

(* test function ten *)
check_pat (TupleP [VariableP "x"; VariableP "y"]);;
check_pat (TupleP [VariableP "x"; VariableP "x"]);;
check_pat (ConstructorP ("C", TupleP [WildcardP; VariableP "a"]));;

(* test function eleven *)
matches (Constant 5) (ConstantP 3);;
matches (Tuple [Constant 3; Constant 8]) (TupleP [VariableP "x"; ConstantP 8]);;
matches (Constructor ("C", Constant 7)) (ConstructorP ("C", VariableP "x"));;

(* test function twelve *)
first_match (Constant 5) [ConstantP 2; ConstantP 3];;
first_match (Constant 5) [];;
