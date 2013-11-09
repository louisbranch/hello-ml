use "hw3provided.sml";

(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["Amor","baixinho","Coracao"] = ["Amor","Coracao"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 ["A","bc","Cd"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 ["A","bc","Cd"] = "Cd"

val test4a= longest_string3 ["A","bc","C"] = "bc"
val test4a_2 = longest_string3 ["A","bc","Cd"] = "bc"

val test4b= longest_string4 ["A","B","C"] = "C"
val test4b_2 = longest_string4 ["A","bc","Cd"] = "Cd"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5_2 = longest_capitalized ["A","bc","Cd"] = "Cd"

val test6 = rev_string "abc" = "cba"
val test6_2 = rev_string "aBcD" = "DcBa"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_2 = (first_answer (fn x => if x > 9 then SOME x else NONE) [1,2,3,4,5] handle NoAnswer => 0) = 0
val test7_3 = (first_answer (fn x => if x > 9 then SOME x else NONE) [] handle NoAnswer => 0) = 0

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
val test8_3 = all_answers (fn x => if x > 4 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE


val test9a = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (TupleP [Wildcard, Wildcard, ConstP(5)]) = 2
val test9a_3 = count_wildcards (ConstructorP ("dt", TupleP [Wildcard, Wildcard, Wildcard])) = 3
val test9a_4 = count_wildcards (TupleP [Wildcard, TupleP [Wildcard, Wildcard], ConstP 5]) = 3
val test9a_5 = count_wildcards (TupleP [ConstP 10, TupleP [Variable "str"], ConstP 5]) = 0

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (TupleP [ConstP 10, TupleP [Variable "str"], ConstP 5]) = 3
val test9b_3 = count_wild_and_variable_lengths (TupleP [Variable "xxxx", TupleP [Variable "str"], Wildcard]) = 8

val test9c = count_some_var ("x", Variable("x")) = 1;
val test9c_2 = count_some_var ("xxx", (TupleP [ConstP 10, TupleP [Variable "str"], ConstP 5])) = 0
val test9c_3 = count_some_var ("xxx", (TupleP [Variable "xxx", TupleP [Variable "xxx"], Wildcard])) = 2

val test10 = check_pat (Variable("x")) = true
val test10_2 = check_pat (TupleP [Variable "xxx", TupleP [Variable "xxx"], Wildcard]) = true
val test10_3 = check_pat (TupleP [Variable "str", TupleP [Variable "xxx"], Wildcard]) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_2 = match (Constructor ("SOME", Const 42), ConstructorP ("SOME", Variable "x")) = SOME [("x", Const 42)]
val test11_3 = match (Constructor ("NONE", Unit), ConstructorP ("SOME", Variable "x")) = NONE
val test11_4 = match (Tuple[Const 7, Const 6, Unit, Const 7], TupleP[Variable "a", Variable "ba", Variable "bba", Variable "bbba"]) =
                     SOME [("a",Const 7),("ba",Const 6),("bba",Unit),("bbba",Const 7)]
val test11_5 = match (Const 11, Variable "z") = SOME [("z", Const 11)]
val test11_6 = match (Tuple[Constructor ("NONE", Unit), Const 6, Unit, Const 7],
TupleP[ConstructorP ("SOME", Variable "a"), Variable "ba", Variable "bba", Variable "bbba"]) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test12_2 = first_match (Constructor ("SOME", Unit)) [UnitP] = NONE
val test12_3 = first_match (Constructor ("SOME", Const 42)) [ConstructorP(("SOME"), Variable "x")] = SOME [("x", Const 42)]
