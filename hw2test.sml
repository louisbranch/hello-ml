use "hw2provided.sml";

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1   = all_except_option("string", ["string"]) = SOME []
val test1_2 = all_except_option("string", ["any", "string", "other"]) = SOME ["any", "other"]
val test1_3 = all_except_option("string", ["other"]) = NONE
val test1_4 = all_except_option("string", []) = NONE

val test2   = get_substitutions1([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1([["foo", "any"],["there"]], "foo") = ["any"]
val test2_3 = get_substitutions1([["foo", "any"],["foo", "other"]], "foo") = ["any", "other"]
val test2_4 = get_substitutions1([], "foo") = []
val test2_5 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test3   = get_substitutions2([["foo"],["there"]], "foo") = []
val test3_2 = get_substitutions2([["foo", "any"],["there"]], "foo") = ["any"]
val test3_3 = get_substitutions2([["foo", "any"],["foo", "other"]], "foo") = ["any", "other"]
val test3_4 = get_substitutions2([], "foo") = []
val test3_5 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test4_2 = similar_names([], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}]
val test4_3 = similar_names([["Elizabeth", "Betty"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}]
val test4_4 = similar_names([["Elizabeth", "Betty"], ["Elizabeth", "Betty"]],
{first="Elizabeth", middle="W", last="Smith"}) = [ {first="Elizabeth",
last="Smith", middle="W"}, {first="Betty", last="Smith", middle="W"}, {first="Betty", last="Smith", middle="W"} ]

val test5 = card_color((Clubs, Num 2)) = Black
val test5_2 = card_color((Spades, Num 2)) = Black
val test5_3 = card_color((Hearts, Num 2)) = Red
val test5_4 = card_color((Diamonds, Num 2)) = Red

val test6 = card_value((Clubs, Num 2)) = 2
val test6_2 = card_value((Clubs, Ace)) = 11

val test7   = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_2 = (remove_card([], (Hearts, Ace), IllegalMove) handle IllegalMove => []) = []
val test7_3 = remove_card([(Clubs, Num 2),(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs, Num 2)]
val test7_4 = remove_card([(Hearts, Ace),(Clubs, Num 2)], (Hearts, Ace), IllegalMove) = [(Clubs, Num 2)]


val test8   = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true
val test8_2 = all_same_color([(Hearts, Ace), (Spades, Ace)]) = false
val test8_3 = all_same_color([]) = true
val test8_4 = all_same_color([(Hearts, Ace)]) = true
val test8_5 = all_same_color([(Hearts, Ace), (Hearts, Num 2), (Spades, Ace)]) = false

val test9   = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
val test9_2 = sum_cards([(Clubs, Num 2)]) = 2
val test9_3 = sum_cards([]) = 0
val test9_4 = sum_cards([(Clubs, Num 2),(Hearts, Ace)]) = 13
val test9_5 = sum_cards([(Clubs, Jack)]) = 10

val test10   = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_2 = score([],10) = 5
val test10_3 = score([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test10_4 = score([(Hearts, Num 9),(Hearts, King)],10) = 13
val test10_5 = score([(Hearts, Num 9),(Spades, King)],10) = 27

val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test11_2 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw,Draw,Draw], 15) = 9
val test11_3 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[], 15) = 7
val test11_4 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw, Draw, Discard (Clubs, Num 4)], 15) = 6
val test11_5 = (officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw, Discard
(Clubs, Num 4)], 15) handle IllegalMove => 0) = 0
