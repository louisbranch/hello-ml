(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str, lst) =
  let
    fun aux([]) = []
      | aux(x::lst') = if same_string(str,x) then aux(lst') else x::aux(lst')
    val result = aux(lst)
  in
    if lst=result then NONE
    else SOME result
  end

fun get_substitutions1([], _) = []
  | get_substitutions1(x::lst', str) =
    case all_except_option(str, x) of
      SOME e => e @ get_substitutions1(lst', str)
    | NONE => [] @ get_substitutions1(lst', str)

fun get_substitutions2(lst, str) =
  let
    fun aux([], acc) = acc
      | aux(x::lst', acc) =
        case all_except_option(str, x) of
          SOME e => aux(lst', acc @ e)
        | NONE => aux(lst', acc)
  in
    aux(lst, [])
  end

fun similar_names(lst, {first, middle, last}) =
    let
      val subs = get_substitutions2(lst, first)
      fun aux([]) = []
        | aux(x::subs') = {first=x, middle=middle, last=last} :: aux(subs')
    in
      {first=first, middle=middle, last=last} :: aux(subs)
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (Clubs, _) = Black
  | card_color (Diamonds, _) = Red
  | card_color (Spades, _) = Black
  | card_color (Hearts, _) = Red

fun card_value (_, Num n) = n
  | card_value (_, Ace) = 11
  | card_value (_, _) = 10

fun remove_card(lst, c, e) =
  let
    fun aux([]) = raise e
      | aux(x::lst') = if x=c then lst' else x::aux(lst')
  in
    aux(lst)
  end

fun all_same_color([]) = true
  | all_same_color(_::[]) = true
  | all_same_color(hd::(nk::tl)) =
    (card_color(hd) = card_color(nk)) andalso all_same_color(nk::tl)

fun sum_cards(lst) =
  let
    fun aux([], acc) = acc
      | aux(hd::tl, acc) = aux(tl, acc + card_value(hd))
  in
    aux(lst, 0)
  end

fun score(lst, goal) =
  let
    val sum = sum_cards(lst)
    val pre = if sum > goal then 3 * (sum - goal)
              else goal - sum
  in
    if all_same_color(lst) then pre div 2 else pre
  end

fun officiate(cards, moves, goal) =
  let
    fun aux(_, held, []) = score(held, goal)
      | aux([], held, Draw::tl) = score(held, goal)
      | aux(cards, held, Discard c::tl) =
        aux(cards, remove_card(held, c, IllegalMove), tl)
      | aux(hd::cards, held, Draw::tl) =
        let
          val score = sum_cards(hd::held)
        in
          if score > goal then score
          else aux(cards, hd::held, tl)
        end
  in
    aux(cards, [], moves)
  end
