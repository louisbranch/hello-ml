(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s,0)))

val longest_string1 = List.foldl (fn(x,acc) => if String.size(x) > String.size(acc) then x else acc) ""

val longest_string2 = List.foldl (fn(x,acc) => if String.size(x) >= String.size(acc) then x else acc) ""

fun longest_string_helper f xs = List.foldl (fn(x, acc) => if f(String.size(x), String.size(acc)) then x else acc) "" xs

val longest_string3 = longest_string_helper (op >)

val longest_string4 = longest_string_helper (op >=)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst =
  let
    fun aux([]) = raise NoAnswer
      | aux(x::xs) = case f(x) of
          SOME e => e
        | NONE => aux(xs)
  in
    aux(lst)
  end

fun all_answers f lst =
  let
    fun aux([], acc) = SOME acc
      | aux(x::xs, acc) = case f(x) of
          SOME e => aux(xs, acc @ e)
        | NONE => NONE
  in
    aux(lst, [])
  end

val count_wildcards = g (fn() => 1) (fn(_) => 0)

val count_wild_and_variable_lengths = g (fn() => 1) (fn(x) => String.size x)

fun count_some_var (s, pat) = g (fn() => 0) (fn(x) => if s=x then 1 else 0) pat

fun check_pat pat =
  let
    fun make_list(p) =
      case p of
          Variable x        => [x]
        | TupleP ps         => List.foldl (fn (p,i) => i @ (make_list p)) [] ps
        | _                 => []
    fun all_equal([])= true
      | all_equal(x::[])= true
      | all_equal(x::xs)= List.exists (fn(s) => s = x) xs
  in
    (all_equal o make_list) pat
  end

fun match (v, p) =
  case (v, p) of
    (Const _, ConstP _) => SOME []
  | (Unit, UnitP) => SOME []
  | (Tuple lst, TupleP ps) => all_answers match(ListPair.zip(lst,ps))
  | (Constructor(s1, x), ConstructorP(s2, Variable y)) =>
      if s1=s2 then SOME [(y, x)]
      else NONE
  | (_, Wildcard) => SOME []
  | (x, Variable y) => SOME [(y,x)]
  | (_,_) => NONE

fun first_match v ps =
  SOME (first_answer (fn (x) => match(v, x)) ps)
  handle NoAnswer => NONE
