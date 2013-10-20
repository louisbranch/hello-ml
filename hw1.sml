fun is_older (a : (int * int * int), b : (int * int * int)) =
  if a = b then false
  else if #1 a = #1 b
  then is_older((#2 a, #3 a, 0), (#2 b, #3 b, 0))
  else #1 a < #1 b

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else let
      val count = if (#2 (hd dates)) = month then 1 else 0
    in
      count + number_in_month(tl dates, month)
    end

fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if (#2 (hd dates)) = month
       then hd dates :: dates_in_month(tl dates, month)
       else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (words : string list, index : int) =
  if index = 1
  then hd words
  else get_nth(tl words, index - 1)

fun date_to_string (date : (int * int * int)) =
  let
    val months = ["January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"]
    fun s (i : int) = Int.toString i
  in
    get_nth(months, #2 date) ^ " " ^ s(#3 date) ^ ", " ^ s(#1 date)
  end

fun number_before_reaching_sum (sum : int, numbers : int list) =
  if null numbers orelse hd numbers >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

fun what_month (day : int) =
  let
    val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum(day, months) + 1
  end

fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates :(int * int * int) list) =
  if null dates
  then NONE
  else
    let
      fun oldest_nonempty (dates : (int * int * int) list) =
        if null (tl dates)
        then hd dates
        else let val tl_ans = oldest_nonempty(tl dates)
             in
               if is_older(hd dates, tl_ans)
               then hd dates
               else tl_ans
             end
    in
      SOME (oldest_nonempty dates)
    end
