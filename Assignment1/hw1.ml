let fst3  (x, _, _) = x
let snd3  (_, x, _) = x
let thd3  (_, _, x) = x

let is_older ((d1 : int * int * int ), (d2 : int * int * int)) = 
	if thd3 d1 < thd3 d2 then
		true
	else if thd3 d1 > thd3 d2 then
		false
	else if snd3 d1 < snd3 d2 then
		true
	else if snd3 d1 > snd3 d2 then
		false
	else
		fst3 d1 <fst3 d2

let rec number_in_month ((dates : (int * int * int) list), (month : int)) = 
	if dates = [] then
		0
	else if snd3 (List.hd dates) = month then
		1 + number_in_month(List.tl dates, month)
	else
		number_in_month(List.tl dates, month)

let rec number_in_months ((dates : (int * int * int) list), (months : int list)) =
	if months =  [] then
		0
	else
		number_in_month(dates, List.hd months) + number_in_months(dates, List.tl months)

let rec dates_in_month ((dates : (int * int * int) list), (month : int)) = 
	if dates = [] then
		[]
	else if snd3 (List.hd dates) = month then
		List.hd dates :: dates_in_month (List.tl dates, month)
	else
		dates_in_month(List.tl dates, month)

let rec dates_in_months ((dates : (int * int * int) list), (months : int list)) = 
	if months = [] then
		[]
	else
		dates_in_month (dates, List.hd months) @ dates_in_months (dates, List.tl months)


let rec get_nth ((strings : string list), (n : int)) = 
	if n = 1 then
		List.hd strings
	else 
		get_nth (List.tl strings, n-1)


let string_of_date (date : int * int * int) = 
	let months = ["January"; "Febuary"; "March"; "April"; "May"; "June"; "July"; "August"; "Spetember"; "October"; "November"; "December"] in
	get_nth (months, snd3 date)
	^"-"^ string_of_int(fst3 date)
	^"-"^ string_of_int(thd3 date)


let rec number_before_reaching_sum ((sum : int), (numbers : int list)) = 
	if sum <= 0 then
		0
	else 
		1 + number_before_reaching_sum(sum - List.hd numbers, List.tl numbers)



let what_month day = 
	if day <= 31 then
		1
	else if day <= 59 then
		2
	else if day <= 90 then
		3
	else if day <= 120 then
		4
	else if day <= 151 then
		5
	else if day <= 181 then
		6
	else if day <= 212 then
		7
	else if day <= 243 then
		8
	else if day <= 273 then
		9
	else if day <= 304 then
		10
	else if day <= 334 then
		11
	else 
		12


let rec month_range ((start : int), (finish : int)) = 
	if start > finish then
		[]
	else
		what_month start :: month_range (start + 1, finish)

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

let cumulative_sum (numbers : int list) = 
	let rec helper numbers sum =
		if  numbers = [] then
			[]
		else 
			(sum + List.hd numbers) :: 
			helper (List.tl numbers)(sum + List.hd numbers)
	in 
	helper numbers 0




















