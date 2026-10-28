#use "hw1.ml";;

is_older((1,1,2020), (1,1,2021));;
is_older((1,1,2021), (1,1,2020));;

number_in_month([(1,1,2020); (5,2,2020); (10,2,2021)], 2);;

number_in_months([(1,1,2020); (5,2,2020); (10,3,2021)], [1;2]);;

dates_in_month([(1,1,2020); (5,2,2020); (10,2,2021)], 2);;

dates_in_months([(1,1,2020); (5,2,2020); (10,3,2021)], [1;3]);;

get_nth(["January"; "February"; "March"], 2);;

string_of_date(15, 9, 2026);;

number_before_reaching_sum(10, [3;4;5;6]);;

what_month 32;;

month_range (30, 32);;

oldest([(10,5,2022); (3,2,2020); (15,8,21)]);;

cumulative_sum[1;2;3;4];;
