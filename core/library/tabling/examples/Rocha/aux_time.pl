
time :- abolish_all_tables,
		 		statistics(cputime,[Init1,_]),
       (time_query ; true),
       statistics(cputime,[End1,_]),
       Time1 is End1 - Init1,

			 abolish_all_tables,

       abolish_all_tables,
       statistics(cputime,[Init2,_]),
       (time_query ; true),
       statistics(cputime,[End2,_]),
       Time2 is End2 - Init2,

			 abolish_all_tables,

       abolish_all_tables,
       statistics(cputime,[Init3,_]),
       (time_query ; true),
       statistics(cputime,[End3,_]),
       Time3 is End3 - Init3,

			 abolish_all_tables,

       Time is (Time1 + Time2 + Time3) / 3,
       write('cputime average three runs is '), write(Time),
       write(' ('), write(Time1), write('/'), write(Time2), 
       write('/'), write(Time3), write(')'), nl.
