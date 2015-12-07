
time :- statistics(cputime,[Init1,_]),
       (time_query ; true),
       statistics(cputime,[End1,_]),
       Time1 is End1 - Init1,

       Time is Time1,
       write('cputime one run is '), write(Time), nl.
