size_min(3).
size_max(10).


html_format("element", no, Name, _, _, [strong(Name)]).
html_format("element", Occurs,
	    [Name, _, _, Occ, MoreField,_],_,_, 
	    [strong(Name),
	     begin(table), 
	     nl, begin(tbody),
	     nl, begin(tr), begin(td), end(td),
	     nl, begin(td),
	     nl, More,
     	     nl, end(td), end(tr),
	     nl, end(tbody),
	     nl, end(table),
	     nl, input(hidden, [name=Occ, value=Occurs])]) :-
   check_if_more(Occurs, Name, MoreField, More).



% html_format(_, Occurs, Handlers, _, _, HTML) :-
% 	(Occurs="OPTIONAL" ; Occurs="ZEROORMORE"),!,
% 	Handlers=[Name, _, _, OccField, MoreField,_|_],
% 	HTML=[strong(Name),
% 	     begin(table), 
% 	     nl, begin(tbody),
% 	     nl, begin(tr), begin(td),
% 	     nl, end(td),
% 	     nl, begin(td),
% 	     nl, em('Check to add an element '), More,
%        	     nl, end(td), end(tr),
% 	     nl, end(tbody),
% 	     nl, end(table),
% 	     nl, input(hidden, [name=OccField, value=Occurs])],
%          make_more(MoreField, More).

html_format("string", Occurs,
	    [Name, Field, OnField, Occ, MoreField, Exp],_, P, 
	    [strong(Name),
	     begin(table), 
	     nl, begin(tbody),
	     nl, begin(tr), begin(td),
	     nl, end(td),
	     nl, begin(td),
	     nl, select([name=OnField], S),
	     nl, input(text, [name=Field, size=28]),
     	     nl, input(button, [onClick=Do_ok, value=ok]),
	     nl, input(button, [onClick=Do_and, value=and]),
	     nl, input(button, [onClick=Do_or, value=or]),
	     nl, input(button, [onClick=Do_lpar, value='(']),
	     nl, input(button, [onClick=Do_rpar, value=')']),\\,
	     input(text, [name=Exp, size=48, value=Val]), 
	     nl, end(td), end(tr),
	     nl, begin(tr), begin(td), end(td),
	     nl, begin(td),
	     nl, More,
     	     nl, end(td), end(tr),
	     nl, end(tbody),
	     nl, end(table),
	     nl, input(hidden, [name=Occ, value=Occurs])]) :-
   do_and(Field, Do_and),
   do_or(Field, Do_or),
   do_ok(Field, Name, Do_ok),
   do_lpar(Field, Do_lpar),
   do_rpar(Field,  Do_rpar),
   process_string(P, Val, S), 
   check_if_more(Occurs, Name, MoreField, More).


html_format("number", Occurs,
	    [Name, Field, OnField, Occ, MoreField, Exp],_, P,
	    [strong(Name), 
	     begin(table), 
	     nl, begin(tbody),
	     nl, begin(tr), begin(td),
	     nl, end(td),
     	     nl, begin(td),
	     nl, select([name=OnField], S),
	     nl, input(number, [name=Field, size=30]),
     	     nl, input(button, [onClick=Do_ok, value=ok]),
	     nl, input(button, [onClick=Do_and, value=and]),
	     nl, input(button, [onClick=Do_or, value=or]), 
	     nl, input(button, [onClick=Do_lpar, value='(']),
	     nl, input(button, [onClick=Do_rpar, value=')']),\\,
	     nl, input(text, [name=Exp, size=48, value=Val]),
	     nl, end(td), end(tr),
	     nl, begin(tr), begin(td), end(td),
	     nl, begin(td),
	     nl, More,
     	     nl, end(td), end(tr),
	     nl, end(tbody),
	     nl, end(table),
	     nl, input(hidden, [name=Occ, value=Occurs])]) :-
   do_and(Field, Do_and),
   do_or(Field, Do_or),
   do_ok(Field, Name, Do_ok),
   do_lpar(Field, Do_lpar),
   do_rpar(Field,  Do_rpar),
   process_number(P, Val, S), 
   check_if_more(Occurs, Name, MoreField, More).

html_format("date", Occurs,
	    [Name, Field, OnField, Occ, MoreField, 
	     Exp, Field1, Field2, Field3],_, P,
	    [strong(Name), em(' (yyyy-mm-dd)') , 
	     begin(table), 
	     nl, begin(tbody),
	     nl, begin(tr), begin(td),
	     nl, end(td),
     	     nl, begin(td),
	     nl, select([name=OnField], S),
	     nl, input(number, [name=Field1,size=4]), verbatim('-'),
	     nl, input(number, [name=Field2, size=2]), verbatim('-'),
	     nl, input(number, [name=Field3, size=2]),
     	     nl, input(button, [onClick=Do_ok, value=ok]),
	     nl, input(button, [onClick=Do_and, value=and]),
	     nl, input(button, [onClick=Do_or, value=or]), 
	     nl, input(button, [onClick=Do_lpar, value='(']),
	     nl, input(button, [onClick=Do_rpar, value=')']),\\,
	     nl, input(text, [name=Exp, size=48, value=Val]),
	     nl, end(td), end(tr),
	     nl, begin(tr), begin(td), end(td),
	     nl, begin(td),
	     nl, More,
     	     nl, end(td), end(tr),
	     nl, end(tbody),
	     nl, end(table),
	     nl, input(hidden, [name=Occ, value=Occurs])]) :-
   do_and(Field, Do_and),
   do_or(Field, Do_or),
   do_ok_date('-', Field, Field1, Field2, Field3, Name, Do_ok),
   do_lpar(Field, Do_lpar),
   do_rpar(Field,  Do_rpar),
   process_number(P, Val, S), 
   check_if_more(Occurs, Name, MoreField, More).

html_format("time", Occurs,
	    [Name, Field, OnField, Occ, MoreField, 
	     Exp, Field1, Field2, Field3],_, P,
	    [strong(Name), em(' (hh:mm:ss)') , 
	     begin(table), 
	     nl, begin(tbody),
	     nl, begin(tr), begin(td),
	     nl, end(td),
     	     nl, begin(td),
	     nl, select([name=OnField], S),
	     nl, input(number, [name=Field1,size=2]), verbatim(':'),
	     nl, input(number, [name=Field2, size=2]), verbatim(':'),
	     nl, input(number, [name=Field3, size=2]),
     	     nl, input(button, [onClick=Do_ok, value=ok]),
	     nl, input(button, [onClick=Do_and, value=and]),
	     nl, input(button, [onClick=Do_or, value=or]), 
	     nl, input(button, [onClick=Do_lpar, value='(']),
	     nl, input(button, [onClick=Do_rpar, value=')']),\\,
	     nl, input(text, [name=Exp, size=48, value=Val]),
	     nl, end(td), end(tr),
	     nl, begin(tr), begin(td), end(td),
	     nl, begin(td),
	     nl, More,
     	     nl, end(td), end(tr),
	     nl, end(tbody),
	     nl, end(table),
	     nl, input(hidden, [name=Occ, value=Occurs])]) :-
   do_and(Field, Do_and),
   do_or(Field, Do_or),
   do_ok_date(':', Field, Field1, Field2, Field3, Name, Do_ok),
   do_lpar(Field, Do_lpar),
   do_rpar(Field,  Do_rpar),
   process_number(P, Val, S), 
   check_if_more(Occurs, Name, MoreField, More).


html_format("enumeration", Occurs,
	    [Name, Field, OnField, Occ, MoreField], Values, P,
	    [strong(Name), 
	     nl, begin(table), 
	     nl, begin(tbody),
	     nl, begin(tr), begin(td),
	     nl, select([name=OnField],S),
	     nl, end(td),
	     nl, begin(td),
	     nl, select([name=Field, multiple, size=Size], Options),
	     nl, end(td), end(tr),
	     nl, begin(tr), begin(td), end(td),
	     nl, begin(td),
	     nl, More,
     	     nl, end(td), end(tr),
	     nl, end(tbody),
	     nl, end(table),
	     nl, input(hidden, [name=Occ, value=Occurs])]) :-
        length(Options, N),
	size_min(IN),
	size_max(MN),
        set_size(N, IN, MN, Size),
        process_enumeration(P, S),
	get_options(Values, P, Options), 
	check_if_more(Occurs, Name, MoreField, More).

set_size(0, _IN, _MN, 0).
set_size(N, IN, _, N) :-	N =< IN.
set_size(N, IN, MN, IN) :-	N > IN, N =< MN.
set_size(_, _, MN, MN).

get_options(Values, [], Options) :- 
	get_options_aux(Values, [], Options).
get_options(Values, [_Op, ValList], Options) :- 
	get_options_aux(Values, ValList, Options).

get_options_aux([], _, []).
get_options_aux([Value|Vs], ValList, [Option|Os]) :-
	name(ValueA, Value),
	(member(ValueA, ValList) ->
	 Option=[nl, option([value=Value, selected], Value)]
	;
	 Option=[nl, option([value=Value], Value)]),
	get_options_aux(Vs, ValList, Os).
     

do_and(Field, Do_and) :-
	atom_concat(['do_and(', '''', Field, '''', ')'], Do_and).

do_or(Field, Do_or) :-
	atom_concat(['do_or(', '''', Field, '''', ')'], Do_or).

do_ok(Field, Name, Do_ok) :-
	atom_concat(['do_ok(', '''', Field, '''', ',', '''', Name, '''', ')'], Do_ok).


do_ok_date(Sep, Field, Field1, Field2, Field3, Name, Do_ok) :-
	atom_concat(['do_ok_date(', '''', Sep, '''', ',', '''', Field, '''', ',', '''', Field1, '''', ',', '''', Field2, '''', ',', '''', Field3, '''', ',', '''', Name, '''', ')'], Do_ok).

do_lpar(Field, Do_lpar) :-
	atom_concat(['do_lpar(', '''', Field, '''', ')'], Do_lpar).

do_rpar(Field, Do_rpar) :-
	atom_concat(['do_rpar(', '''', Field, '''', ')'], Do_rpar).


process_string([], '', S) :-
	S=[nl, option([value=ignore, selected], 'Ignore'),
	   nl, option([value='='], 'Equal'),
	   nl, option([value='!='], 'Not equal')].
process_string([ignore, Val], Val, S) :-
	S=[nl, option([value=ignore, selected], 'Ignore'),
	   nl, option([value='='], 'Equal'),
	   nl, option([value='!='], 'Not equal')].
process_string(['=', Val], Val, S) :-
	S=[nl, option([value=ignore], 'Ignore'),
	   nl, option([value='=', selected], 'Equal'),
	   nl, option([value='!='], 'Not equal')].
process_string(['!=', Val], Val, S) :-
	S=[nl, option([value=ignore], 'Ignore'),
	   nl, option([value='='], 'Equal'),
	   nl, option([value='!=', selected], 'Not equal')].

process_number([], '', S) :-
	S=[nl, option([value=ignore, selected], 'Ignore'),
	   nl, option([value='='], '='),
	   nl, option([value='!='], '!='),
	   nl, option([value='<'], '<'),
	   nl, option([value='<='], '<='),		    
	   nl, option([value='>'], '>'),
	   nl, option([value='>='], '>=')].
process_number([ignore, Val], Val, S) :-
	S=[nl, option([value=ignore, selected], 'Ignore'),
	   nl, option([value='='], '='),
	   nl, option([value='!='], '!='),
	   nl, option([value='<'], '<'),
	   nl, option([value='<='], '<='),		    
	   nl, option([value='>'], '>'),
	   nl, option([value='>='], '>=')].
process_number(['=', Val], Val, S) :-
	S=[nl, option([value=ignore], 'Ignore'),
	   nl, option([value='=', selected], '='),
	   nl, option([value='!='], '!='),
	   nl, option([value='<'], '<'),
	   nl, option([value='<='], '<='),		    
	   nl, option([value='>'], '>'),
	   nl, option([value='>='], '>=')].
process_number(['!=', Val], Val, S) :-
	S=[nl, option([value=ignore], 'Ignore'),
	   nl, option([value='='], '='),
	   nl, option([value='!=', selected], '!='),
	   nl, option([value='<'], '<'),
	   nl, option([value='<='], '<='),		    
	   nl, option([value='>'], '>'),
	   nl, option([value='>='], '>=')].
process_number(['<', Val], Val, S) :-
	S=[nl, option([value=ignore], 'Ignore'),
	   nl, option([value='='], '='),
	   nl, option([value='!='], '!='),
	   nl, option([value='<', selected], '<'),
	   nl, option([value='<='], '<='),		    
	   nl, option([value='>'], '>'),
	   nl, option([value='>='], '>=')].
process_number(['<=', Val], Val, S) :-
	S=[nl, option([value=ignore], 'Ignore'),
	   nl, option([value='='], '='),
	   nl, option([value='!='], '!='),
	   nl, option([value='<'], '<'),
	   nl, option([value='<=', selected], '<='),		    
	   nl, option([value='>'], '>'),
	   nl, option([value='>='], '>=')].
process_number(['>', Val], Val, S) :-
	S=[nl, option([value=ignore], 'Ignore'),
	   nl, option([value='='], '='),
	   nl, option([value='!='], '!='),
	   nl, option([value='<'], '<'),
	   nl, option([value='<='], '<='),		    
	   nl, option([value='>', selected], '>'),
	   nl, option([value='>='], '>=')].
process_number(['>=', Val], Val, S) :-
	S=[nl, option([value=ignore], 'Ignore'),
	   nl, option([value='='], '='),
	   nl, option([value='!='], '!='),
	   nl, option([value='<'], '<'),
	   nl, option([value='<='], '<='),		    
	   nl, option([value='>'], '>'),
	   nl, option([value='>=', selected], '>=')].

process_enumeration([], S) :-
	S=[nl, option([value=ignore, selected], 'Ignore'),
    	   nl, option([value='in'], 'in'),
	   nl, option([value='not in'], 'not in')].
process_enumeration([ignore,_], S) :-
	S=[nl, option([value=ignore, selected], 'Ignore'),
    	   nl, option([value='in'], 'in'),
	   nl, option([value='not in'], 'not in')].
process_enumeration([in,_], S) :-
	S=[nl, option([value=ignore], 'Ignore'),
    	   nl, option([value=in, selected], 'in'),
	   nl, option([value='not in'], 'not in')].
process_enumeration(['not in',_], S) :-
	S=[nl, option([value=ignore], 'Ignore'),
    	   nl, option([value='in'], 'in'),
	   nl, option([value='not in', selected], 'not in')].

% make_more(Field, checkbox(Field, off)).

check_if_more(Occurs, Name, MoreField, More) :-
	(Occurs="ONEORMORE";Occurs="ZEROORMORE"),
	More=[em('Click '),
	      input(submit,[name=MoreField, value=here]),
%	      checkbox(MoreField, off),
	      em(' to add another "'),
	      strong(Name),
	      em('" element to the form')].
check_if_more(_,_,_,[]).
