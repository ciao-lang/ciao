:- module(regexp_code, 
        [
            match_shell/3,
            match_shell/2,
            match_posix/2,
            match_posix/4,
            match_posix_rest/3,
	    match_posix_matches/3,
            match_struct/4,
            match_term/2,
            replace_first/4,
            replace_all/4
        ], 
        [assertions, dcg, regtypes, define_flag]).

:- use_module(library(lists), [append/3, reverse/2]).

:- op(25, fy, (^)).


%% This one does not need to appear in the documentation.
:- doc(nodoc, define_flag/3).

% TODO: this should be a parameter of the match predicates (JFMC)
define_flag(case_insensitive,[on,off],off).


:- export(shell_regexp/1).
:- regtype shell_regexp(P) # "@var{P} is a shell regular expression to
   match against.".

shell_regexp(P) :-
	string(P).

:- regtype string(S) # "@var{S} is a string.".

:- pred match_shell(Exp, IN, Rest) : shell_regexp * string * string
   # "Matches @var{IN} against @var{Exp}. @var{Rest} is
   the longest remainder of the string after the match. For example,
   @tt{match_shell(\"??*\",\"foo.pl\",Tail)} succeeds,
   instantiating @tt{Tail} to @tt{\"o.pl\"}.".

:- pred match_shell(Exp, IN) : shell_regexp * string 
        # "Matches completely @var{IN} (no tail can remain unmatched)
          against @var{Exp} similarly to @pred{match_shell/3}.". 

match_shell(Exp, IN) :- 
	match_shell(Exp, IN, []).

match_shell(Exp, IN, Rest) :-
	is_case_insensitive(Exp,Exp2),
	is_case_insensitive(IN,IN2),
	shell_to_struct(Exp2,Pattern),!,
	match_struct(Pattern, _, IN2, Rest).

shell_to_struct(Exp,Pattern) :-
	get_alternative(Exp,Alternative,yes,Rest),
	complete_shell(Rest,yes,Alternative,yes,Pattern,[]).

complete_shell([],_,Pattern,_,Pattern,[]) :- !.
complete_shell([0'}|RestExp],_,Pattern,_,Pattern,RestExp) :- !.
complete_shell([0'||RestExp],First,Alternative,FirstAlter,Pattern,End) :- 
	get_alternative(RestExp,Alternative2,First,Rest),
	unite_alternatives(FirstAlter,Alternative,Alternative2,Preff),
	complete_shell(Rest,First,Preff,no,List,End),
	(
	    FirstAlter = yes ->
	    reverse(List,ReverseList),
	    Pattern = or(ReverseList)
	;
	    Pattern = List
	).

unite_alternatives(yes,Alternative,Alternative2,[Alternative2,Alternative]).
unite_alternatives(no,Alternative,Alternative2,[Alternative2|Alternative]).

get_alternative([],[],_,[]).
get_alternative([0'*|Rest],Pattern,First,End) :-
        !, get_alternative(Rest,Pattern2,no,End),
	(
	    First == no ->
	    Pattern = [*(any)|Pattern2]
	;
	    Pattern = [not_in_cond([0'.],simple),*(any)|Pattern2]
        ).

get_alternative([0'?|Rest],[Preff|Pattern],First,End) :-
        !, get_alternative(Rest,Pattern,no,End),
	(
	    First == no ->
	    Preff = inter(any,0,1)
	;
	    Preff = not_in_cond([0'.],double)
	).

get_alternative([0'[, 0'^|RestIn],[not_in(Chars)|RestPattern],First,End) :-
        !, read_chars(RestIn,no,_,Chars,Rest),
        get_alternative(Rest,RestPattern,First,End).

get_alternative([0'[|RestIn],[in(Chars)|RestPattern],First,End) :-
        !, read_chars(RestIn,no,_,Chars,Rest),
        get_alternative(Rest,RestPattern,First,End).

get_alternative([0'||Rest],[],_,[0'||Rest]) :- !.

get_alternative([0'}|Rest],[],_,[0'}|Rest]) :- !.

get_alternative([0'{,0'}|Rest],Pattern,First,End) :-
	!, get_alternative(Rest,Pattern,First,End).

get_alternative([0'{|Rest],[Alternatives|RestPattern],First,End) :- 
	!, get_alternative(Rest,Alternative,First,PostAlternative),
        complete_shell(PostAlternative,First,Alternative,yes,Alternatives,PostKey),
	get_alternative(PostKey,RestPattern,First,End).

get_alternative([0'\\,Ch|RestIn],[Ch|Pattern],_,End) :-
        !, get_alternative(RestIn,Pattern,no,End).

get_alternative([Ch|RestIn],[Ch|Pattern],_,End) :-
        !, get_alternative(RestIn,Pattern,no,End).


:- doc(regexp_posix/1,"Special characters for @var{regexp_posix} are:
  @begin{description}
  @item{*} Matches zero or more ocurrences of the previous group of characters.
  @item{+} Matches one or more ocurrences of the previous group of characters.
  @item{.} Matches any single character.
  @item{[...]} Matches any one of the enclosed characters.  A pair of
   characters separated by a minus sign denotes a range; any character
   lexically between those two characters, inclusive, is matched.  If the
   first character following the [ is a ^ then any character not enclosed
   is matched.  No other character is special inside this construct.  To
   include a ] in a character set, you must make it the first character.
   To include a `-', you must use it in a context where it cannot possibly
   indicate a range: that is, as the first character, or immediately after
   a range.
      Finally, certain named classes of characters are predefined within
   bracket expressions, as follows. Their names are self explanatory, and
   they are [:alnum:], [:alpha:], [:cntrl:], [:digit:], [:graph:],
   [:lower:], [:print:], [:punct:], [:space:], [:upper:], and [:xdigit:].
   For  example, [[:alnum:]] means [0-9A-Za-z].
  @item{(...)} Groups characters to form compound elements.
  @item{|} Specifies an alternative.  Two @var{regexp_posix} A and B with
   | in between form an expression that matches anything that either A or B
   will match.
  @item{@{n,m@}} Matches between n and m (both inclusive) ocurrences of
   previous group of characteres. {n} is equivalent to {n,n}, {,n} is
   equivalent to {0,n}, and {n,} is equivalent to {n,infinite}.
  @item{\\} Quotes a special character (including itself).
 @end{description}").

:- export(posix_regexp/1).
:- regtype posix_regexp(P) # "@var{P} is a posix regular expression to match against.".

posix_regexp(P) :-
	string(P).

:- pred match_posix(Exp, IN) : shell_regexp * string 
        # "Matches completely @var{IN} (no tail can remain unmatched)
          against @var{Exp} similarly to @pred{match_posix/3}.". 

match_posix(Exp, IN) :-
	match_posix(Exp, IN, _, []).

:- pred match_posix_rest(Exp, IN, Rest) : posix_regexp * string * string
    # "Matches @var{IN} against @var{Exp}. @var{Tail} is
       the remainder of the string after the match. For example,
       @tt{match_posix(\"ab*c\",\"abbbbcdf\",Tail)} succeeds,
       instantiating @tt{Tail} to @tt{\"df\"}.".

match_posix_rest(Exp, IN, R) :-
	match_posix(Exp, IN, _, R).

:- pred match_posix_matches(Exp, IN, Matches) 
        : shell_regexp * string * list(string)

# "Matches completely @var{IN} against @var{Exp}.  @var{Exp} can
contain @em{anchored expressions} of the form \\(@tt{regexp}\\).
@var{Matches} will contain a list of the @em{anchored expression}
which were matched on success.  Note that since POSIX expressions are
being read inside a string, backslashes will have to be doubled.  For
example,

@begin{verbatim}
?- match_posix_matches(\"\\\\(aa|bb\\\\)\\\\(bb|aa\\\\)\", \"bbaa\", M).
M = [\"bb\",\"aa\"] ? ;
no

?- match_posix_matches(\"\\\\(aa|bb\\\\)\\\\(bb|aa\\\\)\", \"aabb\", M).
M = [\"aa\",\"bb\"] ? ;
no
@end{verbatim}".


match_posix_matches(Exp, IN, Match) :-
	match_posix(Exp, IN, Match, []).

:- pred match_posix(Exp, In, Match, Rest) 
        : shell_regexp * string * list(string) * string
        # "".

match_posix(Exp, IN, Match, Rest) :-
	is_case_insensitive(Exp,Exp2),
	is_case_insensitive(IN,IN2),
	posix_to_struct(Exp2,[],Pattern,[],N),!,
	(
	    var(Match) -> 
	    create_list(N,Match_Old),
	    match(Pattern, _, Match_Old, Match, IN2, Rest)
	;
	    match(Pattern, _, Match, Match, IN2, Rest)
        ).
	
create_list(0,[]) :- !.
create_list(N,[[]|Match]) :- M is N -1, create_list(M,Match).

posix_to_struct(Exp,End,Pattern,RestPattern,M) :-
	next_element(Exp,_,no,Pattern2,RestPattern2,0,N),
	complete_posix(End,RestPattern2,yes,Pattern2,Pattern,RestPattern,N,M).

complete_posix(End,End,yes,Pattern,Pattern,End,N,N) :- !.
complete_posix(End,End,no,Pattern,[Pattern],End,N,N) :- !.
complete_posix(End,[0'||RestExp],First,Pattern2,Pattern,RestPattern,N,M) :- 
	next_element(RestExp,_,no,Pattern3,RestPattern2,N,Temp),
	complete_posix(End,RestPattern2,no,Pattern3,Pattern4,RestPattern,Temp,M),
	test_first(First,Pattern2,Pattern4,Pattern).

test_first(yes,Pattern2,Pattern3,or([Pattern2|Pattern3])).
test_first(no,Pattern2,Pattern3,[Pattern2|Pattern3]).


put_preff(yes,Preff,Rest,[Preff|Rest]).
put_preff(no,_,Pattern,Pattern).

next_element([],Preff,yes,[Preff],[],N,N).
next_element([],_,no,[],[],N,N).
next_element([0'||Rest],Preff,yes,[Preff],[0'||Rest],N,N) :- !.

next_element([0'*|Rest],Preff,YesPreff,Pattern,RestOut,N,M) :- 
	!, YesPreff = yes,      
        next_element(Rest,*(Preff),yes,Pattern,RestOut,N,M).

next_element([0'+|Rest],Preff,YesPreff,Pattern,RestOut,N,M) :- 
	!, YesPreff = yes,
        next_element(Rest,+(Preff),yes,Pattern,RestOut,N,M).

next_element([0'\\,0'(|RestIn],Preff,YesPreff,Pattern,RestOut,N,M) :- 
        !, Temp1 is N + 1, next_element(RestIn,_,no,PatternParent2,RestTemp,Temp1,Temp2),
	complete_posix([0'\\, 0')|_],RestTemp,yes,PatternParent2,PatternParent,[0'\\, 0')|RestTemp2],Temp2,Temp3),
	next_element(RestTemp2,obtain(PatternParent,N),yes,Pattern2,RestOut,Temp3,M),
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'\\, 0')|Rest],Preff,YesPreff,[Preff],RestOut,N,N) :- 
	!, YesPreff = yes, RestOut = [0'\\, 0')|Rest].

next_element([0'\\,Ch|Rest],Preff,YesPreff,Pattern,RestOut,N,M) :-
        !, next_element(Rest,Ch,yes,Pattern2,RestOut,N,M),
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'.|Rest],Preff,YesPreff,Pattern,RestOut,N,M) :-
        !, next_element(Rest,any,yes,Pattern2,RestOut,N,M), 
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'[, 0'^|RestIn],Preff,YesPreff,Pattern,RestOut,N,M) :-
        !, read_chars(RestIn,no,_,Chars,Rest),
        next_element(Rest,not_in(Chars),yes,Pattern2,RestOut,N,M),
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'[|RestIn],Preff,YesPreff,Pattern,RestOut,N,M) :-
        !, read_chars(RestIn,no,_,Chars,Rest),
        next_element(Rest,in(Chars),yes,Pattern2,RestOut,N,M),
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'{|Rest],Preff,YesPreff,Pattern,RestOut,N,M) :- 
	!, YesPreff = yes,
	read_number(Rest,0,N,[Ch|Rest2]),
	read_number2(Ch,Rest2,N,M,Rest3),
        next_element(Rest3,inter(Preff,N,M),yes,Pattern,RestOut,N,M).

next_element([X|Rest],Preff,YesPreff,Pattern,RestOut,N,M) :-
        next_element(Rest,X,yes,Pattern2,RestOut,N,M),
	put_preff(YesPreff,Preff,Pattern2,Pattern).


read_chars([0']|Rest],YesPreff,Preff,[Preff],Rest) :- 
	!, YesPreff == yes.

read_chars([0'\\,Ch,0'-,0'\\,Ch2|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, Ch < Ch2, read_chars(Rest,yes,rank(Ch,Ch2),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'\\,Ch,0'-,Ch2|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, Ch < Ch2, read_chars(Rest,yes,rank(Ch,Ch2),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'\\,Ch|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,Ch,RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([Ch,0'-,0'\\,Ch2|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, Ch < Ch2, read_chars(Rest,yes,rank(Ch,Ch2),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([Ch,0'-,Ch2|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, Ch < Ch2, read_chars(Rest,yes,rank(Ch,Ch2),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'[,0':,0'a,0'l,0'n,0'u,0'm,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'0,0'9),RestChars,RestOut),
	put_preff(yes,rank(0'a,0'z),RestChars,RestChars2),
	put_preff(yes,rank(0'A,0'Z),RestChars2,RestChars3),
	put_preff(YesPreff,Preff,RestChars3,Chars).
read_chars([0'[,0':,0'a,0'l,0'p,0'h,0'a,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'A,0'Z),RestChars,RestOut),
	put_preff(yes,rank(0'a,0'z),RestChars,RestChars2),
	put_preff(YesPreff,Preff,RestChars2,Chars).
read_chars([0'[,0':,0'd,0'i,0'g,0'i,0't,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'0,0'9),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'[,0':,0'l,0'o,0'w,0'e,0'r,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'a,0'z),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'[,0':,0'u,0'p,0'p,0'e,0'r,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'A,0'Z),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([Ch|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,Ch,RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).

digit(X) :- !, X > 47, !, X < 59.

read_number([D|Rest],Prev,N,RestOut) :- 
	(
	    digit(D) ->
	    Prev2 is Prev * 10,
	    Prev3 is Prev2 + D - 48,
	    read_number(Rest,Prev3,N,RestOut)
	;
	    N = Prev,
	    RestOut = [D|Rest]
	).
	
read_number2(0'},Rest,N,N,Rest).
read_number2(0',,Rest,_,N,RestOut) :-
	(
	    Rest = [0'}|RestOut] ->
	    N = inf
        ; 
	    read_number(Rest,0,N,[0'}|RestOut])
        ).


:- doc(regexp_struct/1,"Special predicates for @var{regexp_struct} are:
  @begin{description}
  @pred{*(C)} Matches zero or more ocurrences of the @var{C}.
  @pred{+(C)} Matches one or more ocurrences of the @var{C}.
  @pred{any/0} Matches any single atom.
  @pred{in(L)} Matches any one of the characters in the list @var{L}. In the
   list @var{L} can be atoms and the predicate @pred{rank/2}, that matches
   any character between the first argument and the second one (both inclusive).
  @pred{not_in(L)} Is the opposite of @pred{in/1}.
  @item{L} Matches secuences according to the list @var{L}.
  @item{or(L)} Specifies an alternative. Matches any of the element of the list @var{L}.
  @item{inter(X,N,M} Matches between @var{N} and @var{M} (both inclusive) ocurrences of
   the atom @var{X}. @var{M} can be inf.
  @op{^} Quotes a special predicate (including itself).
 @end{description}").

:- export(struct_regexp/1).
:- regtype struct_regexp(P) # "@var{P} is a struct regular expression to match against.".

struct_regexp(_).

:- pred match_struct(Exp, IN, Rest, Tail)
	:: (struct_regexp(Exp), string(IN), string(Rest))
    # "Matches @var{IN} against @var{Exp}. @var{Tail} is
       the remainder of the list of atoms @var{IN} after the match. For example,
       @tt{match_struct([a,*(b),c],[a,b,b,b,c,d,e],Tail)} succeeds,
       instantiating @tt{Tail} to @tt{[d,e]}.".

insert(0,Match,[_|Rest],[Match|Rest]) :- !.
insert(N,Match,[H|RestMatch_Old],[H|RestMatch_New]) :- 
	M is N -1, 
	insert(M,Match,RestMatch_Old,RestMatch_New).

match_struct(Pattern,L_Match) --> !,  %There is not each OBTAIN --> do []
	[], 
	match(Pattern,_,L_Match,L_Match).

match(^(X),[X],L,L) --> !, [X].
match([],[],L,L) --> !, [].
match([E|Es],Match,L_Match_Old,L_Match_New) --> !, 
	match(E,Match_E,L_Match_Old,L_Match_Tmp), 
	match(Es,Match_Es,L_Match_Tmp,L_Match_New), 
	{append(Match_E,Match_Es,Match)}.
match(*(X), Match,L_Match_Old,L_Match_New) --> !, 
	([], {Match = [], L_Match_New = L_Match_Old} ;
	 match(X,Match_X,L_Match_Old,L_Match_Tmp), 
	 match(*(X),Match_Star,L_Match_Tmp,L_Match_New), 
	 {append(Match_X,Match_Star,Match)}).
match(+(X),Match,L_Match_Old,L_Match_New) --> !, 
	match(X,Match_X,L_Match_Old,L_Match_Tmp), 
	match(*(X), Match_Plus,L_Match_Tmp,L_Match_New), 
	{append(Match_X,Match_Plus,Match)}.
match(any,[X],L,L) --> !, [X].
match(obtain(Pattern,N),Match,L_Match_Old,L_Match_New) --> !, 
	[], 
	match(Pattern,Match,L_Match_Old,L_Match_Tmp),
	{insert(N,Match,L_Match_Tmp,L_Match_New)}.
match(or([X|Xs]),Match,L_Match_Old,L_Match_New) --> !, 
	(match(X,Match,L_Match_Old,L_Match_New) ; 
	 match(or(Xs),Match,L_Match_Old,L_Match_New)). 
match(inter(X,N,M),Match,L_Match_Old,L_Match_New) --> !, 
	({N == 0}, [], {Match = [], L_Match_New = L_Match_Old}; 
	 {N == 0}, {(M > 0; M = inf)}, 
	 match(X,Match_X,L_Match_Old,L_Match_Tmp), 
	 {decrement(M,M2)}, 
	 match(inter(X,0,M2),Match_Inter,L_Match_Tmp,L_Match_New), 
	 {append(Match_X,Match_Inter,Match)} ; 
	 {N > 0}, match(X,Match_X,L_Match_Old,L_Match_Tmp), 
	 {N2 is N - 1, decrement(M,M2)}, 
	 match(inter(X,N2,M2),Match_Inter,L_Match_Tmp,L_Match_New), 
	 {append(Match_X,Match_Inter,Match)}).
match(not_in(Chars),[Ch],L,L) --> !, 
	[Ch], 
	{\+(contains(Chars,Ch))}.
match(in(Chars),[Ch],L,L) --> !, 
	[Ch], 
	{contains(Chars,Ch)}.
match(not_in_cond(Chars,Type),Match,L,L) --> !,
	not_in_cond(Chars,Type,Match).
match(Ch,[Ch],L,L) --> [Ch].

decrement(inf,inf).
decrement(M,M2) :- M2 is M - 1.

not_in_cond(_,_,[],[],[]).
not_in_cond([],_,[],Rest,Rest).
not_in_cond([],double,[Ch],[Ch|Rest],Rest).
not_in_cond([First|RestChars],Type,Match,[Ch|Rest],RestOut) :-
	First =\= Ch,
	not_in_cond(RestChars,Type,Match,[Ch|Rest],RestOut).

contains([rank(A,B)|_],Ch) :-
	Ch >= A,
	Ch =< B,
	!.
contains([Ch|_],Ch2) :-
	Ch == Ch2, !.
contains([_|Rest],Ch2) :-
	contains(Rest,Ch2).

is_case_insensitive(IN,OUT) :-
	current_prolog_flag(case_insensitive,Value),
	is_case_insensitive_(Value,IN,OUT).

is_case_insensitive_(off,OUT,OUT).
is_case_insensitive_(on,[],[]).
is_case_insensitive_(on,[Ch|Rest],[ChLow|RestLow]) :-
	Ch >= 0'A, 
	Ch =< 0'Z, !,
	ChLow is Ch - 0'A + 0'a,
	is_case_insensitive_(on,Rest,RestLow).

is_case_insensitive_(on,[Ch|Rest],[Ch|RestLow]) :-
	is_case_insensitive_(on,Rest,RestLow).
	
:- pred match_term(Term1, Term2) # "Tests if two terms
   @var{Term1} and @var{Term2} match using shell regular expressions.".

match_term(Pattern, Term):-
	(var(Pattern); var(Term)), !.
match_term(Pattern, Term):-
	Pattern =.. PatternList,
	Term =.. PatternTerm,
	match_term_(PatternList, PatternTerm).

match_term_([], []).
match_term_([A1|A1s], [A2|A2s]):-
	(var(A1); var(A2)), !,
	match_term_(A1s, A2s).
match_term_([A1|A1s], [A2|A2s]):-
	name(A1, A1l),
	is_case_insensitive(A1l,A1llow),
	name(A2, A2l),
	is_case_insensitive(A2l,A2llow),
	match_shell(A1llow, A2llow,[]),
	match_term_(A1s, A2s).

:- pred replace_first(IN,Old,New,Resul) : string * posix_regexp * string * string
   # "Replace the first ocurrence of the @var{Old} by @var{New} in 
   @var{IN} and copy the result in @var{Resul}.".

replace_first(IN,Old,New,Resul) :- 
	posix_to_struct(Old,[],Pattern,[],_),
	replace(IN,first,Pattern,New,Resul).

:- pred replace_all(IN,Old,New,Resul) : string * posix_regexp * string * string
   # "Replace all ocurrences of the @var{Old} by @var{New} in 
   @var{IN} and copy the result in @var{Resul}.".

replace_all(IN,Old,New,Resul) :- 
	posix_to_struct(Old,[],Pattern,[],_),
	replace(IN,all,Pattern,New,Resul).

replace([],_,_,_,[]).
replace([IN|RestIN],Type,Pattern,New,Resul) :- 
	(
	    match_struct(Pattern,_,[IN|RestIN],Rest) ->
	    construct(Type,New,Pattern,Rest,Resul)
	;
	    replace(RestIN,Type,Pattern,New,ResulRest),
	    Resul = [IN|ResulRest]
	). 

construct(first,Preff,_,Rest,Resul) :- append(Preff,Rest,Resul).
construct(all,Preff,Pattern,Rest,Resul) :- 
	replace(Rest,all,Pattern,Preff,RestConvert),
	append(Preff,RestConvert,Resul).
