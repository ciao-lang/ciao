:- module(term_list,
        [
            push_term/3,
            push_meta/3,
            collapse_terms/3
        ],
        [assertions, regtypes]).

:- use_module(library(iso_misc), [compound/1]). % see bug comment in iso_misc
:- use_module(library(lists),    [select/3]).
:- use_module(library(assertions/native_props), [nonground/1]).

:- doc(author, "Edison Mera").
:- doc(author, "Nataliia Stulova (documentation)").

:- doc(module, "Predicates tht manipulate lists of terms during the
        rtchecks source expansion.").

:- regtype eq/1.

eq(_=_).

% ----------------------------------------------------------------------

% push_term(E,L,V)
% push_meta(E,L,V)
%
% INPUT
% E | location information of the form:
%   |   loc(+sourcename,+int,+int) assertion locator term
%   |   predloc(-var,-var)
%   |   asrloc(loc(+sourcename,+int,+int))
%   | or goal expansion of the form
%   |   '$meta$rtc'(+struct,-struct)
%
% OUTPUT
% L | list of the expanded PName goal PredName and all locations data
%   |   ['$meta$rtc'(PName,PredName),  \
%   |    loc(SRCp,LBp,LEp)=Lp,          \ either of these elements is
%   |    predloc(PredName,Lp)=Lpa,      / added to L if it was not there
%   |    asrloc(loc(SRCa,LBa,LEa))=La  /  on call time
%   |   | _ ]
% V | free variable from the term L that corresponds to E, be it Lp, Lpa
%   | or La; or the 2nd argument of the '$meta$rtc/2' term

:- pred push_term(E, L, V) :: term * list(eq) * term
        # "if @var{E} is missing from @var{L}, a pair of the form
           @var{E} = @var{V} is added to @var{L}".

push_term(E, L, V) :-
	member(T=V, L),
	(
	    T==E
	;
	    var(T),
	    T = E
	),
	!.

push_meta(E, L, V) :-
	member('$meta$rtc'(T, V), L),
	(
	    T==E
	;
	    var(T),
	    T = E
	),
	!.

% ----------------------------------------------------------------------

:- pred collapse_terms(Terms, L0, L)
        : (list(Terms), list(L0), var(L)) => list(L).
collapse_terms(G, L0, L) :-
	count_vars(G, [], C),
	collapse_term(L0, C, L, []).

% collapse_term(Terms, Counts, CTerms, Tail)
%
% INPUT
% Terms  | list of the expanded PName goal PredName and locations data
%        |   ['$meta$rtc'(PName,PredName),
%        |    loc(SRCp,LBp,LEp)=Lp,
%        |    predloc(PredName,Lp)=Lpa,
%        |    asrloc(loc(SRCa,LBa,LEa))=La | ...]
% Counts | list of variable counters of the form =(var,nnegint)
% Tail   | tail of the CTerms list
%
% OUTPUT
% Counts | list of counters of the form =(var,nnegint) or
%        | =(struct,nnegint) where strict is of the form of
%        | the list elements in Terms if variable unifications
%        | took place
% CTerms | reduced version of Terms. Contains only those elements
%        | whose counters are >1 or the '$meta$rtc'/2 elements

:- pred collapse_term(Terms, Counts, CTerms, Tail)
        :  (list(Terms, eq), list(Counts, eq), var(CTerms), list(Tail))
        => list(CTerms)
        # "Given the list of variable counters @var{Counts} and a list
           of terms @var{Terms}, where some of the variables from
           @var{Counts} appear in list elements, obtains from
           @var{Terms} a reduced list @var{CTerms} with tail
           @var{Tail}. On success some variables in @var{Counts}
           become more instantiated.".
collapse_term([],       _,  R,  R).
collapse_term([E|L0], C0, R0, R) :-
	!,
	(
	    E = (T=V),
	    (
		(
		    member(V0=N, C0),
		    V0==V ->
		    (
			(N==1 ; atomic(T)) ->
			 T=V, % unification!
			 R0 = R1
		    ;
			R0 = [T=V|R1]
		    ),
		    count_vars(T, C0, C)
                ;
		    R0 = R1,
		    C = C0
		)
	    ) -> true
	;
	    R0 = [E|R1],
	    C = C0
	),
	collapse_term(L0, C, R1, R).

%% What is the intended meaning of the predicate above?
%% (third case is strange)
%%
%% ?- collapse_term([f(Y,Y,Z)=X,g(a)=Y,b=Z], [X=0,Y=0,Z=0], R, []).
%%
%% R = [f(Y,Y,b)=X,g(a)=Y],
%% Z = b ?
%%
%% yes
%% ?- collapse_term([f(Y,Z)=X,g(a)=Y,b=Z], [X=0,Y=0,Z=0], R, []).
%%
%% R = [f(g(a),b)=X],
%% Y = g(a),
%% Z = b ?
%%
%% yes
%% ?- collapse_term([f(Y,Z)=X,g(a)=Y,b(Y)=Z], [X=0,Y=0,Z=0], R, []).
%%
%% R = [f(g(a),b(g(a)))=X],
%% Y = g(a),
%% Z = b(g(a)) ?
%%
%% yes

% intended use
:- pred count_vars(Term, Tail, VarCounts)
        :: (list(Term), list(Tail))
        :  (nonground(Term), var(VarCounts)) => list(VarCounts,eq)
        # "For a term @var{Term} produces a list @var{VarCounts} of
           counters of all free variables that appear in @var{Term}.
           @var{Tail} is the tail of @var{VarCounts}. Each element of
           @var{VarCounts} is of the form =(var,nnegint).".
        % see unittest_base:group_list/3 for a similar functionality
% actual use
:- pred count_vars(Term, Counts0, Counts)
        :  (ground(Term), list(Counts0,eq), var(Counts))
        => (Counts0 = Counts)
        # "For a ground term @var{Term} simply unifies its 2nd and 3rd
           arguments on success.".
:- pred count_vars(Term, Counts0, Counts)
        :  (nonground(Term), list(Counts0,eq), var(Counts))
        => list(Counts, eq)
        # "Increments the counters of those variables in @var{Tail},
           that are free in the @var{Term}, and moves them to the head
           of the list, thus obtaining the @var{Counts} from
           @var{Counts0}. Each element of @var{Counts0} is of the form
           =(var,nnegint) or =(gndstr,nnegint).".
count_vars(Term, C0, C) :-
	var(Term),
	!,
	(
	    select(T=N, C0, C1),
	    T==Term ->
	    N1 is N + 1,
	    C=[Term=N1|C1]
	;
	    C=[Term=1|C0]
	).
count_vars(Arg, C0, C) :-
	compound(Arg),
	!,
	count_var(1, Arg, C0, C).
count_vars(_, C, C).

count_var(N, Term, C0, C) :-
	arg(N, Term, Arg),
	!,
	count_vars(Arg, C0, C1),
	N1 is N + 1,
	count_var(N1, Term, C1, C).
count_var(_, _, C, C).
