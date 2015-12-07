:- module(factsdb_rt,
	[ asserta_fact/1, assertz_fact/1, call/1, current_fact/1,
	  retract_fact/1 ],
	[ assertions ]).

:- use_module(engine(internals), [term_to_meta/2]).
:- use_module(library(counters)).
:- use_module(library(read), [read/2]).
:- use_module(library(persdb/persdb_cache)).

:- doc(title, "Filed predicates (runtime)").
:- doc(author, "Francisco Bueno").
:- doc(module,"Runtime module for the @lib{factsdb} package.").

:- doc(doinclude,facts/2).
:- decl facts(PredDesc,Keyword) => predname * keyword
# "Declares the predicate @var{PredDesc} as filed. @var{Keyword} is
   the @concept{identifier of a location} where the file DB
   for the predicate is kept. The location @var{Keyword} is described in
   the @pred{file_alias} predicate, which must contain a fact in
   which the first argument unifies with @var{Keyword}.".

:- doc(doinclude,keyword/1).
:- doc(keyword/1,"See @lib{persdb_rt}. The same conventions for location
	of DB files apply in both packages.").

% ------------------------------------------------------------------------

:- meta_predicate current_fact(fact).
:- pred current_fact(Fact) : callable
# "Version of @pred{data_facts:current_fact/1} for filed predicates.
   The current instance of @var{Fact} is interpreted as a fact and is
   unified with an actual fact in the current definition of the corresponding
   predicate. Therefore, with a fact previously asserted or filed in the DB
   file for the predicate, if it has not been retracted.
   The predicate concerned must be declared as @decl{facts}; if it is not,
   then @pred{data_facts:current_fact/1} is used.".

current_fact(H):-
	cached_call(H,File,T,D), !,
	cache_call(File,T,D).
current_fact(H):-
	data_facts:current_fact(H).

:- multifile '$factsdb$cached_goal'/3.
:- doc('$factsdb$cached_goal'(Spec,Spec,Key), "Predicate @var{Spec} is
   filed within database @var{Key}. Programmers should not define this 
   predicate directly in the program.").

:- data cached/4.

:- multifile persistent_dir/2.
:- data persistent_dir/2.
:- doc(persistent_dir/2, "See @lib{persdb}.").
:- multifile file_alias/2.
:- data file_alias/2.
:- doc(file_alias/2, "See @lib{symfnames}. This predicate is used
	only if @pred{persistent_dir/2} fails.").

% H,T,D have all the same arguments, and their functors are related by:
% term_to_meta(T,H), functor(T,P,A), module_concat(M,P,F), functor(D,F,A)
cached_call(H,File,T,D):-
	data_facts:current_fact(cached(H,T,D,File)), !.
cached_call(H,File,T,D):-
	'$factsdb$cached_goal'(H,D,Sym),
	term_to_meta(T,H),
	functor(T,F,A),
	( ( persistent_dir(Sym,Dir) ; file_alias(Sym,Dir) )
	-> get_pred_files(Dir,default,F,A,File,_ops,_bak)
	 ; throw(error(unknown_symbolic_name(Sym),'factsbd_rt:call'/1))
	),
	functor(D,N,A),
	functor(Data,N,A),
	'$factsdb$cached_goal'(Meta,Data,Sym),
	term_to_meta(Term,Meta),
	data_facts:asserta_fact(cached(Meta,Term,Data,File)).

:- meta_predicate call(fact).
:- pred call(Fact) : callable
# "Same as @pred{current_fact/1} if the predicate concerned is declared as
   @decl{facts}. If it is not, an exception is raised.".

call(H):-
	cached_call(H,File,T,D), !,
	cache_call(File,T,D).
call(H):-
	throw(error(unknown_call(H),'factsbd_rt:call'/1)).

cache_call(File,T,D):-
	count(T,Val),
	copy_db(T,Val),
	call_cached(T,Val,File,D).

call_cached(T,Val,_File,_):-
	data_facts:current_fact(asserta_ed(T,Val)).
call_cached(T,Val,File,D):-
        open(File,read,S),
	exe(S,T,Val,D).
call_cached(T,Val,_File,_):-
	data_facts:current_fact(assertz_ed(T,Val)).

exe(S,T,Val,D):-
	repeat,
	  read(S,X),
	  ( X==end_of_file,
	    !,
	    close(S),
	    fail
	  ; X=D,
	    \+ data_facts:retract_fact(retract_on_file(T,Val))
	  ).

:- data asserta_ed/2, assertz_ed/2, retract_on_file/2.

copy_db(H,Val):-
	data_facts:current_fact(asserta_ed(H)),
	data_facts:assertz_fact(asserta_ed(H,Val)),
	fail.
copy_db(H,Val):-
	data_facts:current_fact(assertz_ed(H)),
	data_facts:assertz_fact(assertz_ed(H,Val)),
	fail.
copy_db(H,Val):-
	data_facts:current_fact(retract_on_file(H)),
	data_facts:assertz_fact(retract_on_file(H,Val)),
	fail.
copy_db(_H,_Val).

count(H,Val):-
	inccounter(H,Val), !.
count(H,1):-
	setcounter(H,2).

:- data asserta_ed/1, assertz_ed/1, retract_on_file/1.

:- meta_predicate asserta_fact(fact).
:- pred asserta_fact(Fact) : callable
# "Version of @pred{data_facts:asserta_fact/1} for filed predicates.
   The current instance of @var{Fact} is interpreted as a fact and is
   added at the beginning of the definition of the corresponding
   predicate. Therefore, before all the facts filed in the
   DB file for the predicate. The predicate concerned must be declared
   as @decl{facts}; if it is not, then @pred{data_facts:asserta_fact/1}
   is used.".

asserta_fact(H):-
	cached_call(H,_File,T,D), !,
	add_term_to_file_db(a(D),T),
	data_facts:asserta_fact(asserta_ed(T)).
asserta_fact(H):-
	data_facts:asserta_fact(H).

:- meta_predicate assertz_fact(fact).
:- pred assertz_fact(Fact) : callable
# "Version of @pred{data_facts:assertz_fact/1} for filed predicates.
   The current instance of @var{Fact} is interpreted as a fact and is
   added at the end of the definition of the corresponding
   predicate. Therefore, after all the facts filed in the
   DB file for the predicate. The predicate concerned must be declared
   as @decl{facts}; if it is not, then @pred{data_facts:assertz_fact/1}
   is used.".

assertz_fact(H):-
	cached_call(H,_File,T,D), !,
	add_term_to_file_db(z(D),T),
	data_facts:assertz_fact(assertz_ed(T)).
assertz_fact(H):-
	data_facts:assertz_fact(H).

:- meta_predicate retract_fact(fact).
:- pred retract_fact(Fact) : callable
# "Version of @pred{data_facts:retract_fact/1} for filed predicates.
   The current instance of @var{Fact} is interpreted as a fact and is
   unified with an actual fact in the current definition of the corresponding
   predicate; such a fact is deleted from the predicate definition. This
   is true even for the facts filed in the DB file for the predicate; but 
   these are NOT deleted from the file (unless the predicate is persistent).
   The predicate concerned must be declared as @decl{facts}; if it is not,
   then @pred{data_facts:retract_fact/1} is used.".

retract_fact(H):-
	cached_call(H,_File,T,D), !,
	retract_cached_fact(T),
	add_term_to_file_db(r(D),T).
retract_fact(H):-
	data_facts:retract_fact(H).

retract_cached_fact(F):-
	data_facts:retract_fact(asserta_ed(F)).
retract_cached_fact(F):-
	data_facts:assertz_fact(retract_on_file(F)).
retract_cached_fact(F):-
	data_facts:retract_fact(assertz_ed(F,_)).

add_term_to_file_db(X,T):-
	add_term_to_file_db(X,T,default).

% ------------------------------------------------------------------------

% 1021 files opened!
% time(ffib(15,X),T). from T=10 to T=1100 (1360 with memoization, NO! 10)
