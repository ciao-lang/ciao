:- module(indexer_tr, [expand_index_sent/3, expand_index_goal/3], [assertions]).

% An indexer package for Ciao.
% Jose F. Morales
%
% Based on indexer.pl (1.6 06 Oct 1993) by Tom Howland
% (originally ported to Ciao by Francisco Bueno, 8 Jan 2001).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(dynamic), [assert/1, retractall/1]).
:- use_module(library(lists), [append/3]).
:- use_module(library(terms), [atom_concat/2]).

% ===========================================================================

:- data index_spec/2. % Indexing specs
:- data index_scheme/3.
:- data clause_count/4.

% index_newhead(Head, Mod, ClauseNumber, NewHead): new head for indexing
%
%   Arguments may be shuffled and ClauseNumber may be introduced in an
%   internally indexed position.
:- data index_newhead/4.

% ---------------------------------------------------------------------------
% Translation hooks

% Sentence translation
expand_index_sent(0, [], Mod) :- !,
%expand_index_sent(end_of_file, [], Mod) :- !,
	% TODO: cannot use flush_aux(Mod, Sents, []) here 'end_of_file'
	clean_indexer_db(Mod).
% Cannot use 'end_of_file' to clean the DB because the goal
% translation needs it later.
%expand_index_sent(end_of_file, [], Mod) :- !,
%	% TODO: cannot use flush_aux(Mod, Sents, []) here 'end_of_file'
%	clean_indexer_db(Mod).
expand_index_sent((:- index(Specs)), [], Mod) :- !,
	check_index_specs(Specs, Mod).
expand_index_sent((:- meta_predicate(Head)), Sents, Mod) :-
	% TODO: does not work if index/ is placed before the index
	% Ensure that the indexer is processed
	is_indexed(Mod, Head),
	functor(Head, Name, Arity),
	compile_indexer(Mod, Name, Arity, Sents, Sents1),
	rename_index1p_goal(Head, Head2, Mod), % (may fail)
	!,
	% Include a meta_predicate for the renamed pred too
	Sents1 = [(:- meta_predicate(Head)),
	          (:- meta_predicate(Head2))].
expand_index_sent((:- _), _, _Mod) :- !, fail.
expand_index_sent(Clause, Sents, Mod) :-
	head(Clause, Head),
	is_indexed(Mod, Head),
	!,
	treat_clause(Mod, Clause, Sents).

% Goal translation
% TODO: 'end_of_file' as end of goal translations is not a good idea
expand_index_goal(end_of_file, _, Mod) :- !,
	clean_indexer_db(Mod), % I would like to call it later...
	fail.
expand_index_goal(G0, G, Mod) :-
	% (this avoids one call)
	rename_index1p_goal(G0, G, Mod),
	!.

% Rename goals if the index scheme is index1p(I), I > 1
rename_index1p_goal(G0, G, Mod) :-
	functor(G0, Name, Arity),
	functor(Head, Name, Arity),
	index_scheme(Head, Scheme, Mod),
	Scheme = index1p(I),
	I > 1,
	index_newhead(G0, Mod, _ClNum, G).

% ---------------------------------------------------------------------------
% Clean the indexer status

clean_indexer_db(Mod) :-
	retractall_fact(clause_count(_, _, Mod, _)),
	retractall_fact(index_newhead(_, Mod, _, _)),
	retractall_fact(index_spec(_, Mod)),
	retractall_fact(index_scheme(_, _, Mod)),
	retractall(index_hash_index(_, Mod, _, _)),
	clean_aux(Mod).

% ---------------------------------------------------------------------------
% Head classifier (implemented as a dynamic predicate)

% TODO: Using dynamic clauses.
%       Isolate in other module, or implement in other way?

:- push_prolog_flag(unused_pred_warnings, no).
% Note: hash_term/2 is invoked from dynamically asserted clauses of
% index_hash_index/4
:- use_module(library(indexer/hash), [hash_term/2]).
:- pop_prolog_flag(unused_pred_warnings).

:- dynamic index_hash_index/4.

% Add a new clause indexer for @var{Head} using @var{Test} and
% @var{ComputeHash}
compute_clause_indexer(Mod, Head, Indexer, ClNum) :-
	Indexer = gen_indexer(Test, ComputeHash0, CalltoIndex, _),
	list_to_conj(ComputeHash0, ComputeHash),
	Comp = (
          index_hash_index(Head, Mod, ClNum, CalltoIndex) :-
             ( Test -> % is the head included as part of the test?
	         ComputeHash % this will bind ClNum % TODO: right?
	     ; % the head is not included as the states described by the test,
	       % include the clause in the try-list
	       true
	     )
	),
	assert(Comp).

% Use all the known clause indexers to distribute the clause number
% @var{ClNum} with head @var{Head}. @var{Sents} is the list of all the
% entries that has to be included in the indexer.
%
% TODO: document as set intersections, implication, precondition?
distribute_clause(Mod, Head, ClNum, Sents) :-
	findall(CalltoIndex,
	        index_hash_index(Head, Mod, ClNum, CalltoIndex),
		Sents).

% ---------------------------------------------------------------------------
% Do the head belong to an indexed predicate?

is_indexed(Mod, Head) :-
	functor(Head, Name, Arity),
	functor(Spec, Name, Arity),
	index_spec(Spec, Mod),
	!.

% ---------------------------------------------------------------------------
% Treat a clause that belongs to an indexed predicate

treat_clause(Mod, Clause, Sents) :-
	head(Clause, Head),
	% Compile the indexer (if it is not compiled)
	% (this also fills the clause classifier)
	functor(Head, Name, Arity),
	compile_indexer(Mod, Name, Arity, Sents, Sents1),
	% Link the clause in the indexed predicate
	index_scheme(Head, Scheme, Mod),
	( Scheme = index1p(1) ->
	    Sents1 = [Clause]
	; Scheme = index1p(_) ->
	    index_newhead(Head, Mod, _, NewHead),
	    replace_head(Clause, NewHead, NewClause),
	    Sents1 = [NewClause]
        ; inc_clause_no(Mod, Name, Arity, ClNum),
	  index_newhead(Head, Mod, ClNum, NewHead),
	  replace_head(Clause, NewHead, NewClause),
	  Sents1 = [NewClause|Sents0],
	  distribute_clause(Mod, Head, ClNum, Sents0)
	).

% Obtain the current clause number (and increment it)
inc_clause_no(Mod, Name, Arity, ClNum) :-
	( retract_fact(clause_count(Name, Arity, Mod, ClNum)) ->
	    true
	; ClNum = 0
	),
	ClNum1 is ClNum+1,
	asserta_fact(clause_count(Name, Arity, Mod, ClNum1)).

% ---------------------------------------------------------------------------
% Generate the indexer code for predicate Mod:Name/Arity
% This includes the wrapper predicate and indexing tables.

compile_indexer(Mod, Name, Arity, Sents, Sents0) :-
	functor(Head, Name, Arity),
	index_scheme(Head, _, Mod),
	!,
	% indexer already compiled, do nothing
	Sents = Sents0.
compile_indexer(Mod, Name, Arity, Sents, Sents0) :-
	init_indexer(Mod, Name, Arity, IndexerSents),
	append(IndexerSents, Sents1, Sents),
	flush_aux(Mod, Sents1, Sents0).

init_indexer(Mod, Name, Arity, Sents) :-
	% Determine the indexer scheme (checking all the indexer specs)
        functor(Head, Name, Arity),
        findall(Head, index_spec(Head, Mod), Specs),
	indexer_scheme(Specs, Scheme),
	assertz_fact(index_scheme(Head, Scheme, Mod)),
	% Compute the mapping between head and newhead
	newhead_mapping(Scheme, Mod, Name, Arity),
	% Generate the wrapper clause from head to newhead
	% (optionally, inserts the indexing code)
	( Scheme = index1p(1) ->
	    Sents = [] % use internal indexer
	; Scheme = index1p(_) -> % shuffle arguments
	    index_newhead(Head, Mod, _ClNum, NewHead),
	    functor(NewHead, NewName, NewArity),
	    Sents = [(Head :- NewHead)]
	; %
	  parse_index_specs1(Specs, Head, Mod, ClNum, 1, Is),
	  indexers_get_select_try_list(Mod, Is, SelectTryList),
	  indexers_get_defsents(Is, Dis),
	  %
	  %list_to_conj(HAB0, HAB),
	  %list_to_conj(HRB0, HRB)
	  % (for static predicates)
	  index_newhead(Head, Mod, ClNum, NewHead),
	  functor(NewHead, NewName, NewArity),
	  append(Dis, [(:- discontiguous NewName/NewArity), (Head :- SelectTryList,NewHead)], Sents)
	).

% SelectTryList:
%
%   This goal (generated) has two parts. The first one is fully
%   deterministic selection of the try-list that matches the
%   current head arguments. I.e.,
%
%     c_1 ... c_n are the predicate clauses
%     
%     try-list = { c_i so that head of c_i is inside the
%                  states included in Indexer }
%
%   Once the try-list is selected, the second part activates
%   the try-list. The current implementation does it by
%   calling the 'index' indirection table with the computed
%   hash value.
%
% TODO: I do not know if try-list is the standard terminology
%
% TODO: cuts (!) may not work as expected! we must use choice
%   idiom and cut idiom. A better solution would integrate
%   this code in the code normalization that is executed
%   before the bytecode compiler.
% 

% ===========================================================================

% Obtain the indexer scheme from the spec:
%   index1p(I): 1st level indexing on the I-th argument
%   general: other multiple argument indexing (hash based)
indexer_scheme([Spec], Scheme) :-
	functor(Spec, _, N),
	% Check that Specs have the form P(?, ..., ?, +, ?, ..., ?)
	count_args(1, N, I, Spec, ?),
	arg(I, Spec, +),
	I1 is I + 1,
	count_args(I1, N, I2, Spec, ?),
	I2 > N,
	!,
	Scheme = index1p(I).
indexer_scheme(_, general).

% Stop is the first (starting at the I-th argument) argument of X that
% is not equal to A. Stop is Last + 1 if all arguments are equal to A.
count_args(I, Last, Stop, _X, _A) :- I > Last, !, Stop = I.
count_args(I, Last, Stop, X, A) :-
	( arg(I, X, A) ->
	    I1 is I + 1,
	    count_args(I1, Last, Stop, X, A)
	; Stop = I
	).

parse_index_specs1([], _Head, _Mod, _ClNum, _IndexN, []).
parse_index_specs1([P1|P2], Head, Mod, ClNum, IndexN, [Indexer|Is]) :-
	parse_index_spec(Mod, P1, IndexN,
	                 Head, ClNum, 
	                 Indexer),
	IndexN1 is IndexN + 1,
	parse_index_specs1(P2, Head, Mod, ClNum, IndexN1, Is).

% ---------------------------------------------------------------------------
% Indexers: a list of indexer

% The try-list selection code
indexers_get_select_try_list(Mod, Is, SelectTryList) :-
	indexers_get_select_try_list_(Is, Mod, SelectTryList0),
	% If no indexer test succeeds, select the most general
	% try-list (i.e., do not bound the clause number)
	append(SelectTryList0, [true], SelectTryList1),
	list_to_disj(SelectTryList1, SelectTryList).

indexers_get_select_try_list_([], _Mod, []).
indexers_get_select_try_list_([I|Is], Mod, [SelectTryList|Bs]) :-
	I = gen_indexer(Test, ComputeHash0, CalltoIndex, _),
        % Generate the code to select the try-list for this IndexN
	Call = Mod:CalltoIndex, % add module qualifier to avoid name clashes
	append(ComputeHash0, [Call], Then0),
	list_to_conj(Then0, Then),
        SelectTryList = (Test -> Then),
	indexers_get_select_try_list_(Is, Mod, Bs).

% Sentences to define the index tables
indexers_get_defsents([], []).
indexers_get_defsents([I|Is], [Dis|Bs]) :-
	I = gen_indexer(_, _, _, Dis),
	indexers_get_defsents(Is, Bs).

% Compile one indexer
parse_index_spec(Mod, Spec, IndexN,
	         Head, ClNum,
		 Indexer) :-
 	Indexer = gen_indexer(Test, % precondition testing code of the indexer
	                      ComputeHash0,
			      CalltoIndex,
			      Dis), % definition of the index table
	% TODO: Codes for dynamic cases:
        %   CodeToAssert = (ComputeHash, assert(Call, Ref))
        %   CodeToRetract = (erase(Ref))
        % where Ref may be in the gen_indexer structure
        %
	functor(Head, Name, Arity),
	% Generate the code that computes the hash values
	% and the test 
	parse_spec(Arity, Mod, Spec, Head, Test0, Action0, Hashes),
	list_to_conj(Test0, Test),
	append(Action0, LastAct, ComputeHash0),
	xor_hashes(Hashes, Hash, LastAct),
	%
	aux_pred_name(index(IndexN), Name, Arity, IndexName),
	functor(CalltoIndex, IndexName, 2),
	arg(1, CalltoIndex, Hash),
	arg(2, CalltoIndex, ClNum),
	% The table for index IndexN
	Dis = (:- discontiguous(IndexName/2)),
	% TODO: Add this one (instead of Dis? or in addition?) for dynamic indices
	%  [:- dynamic(IndexName/2)]
	% Compute the clause indexer
	compute_clause_indexer(Mod, Head, Indexer, ClNum).

% TODO: This generates tests and hashs in reverse argument order, do we want it?
parse_spec(0, _Mod, _, _, [], [], []) :- !.
parse_spec(N, Mod, Spec, Head, [Test0|Test], Act, [H|Hashes]) :-
	arg(N, Spec, Arg),
	arg(N, Head, Var),
	hash_spec(Arg, Mod, Var, Test0, Act, Act1, H),
	!,
	M is N-1,
	parse_spec(M, Mod, Spec, Head, Test, Act1, Hashes).
parse_spec(N, Mod, Spec, Head, Test, Action, Hashes) :-
	M is N-1,
	parse_spec(M, Mod, Spec, Head, Test, Action, Hashes).

% Obtain the testing code and hashing code from an indexing specifier.
% The testing code succeeds when the this indexing spec can be used.
% The hashing code obtains the hash value for indexing.
hash_spec(+, Mod, Var, Test, Act, Act0, H) :-
	Test = term_typing:nonvar(Var),
	Act = [term_basic:functor(Var, N, _), hash:hash_term(N, H)|Act0],
	import_hash_term(Mod).
hash_spec(*, Mod, Var, Test, Act, Act0, H) :-
	Test = term_typing:ground(Var),
	Act = [hash:hash_term(Var, H)|Act0],
	import_hash_term(Mod).
hash_spec(i, _Mod, Var, Test, Act, Act0, Var) :-
	Test = term_typing:integer(Var),
	Act = Act0.
hash_spec(n, _Mod, Var, Test, Act, Act0, Var) :-
	Test = term_typing:nonvar(Var),
	Act = Act0.
% ? spec is just ignored

import_hash_term(Mod) :-
	ensure_import(Mod, library(indexer/hash), hash_term/2).

% TODO: Check reason for this in README in the original implementation
xor_hashes([],    _,    []) :- !.
xor_hashes([H|T], Left, Expr) :-
	xor_hashes1(T, H, Xors),
	( var(Xors) ->
	    Expr = [], Left = Xors
	; Expr = [Left is Xors]
	).

%% Was \ instead of # --I guess it was Quintus xor (PBC)
xor_hashes1([],    H, H) :- !.
xor_hashes1([H|T], Z, #(Z, X)) :- xor_hashes1(T, H, X).

% Auxiliary name for predicate Name/Arity (depending on Sub)
aux_pred_name(Sub, Name, Arity, AuxName) :-
	( Sub = index(N) ->
	    % Names for the index N (we will have several indices)
	    number_atom(N, Na),
	    atom_concat('_index_', Na, AuxSub)
	; Sub = new_name ->
	    % New name for the original predicate clauses, that will
	    % be prefixed with the clause number.
	    AuxSub = ''
	; fail
	),
	number_atom(Arity, ArityA),
	atom_concat(['\6\indexer_', Name, '/', ArityA, AuxSub], AuxName).

number_atom(N, A) :- number_codes(N, C), atom_codes(A, C).

% How 'newhead' is formed from 'head'
newhead_mapping(Scheme, Mod, Name, Arity) :-
        % Clause for dynamic indexing: [(Hash:-SelectTryList)]
	functor(Head, Name, Arity), % -Head, +Name, +Arity	
	aux_pred_name(new_name, Name, Arity, NewName),
	( Scheme = general ->
	    % include the clause number as the first argument
	    NewArity is Arity + 1,
	    functor(NewHead, NewName, NewArity),
	    arg(1, NewHead, ClNum),
	    unify_args(NewArity, Head, NewHead)
	; Scheme = index1p(I) ->
	    % swaping the 1st and Ith argument
	    functor(NewHead, NewName, Arity),
	    swap_args(1, I, 1, Arity, Head, NewHead)
	; fail
	),
	%
	asserta_fact(index_newhead(Head, Mod, ClNum, NewHead)).

swap_args(_From,_To,I,Arity,_X0,_X) :- I > Arity, !.
swap_args(From,To,I,Arity,X0,X) :-
	( I = From -> arg(To,X0,Arg), arg(I,X,Arg)
	; I = To -> arg(From,X0,Arg), arg(I,X,Arg)
	; arg(I,X0,Arg), arg(I,X,Arg)
	),
	I1 is I + 1,
	swap_args(From,To,I1,Arity,X0,X).

%% [Old code to enable indexing on dynamic predicates - modified and commented]
%
% dynamic_indexer_term_expansion((:-dynamic_index(Specs)), Clauses) :-
%     dite(Specs, Clauses, _).
% dynamic_indexer_term_expansion((:-dynamic_volatile_index(Specs)),
% 			       [(:-volatile(X))|Clauses]) :-
%     dite(Specs, Clauses, X).
% 
% dite(Specs, Clauses, PS) :-
%     PS=NewTrueName/NewTrueArity,
%     index_clauses(Specs, Clauses2, [], Head, _, Mod, _, _,
% 		  ClauseNumber, SelectTryList, Hash, Hash),
%     Clauses1 = [(:-dynamic(PS)),
% 		(:-discontiguous('** indexed assert **'/1)),
% 		(:-discontiguous('** indexed retract **'/1)),
% 		(:-discontiguous('** indexed **'/1)),
% 		'** indexed **'(Head),
% 		(Head:-Hash,TrueNewHead),
% 		HashAssert, HashRetract | Clauses2],
%     parse_index_specs(Specs, Head, Mod, Mod, ClauseNumber, SelectTryList,
%        HAB1, % code to assert
%        HRB, % code to retract
%        Refs, % references
%        _, _, Dyn, _, _, _),
%     tidy(HAB1,HAB),
%
%     functor(Head, Name, Arity),
%     concat_atom(['** ',Name,'/',Arity,' **'], NewTrueName),
%     Head =.. [_|HeadArgs],
%     append([NewTrueName, ClauseNumber], HeadArgs, X1),
%     append(X1, Refs, TrueNewHeadList),
%     TrueNewHead =.. TrueNewHeadList,
%     functor(TrueNewHead, _, NewTrueArity),
%
%     concat_atom(['clause count ', Name, '/', Arity], ClauseCount),
%     functor(CCi,ClauseCount,1),
%     arg(1, CCi, ClauseNumber),
%     functor(CCf,ClauseCount,1),
%     arg(1, CCf, CC),
%     HashAssert=('** indexed assert **'(Head):-
% 	       (   retract(CCi)
% 	       ->  true
% 	       ;   ClauseNumber is 0
% 	       ),
% 	       CC is ClauseNumber+1,
% 	       assert(CCf),
% 	       HAB,
% 	       assert(TrueNewHead)),
%     HashRetract=('** indexed retract **'(Head):-Hash,
% 		retract(TrueNewHead),HRB),
%     append(Dyn, Clauses1, Clauses).

% ---------------------------------------------------------------------------
% Do check_index_spec/2 for a conjunction of index specs
check_index_specs((Spec, Specs), Mod) :- !,
	check_index_spec(Spec, Mod),
	check_index_specs(Specs, Mod).
check_index_specs(Spec, Mod) :-
	check_index_spec(Spec, Mod).

% Check one index spec and assert them in the spec db
check_index_spec(Spec, Mod) :-
	must_be(callable, Spec, 1, :-(index(Spec))),
	functor(Spec, _Name, Arity),
	check_index_args(Arity, Spec),
	assertz_fact(index_spec(Spec, Mod)).

check_index_args(N, Spec) :-
	( N =:= 0 -> true
	; arg(N, Spec, Arg),
	  check_index_arg(Arg, Spec),
	  N1 is N - 1,
	  check_index_args(N1, Spec)
	).

check_index_arg(Arg, Spec) :-
	must_be(oneof([+, *, -, ?, i, n]), Arg, 1, :-(index(Spec))).

unify_args(N, T1, T2) :-
	( N =< 1 ->
	    true
	; N1 is N - 1,
	  arg(N1, T1, Arg),
	  arg(N,  T2, Arg),
	  unify_args(N1, T1, T2)
	).

must_be(callable,    _, _, _). % currently, cannot be checked 
must_be(oneof(List), A, _, Source) :-
	( member(A, List) -> true
	; message(warning, ['In ', Source, ' argument must be one of ', List,
		    ': ignored'])
	).

% ===========================================================================
% Auxiliary formulae 

head((X:-_), X) :- !.
head(X, X).

replace_head((_:-Y), N, (N:-Y)) :- !.
replace_head(_, N, N).

list_to_disj([], fail).
list_to_disj([A], A) :- !.
list_to_disj([A|As], (A;B)) :- list_to_disj(As, B).

list_to_conj([], true).
list_to_conj([A], A) :- !.
list_to_conj([A|As], (A,B)) :- list_to_conj(As, B).

% ===========================================================================
% Auxiliary code to ensure exports in translations
%
% TODO: Move as a expansion_tools lib or integrate in c_itf_internals?

:- data import_done/4.
:- data import_pending/4.

clean_aux(Mod) :-
	retractall_fact(import_done(_, _, _, Mod)),
	retractall_fact(import_pending(_, _, _, Mod)).

% Emit pending sentences (e.g., to import pending imports)
flush_aux(Mod, Sents, Sents0) :-
	findall(import(Lib, F, A), pick_import(Mod, Lib, F, A), Is),
	flush_aux_imports(Is, Sents, Sents0).

flush_aux_imports([], Sents, Sents).
flush_aux_imports([import(Lib, F, A)|Is], Sents, Sents0) :-
	Sents = [(:- use_module(Lib, [F/A]))|Sents1],
	flush_aux_imports(Is, Sents1, Sents0).

% Add Lib:F/A to the pending import set (it if was not imported before)
ensure_import(Mod, Lib, F/A) :-
	import_done(F, A, Lib, Mod), !. % already imported
ensure_import(Mod, Lib, F/A) :-
	import_pending(F, A, Lib, Mod), !. % already pending
ensure_import(Mod, Lib, F/A) :-
	assertz_fact(import_pending(F, A, Lib, Mod)).

% Remove Lib:F/A from the pending import set, mark it as imported
pick_import(Mod, Lib, F, A) :- % nondet
	retract_fact(import_pending(F, A, Lib, Mod)),
	assertz_fact(import_done(F, A, Lib, Mod)).

% ===========================================================================
% TODO: Future improvements
%
%  * Cannot index dynamic predicates (the original port to Ciao removed
%    this feature, but it could be added again).
% 
%  * Add user-defined indexing specs.
%
%  * Integrate with internal indexing (WAM compiler)
% 
%    The auxiliary predicates generated in this expansion are really
%    similar to the internal indexing tables (the integration with the
%    bytecode compiler could not be too hard).
%
%    The auxiliary clauses for indices look like:
% 	  'indexer_foo/4_index_1'(4262624028,0).
% 	  'indexer_foo/4_index_2'(9,0).
%    The same happens with the clause number id (which can be removed
%    from the clause).

% ===========================================================================
% TODO: Recover indexing of dynamic/data predicates
% 
%  - require the module 'indexed_dynamic_rt.pl'
%  - require a new family of assert/retract predicates
%  - ideally, we could offer the same interface (like for concurrent?)
%  - add support for 'data' predicates too
%  - 
% 
% Example code:
%   :- dynamic_index foo(+,?,*,i), foo(?,?,?,i).
%   kill_foo(A, B, C, D) :- indexed_retract(foo(A,B,C,D)).
% 
% The translation is as follows (kill_foo/4 is unchanged):
%
% TODO: They could be data
% :- dynamic '** foo/4 index 1 **'/2.
% :- dynamic '** foo/4 index 2 **'/2.
% :- dynamic '** foo/4 **'/7.
% 
% TODO: They should be multifile
% % (hooks for indexed dynamic predicates)
% :- discontiguous '** indexed assert **'/1.
% :- discontiguous '** indexed retract **'/1.
% :- discontiguous '** indexed **'/1.
% 
% '** indexed **'(foo(A,B,C,D)).
% 
% foo(A,B,C,D) :- 
%         '** hash foo/4 **'(A,B,C,D,E),
%         '** foo/4 **'(E,A,B,C,D,F,G).
% 
% '** indexed assert **'(foo(A,B,C,D)) :- 
%         (   retract('clause count foo/4'(E)) ->
%             true
%         ;   E is 0
%         ),
%         F is E+1,
%         assert('clause count foo/4'(F)),
%         hash_term(C,G),
%         functor(A,H,I),
%         hash_term(H,J),
%         K is\(D,\(G,J)),
%         assert('** foo/4 index 1 **'(K,E),L),
%         assert('** foo/4 index 2 **'(D,E),M),
%         assert('** foo/4 **'(E,A,B,C,D,L,M)).
% 
% '** indexed retract **'(foo(A,B,C,D)) :- 
%         '** hash foo/4 **'(A,B,C,D,E),
%         retract('** foo/4 **'(E,A,B,C,D,F,G)),
% 	% remove the clause from index tables
%         erase(F),
%         erase(G).
% 
% '** hash foo/4 **'(A,B,C,D,E) :- 
%         ( integer(D), ground(C), nonvar(A) ->
%             hash_term(C,F),
%             functor(A,G,H), hash_term(G,I),
%             J is\(D,\(F,I)),
%             '** foo/4 index 1 **'(J,E)
%         ; integer(D) ->
%             '** foo/4 index 2 **'(D,E)
%         ;   true
%         ).

% TODO: retract is optimized (using erase/1) to remove the clause from
%       the index tables without extra look ups. Can it be done
%       better?

% TODO: in-source definitions of indexed predicates are not properly
%       indexed. Provide an initialization directive to call
%       indexed_assert/1 for each of them, a more direct method, or
%       emit an error if they are detected.

% ===========================================================================

:- doc(bug, "Missing port of dynamic_indexer.pl").

:- doc(bug, "Hash is too costly").

:- doc(bug, "index/1 declarations must appear before each indexed
       predicate I would like to fix this, but I need a end_of_file
       translation that can include use_module (currently I
       cannot).").

:- doc(bug, "Internal indexing is able to scan some goals at the
       beginning of each body. This translation is not.").

