:- module(backward_compatibility, [set_semantics/2, 
	                           arg/3, 
				   functor/3, 
				   (=..)/2],
				   [assertions]).

:- use_module(library(messages), [warning_message/2]).

:- pred arg/3 + deprecated.

:- discontiguous(set_semantics/2).

set_semantics(X, _):- var(X), !, fail.

:- data arg_3_semantics/1.

arg_3_semantics(full_backward).

set_semantics(arg/3, full_backward):- 
	set_fact(arg_3_semantics(full_backward)).
set_semantics(arg/3, warning):- 
	set_fact(arg_3_semantics(warning)).
set_semantics(arg/3, almost_iso):- 
	set_fact(arg_3_semantics(almost_iso)).

arg(N, Term, Arg):-
	catch(term_basic:arg(N, Term, Arg), Exception, 
	      (
		  current_fact(arg_3_semantics(warning)) ->
		  warning_message("arg/3: exception ignored : ~w", [Exception])
	      ;
		  current_fact(arg_3_semantics(almost_iso)) ->
		  throw(Exception)
	      ;
		  fail
	      )).

:- data functor_3_semantics/1.

functor_3_semantics(full_backward).

set_semantics(functor/3, full_backward):- 
	set_fact(functor_3_semantics(full_backward)).
set_semantics(functor/3, warning):- 
	set_fact(functor_3_semantics(warning)).
set_semantics(functor/3, iso):- 
	set_fact(functor_3_semantics(iso)).

:- pred functor/3 + deprecated.

functor(Term, F, N):-
	catch(term_basic:functor(Term, F, N), Exception, 
	      (
		  current_fact(functor_3_semantics(warning)) ->
		  warning_message("functor/3: exception ignored : ~w", [Exception])
	      ;
		  current_fact(functor_3_semantics(iso)) ->
		  throw(Exception)
	      ;
		  fail
	      )).


:- pred ( _ =.. _ )  + deprecated.

:- data univ_2_semantics/1.

univ_2_semantics(full_backward).

set_semantics(univ/2, full_backward):- 
	set_fact(univ_2_semantics(full_backward)).
set_semantics(univ/2, warning):- 
	set_fact(univ_2_semantics(warning)).
set_semantics(univ/2, iso):- 
	set_fact(univ_2_semantics(iso)).

Term =.. List :-
	catch(term_basic:(Term =.. List), Exception, 
	      (
		  current_fact(univ_2_semantics(warning)) ->
		  warning_message("=../3: exception ignored : ~w", [Exception])
	      ;
		  current_fact(univ_2_semantics(iso)) ->
		  throw(Exception)
	      ;
		  fail
	      )).

	      	      	      	      
