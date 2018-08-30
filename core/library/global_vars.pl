:- module(global_vars, [setval/2, getval/2, current/2], [hiord, assertions]).

:- doc(title, "Backtrackable Global Variables").
:- doc(author, "Jose F. Morales").
:- doc(author, "R@'{e}my Haemmerl@'{e}").

:- doc(module, "This module provides a simple way to assign and read
   fully backtrackable global variables.

   Global variables differ from storing information using dynamic
   predicates:
   @begin{itemize}
   @item Global variables are semantically equivalent to a dictionary 
     passed around as an implicit pair of variables:
     @begin{itemize}
     @item only one value is associated to a variable at a time
     @item the value associated to a variable can be replaced
     @item any assignment is undone on backtracking
     @item access cost is proportional to a unification
     @item variable sharing is preserved during assignment
     @end{itemize}
   @item Keys of global variables are local to each module.
   @item Contrary to dynamic predicates, there is no copy of
     terms. This is particularly interesting for large terms.
   @end{itemize}

   The implementation is based on a globally reachable structure in
   the heap, where each entry is allocated dynamically for each key.

   Note that the current implementation has some limitations (see bug
   entries in this module).
").

:- doc(bug, "The use of global variables may produce incorrect results
   for sharing analysis.").
:- doc(bug, "We still do not support statically declared global variables").
:- doc(bug, "Allocation of entries is not undone on backtracking, which
   may exhaust available space quickly for some programs").
:- doc(bug, "No more than 255 modules can use global variables in a program.").
:- doc(bug, "No more than 255 global variables can be used in a module.").

:- use_module(engine(data_facts)).
:- use_module(engine(internals), ['$global_vars_get'/2]).

:- data(module_counter/1).
:- data(key_counter/3).
:- data(key/4).

module_counter(1).

get_info(Module, Key, I, J):-
	key(Module, Key, I, J), !.
get_info(Module, Key, I, J):-
	retract_fact(key_counter(Module, I, J)), !, 
	K is J + 1, assertz_fact(key_counter(Module, I, K)),
	assertz_fact(key(Module, Key, I, J)).
get_info(Module, Key, I, 1):-
	retract_fact(module_counter(I)), 
	J is I + 1, assertz_fact(module_counter(J)), 
	assertz_fact(key_counter(Module, I, 2)), 
	assertz_fact(key(Module, Key, I, 1)).
	
:- use_module(library(mutables)).

:- if(defined(optim_comp)).
% TODO: use low-level implementation (a hash table 
%   associated with the module)
:- use_module(engine(internals), ['$global_vars_set'/2]).
global_array(GlobalArray) :-
	% (See engine(internals) for reserved low level global variables)
	( '$global_vars_get'(11, 0) -> % (default value in optim_comp)
	    '$global_vars_set'(11, GlobalArray)
	; '$global_vars_get'(11, GlobalArray)
	).
:- else.
global_array(GlobalArray) :-
	% (See engine(internals) for reserved low level global variables)
	'$global_vars_get'(11, GlobalArray).
:- endif.

global(Module, Key, Value):-
	get_info(Module, Key, I, J),
	global_array(GlobalArray),
	functor(GlobalArray, t, 255), 
	arg(I, GlobalArray, Array), 
	functor(Array, t, 255), 
	arg(J, Array, Value).

:- pred setval(Name,Value) # "Associate the term @var{Value} with the
   atom @var{Name}. If @var{Name} does not refer to an existing global
   variable, an unbounded global variable @var{Value} is created and
   unified to @var{Value}. On backtracking the assignment is
   reversed. If @var{Name} is not a atom the predicate silently
   fails.".

:- if(defined(optim_comp)).
:- '$context'(setval/2, module).
setval(Key, Value) :- '$module'(M), setval_(Key, M, Value).
:- else.
:- meta_predicate setval(addmodule, +).
setval(Key, M, Value) :- setval_(Key, M, Value).
:- endif.

setval_(Key, _Module, _Value):-
	\+ atom(Key), !, fail.
setval_(Key, Module, Value):-
	global(Module, Key, Mutable), 
	set(Mutable, Value). 

:- pred getval(Name,Value) # "Unifies @var{Value} with the current
   value of the global variable refered to by the atom @var{Name}. If
   @var{Name} does not refer to an exisiting global variable, a free
   unbound variable is created and unified with @var{Value}. If
   @var{Name} is not an atom the predicate silently fails.".

:- if(defined(optim_comp)).
:- '$context'(getval/2, module).
getval(Key, Value) :- '$module'(M), getval_(Key, M, Value).
:- else.
:- meta_predicate getval(addmodule, +).
getval(Key, M, Value) :- getval_(Key, M, Value).
:- endif.

getval_(Key, _Module, _Value):-
	\+ atom(Key), !, fail.
getval_(Key, Module, Value):-
	global(Module, Key, Mutable),
	get(Mutable, Value).

:- pred current(Name,Value) # "Enumerate all defined variables with
   their value. The order of enumeration is undefined.".

:- if(defined(optim_comp)).
:- '$context'(current/2, module).
current(Key, Value) :- '$module'(M), current_(Key, M, Value).
:- else.
:- meta_predicate current(addmodule, +).
current(Key, M, Value) :- current_(Key, M, Value).
:- endif.

current_(Key, Module, Value):-
	global(Module, Key, Mutable),
	get(Mutable, Value).

set(Mutable, Value):-
	( mutable(Mutable) ->
	    update_mutable(Value, Mutable)
	; create_mutable(Value, Mutable)
	).

get(Mutable, Value):-
	( mutable(Mutable) ->
	    get_mutable(Value, Mutable)
	; create_mutable(Value, Mutable)
	).
