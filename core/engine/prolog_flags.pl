:- module(prolog_flags, [
		set_prolog_flag/2, current_prolog_flag/2, prolog_flag/3,
		push_prolog_flag/2, pop_prolog_flag/1,
		set_ciao_flag/2, current_ciao_flag/2, ciao_flag/3,
		push_ciao_flag/2, pop_ciao_flag/1,
		prompt/2,
		gc/0, nogc/0, fileerrors/0, nofileerrors/0],
	    [assertions, nortchecks, isomodes, define_flag]).

:- doc(title, "Changing system behaviour and various flags").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Mats Carlsson").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "@cindex{prolog flag} Flags define some parameters of
   the system and control the behavior of system or library predicates.
   Each flag has a name and an associated predefined value, and except
   some system flags which are fixed, in general their associated value
   is changeable.  Predefined flags in the system are:

@begin{description}

@item{@tt{version}} The Ciao version, as a term
      @tt{ciao}(@var{Version},@var{Patch},@var{CommitInfo}).
      @var{Version} and @var{Patch} are atoms.  @var{CommitInfo} is a
      structure describing the commit information (branch, id, date,
      description).  Unchangeable.

@item{@tt{dialect}} Value set to @tt{ciao}.  Used for compatibility
      with other systems when in Prolog mode.  Unchangeable.

@item{@tt{argv}} Its value is a list of atoms representing the program
      arguments supplied when the current executable was invoked.
      This is the value to which the argument of the @pred{main/1}
      predicate is instantiated at executable startup.  Unchangeable.

@item{@tt{bounded}} It is @tt{false}, to denote that the range of
      integers can be considered infinite (but see @pred{int/1}).
      Unchangeable.  @iso

@item{@tt{fileerrors}} If @tt{on}, predicates handling files produce
      errors (throw exceptions) when a file is non-existent or an
      operation is not allowed.  If @tt{off}, a failure will occur
      instead for those conditions.  Initially @tt{on}.

@item{@tt{gc}} Controls whether garbage collection is performed.  May
      be @tt{on} (default) or @tt{off}.

@item{@tt{gc_margin}} An integer @var{Margin}.  If less than
      @var{Margin} kilobytes are reclaimed in a garbage collection
      then the size of the garbage-collected area should be increased.
      Also, no garbage collection is attempted unless the
      garbage-collected area has at least @var{Margin} kilobytes.
      Initially 500.

@item{@tt{gc_trace}} Governs garbage collection trace messages.  An
      element off @tt{[on,off,terse,verbose]}. Initially @tt{off}.

@item{@tt{integer_rounding_function}} It is @tt{toward_zero}, so that
      @tt{-1 =:= -3//2} succeeds.  Unchangeable.  @iso

@item{@tt{max_arity}} It is 255, so that no compound term (or predicate)
      can have more than this number of arguments.  Unchangeable.  @iso

@item{@tt{quiet}} Controls which messages issued using @lib{io_aux}
      are actually written.  As the system uses that library to emit
      its messages, this flag controls the @em{verbosity} of the
      system.  Possible states of the flag are:

  @begin{description}

  @item{@tt{on}} No messages are reported.

  @item{@tt{error}} Only error messages are reported.

  @item{@tt{warning}} Only error and warning messages are reported.

  @item{@tt{off}} All messages are reported, except debug messages.
        This is the default state.

  @item{@tt{debug}} All messages, including debug messages, are
        reported.  This is only intended for the system implementators.

  @end{description}

@item{@tt{unknown}} Controls action on calls to undefined predicates.
      The possible states of the flag are:

  @begin{description}

  @item{@tt{error}} An error is thrown with the @concept{error term}
       @tt{existence_error(procedure, F/A)}.

  @item{@tt{fail}} The call simply fails.

  @item{@tt{warning}} A warning is written and the call fails.

  @end{description}

  The state is initially @tt{error}. @iso

@end{description}
  ").

:- use_module(engine(internals), [
		'$unknown'/2, '$ferror_flag'/2, '$prompt'/2, '$unix_argv'/1,
		'$quiet_flag'/2, '$gc_trace'/2, '$gc_margin'/2, '$gc_mode'/2,
		'$compiling'/2, '$ciao_version'/6]).

%doinclude's below commented out because LPdoc does not allow yet a 
%declaration and a predicate to have the same name.

:- doc(doinclude, set_prolog_flag/1).

:- check decl set_prolog_flag(Flag, Value) : atm * term + iso # "Sets
	the @concept{prolog flag} of name @var{Flag} to value
	@var{Value} in the rest of the current text (its scope is
	local).".

%% :- doc(doinclude,push_prolog_flag/1).

:- decl push_prolog_flag(Flag, Value) : atm * term # "Sets the
	@concept{prolog flag} of name @var{Flag} to value @var{Value},
	but storing current value of @var{Flag} to restore it with
	@decl{pop_prolog_flag/1} (its scope is local).".

%% :- doc(doinclude,pop_prolog_flag/1).

:- decl pop_prolog_flag(Flag) : atm # "Restores the value of
	@var{Flag} previous to the last non-canceled declaration
	@decl{push_prolog_flag/2} on it.".

:- doc(define_flag(Flag, Values, Default), "New flags can be defined
   by writing facts of this predicate.  @var{Flag} is the name of the new
   flag, @var{Values} defines the posible values for the flag (see
   below) and @var{Default} defines the predefined value associated with
   the flag (which should be compatible with @var{Values}).").

:- doc(define_flag(Flag, atom, Default), "Posible values for the
      flag are atoms.@p Example:

@begin{verbatim}
:- multifile define_flag/3.
define_flag(tmpdir, atom, '/tmp').
@end{verbatim}
").

:- doc(define_flag(Flag, integer, Default), "Posible values for
     the flag are integers.@p Example:

@begin{verbatim}
:- multifile define_flag/3.
define_flag(max_connections, integer, 10).
@end{verbatim}
").

:- doc(define_flag(Flag, Values, Default), "Posible values for the
     flag are the elements of @var{Values}.@p Example:

@begin{verbatim}
:- multifile define_flag/3.
define_flag(debug, [on,debug,trace,off], off).
@end{verbatim}
").

:- use_package(define_flag).

:- doc(set_prolog_flag(FlagName, Value),
	    "Set existing flag @var{FlagName} to @var{Value}.").

:- pred set_prolog_flag(+atm, +term) => atm * term + iso.

set_prolog_flag(X, Y) :- nonvar(X), prolog_flag(X, _, Y), !. /* ISO */

:- pred set_ciao_flag(FlagName, Value)
	+ equiv(set_prolog_flag(FlagName, Value)).

set_ciao_flag(FlagName, Value) :- set_prolog_flag(FlagName, Value).

:- doc(current_prolog_flag(FlagName, Value),
"@var{FlagName} is an existing flag and @var{Value} is the
           value currently associated with it.").


:- pred current_prolog_flag/2 => atm * term.
% inferred:
%:- true pred current_prolog_flag(A,B)
%         : ( term(A), term(B) )
%        => ( rt274(A), term(B) ).


current_prolog_flag(X, Y) :- prolog_flag(X, Y, Y). /* ISO */

:- pred current_ciao_flag(FlagName, Value)
	+ equiv(current_prolog_flag(FlagName, Value)).

current_ciao_flag(FlagName, Value) :- current_prolog_flag(FlagName, Value).

:- doc(prolog_flag(FlagName, OldValue, NewValue), "@var{FlagName} is
           an existing flag, unify @var{OldValue} with the value
           associated with it, and set it to new value @var{NewValue}.").



:- pred prolog_flag(A, B, C) : (term(C), nonvar(C)) => (atm(A), term(B)).

:- pred prolog_flag(FlagName, OldValue, NewValue)
	: (var(OldValue), var(NewValue), atm(FlagName))
	=> (term(OldValue), term(NewValue))
# "Same as @tt{current_prolog_flag(@var{FlagName},
          @var{OldValue})}.  @var{OldValue} and @var{NewValue} must be
          strictly identical variables.".

prolog_flag(Flag, Old, New) :- var(Flag), !,
	prolog_flag_2(Flag, Old, New).
prolog_flag(Flag, Old, New) :-
	prolog_flag_2(Flag, Old, New), !.

:- pred ciao_flag(Flag, Old, New) + equiv(prolog_flag(Flag, Old, New)).

ciao_flag(Flag, Old, New) :- prolog_flag(Flag, Old, New).

prolog_flag_2(compiling, Old, New) :-
	flag_value(Old, New, [unprofiled, profiled]),
	'$compiling'(Old, New).
prolog_flag_2(fileerrors, Old, New) :-
	flag_value(Old, New, [on, off]),
	'$ferror_flag'(Old, New).
prolog_flag_2(gc, Old, New) :-
	flag_value(Old, New, [on, off]),
	'$gc_mode'(Old, New).
prolog_flag_2(gc_margin, Old, New) :-
	flag_value(Old, New, integer),
	'$gc_margin'(Old, New).
prolog_flag_2(gc_trace, Old, New) :-
	flag_value(Old, New, [on, off, terse, verbose]),
	'$gc_trace'(Old, New).
prolog_flag_2(unknown, Old, New) :-
	flag_value(Old, New, [error, fail, warning]),
	'$unknown'(Old, New).
prolog_flag_2(quiet, Old, New) :-
	flag_value(Old, New, [on, error, warning, debug, off]),
	'$quiet_flag'(Old, New).
prolog_flag_2(version, Version_Term, Version_Term) :-
	'$ciao_version'(Version, Patch,
	                CommitBranch, CommitId, CommitDate, CommitDesc),
	CommitInfo = commit_info(CommitBranch, CommitId, CommitDate, CommitDesc),
	Version_Term = ciao(Version, Patch, CommitInfo).
prolog_flag_2(dialect, ciao, ciao).
prolog_flag_2(argv,    Args, Args) :-
	'$unix_argv'(Args).
prolog_flag_2(main_module, Old, New) :-
	flag_value(Old, New, atom),
	set_flag(main_module, '', Old, New).
prolog_flag_2(bounded,                   false,       false). % ISO 
prolog_flag_2(double_quotes,             chars,       chars). % ISO
prolog_flag_2(integer_rounding_function, toward_zero, toward_zero). % ISO
prolog_flag_2(max_arity,                 255,         255). % ISO
prolog_flag_2(Flag,                      Old,         New) :-
	define_flag(Flag, Values, Default),
	flag_value(Old, New, Values),
	set_flag(Flag, Default, Old, New).

flag_value(Old, New, _) :- var(New), !, Old==New.
flag_value(_,   New, Xs) :- flag_value_check(Xs, New).

flag_value_check(atom,    X) :- atom(X).
flag_value_check(integer, X) :- integer(X).
flag_value_check([X|_],   X) :- !.
flag_value_check([_|Xs],  X) :- flag_value_check(Xs, X).

:- data flag/2.
set_flag(Flag, Default, Old, New) :-
	( current_fact(flag(Flag, Tmp), Ptr)
	-> Tmp=Old
	; asserta_fact(flag(Flag, Default), Ptr),
	    Default=Old ),
	( Old==New
	-> true
	; erase(Ptr),
	    asserta_fact(flag(Flag, New)) ).

:- data old_flag/2.

:- doc(push_prolog_flag(Flag, NewValue), "Same as
   @pred{set_prolog_flag/2}, but storing current value of @var{Flag} to
   restore it with @pred{pop_prolog_flag/1}.").


:- pred push_prolog_flag(+atm, +term) => atm * term.

push_prolog_flag(Flag, NewValue) :-
	nonvar(Flag),
	prolog_flag(Flag, OldValue, NewValue),
	asserta_fact(old_flag(Flag, OldValue)).

:- doc(pop_prolog_flag(Flag), "Restore the value of @var{Flag}
   previous to the last non-canceled @pred{push_prolog_flag/2} on it.").

:- pred push_ciao_flag(Flag, NewValue)
	+ equiv(push_prolog_flag(Flag, NewValue)).

push_ciao_flag(Flag, NewValue) :- push_prolog_flag(Flag, NewValue).

:- pred pop_prolog_flag(+atm) => atm.

pop_prolog_flag(Flag) :-
	nonvar(Flag),
	retract_fact(old_flag(Flag, OldValue)),
	!, % to avoid removal on backtracking --EMM
	prolog_flag(Flag, _, OldValue).

:- pred pop_ciao_flag(Flag) + equiv(pop_prolog_flag(Flag)).

pop_ciao_flag(Flag) :- pop_prolog_flag(Flag).

:- doc(prompt(Old, New), "Unify @var{Old} with the current prompt
   for reading, change it to @var{New}.").


:- pred prompt(A, B) : atm(B) => atm(A).


:- pred prompt(Old, New) : (var(Old), var(New)) => ( atm(Old),
	    atm(New) ) # "Unify @var{Old} with the current prompt for
	reading without changing it.  On calls, @var{Old} and
	@var{New} must be strictly identical variables.".

prompt(Old, New) :-
	flag_value(Old, New, atom),
	'$prompt'(Old, New).

:- pred fileerrors/0 + equiv(set_prolog_flag(fileerrors, on))
# "Enable reporting of file errors.  Equivalent to
        @tt{set_prolog_flag(fileerrors, on)}".

fileerrors :- '$ferror_flag'(_, on).

:- pred nofileerrors/0 + equiv(set_prolog_flag(fileerrors, off))
# "Disable reporting of file errors.  Equivalent to
	@tt{set_prolog_flag(fileerrors, off)}".

nofileerrors :- '$ferror_flag'(_, off).


:- pred gc/0 + equiv(set_prolog_flag(gc, on)) # "Enable garbage
	collection.  Equivalent to @tt{set_prolog_flag(gc, on)}".

gc :- '$gc_mode'(_, on).


:- pred nogc/0 + equiv(set_prolog_flag(gc, off)) # "Disable garbage
	collection.  Equivalent to @tt{set_prolog_flag(gc, off)}".

nogc :- '$gc_mode'(_, off).


% inferred types:
% :- prop rt274/1 + regtype.

% rt274(argv).
% rt274(bounded).
% rt274(compiling).
% rt274(fileerrors).
% rt274(gc).
% rt274(gc_margin).
% rt274(gc_trace).
% rt274(integer_rounding_function).
% rt274(max_arity).
% rt274(quiet).
% rt274(unknown).
% rt274(version).
