:- module(runtime_control, [
        statistics/0, statistics/2,
	clockfreq_result/1,
	tick_result/1,
	% regtypes {
	symbol_result/1,
	gc_result/1,
	memory_result/1,
	time_result/1,
	symbol_option/1,
	garbage_collection_option/1,
	memory_option/1,
	clockfreq_option/1,
	tick_option/1,
	time_option/1,
	% },
	%
        current_atom/1,
        new_atom/1,
	current_module/1,
	module_unconcat/3,
	%
	predicate_property/2,
	predicate_property/3, % (+1 because of addmodule)
	%
	garbage_collect/0,
        %
        set_heap_limit/1, current_heap_limit/1,
	%
	set_prolog_flag/2, current_prolog_flag/2, prolog_flag/3,
	push_prolog_flag/2, pop_prolog_flag/1,
	%
	prompt/2,
	%
	gc/0, nogc/0, fileerrors/0, nofileerrors/0
        ],
        [assertions, isomodes, nortchecks, define_flag, datafacts]).

:- use_module(engine(internals)).

:- doc(title, "Runtime system control").
:- doc(author, "Manuel Carro").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "Mats Carlsson (original author)").

:- doc(module, "This module implements some predicates which provide
   internal control and access to the Ciao runtime, like some internal
   statistics, loaded modules, special properties of the predicates,
   garbage collection, system flags, etc.

   @cindex{prolog flag} Flags define some global parameters of the
   system and control the behavior of system or library predicates.
   Each flag has a name and an associated predefined value, and except
   some system flags which are fixed, in general their associated
   value is changeable.  Predefined flags in the system are:

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

@item{@tt{quiet}} Controls which messages issued using @lib{messages_basic}
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

% ---------------------------------------------------------------------------

:- doc(bug, "The space used by the process is not measured here:
   process data, code, and stack also take up memory.  The memory
   reported for atoms is not what is actually used, but the space used
   up by the hash table (which is enlarged as needed).").

:- trust pred statistics # "Prints statistics about the system.".
:- impl_defined(statistics/0).

:- pred statistics(Tick_option, Tick_result) 
	: tick_option * term => tick_option * tick_result 
   # "Gather information about clock ticks (either run, user, system
     or wall tick) since last consult or since start of program.  A
     tick is the smallest amount of time that a clock can measure.".

:- pred statistics(Clockfreq_option, Clockfreq_result) 
	: clockfreq_option * term => clockfreq_option * clockfreq_result 
   # "Gather information about frequency of the clocks used to measure
     the ticks (either run-user, system or wall clock).  Results are
     returned in hertz.  This value also can be defined as the amount
     of ticks that a clock can measure in one second.".

:- pred statistics(Time_option, Time_result) 
	: time_option * term => time_option * time_result 
   # "Gather information about time (either process time or wall time)
     since last consult or since start of program.  Results are
     returned in milliseconds. Note that internally, time is
     calculated as:

@begin{verbatim}
  Time_result = (Tick_result / Clockfreq_result) * 1000
@end{verbatim}

".

:- pred statistics(Memory_option, Memory_result) 
	: memory_option * term => memory_option * memory_result
   # "Gather information about memory consumption.".

:- pred statistics(Garbage_collection_option, Gc_result)
	: garbage_collection_option * term => garbage_collection_option * gc_result 
   # "Gather information about garbage collection.".

:- pred statistics(Symbol_option, Symbol_result) 
	: symbol_option * term => symbol_option * symbol_result 
   # "Gather information about number of symbols and predicates.".

:- pred statistics(Option, ?term) # "If @var{Option} is unbound,
   it is bound to the values on the other cases.".

% ---------------------------------------------------------------------------

:- doc(doinclude, time_option/1).  

:- true prop time_option(M) + regtype # "Options to get information
about execution time.  @var{M} must be one of @tt{runtime},
@tt{usertime}, @tt{systemtime} or @tt{walltime}.".

time_option(runtime).
time_option(usertime).
time_option(systemtime).
time_option(walltime).

:- doc(doinclude, tick_option/1).  

:- true prop tick_option(M) + regtype # "Options to get information about
   execution ticks.".

tick_option(runtick).
tick_option(usertick).
tick_option(systemtick).
tick_option(walltick).  

:- doc(doinclude, clockfreq_option/1).  

:- true prop clockfreq_option(M) + regtype # "Options to get information about
   the frequency of clocks used to get the ticks.".

clockfreq_option(runclockfreq).
clockfreq_option(userclockfreq).
clockfreq_option(systemclockfreq).
clockfreq_option(wallclockfreq).

:- doc(doinclude, memory_option/1).

:- true prop memory_option(M) + regtype # "Options to get information about
memory usage.".

memory_option(memory).
memory_option(symbols).
memory_option(program).
memory_option(global_stack).
memory_option(local_stack).
memory_option(trail).
memory_option(choice).

:- doc(doinclude, garbage_collection_option/1).

:- true prop garbage_collection_option(M) + regtype # "Options to get
   information about garbage collection.".

garbage_collection_option(garbage_collection).
garbage_collection_option(stack_shifts).

:- doc(doinclude, symbol_option/1).

:- true prop symbol_option(M) + regtype # "Option to get information
   about the number of symbols in the program.".

symbol_option(symbols).

:- doc(doinclude, time_result/1).

:- true prop time_result(Result) + regtype # "@var{Result} is a
two-element list of numbers.  The first number is the time since the
start of the execution; the second number is the time since the
previous consult to time.".

time_result([A, B]):- num(A), num(B).

:- doc(doinclude, tick_result/1).

:- true prop tick_result(Result) + regtype # "@var{Result} is a
two-element list of numbers.  The first number is the number of ticks
since the start of the execution; the second number is the number of
ticks since the previous consult to tick.".

tick_result([A, B]):- num(A), num(B).

:- doc(doinclude, clockfreq_result/1).

:- true prop clockfreq_result(Result) + regtype # "@var{Result} is a
number.  It gives the frequency in hertz used by the clock get the
ticks.".

clockfreq_result(A):- num(A).

:- doc(doinclude, memory_result/1).

:- true prop memory_result(Result) + regtype # "Result is a
two-element list of integers.  The first element is the space taken up
by the option selected, measured in bytes; the second integer is zero
for program space (which grows as necessary), and the amount of free
space otherwise.".

memory_result([A, B]):- int(A), int(B).

:- doc(doinclude, gc_result/1).

:- true prop gc_result(Result) + regtype # "@var{Result} is a
tree-element list of integers, related to @concept{garbage collection}
and @concept{memory management}.  When @tt{stack_shifts} is selected,
the first one is the number of shifts (reallocations) of the local
stack; the second is the number of shifts of the trail, and the third
is the time spent in these shifts.  When @tt{garbage_collection} is
selected, the numbers are, respectively, the number of garbage
collections performed, the number of bytes freed, and the time spent
in garbage collection.".

gc_result([A, B, C]):- int(A), int(B), int(C).

:- doc(doinclude, symbol_result/1).

:- true prop symbol_result(Result) + regtype # "@var{Result} is a
   two-element list of integers.  The first one is the number of atom,
   functor, and predicate names in the symbol table.  The second is
   the number of predicates known to be defined (although maybe
   without clauses).".

symbol_result([A, B]):- int(A), int(B).

 %% memory_option(core).
 %% memory_option(heap).

statistics(runtime,    L) :- '$runtime'(L).
statistics(usertime,   L) :- '$usertime'(L).
statistics(systemtime, L) :- '$systemtime'(L).
statistics(walltime,   L) :- '$walltime'(L).

statistics(runtick,    L) :- '$runtick'(L).
statistics(usertick,   L) :- '$usertick'(L).
statistics(systemtick, L) :- '$systemtick'(L).
statistics(walltick,   L) :- '$walltick'(L).

statistics(runclockfreq,    L) :- '$runclockfreq'(L).
statistics(userclockfreq,   L) :- '$userclockfreq'(L).
statistics(systemclockfreq, L) :- '$systemclockfreq'(L).
statistics(wallclockfreq,   L) :- '$wallclockfreq'(L).

statistics(memory, L) :- '$total_usage'(L).
statistics(symbols, L) :- '$internal_symbol_usage'(L).
statistics(program, L) :- '$program_usage'(L).
statistics(global_stack, L) :- '$termheap_usage'(L).
statistics(local_stack, L) :- '$envstack_usage'(L).
statistics(trail, L) :- '$trail_usage'(L).
statistics(choice, L) :- '$choice_usage'(L).
statistics(core, L) :- statistics(memory, L).
statistics(heap, L) :- statistics(program, L).

statistics(garbage_collection, L) :- '$gc_usage'(L).
statistics(stack_shifts, L) :- '$stack_shift_usage'(L).

% ---------------------------------------------------------------------------

:- trust pred current_atom(Atom) => atm
   # "Enumerates on backtracking all the existing atoms in the system.".
:- impl_defined(current_atom/1).

% ---------------------------------------------------------------------------

:- trust pred new_atom(Atom) : var => atm # "Returns, on success, a new
atom, not existing before in the system.  The entry argument must be a
variable.  The idea behind this atom generation is to provide a fast
source of identifiers for new objects, concurrent predicates, etc. on
the fly.".
:- impl_defined(new_atom/1).

% ---------------------------------------------------------------------------

:- pred current_module(Module) => internal_module_id + native #
   "Retrieves (on backtracking) all the loaded modules (either
    statically or dynamically).".

:- doc(current_module/1,
	"This predicate will successively unify its argument with all
	 module names currently loaded. Module names will be simple atoms.

         When called using a free variable as argument, it will
         retrieve on backtracking all modules currently loaded. This
         is useful when called from the Ciao @apl{toplevel}.

         When called using a module name as argument it will check
         whether the given module is loaded or not. This is useful
         when called from user programs.
        ").

current_module(Module) :- '$current_module'(Module).

% ---------------------------------------------------------------------------

% The reverse of internals:module_concat/3
% TODO: move together with internals:module_concat/3?
% TODO: inefficient, write in C or adopt a hash-table approach like in optim_comp
module_unconcat(MF, M, F) :-
	atom_codes(MF, MFc),
	append(Mc, [0':|Fc], MFc), !,
	atom_codes(M, Mc),
	atom_codes(F, Fc).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

% ---------------------------------------------------------------------------

% TODO: This may be safe to include in a intronspection (or reflection
%   if some form of self-modification is allowed)

% TODO: Define an unsafe version of predicate_property/2 that do not
%   restricts to visible predicates?

:- doc(bug, "
  The predicate @pred{predicate_property/2} needs more work:
  @begin{itemize}

  @item{Efficiency:} In order to be complete and efficient, this needs
    to be a built-in predicate of our module system. Consulting
    predicate properties does not seem a dangerous operation (except
    that, when it cannot be resolved at compile-time, it prevents
    removal of module runtime information).

  @item{Correctness:} The head is automatically module-expanded on
    call. If the head is not module-expanded, there are consistency
    problems.  Other systems avoid those problems by disallowing the
    import of two predicates with the same name from different
    modules. That is clearly not a solution in Ciao.

  @end{itemize}
").   

:- doc(bug, "Implement a @pred{'$predicate_property'/2} where the
	module can be specified. That will simplify the
	@pred{predicate_property/2} implementation").

% TODO: This was disabled (it should not) --JFMC
%:- primitive_meta_predicate(predicate_property(fact,?)).
:- primitive_meta_predicate(predicate_property(fact,addmodule)).

:- test predicate_property(Head, Prop) :
	( Head = true ) => ( Prop = compiled )
	# "Predicate property of @pred{true/0} is compiled".

:- pred predicate_property(Head, Property)
   => callable * atm
   # "The predicate @var{Head}, visible from the current module, (a
     goal) has the property @var{Property}.".

% (CallerM added by 'addmodule')
predicate_property(Head, Prop, CallerM) :- nonvar(Head), !,
	'$predicate_property'(Head, Entry, Bits), % xref nondet.c
	( predicate_property_bits(Entry, Bits, Prop)
	; % TODO: The slow part, reimplement
	  functor(Head, MF, N),
	  module_unconcat(MF, EM, F),
	  % TODO: bug: the resolved predicate only give us the
	  %       effective module name, so we cannot distinguish
	  %       between querying the properties of a reexported
	  %       predicate or a directly exported one. We will give
	  %       the results of only for the more direct one at this
	  %       point. (JFMC) Using 'addterm' could partially solve
	  %       this problem.
	  %
	  %       Test case:
          %         - predicate p imported from mod_a, mod_b
          %	    - mod_a: defines p
	  %	    - mod_b: reexports p from a
	  %	    - mod_c: imports mod_a, imports mod_b
	  %
	  %       Then the following queries from mod_c will:
	  %         - predicate_property(a:p, P): works as expected
	  %         - predicate_property(b:p, P): works as expected
	  %         - predicate_property(p, P): may give wrong results
	  %             
          %       In the last query, p may refer to 'a:p' or 'b:p'
	  %       depending on the choice of the module system but
	  %       predicate_property will always try the directly
	  %       exported predicate (we must make a choice because
	  %       meta expansion hides the importing module from us).
	  %
	  % obtain the imported module from the effective one
	  % TODO: see 'bug' entry above
	  ( '$imports'(CallerM, EM, F, N, EM) -> IM = EM % try direct import route first
	  ; '$imports'(CallerM, IM, F, N, EM) -> true % try as a reexport
	  ),
	  predicate_property_mod(IM, EM, F, N, Prop)
	).
% Note: be careful with this case, non-instantiated meta-arguments may
%       not behave as expected yet
predicate_property(Head, Prop, CallerM) :-
	% (enumerate all predicates defined or visible from the module)
	% TODO: we need a '$predicate_property' that asks for the
	%       module, this is slow too.
	( '$defines'(CallerM, F, N), EM = CallerM, IM = CallerM
	; '$imports'(CallerM, IM, F, N, EM)
	),
	%
	module_concat(EM, F, MF),
	functor(Head, MF, N),
	'$predicate_property'(Head, Entry, Bits), % xref nondet.c
	%
	( predicate_property_bits(Entry, Bits, Prop)
	; % TODO: The slow part, reimplement
	  predicate_property_mod(IM, EM, F, N, Prop)
	).

predicate_property_bits(Entry, Bits, Prop) :-
	(   Entry=8 -> BaseProp=interpreted	% xref objdefs.h
        ;   BaseProp=compiled
        ),
	predicate_property_(Bits, BaseProp, Prop).

predicate_property_(0, P, P) :- !.
predicate_property_(_, P, P).
predicate_property_(Bits0, _, P) :-
	% Bits0 cannot be 0,
	% obtain in B the smaller power of 2 so that
	%   Bits0 = Bits LOGICAL_XOR B
	%   (e.g. Bits0 = 2'10101010, Bits = 2'10101000, B = 2'10)
	Bits is Bits0/\(Bits0-1),
	B is Bits0-Bits,
	%
	bit_decl(B, Prop),
	predicate_property_(Bits, Prop, P).

bit_decl(1, (concurrent)).
bit_decl(2, (dynamic)).
bit_decl(4, (wait)).
bit_decl(8, (multifile)).

% TODO: Prop=exported cannot be implemented until '$exported' is added
predicate_property_mod(IM, _EM, _F, _N, Prop) :-
	Prop = imported_from(IM).
predicate_property_mod(_IM, EM, F, N, Prop) :-
	functor(G, F, N),
	% TODO: It repeats solutions!
	'$meta_args'(EM, G),
	Prop = meta_predicate(G).

%% % from the analysis:
%% %LibCert% :- true success predicate_property(A,B) => ( callable(A), rt20(B) ).
%% 
%% :- prop rt20/1 + regtype.
%% 
%% rt20(compiled).
%% rt20(concurrent).
%% rt20(dynamic).
%% rt20(interpreted).
%% rt20(multifile).
%% rt20(wait).

% ---------------------------------------------------------------------------

:- trust pred garbage_collect # "Forces garbage collection when called.".
:- impl_defined(garbage_collect/0).

:- doc(hide, set_heap_limit/1).
:- doc(hide, current_heap_limit/1).

:- pred set_heap_limit(Limit) : integer(Limit) # "Sets the
@concept{heap limit} to the largest multiple of word size smaller than
@var{Limit}.  If more than @concept{heap limit} kilobytes of heap are
used then throw an exception. This behaviour is disabled if the flag
is set to a null value.  This limit does not directly influence the
real size of the heap, but just limits the amount of memory used
within. Initially @concept{heap limit} is set to 0.".

set_heap_limit(0) :- !,             % Call without heap consumption 
	'$heap_limit'(0).
set_heap_limit(Limit):- 
	integer(Limit), 
	NewLimit is Limit // 4,     % 4 stands for sizeof(tagged_t)
	(
	    try_to_set_heap_limit(NewLimit) ->
	    true
	;
	    garbage_collect, 
	    try_to_set_heap_limit(NewLimit) ->
	    true
	;
	    throw(error(resource_error(heap), set_heap_limit/1))
	).

try_to_set_heap_limit(NewLimit):-
	statistics(global_stack, [GlobalStack, _]), 
	GlobalStackSize is (GlobalStack + 1) // 4, 
	GlobalStackSize < NewLimit, 
	'$heap_limit'(NewLimit).


:- pred current_heap_limit(Limit) : true => integer(Limit) # "Unifies 
@var{Limit} to the current @concept{heap limit}".

current_heap_limit(Limit) :-
	'$heap_limit'(CurrentLimit), 
	Limit is CurrentLimit * 4.

% ---------------------------------------------------------------------------

% :- use_module(engine(internals), [
% 	'$unknown'/2, '$ferror_flag'/2, '$prompt'/2, '$unix_argv'/1,
% 	'$quiet_flag'/2, '$gc_trace'/2, '$gc_margin'/2, '$gc_mode'/2,
% 	'$compiling'/2, '$ciao_version'/6]).

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

:- doc(current_prolog_flag(FlagName, Value),
"@var{FlagName} is an existing flag and @var{Value} is the
           value currently associated with it.").
:- pred current_prolog_flag/2 => atm * term.
% inferred:
%:- true pred current_prolog_flag(A,B)
%         : ( term(A), term(B) )
%        => ( rt274(A), term(B) ).

current_prolog_flag(X, Y) :- prolog_flag(X, Y, Y). /* ISO */

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
:- pred pop_prolog_flag(+atm) => atm.

pop_prolog_flag(Flag) :-
	nonvar(Flag),
	retract_fact(old_flag(Flag, OldValue)),
	!, % to avoid removal on backtracking --EMM
	prolog_flag(Flag, _, OldValue).


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
