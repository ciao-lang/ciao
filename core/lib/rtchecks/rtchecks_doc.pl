:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title, "Run-time checking of assertions").

:- doc(author, "The Ciao Development Team").

:- doc(module,"
The @lib{rtchecks} package enables run-time checks of predicate and
program-point assertions. The program is instrumented to capture 
violation of assertions at run time.

For a given predicate, the main uses of run-time assertion checking
are:

@begin{itemize}

@item Checking that the predicate @bf{behaviour} w.r.t. the assertions
  holds (e.g., properties on the success part of the assertion),
  detecting problems in its implementation.

@item Checking that the predicate is @bf{called} correctly (e.g.,
  properties on calls part of the assertion), detecting problems in
  its usage. In some cases, the run-time check instrumentation
  performed by this package can simplify the manual introduction of
  @em{defensive} checks in predicates (e.g., throwing exceptions when
  a wrong usage is detected), resulting in cleaner code.

@end{itemize}

@begin{note}
Run-time checks may incur a noticiable run-time overhead, and in the
case of recursive code with complex checks, worsen the algorithmic
complexity. If performance is a problem, check @ref{Controlling
assertion checking levels} and @ref{Optimization of run-time checks
(experimental)} for more information.
@end{note}

@section{Examples of using run-time checks to detect program errors}

As described in @em{\"@ref{The Ciao assertion language}\"} and
@em{\"@ref{Types and properties related to assertions}\"}, Ciao
assertions can be used to provide predicate specifications, and in
particular, which properties should hold on predicate calls and
successes.  There can be several assertions for one predicate, e.g.,
in order to specify several possible calls and success patterns.

Usually they are provided before the predicate definition in the
source code:

```ciao
% calculating length (as number of elements) N of a list L

:- pred len(L,N) : list(L)              => num(N) # \"A1\".
:- pred len(L,N) : (nnegint(N), var(L)) => list(L)# \"A2\".

len([],0).
len([_|T],N) :- len(T,M), N is M + 1.
```

The specification (assertions @var{A1} and @var{A2}) for the
@tt{len/2} predicate enumerates two possible ways for this predicate
to be called:

@begin{itemize}
@item @var{A1}: if called with its first argument @var{L} a list
      (@lib{basic_props}@tt{:}@pred{list/1})
      then on success
      its second argument @var{N} should be a number
      (@lib{basic_props}@tt{:}@pred{num/1}).
      Here, as no property is specified to hold on calls for the second
      argument @var{N}, the most general
      property (@lib{basic_props}@tt{:}@pred{term/1}) is assumed for it;
@item @var{A2}: if called with its second argument @var{N} a non-negavive
      integer (@lib{basic_props}@tt{:}@pred{nnegint/1}), and the first
      argument @var{L} a free variable (@lib{term_typing}@tt{:}@pred{var/1}),
      then on success
      its first argument @var{L} should be a list;
@end{itemize}

If no @em{assertion status} is specified explicitly, it is assumed
that an assertion has a status @tt{check}.  By default, all assertions
for a predicate with the @tt{check} status are turned into run-time
checks (see @ref{Controlling assertion checking levels}).

There are several cases in which a run-time check may fail:

@begin{enumerate}

@item a predicate was called with arguments that are outside of the
      acceptable call patterns (@em{calls check} failure);
      which can happen due to:

@item a predicate succeeded with argument values that are incompatible
      with the expected success pattern (@em{success check} failure),
      which can happen due to:

      @begin{enumerate}
      @item a bug in the implementation of the predicate (most often cause);
      @item a too restrictive specification;
      @end{enumerate}

@end{enumerate}

Below we illustrate each of the cases mentioned above.

@bf{Case 1:} suppose that we call @tt{len/2} with the wrong arguments,
run-time checks will detect it and report this message:

```ciao-inferior
?- len(a,z).
{In foo.pl
ERROR: (lns 16-17) Run-time check failure in assertion for:
    foo:len(L,N).
In *calls*, unsatisfied property:
    list(L).
Because:
    ['L'=a].
ERROR: (lns 18-21) Failed in foo:len(L,N).
}

{In foo.pl
ERROR: (lns 17-18) Run-time check failure in assertion for:
    foo:len(L,N).
In *calls*, unsatisfied property:
    nnegint(N).
Because:
    ['N'=z].
ERROR: (lns 18-21) Failed in foo:len(L,N).
}
```

Note how run-time checks report the first violated property:
in the two error messages here for two @tt{pred} assertions only
one property violation is mentioned in each, although the
@tt{var(L)} from @var{A2} is clearly violated too.

However, this situation might also occur when the initial
call is made with the acceptable arguments, but the computation
is performed in a way that is not compatible with the specification:

```ciao-inferior
?- len(L,1).
{In foo.pl
ERROR: (lns 16-17) Run-time check failure in assertion for:
    foo:len(L,N).
In *calls*, unsatisfied property:
    list(L).
Because:
    ['L'=L].
ERROR: (lns 18-21) Failed in foo:len(L,N).
ERROR: (lns 18-21) Failed during invocation of len(_,_)
}

{In foo.pl
ERROR: (lns 17-18) Run-time check failure in assertion for:
    foo:len(L,N).
In *calls*, unsatisfied property:
    nnegint(N).
Because:
    ['N'=N].
ERROR: (lns 18-21) Failed in foo:len(L,N).
ERROR: (lns 18-21) Failed during invocation of len(_,_)
}
```

Notice, how in the recursive clause of the @tt{len/2} the
unification for @var{M} happens after the evaluation of
@tt{len(T,M)} goal (due to the system-default
left-to-right goal sequence reduction).

In this case we can re-write the @tt{len/2} predicate, making
it left-recursive and fully complying with its specification:

```ciao
len([],0).
len([_|T],N) :- M is N - 1, len(T,M).
```

@bf{Case 2.1:} imagine that incidentally a bug was introduced in
the first clause of @tt{len/2}:

```ciao
len(_,0).
len([_|T],N) :- M is N - 1, len(T,M).
```

Run-time checks will detect that now @tt{len/2} constructs not
null-terminating lists:

```ciao-inferior
?- len(L,1).
{In foo.pl
ERROR: (lns 17-18) Run-time check failure in assertion for:
    foo:len(L,N).
In *success*, unsatisfied property:
    list(L).
ERROR: (lns 18-21) Failed in foo:len(L,N).
ERROR: (lns 18-21) Failed during invocation of len(_,_)
}

{In foo.pl
ERROR: (lns 17-18) Run-time check failure in assertion for:
    foo:len(L,N).
In *success*, unsatisfied property:
    list(L).
Because:
    ['L'=[_|_]].
ERROR: (lns 18-21) Failed in foo:len(L,N).
}
```

@bf{Case 2.2:} suppose the specification for @tt{len/2} instead of
@var{A2} contains @var{A2_too_strict}, which requires that on success
@var{L} must be a list of integers, not just any terms.

```ciao
:- pred len(L,N) : (nnegint(N), var(L)) => list(int,L). % A2_too_strict
```

Run-time checks will detect a failure of such assertion:

```ciao-inferior
?- len(L,1).
{In foo.pl
ERROR: (lns 17-19) Run-time check failure in assertion for:
    foo:len(L,N).
In *success*, unsatisfied property:
    list(int,L).
Because:
    ['L'=[_]].
ERROR: (lns 19-21) Failed in foo:len(L,N).
}
```

@comment{NS: added example from my thesis. look also at rtchecks_cost?}

@bf{Note on performance}: run-time checks usually affect the
complexity of the program, which results in noticeable run-time
overhead. Consider a simplified specification of the @tt{len/2}
predicate:

```ciao
:- pred len(L,N) : list(L) => num(N).

len([],0).
len([_|T],N) :- M is N - 1, length(T,M).
```

Checking that the first argument of the @tt{len/2}
predicate is a list at each recursive step turns the standard @em{O(n)}
algorithm into @em{O(n^2)} one.

@section{Controlling assertion checking levels}

The @lib{rtchecks} package supports several assertion checking levels
for different scenarios, which allow controlling the trade-offs
between the number of run-time checks (@em{overhead}) and the
respective code behavior guarantees (@em{safety}).

The run-time checking level can be configured using prolog flags.
Below we itemize the valid prolog flags with its values and a brief
explanation of the meaning:

@begin{itemize}

@item @code{rtchecks_level}
  @begin{itemize}
   @item @code{exports}: Only use rtchecks for external calls of the
                     exported predicates.
   @item @code{inner}  : Use also rtchecks for internal calls. Default.
  @end{itemize}

@comment{% TODO: T256 common run-time checks flags behavior}
@comment{% TODO: T232}

@comment{NS: TODO: wrong default for rtchecks_trust?}
@item @code{rtchecks_trust}: By definition @pred{trust/1} assertions
  are @em{trusted} by the compiler and analyzer and their information
  is taken to be true. Enable to detect cases where these assertions
  may be erroneous).

  @begin{itemize}
   @item @code{no}     : Disable rtchecks for trust assertions.
   @item @code{yes}    : Enable  rtchecks for trust assertions. Default.
  @end{itemize}

@comment{JF, NS: deprecate with rtchecks_opt this flag might make no sense anymore}
@item @code{rtchecks_entry} (deprecated in @lib{rtchecks_opt})
  @begin{itemize}
   @item @code{no}     : Disable rtchecks for entry assertions.
   @item @code{yes}    : Enable  rtchecks for entry assertions. Default.
  @end{itemize}

@comment{JF, NS: deprecate with rtchecks_opt this flag might make no sense anymore}
@item @code{rtchecks_exit} (deprecated in @lib{rtchecks_opt})
  @begin{itemize}
   @item @code{no}     : Disable rtchecks for exit assertions.
   @item @code{yes}    : Enable  rtchecks for exit assertions. Default.
  @end{itemize}

@end{itemize}

@begin{note}
See the experimental @lib{rtchecks_opt} package (@ref{Compile-time
checks and assertion simplification}) for an alternative configuration
of checking levels.
@end{note}

@section{Custom implementation for property checks}

The run-time check instrumentation can use custom property checks
implementations. This is useful when defining native properties
(without a clause-based definition supported by the default
instrumentation) or when the generic run-time checks are not efficient
enough. The custom implementation can be used specifically in run-time
checks, while keeping a declarative version (which might be easier to
read or is better understood by some static analyzer).

To provide the custom property implementation for run-time checking it
is recommended to edit two files (suppose the original property is
defined in a module @file{foo.pl}):

@begin{itemize}
@item the @file{foo_rtc.pl} module that contains the custom property
  implementation and is placed in the same folder as
  @file{foo.pl}. Make sure the custom property implementation is
  exported from both modules.
@item @file{core/lib/rtchecks/rtchecks_rt_propimpl.pl}, a
  database file that stores the links between different property
  implementations in the format of declarations '@tt{:-
  rtc_impl(ModOr:PropOr/ArityOr, ModRt:PropRt/ArityRt).}' where
  @var{ModOr} and @var{ModRt} are names of the two modules that
  contain the original and the custom property definitions,
  @var{PropOr} and @var{PropRt} are the two different property
  implementations with respective arities @var{ArityOr} and
  @var{ArityRt}.
@end{itemize}

After these edits the @lib{rtchecks} library needs to be rebuilt.

For an example of a system library that uses this feature see the
@lib{assertions/native_props} library.

@section{Other run-time checks configuration flags}

@comment{TODO: T256 common run-time checks flags behavior; review these flags}

@begin{itemize}

@comment{% TODO: T232}
@comment{NS: TODO: wrong default for rtchecks_test?}

@item @code{rtchecks_test} (internal flag, used in test case generation)
  @begin{itemize}
   @item @code{no}     : Disable rtchecks for test assertions. Default.
   @item @code{yes}    : Enable  rtchecks for test assertions.
  @end{itemize}

@item @code{rtchecks_asrloc}
  Controls the usage of locators for the assertions in the error messages.
  The locator says the file and lines that contains the assertion that had
    failed. Valid values are:
  @begin{itemize}
   @item @code{no}     : Disabled.
   @item @code{yes}    : Enabled. Default.
  @end{itemize}

@item @code{rtchecks_predloc}
    Controls the usage of locators for the predicate that caused the run-time
    check error.  The locator says the first clause of the predicate that
    the violated assertion refers to.
  @begin{itemize}
   @item @code{no}     : Disabled.
   @item @code{yes}    : Enabled, Default.
  @end{itemize}

@item @code{rtchecks_callloc}
  @begin{itemize}
   @item @code{no}       : Do not show the stack of predicates that caused
                       the failure
   @item @code{predicate}: Show the stack of predicates that caused the
                       failure. Instrument it in the predicate. Default.
   @item @code{literal}  : Show the stack of predicates that caused the
                       failure. Instrument it in the literal. This mode
                       provides more information, because reports also
                       the literal in the body of the predicate.
  @end{itemize}

@comment{JF: short of long by default? NS version had short but impl is not consistent}
@item @code{rtchecks_namefmt}
  @begin{itemize}
   @item @code{long}   : Show the name of predicates, properties and the
                     values of the variables. Default.
   @item @code{short}  : Only show the name of the predicate in a reduced
                     format.
  @end{itemize}

@end{itemize}

").

:- doc(usage,"Add the @tt{rtchecks} package to the module declaration as
@tt{:- module(...,...,[...,rtchecks]).} or include it explicitly as
@tt{:- use_package(rtchecks).}").

:- doc(appendix,"
Known issues:
@begin{itemize}
@item The @tt{assertions} package must always be included together
  with the @tt{rtchecks} package (see
  @tt{core/lib/compiler/global_module_options.pl} for details).

@item Currently not all @tt{rtchecks*} packages support
  interleaving assertions and predicate clauses. See
  @tt{rtchecks/rtchecks_tr.pl} for details.

@item The interaction of custom implementation of property checks with
  other user-defined properties like regular types is buggy.
@end{itemize}

@section{Optimization of run-time checks (experimental)}

@begin{note}
@bf{Usage:} The @lib{rtchecks_opt} package provides three basic
assertion checking levels plus one optimization (see below):
@begin{itemize}
@item @tt{unsafe}: no assertions are turned into run-time checks, zero
run-time overhead added to program execution;
@item @tt{client_safe}: only assertions for the exported predicates are
turned into run-time checks, the rest are ignored;
@item @tt{safe_rt}: all @tt{check} assertions are turned into run-time
checks;
@end{itemize}
@end{note}

Example of a module using run-time checks on one of the default
checking levels:

```ciao
:- module(ex_levels,[test/0,foo/2],[assertions,regtypes]).

:- use_module(engine(io_basic), [display/1,nl/0]).
:- use_package(library(rtchecks_opt/opt_rt_unsafe)).        % no checks
% :- use_package(library(rtchecks_opt/opt_rt_client_safe)). % interface
% :- use_package(library(rtchecks_opt/opt_rt_safe_rt)).     % all (interface + internal)

test :- foo(1,X), bar(X,Y), display(Y), nl.

:- pred foo(A,B) : int(A) => atm(B) # \"A1\".

foo(1,a).

:- pred bar(C,D) : atm(C) => atm(D) # \"A2\".

bar(a,z).
```

On the @tt{unsafe} checking level no assertions will be checked, on the
@tt{client_safe} only @var{A1} will be checked, and on @tt{safe_rt}
both @var{A1} and @var{A2} will be checked.

@subsection{Compile-time checks and assertion simplification}

@comment{TODO: review together with NS, IG, JF, and MH for the use of
CiaoPP and `trust` assertions here}

As an optimization for the @tt{safe_rt} assertion checking level,
a variant of it, @tt{safe_ctrt} level, is available:

@begin{itemize}

@item @tt{safe_ctrt}: the program is subjected to static analysis
pass, remaining @tt{check} assertions after it are turned into
run-time checks. In any case, @tt{calls} assertions for exported
predicates will always be turned into checks.

@end{itemize}

It is also available through the @lib{rtchecks_opt} package:

```ciao
:- use_package(library(rtchecks_opt/opt_rt_safe_ctrt)). % all + static analysis
```

If the @tt{ex_levels} module from the example above was compiled
on this checking level, the analysis would be able to prove
@var{A2} to always hold, as well as the success part of @var{A1},
thus leaving only the calls part of @var{A1} for run-time checking:

```ciao
%% %% :- check pred foo(A,B)
%% %%    : int(A)
%% %%   => atm(B).

:- check calls foo(A,B)
     : int(A).

:- true success foo(A,B)
     : int(A)
    => atm(B).

%% %% :- check pred bar(C,D)
%% %%    : atm(C)
%% %%   => atm(D).

:- true success bar(C,D)
     : atm(C)
    => atm(D).

:- checked calls bar(C,D)
     : atm(C).
```

More details are available from the related publication
@cite{optchk-journal-scp}.

@subsection{Shallow run-time checking}

Shallow run-time checking is an optimization of run-time checks of
regular types in predicate calls across module boundaries in presence
of hidden functors (via @tt{:- hide} declaration of the @lib{termhide}
package). It can significantly reduce the run-time checking overhead,
especially in the cases where run-time checks change complexity of
predicate execution. More details are available from the related
publication @cite{termhide-padl2018}.

@begin{note}
@bf{Usage:} Include @lib{rtchecks_shallow}, together with the
@lib{termhide} package:
```ciao
:- module(MOD, _, [assertions,termhide,rtchecks_shallow]).
```
@end{note}

Example:

```ciao
:- module(bintrees,[root/2],[assertions,regtypes,nativeprops]).
%:- use_package(library(rtchecks_opt/shallow_rt_unsafe)).     % no checks
:- use_package(library(rtchecks_opt/shallow_rt_client_safe)). % interface
%:- use_package(library(rtchecks_opt/shallow_rt_safe_rt)).    % all (interface + internal)
%:- use_package(library(rtchecks_opt/shallow_rt_safe_ctrt)).  % all + static analysis

:- hide(empty/0).
:- hide(tree/3).

:- regtype val_t/1.
val_t(X) :- int(X).

:- regtype tree/1.
tree(empty).
tree(tree(LC,X,RC)) :- tree(LC),val_t(X),tree(RC).

:- pred root/2 : tree * term => tree * val_t.

root(tree(_,X,_),X).
```

Here, the precondition checks for the @tt{root/1} predicate change its
complexity from constant to linear in the size of the input argument,
as the checking code will need to traverse the input tree term to
verify that it is a well-formed tree.

In presence of hidden functor terms, however, it might be possible to
infer data shapes produced by the module, and simplify run-time
checks w.r.t. this information.

```ciao
:- regtype 'tree$shallow'/1.
'tree$shallow'(empty).
'tree$shallow'(tree(LC,X,RC)) :- term(LC),term(X),term(RC).
```

@begin{note}
@bf{Note}: currently it is not possible to print/output the inferred
shallow regular types, as it is implemented as an internal compiler
pass. However, they can be viewed at the WAM level. To produce the
@tt{MODULE.wam} expansion of the target module, in the Ciao shell do:
```ciao-inferior
?- use_module(library(compiler)).
Note: module compiler already in executable, just made visible

yes
?- make_wam(MODULE).
```
@end{note}

The resulting shallow regular types for the @tt{bintrees} module from the
example above will look like this:

```ciao
clause('bintrees:tree$shallow/1/1',
   [ifshallow
   ,neck(1)
   ,else
   ,endif
   ,get_constant_x0('bintrees:empty')
   ,proceed]).

clause('bintrees:tree$shallow/1/2',
   [ifshallow
   ,neck(1)
   ,else
   ,endif
   ,get_structure_x0(/('bintrees:tree',3))
   ,unify_x_variable(0)
   ,allocate
   ,unify_y_variable(1)
   ,unify_y_variable(0)
   ,init([])
   ,call(/('basic_props:term',1),2)
   ,put_y_value(1,0)
   ,call(/('basic_props:term',1),1)
   ,put_y_value(0,0)
   ,deallocate
   ,execute(/('basic_props:term',1))]).
```

@subsection{Caching run-time checks}

@begin{note}
@bf{Usage}: Use the @lib{rtchecks_cached} package:
```ciao
:- module(..., ..., [...,assertions,rtchecks_cached]).
```
@end{note}

More details are available from the related publication
@cite{cached-rtchecks-iclp2015}.

@begin{alert}
 @bf{Bug:} currently checks only for a limited subset of predefined
 regtypes can be cached. See @lib{rtchecks_cached}@tt{/README} for
 details.
@end{alert}

Example of use:

```ciao
:- module(amqueue,[enqueue/3],[assertions,regtypes,rtchecks_cached]).

:- def_impl(var/1,fast_var/2).
:- def_impl(term/1,fast_term/2).
:- def_impl(pair_list/1,fast_listpair_cached/2).

:- pred enqueue/3 :  pair_list * term * var
              => pair_list * term * pair_list.

enqueue((Ins,Del),Elem,([Elem|Ins],Del)).

:- regtype pair_list/1.

pair_list((X,Y)) :- list(X), list(Y).
```

The @tt{def_impl/2} declaration is needed at the moment to relate the
regtype definitions in the source code and the corresponding checks
with caching.

See @lib{testsuite}@tt{/rtchecks_cached/} for examples and benchmarks.

@subsection{Description of the run-time checks intrumentation}

@begin{alert}
For testing instructions of a specific run-time checks implementation
(packages @tt{rtchecks}, @tt{rtchecks_cached}, @tt{rtchecks_opt}, and
@tt{rtchecks_shallow}) see the README files of the respective
@tt{testsuite/rtchecks*} directory.
@end{alert}

The run-time check instrumentation of a program transforms it as
follows:
@begin{itemize}
@item assertions are expanded into checkable @em{calls}
and @em{success} assertion conditions;
@item a wrapper clause is generated for every instrumented predicate
@end{itemize}

Schematically, the run-time checks instrumentation looks as follows:

```ciao
% original program source
:- pred P : Pre1 => Post1.
...
:- pred P : PreN => PostN.

P :- Body.
```

```ciao
% instrumented program source
P :-
    checkC(Pre1;...;PreN),
    Pnew,
    checkS(Pre1,Post1),...,checkS(PreN,PostN).

Pnew :- Body.
```

  where:

@begin{itemize}

@item @tt{checkC/1} is a simplified version of the @em{preconditions} check
      produced from the calls assertion condition, that checks if at least one
      calls pattern is satisfied in the predicate call, and emits a run-time
      error otherwise;

@item @tt{checkS/1} is a simplified version of the @em{postcondition} checks,
      produced from a success assertion condition,
      where each individual postcondition check is performed if the respective
      precondition held (with the possibility of emitting a run-time error if
      some property in a postcondition does not hold), or not performed if the
      precondition did not hold (and thus the postcondition is not
      applicable in the current predicate call).

@item @tt{P :- Pnew.} is the wrapper clause that maintains all original calls to
      the predicate @var{P} in the program, while redirecting them to
      @var{Pnew}, so that run-time checks are executed during the execution of
      @var{P}.

@end{itemize}
").

% TODO: T227
%   Notes from JF and NS for future work:
%   - `rtchecks_opt` should be a flag of `rtchecks` package, and not a
%     package itself
%   - `safe_rt` mode should be the default mode
%   - `safe_ctrt` mode should be a modifier of `safe_rt`

% (Deprecated)
% @item @code{rtchecks_abort_on_error}
% 
%   Controls if run time checks must abort the execution of a program
%   (by raising an exception), or if the execution of the program have
%   to continue.
% 
%   Note that this option only affect the default handler and the
%   predicate @pred{call_rtc/1}, so if you use your own handler it will
%   not have effect.
% 
%   @begin{itemize}
%    @item @code{yes} : Raising a run time error will abort the program. Default
%    @item @code{no}  : Raising a run time error will not stop the execution,
%                       but a message will be shown.
%   @end{itemize}

