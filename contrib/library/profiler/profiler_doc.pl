:- use_package(assertions).
:- doc(nodoc, assertions).

:- doc(title, "The Ciao Profiler").
:- doc(subtitle_extra, "REFERENCE MANUAL").
:- doc(subtitle_extra, "@em{Generated/Printed on:} @today{}").

:- doc(author, "Edison Mera").
:- doc(copyright,"Copyright @copyright{} Manuel Hermenegildo

@include{DocCopyright.lpdoc}

").

:- doc(summary, "The profiler module provides predicates and
   declarations to automatically handle profiling of the program to
   measure the time spent by predicates (or literals), as well as
   other cumulative statistics such as number of calls.  It is made up
   of two parts: one implemented at the Ciao level, and another one
   implemented in C. The latter requires compiling the engine with the
   @tt{--debug-level=profile} or @tt{--debug-level=profile-debug}
   options. For more information, see the @tt{./ciao-boot.sh help}.").

:- doc(module, "The Ciao profiler provides a high-level, flexible
way to mark a predicate (or a literal) for profiling. This is done by
using declarations to indicate if a program element must be
instrumented or not.

By default, if the user does not specify anything, no predicate inside
the module will be instrumented as cost center for profiling.  The use
of at least one declaration saying that a specific predicate must be
instrumented overrides this behavior.

The declaration is as follows:

@begin{verbatim}
	:- cost_center pred1/Arity1, ... predN/ArityN.
@end{verbatim}

where @pred{pred1/Arity1}, ..., @pred{predN/ArityN} are the predicates to be 
instrumented as cost centers. They can be separated by commas or the can be 
in a list. 

By default the engine hooks of all defined cost center are
active. The declaration:

@begin{verbatim}
      (predN/ArityN,nohooks)	
@end{verbatim}

will deactivate them.

Another useful declaration makes possible to indicate that a given
predicate is not going to be instrumented as cost center:

@begin{verbatim}
	:- no_cost_center pred1/Arity1, ... predN/ArityN.
@end{verbatim}

where @pred{pred1/Arity1}, ..., @pred{predN/ArityN} are the predicates
that will not be instrumented as cost centers. There are two options (as in
the previous case): write one assertion for each predicate or declare
more than one predicate (separated by commas or in a list) in only one 
assertion. 

The following assertions define the behavior of all the predicates of 
the module: 

@begin{verbatim}
	:- all_cost_center.
@end{verbatim}

@begin{verbatim}
	:- all_no_cost_center.
@end{verbatim}

They specify respectively that all the predicates in the module will be 
instrumented as cost centers and that no predicate in the module will be 
instrumented as cost center. In the first assertion the engine hooks of 
all defined cost centers are active. The declaration 
@tt{:- all_cost_center(nohooks).} will deactivate them.

Cost centers can be also defined at literal level replacing the literal 
by the declaration:

@begin{verbatim}
	:- cost_center(name_cc, literal)
@end{verbatim}

where @var{literal} is the program literal and @var{name_cc} is the name of its
associated cost center. At predicate level the name of both cost
center and predicate are equal. ").
