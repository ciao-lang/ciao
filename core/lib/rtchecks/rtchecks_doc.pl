:- use_package(assertions).
:- doc(nodoc, assertions).

:- doc(title, "Run-time checking of assertions").

:- doc(author, "Edison Mera").
:- doc(author, "Nataliia Stulova").

:- doc(summary, "This package provides a complete implementation
	of run-time checks of predicate assertions. The program is
	instrumented to check such assertions at run time, and in case
	a property does not hold, the error is reported. It is possible
        to define two different property versions: one purely declarative
        and one specifically crafted for run-time checking. Note that
	there is also an older package called rtchecks, by
	@author{David Trallero Mena}. The advantage of this one is
	that it can be used independently of CiaoPP and also has
	updated functionality.").

:- doc(module,

"This package provides a complete implementation of run-time checks of
predicate assertions. The program is instrumented to check such
assertions at run time, and in case a property does not hold, the
error is reported.  Note that there is also an older package called
rtchecks, by David Trallero. The advantage of this one is that it can
be used independently of CiaoPP and also has updated functionality.

There are two main applications of run-time checks:

@begin{itemize}

  @item To improve debugging of certain predicates, specifying some
  expected behavior that is checked at run-time with the assertions.

  @item To avoid manual implementation of run-time checks that should
  be done in some predicates, leaving the code clean and
  understandable.

@end{itemize}

@section{Run-time checks configuration with flags}

The run-time checks can be configured using prolog flags.  Below we
itemize the valid prolog flags with its values and a brief
explanation of the meaning:

@begin{itemize}

@item @code{rtchecks_level}
  @begin{itemize}
   @item @code{exports}: Only use rtchecks for external calls of the
                         exported predicates.
   @item @code{inner}  : Use also rtchecks for internal calls. Default.
  @end{itemize}

@item @code{rtchecks_trust}
  @begin{itemize}
   @item @code{no}     : Disable rtchecks for trust assertions.
   @item @code{yes}    : Enable  rtchecks for trust assertions. Default.
  @end{itemize}

@item @code{rtchecks_entry}
  @begin{itemize}
   @item @code{no}     : Disable rtchecks for entry assertions.
   @item @code{yes}    : Enable  rtchecks for entry assertions. Default.
  @end{itemize}

@item @code{rtchecks_exit}
  @begin{itemize}
   @item @code{no}     : Disable rtchecks for exit assertions.
   @item @code{yes}    : Enable  rtchecks for exit assertions. Default.
  @end{itemize}

@item @code{rtchecks_test}
  @begin{itemize}
   @item @code{no}     : Disable rtchecks for test assertions. Default.
   @item @code{yes}    : Enable  rtchecks for test assertions. Used for
                 debugging purposes, but is better to use the unittest library.
  @end{itemize}

@item @code{rtchecks_inline}
  @begin{itemize}
   @item @code{no}     : Instrument rtchecks using call to library
			 predicates present in @lib{rtchecks_rt.pl},
			 @lib{nativeprops.pl} and @lib{basic_props.pl}.
			 In this way, space is saved, but sacrifying
			 performance due to usage of meta calls and
			 external methods in the libraries. Default.
   @item @code{yes}    : Expand library predicates inline as far as possible.
			 In this way, the code is faster, because its avoids
			 metacalls and usage of external methods, but the final
			 executable could be bigger.
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

@item @code{rtchecks_namefmt}
  @begin{itemize}
   @item @code{long}   : Show the name of predicates, properties and the
			 values of the variables
   @item @code{short}  : Only show the name of the predicate in a reduced
			 format. Default.
  @end{itemize}

@item @code{rtchecks_abort_on_error}

  Controls if run time checks must abort the execution of a program
  (by raising an exception), or if the execution of the program have
  to continue.

  Note that this option only affect the default handler and the
  predicate @pred{call_rtc/1}, so if you use your own handler it will
  not have effect.

  @begin{itemize}
   @item @code{yes} : Raising a run time error will abort the program.
   @item @code{no}  : Raising a run time error will not stop the execution,
                      but a message will be shown. Default.
  @end{itemize}

@end{itemize}

@section{Creating versions of properties}

@begin{alert}
 @bf{Note:} this is a new feature and under active development. The
 documentation may be partial/obsolete.
@end{alert}

In some cases declarative property definitions are not efficient enough
to be used in the run-time checks instrumentation. In Ciao it is
possible to write an alternative version of property that can be used
specifically in run-time checks, while keeping the main version 
(which might be easier to read or is better understood by some static
analyzer).

To provide the custom property implementation for run-time checking
it is necessary to edit two files (suppose the original property is
defined in a module @file{foo.pl}):

@begin{itemize}

  @item the @file{foo_rtc.pl} module that contains the custom
    property implementation and is placed in the same folder as
    @file{foo.pl}. Make sure the custom property implementation is
    exported from both modules.


  @item @file{$CIAOROOT/core/lib/rtchecks/rtcheks_rt_propimpl.pl}, a
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

").

:- doc(usage,":- module(...,...,[...,rtchecks]).").

:- doc(appendix,
        "@begin{alert}
           @bf{Note:} the @tt{assertions} package must always be included
           together with the @tt{rtchecks} package
           (see @tt{core/lib/compiler/global_module_options.pl} for details).
         @end{alert}").
