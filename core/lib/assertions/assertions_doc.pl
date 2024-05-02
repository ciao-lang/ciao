:- use_package([assertions]).

:- doc(filetype,package).

:- use_module(library(assertions/assertions_props)).

:- doc(title,"The Ciao assertion language").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Francisco Bueno").
:- doc(author, "German Puebla").

:- doc(copyright,"
Copyright @copyright{} 1989-2002 The CLIP Group / UPM

@include{FreeDocLicense.lpdoc}
").

:- doc(summary,"This library provides packages and modules which allow
   including @concept{program assertions} in user
   programs. Such assertions can be used to describe predicates,
   properties, modules, applications, etc. These descriptions can be
   contain formal specifications (such as sets of preconditions,
   post-conditions, or descriptions of computations) as well as
   machine-readable textual comments. The information contained in the
   assertions will be used as input by other tools for tasks such as
   static or dynamic debugging, verification, automatic documentation
   generation, etc.").

:- doc(usage, "@tt{:- use_package(assertions).} or @tt{:-
   module(...,...,[assertions]).}. Use the @lib{assertions_basic}
   package to disable the implicit use of the @lib{basic_props}
   module.").

:- doc(module,"The @lib{assertions} package adds a number of new
   declaration definitions and new operator definitions which allow
   including @concept{program assertions} in user
   programs. Such assertions can be used to describe predicates,
   properties, modules, applications, etc. These descriptions can
   contain formal specifications (such as sets of preconditions,
   post-conditions, or descriptions of computations) as well as
   machine-readable textual comments.

   This module is part of the @lib{assertions} library. It defines the
   basic code-related assertions, i.e., those intended to be used
   mainly by compilation-related tools, such as the static analyzer or
   the run-time test generator.

   Here we document mainly the use of assertions for providing
   @concept{specifications} for predicates and other program elements,
   as well as the locations within assertions where machine-readable
   documentation strings can be placed.  The commands that can be used
   in the documentation strings and other directives that can be used
   to provide additional machine-readable comments @cindex{comments,
   machine readable} are described in the autodocumenter (@apl{lpdoc}
   @cite{knuth-lit,lpdoc-tr}) manual.

   There are two kinds of assertions: predicate assertions and program
   point assertions.  Predicate assertions are placed as directives in
   the source code, i.e., preceded by ``@tt{:-}''.  Program point
   assertions are placed as literals in clause bodies.  Additional
   documentation on the syntax and fields of predicate assertions can
   be found in the @ref{Types and properties related to assertions}
   module.

   @section{Getting more information} 

   This documentation is intended to provide information at a
   ``reference manual'' level. For more tutorial introductions to the
   assertion language and more examples please see
   @cite{full-prolog-esop96,prog-glob-an,assert-lang-disciplbook,ciaopp-sas03-journal-scp,hermenegildo11:ciao-design-tplp}
   and the @apl{ciaopp} tutorial.
@comment{
% the document ``An Assertion Language for
%    Debugging of Constraint Logic Programs (Technical Report
%    CLIP2/97.1)''. 
}
%
   The assertion language as implemented in this library essentially follows these
   documents, although, due to its evolution, it may differ in some
   details. The purpose of this manual is to document precisely what
   the implementation of the library supports at any given point in
   time.

   @section{Some attention points} 

   @begin{itemize}

   @item @bf{Formatting commands within text strings:} @cindex{formatting
   commands} many of the predicates defined in these modules include
   arguments intended for providing textual information. This includes
   titles, descriptions, comments, etc. The type of this argument is a
   character string. In order for the automatic generation of
   documentation to work correctly, this @concept{character string}
   should adhere to certain conventions. See the description of the
   @pred{docstring/1} type/grammar for details.

   @item @bf{Referring to variables:} In order for the automatic
   documentation system to work correctly, @concept{variable names}
   (for example, when referring to arguments in the head patterns of
   @em{pred} declarations) must be surrounded by an @tt{@@var}
   command. For example, @tt{@@var@{VariableName@}} should be used for
   referring to the variable ``VariableName'', which will appear then
   formatted as follows: @var{VariableName}. See the description of
   the @pred{docstring/1} type/grammar for details.

   @end{itemize}

").

% ----------------------------------------------------------------------------
:- doc(section,"Predicate-level assertions").
% ----------------------------------------------------------------------------

:- doc(pred/1, "@cindex{pred assertion} @tt{pred} assertions are the
    most general type of assertion, and they are used to provide
    information on the set of admissible calls to a predicate, the set
    of successes, global properties, and documentation.  The body of a
    @tt{pred} assertion (its only argument) contains properties or
    comments in different fields following the formats specified by
    @pred{assrt_body/1}.

    There can be more than one of these assertions per predicate, in
    which case each one represents a possible ``@concept{mode}'' of
    use (@concept{usage}) of the predicate. The exact scope of the
    usage is defined by the properties given for calls in the body of
    each assertion (which should thus distinguish the different usages
    intended). Predicates are typically specified using a @em{set} of
    @tt{pred} assertions, so that together they cover all ways in
    which the predicate is intended be used (all usages). Each
    @tt{pred} assertion is translated internally to a @tt{calls}
    assertion covering all the calling modes and a @tt{success}
    assertion covering the successes for those calls.

    For example, the following assertions would describe all intended
    modes (and the only modes) of use of a predicate @tt{length/2}
    (see @lib{lists}):
    @begin{verbatim}
:- pred length(L,N) : list * var => list * integer
    # ""Computes the length of @var{L}."".
:- pred length(L,N) : var * integer => list * integer
    # ""Outputs @var{L} of length @var{N}."".
:- pred length(L,N) : list * integer => list * integer
    # ""Checks that @var{L} is of length @var{N}."".
    @end{verbatim}
").
:- decl pred(AssertionBody) : assrt_body.



:- doc(pred/2, "@cindex{pred assertion} A @tt{pred} assertion (see
    @pred{pred/1}) can be preceded (as most other assertions) by an
    assertion status (@pred{assrt_status/1}). If no assertion status
    is present (i.e., in @pred{pred/1} assertions) the status is
    assumed to be @tt{check}.

    For example, the following assertion:
     @begin{verbatim}
:- pred length(L,N) : list * var => list * integer.
     @end{verbatim}
     is equivalent to:
     @begin{verbatim}
:- check pred length(L,N) : list * var => list * integer.
     @end{verbatim}
").
:- decl pred(AssertionStatus,AssertionBody) : assrt_status * assrt_body.

% ----------------------------------------------------------------------------

:- doc(calls/1, "@cindex{calls assertion} @tt{calls} assertions are
    similar to @pred{pred/1} assertions but the only provide
    information about the calls to a predicate.  The set of calls
    assertions for a predicate describe @em{all} possible calls to the
    predicate.

    For example, the following assertion describes all possible calls
    to predicate @tt{is/2} (see @lib{arithmetic}):
    @begin{verbatim}
:- calls is(term,arithexpression).
    @end{verbatim}
").
:- decl calls(AssertionBody) : c_assrt_body.

:- doc(calls/2, "@cindex{calls assertion} A @tt{calls} assertion can
    be preceded (as most other assertions) by an assertion status
    (@pred{assrt_status/1}). If no assertion status is present (i.e.,
    in @pred{calls/1} assertions) the status is assumed to be
    @tt{check}.").

:- decl calls(AssertionStatus,AssertionBody) : assrt_status * c_assrt_body.

% ----------------------------------------------------------------------------

:- doc(success/1,
    "@cindex{success assertion} 
     @tt{success} assertions specify properties of the answers of a
     predicate, similarly to the corresponding @tt{success} field of a
     @tt{pred} assertion. The assertion can be limited to apply to a
     particular way of calling the predicate if a @tt{calls} field is
     present. However, unlike @tt{pred} or @tt{calls} assertions, the
     predicate is not forced to be called only that way.

     For example, the following assertion specifies the answers of the
     @tt{length/2} predicate @em{if} it is called as in the first mode
     of usage above (note that the previous pred assertion already
     conveys such information, however it also restricts the set of
     admissible @tt{calls}, while the success assertion does not):
     @begin{verbatim}
:- success length(L,N) : list * var => list * integer.
     @end{verbatim}
").
:- decl success(AssertionBody) : s_assrt_body.

:- doc(success/2, "@cindex{success assertion} A @tt{success} assertion
    can be preceded (as most other assertions) by an assertion status
    (@pred{assrt_status/1}). If no assertion status is present (i.e.,
    in @pred{success/1} assertions) the status is assumed to be
    @tt{check}.").

:- decl success(AssertionStatus,AssertionBody) : assrt_status * s_assrt_body.

% ----------------------------------------------------------------------------

:- doc(comp/1, "@cindex{comp assertion} @tt{comp} assertions specify
    global properties of the execution of a predicate, similarly to
    the corresponding @tt{comp} field of a @tt{pred} assertion. The
    assertion can be limited to apply to a particular way of calling
    the predicate if a @tt{calls} field is present. However, unlike
    @tt{pred} or @tt{calls} assertions, the predicate is not forced to
    be called only that way.

    For example, the following assertion specifies that the
    computation of @tt{append/3} (see @lib{lists}) will not fail
    @em{if} it is called as described (but does not force the
    predicate to be called only that way):
    @begin{verbatim}
:- comp append(Xs,Ys,Zs) : var * var * var + not_fail.
    @end{verbatim}
").
:- decl comp(AssertionBody) : g_assrt_body.

:- doc(comp/2, "@cindex{comp assertion} A @tt{comp} assertion can be
    preceded (as most other assertions) by an assertion status
    (@pred{assrt_status/1}). If no assertion status is present (i.e.,
    in @pred{comp/1} assertions) the status is assumed to be
    @tt{check}.").

:- decl comp(AssertionStatus,AssertionBody) : assrt_status * g_assrt_body.

% ----------------------------------------------------------------------------

:- doc(prop/1, "@cindex{prop assertion} @tt{prop} assertions are
    similar to a @tt{pred/1} assertions but they flag that the
    predicate being documented is also a ``@concept{property}.''

    Properties are standard predicates, but which are @em{guaranteed
    to terminate for any possible instantiation state of their
    argument(s)}, do not perform side-effects which may interfere with
    the program behaviour, and do not further instantiate their
    arguments or add new constraints.

    Provided the above holds, properties can thus be safely used as
    @concept{run-time checks}. The program transformation used in
    @tt{ciaopp} for run-time checking guarantees the third
    requirement.  It also performs some basic checks on properties
    which in most cases are enough for the second
    requirement. However, it is the user's responsibility to guarantee
    termination of the properties defined.  (See also @ref{Declaring
    regular types} for some considerations applicable to writing
    properties.)

    The set of properties is thus a strict subset of the set of
    predicates. Note that properties, in addition to being used to
    describe characteristics of arguments in assertions, they can also
    be executed (called) as any other predicates.  ").

:- decl prop(AssertionBody) : assrt_body.

:- doc(prop/2,
    "@cindex{prop assertion} This assertion is similar to a
     @pred{prop/1} assertion but it is explicitely qualified.
     Non-qualified @pred{prop/1} assertions are assumed the qualifier
     @tt{check}.
").
:- decl prop(AssertionStatus,AssertionBody) : assrt_status * assrt_body.

% ----------------------------------------------------------------------------
:- doc(section,"Assertions for testing").
% ----------------------------------------------------------------------------

:- decl test(AssertionBody) : s_assrt_body.

:- doc(test/2, "@cindex{test assertion} This assertion is similar
   to a @pred{test/1} assertion but it is explicitely qualified with
   an @concept{assertion status}.  Non-qualified @pred{test/1}
   assertions are assumed to have  @tt{check} status.  In this context,
   check means that the test should be executed when the developer
   runs the test battery.

").
:- decl test(AssertionStatus,AssertionBody) : assrt_status * s_assrt_body.

:- doc(texec/1,
  "@cindex{texec assertion} This assertion is similar to a
   @pred{calls/1} assertion but it is used to provide
   input data and execution commands for run-time testing.
").

:- decl texec(AssertionBody) : c_assrt_body.

:- doc(texec/2, "@cindex{texec assertion} This assertion is
    similar to a @pred{texec/1} assertion but it is explicitely
    qualified with an @concept{assertion status}.  Non-qualified
    @pred{texec/1} assertions are assumed to have @tt{check} status.
    ").  

:- decl texec(AssertionStatus,AssertionBody) : assrt_status * c_assrt_body.

:- doc(test/1, "@cindex{test assertion} This assertion is similar
   to a success assertion but it specifies a concrete test case to be run  
   in order verify (partially) that the predicate is working as expected.  
   For example, the following test will verify that the length predicate 
   works well for the particular list given:
   @begin{verbatim}
:- test length(L,N) : ( L = [1,2,5,2] ) => ( N = 4 ).
   @end{verbatim}
").

% ----------------------------------------------------------------------------
:- doc(section,"Module-level assertions").
% ----------------------------------------------------------------------------

:- doc(entry/1, "@cindex{entry assertion} @tt{entry} assertions
    provide information about the @em{external} calls to a
    predicate. They are identical syntactically to a @pred{calls/1}
    assertion. However, they describe only the external calls, i.e.,
    the calls to the exported predicates of a module from outside the
    module, or calls to the predicates in a non-modular file from
    other files (or the user).

    These assertions are @em{trusted} by the compiler, i.e., they are
    similar to writing a @tt{trust calls} assertion (except for
    referring only to the external calls).  As a result, if they are
    erroneous they can introduce bugs in programs.  Thus, @tt{entry}
    assertions should be written with care.

    An important use of these assertions is in @concept{providing
    information to the compiler} which it may not be able to infer
    from the program. The main use is in providing information on the
    ways in which exported predicates of a module will be called from
    outside the module. This will greatly improve the precision of the
    analyzer, which otherwise has to assume that the arguments that
    exported predicates receive are any arbitrary term.

    The distinction between external and internal calls is not always
    relevant and in those cases the use of @tt{trust calls} assertions
    is preferred. Because of this, @tt{entry} assertions may be
    deprecated in the future, since the distinction between external
    and internal calls can also be achieved by means of a bridge
    predicate.

").
:- decl entry(AssertionBody) : c_assrt_body.


% ----------------------------------------------------------------------------

:- doc(exit/1,

    "@cindex{exit assertion} This type of assertion provides
     information about the answers that an (exported) predicate
     provides for @em{external} calls. It is identical syntactically
     to a @pred{success/1} assertion. However, it describes only
     external answers, i.e., answers to the exported predicates of a
     module from outside the module, or answers to the predicates in a
     non-modular file from other files (or the user). The described
     answers may be conditioned to a particular way of calling the
     predicate. E.g.:

     @begin{verbatim}
:- exit length(L,N) : list * var => list * integer.
     @end{verbatim}

     These assertions are @em{trusted} by the compiler, i.e., they are
     similar to writing a @tt{trust success} assertion (except for
     referring only to the external calls).  As a result, if they are
     erroneous they can introduce bugs in programs.  Thus, @tt{exit}
     assertions should be written with care.

    The distinction between external and internal calls is not always
    relevant and in those cases the use of @tt{trust success}
    assertions is preferred. Because of this, @tt{entry} assertions
    may be deprecated in the future, since the distinction between
    external and internal calls can also be achieved by means of a
    bridge predicate.

").  
:- decl exit(AssertionBody) : s_assrt_body.

:- doc(exit/2, "@concept{exit assertion} This assertion is similar
    to an @pred{exit/1} assertion but it is explicitely qualified with
    an @concept{assertion status}.  Non-qualified @pred{exit/1} assertions are
    assumed the qualifier @tt{check}.  ").  
:- decl exit(AssertionStatus,AssertionBody) : assrt_status * s_assrt_body.


% ----------------------------------------------------------------------------
:- doc(section,"Assertion macros").
% ----------------------------------------------------------------------------

:- doc(modedef/1,
  "This assertion is used to define modes. A mode defines in a compact
   way a set of call and success properties. Once defined, modes can
   be applied to predicate arguments in assertions. The meaning of
   this application is that the call and success properties defined by
   the mode hold for the argument to which the mode is applied. Thus,
   a mode is conceptually a ``property macro.''

   The syntax of mode definitions is similar to that of @tt{pred}
   declarations. For example, the following set of assertions:
 
@begin{verbatim}
:- modedef +A : nonvar(A) # ""@var{A} is bound upon predicate entry."".

:- pred p(+A,B) : integer(A) =>  ground(B).
@end{verbatim}

  is equivalent to:

@begin{verbatim}
:- pred p(A,B) : (nonvar(A),integer(A)) =>  ground(B)
   # ""@var{A} is bound upon predicate entry."".
@end{verbatim}
").
:- decl modedef(AssertionBody) : assrt_body.

% ----------------------------------------------------------------------------

:- doc(decl/1,
    "@cindex{decl assertion} This assertion is similar to a
     @pred{pred/1} assertion but it is used to describe declarations
     instead of predicates.
").
:- decl decl(AssertionBody) : assrt_body.

:- doc(decl/2, "@cindex{decl assertion} This assertion is similar to a
     @pred{decl/1} assertion but it also has a status.  Non-qualified
     @pred{decl/1} assertions are assumed to have status @tt{check}.
     ").

:- decl decl(AssertionStatus,AssertionBody) : assrt_status * assrt_body.

:- decl doc(Pred,Comment) : head_pattern * docstring
# "Documentation @cindex{comment assertion}. This assertion provides a
   text @var{Comment} for a given predicate @var{Pred}, as well as
   other directives for the documenter.".

% TODO: how to deprecate?
:- decl comment(Pred,Comment) : head_pattern * docstring + deprecated
  # "An alias for @decl{doc/2} (deprecated, for compatibility with
    older versions).".

% ----------------------------------------------------------------------------
:- doc(section,"Program-point assertions").
% ----------------------------------------------------------------------------

:- pred check(PropertyConjunction) : property_conjunction
   # "@cindex{check assertion} This assertion provides information on
     a clause program point (position in the body of a clause). Calls
     to a @pred{check/1} assertion can appear in the body of a clause
     in any place where a literal can normally appear. The property
     defined by @var{PropertyConjunction} should hold in all the
     run-time stores corresponding to that program point.
     See also @ref{Run-time checking of assertions}.".

check(_).

:- pred trust(PropertyConjunction) : property_conjunction
   # "@cindex{trust assertion} This assertion also provides information on
     a clause program point. It is identical syntactically to a @pred{check/1}
     assertion. However, the properties stated are not taken as
     something to be checked but are instead @em{trusted} by the
     compiler. While the compiler may in some cases detect an
     inconsistency between a @pred{trust/1} assertion and the program,
     in all other cases the information given in the assertion will be
     taken to be true.  As a result, if these assertions are erroneous
     they can introduce bugs in programs. Thus, @pred{trust/1}
     assertions should be written with care.

     An important use of these assertions is in @concept{providing
     information to the compiler} which it may not be able to infer
     from the program (either because the information is not present or
     because the analyzer being used is not precise enough). In
     particular, providing information on external predicates which
     may not be accessible at the time of compiling the module can
     greatly improve the precision of the analyzer. This can be easily
     done with trust assertion. ".

trust(_).

:- pred true(PropertyConjunction) : property_conjunction
   # "@cindex{true assertion} This assertion is identical
     syntactically to a @pred{check/1} assertion. However, the
     properties stated have been proved to hold by the analyzer. Thus,
     these assertions often represent the @concept{analyzer output}.".

true(_).

:- pred false(PropertyConjunction) : property_conjunction
   # "@cindex{false assertion} This assertion is identical
     syntactically to a @pred{check/1} assertion. However, the
     properties stated have been proved not to hold by the
     analyzer. Thus, these assertions often represent the
     @concept{analyzer output}.".

false(_).
