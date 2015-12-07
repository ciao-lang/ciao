% ----------------------------------------------------------------------------

%%% THIS REALLY NEEDS (not so) SERIOUS UPDATING 

:- module(assertions_props,[
	           assrt_body/1,
		   head_pattern/1,
		   complex_arg_property/1,
		   property_conjunction/1,
		   property_starterm/1,
		   complex_goal_property/1,

		   nabody/1,
		   dictionary/1,

                   c_assrt_body/1,
		   s_assrt_body/1,
		   g_assrt_body/1,

		   assrt_status/1,
		   assrt_type/1,
		   predfunctor/1,
		   propfunctor/1,
		   docstring/1
		   ],
         [ dcg,assertions,regtypes ]).

% ----------------------------------------------------------------------------

:- doc(title,"Types and properties related to assertions").

:- doc(author,"Manuel Hermenegildo").

:- doc(module,"This module is part of the @lib{assertions}
   library. It provides the formal definition of the syntax of several
   forms of assertions and describes their meaning. It does so by
   defining types and properties related to the assertions
   themselves. The text describes, for example, the overall fields
   which are admissible in the bodies of assertions, where properties
   can be used inside these bodies, how to combine properties for a
   given predicate argument (e.g., conjunctions) , etc. and provides
   some examples.").

% ----------------------------------------------------------------------------

:- prop assrt_body(X) + regtype
   # "@var{X} is an assertion body.".

:- doc(assrt_body/1,"
   @cindex{assertion body syntax} This predicate defines the different
   types of syntax admissible in the bodies of @pred{pred/1},
   @pred{decl/1}, etc. assertions. Such a body is of the form:

     @begin{verbatim}
      Pr [:: DP] [: CP] [=> AP] [+ GP] [# CO]
     @end{verbatim}

     where (fields between [...] are optional):
   
     @begin{itemize}

     @item @var{Pr} is a @concept{head pattern} (@pred{head_pattern/1})
     which describes the predicate or property and possibly gives
     some implicit call/answer information.

     @item @var{DP} is a (possibly empty) @concept{complex argument
     property} (@pred{complex_arg_property/1}) which expresses
     properties which are @concept{compatible} with the predicate,
     i.e., instantiations made by the predicate are @em{compatible}
     with the properties in the sense that applying the property at
     any point would not make it fail.

     @item @var{CP} is a (possibly empty) @concept{complex argument
     property} (@pred{complex_arg_property/1}) which applies to the
     @em{calls} to the predicate.

     @item @var{AP} is a (possibly empty) @concept{complex argument
     property} (@pred{complex_arg_property/1}) which applies to the
     @em{answers} to the predicate (if the predicate succeeds). These
     only apply if the (possibly empty) properties given for calls in
     the assertion hold.

     @item @var{GP} is a (possibly empty) @concept{complex goal property}
     (@pred{complex_goal_property/1}) which applies to the @em{whole
     execution} of a call to the predicate. These only apply if the
     (possibly empty) properties given for calls in the assertion
     hold.

     @item @var{CO} is a @concept{comment string}
     (@pred{docstring/1}). This comment only applies if the (possibly
     empty) properties given for calls in the assertion hold. The
     usual formatting commands that are applicable in comment strings
     can be used (see @pred{stringcommand/1}).

     See the @apl{lpdoc} manual for documentation on assertion comments.

     @end{itemize} 

").

assrt_body((Pr :CP=>AP+GP#CO)):- n_assrt_body(nabody(Pr,CP,AP,GP,CO)).
assrt_body((Pr :CP=>AP+GP)):-    n_assrt_body(nabody(Pr,CP,AP,GP,true)).   
assrt_body((Pr :CP=>AP#CO)):-    n_assrt_body(nabody(Pr,CP,AP,true,CO)).   
assrt_body((Pr :CP=>AP)):-       n_assrt_body(nabody(Pr,CP,AP,true,true)).
assrt_body((Pr :CP +GP#CO)):-    n_assrt_body(nabody(Pr,CP,true,GP,CO)).   
assrt_body((Pr :CP +GP)):-       n_assrt_body(nabody(Pr,CP,true,GP,true)).
assrt_body((Pr :CP #CO)):-       n_assrt_body(nabody(Pr,CP,true,true,CO)).
assrt_body((Pr :CP)):-           n_assrt_body(nabody(Pr,CP,true,true,true)).
assrt_body((Pr=>AP +GP#CO)):-    n_assrt_body(nabody(Pr,true,AP,GP,CO)).   
assrt_body((Pr=>AP +GP)):-       n_assrt_body(nabody(Pr,true,AP,GP,true)).
assrt_body((Pr=>AP #CO)):-       n_assrt_body(nabody(Pr,true,AP,true,CO)).
assrt_body((Pr=>AP )):-          n_assrt_body(nabody(Pr,true,AP,true,true)).
assrt_body((Pr +GP #CO)):-       n_assrt_body(nabody(Pr,true,true,GP,CO)).
assrt_body((Pr +GP)):-           n_assrt_body(nabody(Pr,true,true,GP,true)).
assrt_body((Pr #CO)):-           n_assrt_body(nabody(Pr,true,true,true,CO)).
assrt_body((Pr)):-               n_assrt_body(nabody(Pr,true,true,true,true)).

:- doc(head_pattern/1,"A @concept{head pattern} can be a predicate
   name (functor/arity) (@pred{predname/1}) or a term. Thus, both
   @tt{p/3} and @tt{p(A,B,C)} are valid head patterns. In the case in
   which the head pattern is a term, each argument of such a term can
   be:

   @begin{itemize} 

   @item A variable. This is useful in order to be able to refer to
   the corresponding argument positions by name within properties and
   in comments. Thus, @tt{p(Input,Parameter,Output)} is a valid head
   pattern.

   @item A variable, as
   above, but preceded by a ``@concept{mode}.'' This mode determines
   in a compact way certain call or answer properties. For example,
   the head pattern @tt{p(Input,+Parameter,Output)} is valid,
   as long as @pred{+/1} is declared as a mode.

   Acceptable modes are documented in @lib{library(basicmodes)} and
   @lib{library(isomodes)}.  User defined modes are documented in
   @pred{modedef/1}. @cindex{acceptable modes}

   @item Any term. In this case this term determines the instantiation
   state of the corresponding argument position of the predicate calls
   to which the assertion applies.

   @item A ground term preceded by a ``@concept{mode}.'' The ground
   term determines a property of the corresponding argument. The mode
   determines if it applies to the calls and/or the successes.
   The actual property referred to is
   that given by the term but with one more argument added at the
   beginning, which is a new variable which, in a rewriting of the
   head pattern, appears at the argument position occupied by the term.
   For example, the head pattern @tt{p(Input,+list(int),Output)} is
   valid for mode @pred{+/1} defined in @lib{library(isomodes)}, and
   equivalent in this case to having the head pattern
   @tt{p(Input,A,Output)} and stating that the property
   @tt{list(A,int)} holds for the calls of the  predicate.

   @item Any term preceded by a ``@concept{mode}.'' In this case, only
   one variable is admitted, it has to be the first argument of the mode,
   and it represents the argument position.
   I.e., it plays the role of the new variable mentioned above. Thus,
   no rewriting of the head pattern is performed in this case.
   For example, the head pattern @tt{p(Input,+(Parameter,list(int)),Output)}
   is valid for mode @pred{+/2} defined in @lib{library(isomodes)}, and
   equivalent in this case to having the head pattern
   @tt{p(Input,Parameter,Output)} and stating that the property
   @tt{list(Parameter,int)} holds for the calls of the  predicate.

   @end{itemize} 

").

%%    @tt{+} (@pred{nonvar/1} at call time),
%%    @tt{-} (@pred{var/1} at call time),
%%    @tt{@@
%%    (@pred{not_further_inst/1}), and @tt{?}  (no information).
%% 
%%    **** The concept of user-definable modes has to be documented ****

:- prop head_pattern(Pr) 
   # "@var{Pr} is a head pattern.".

head_pattern(Pr) :-
	predname(Pr),
	!.
head_pattern(Pr) :-
	nonvar(Pr),
	!,
	Pr =.. [P|Args],
	atom(P),
	acceptable_args(Args).
head_pattern(_).

acceptable_args([]).
acceptable_args([A|As]) :-
	acceptable_arg(A),
	acceptable_args(As).

acceptable_arg(A) :- 
	var(A).
acceptable_arg(A) :- 
	ground(A).
acceptable_arg(A) :- 
	A =.. [M,V],
	%%% Needs to be updated for the new mode definitions!
	mode(M),
	acceptable_arg(V).

% imprecise...
mode(_).

:- doc(complex_arg_property(Props),
     "@var{Props} is a (possibly empty) @concept{complex argument
     property}. Such properties can appear in two formats, which are
     defined by @pred{property_conjunction/1} and
     @pred{property_starterm/1} respectively. The two formats can be
     mixed provided they are not in the same field of an
     assertion. I.e., the following is a valid assertion:

     @tt{:- pred foo(X,Y) : nonvar * var => (ground(X),ground(Y)).}
     ").

:- prop complex_arg_property(Props) + regtype
   # "@var{Props} is a (possibly empty) complex argument property".

% imprecise...
complex_arg_property(CP) :- 
	property_conjunction(CP).
complex_arg_property(CP) :- 
	property_starterm(CP).

:- doc(property_conjunction/1,"This type defines the first,
   unabridged format in which properties can be expressed in the
   bodies of assertions. It is essentially a conjunction of properties
   which refer to variables. The following is an example of a complex
   property in this format:

   @begin{itemize} 

   @item @tt{(integer(X),list(Y,integer))}: @var{X} has the property
   @pred{integer/1} and @var{Y} has the property @pred{list/2}, with
   second argument @tt{integer}.

   @end{itemize}
   ").

:- prop property_conjunction(Props) + regtype 
   # "@var{Props} is either a term or a @em{conjunction} of terms. The
     main functor and arity of each of those terms corresponds to the
     definition of a property. The first argument of each such term is
     a variable which appears as a head argument.".

property_conjunction(P) :-
	property(P).
property_conjunction((P1,P2)) :-
	property_conjunction(P1),
	property_conjunction(P2).
% Fixed Using rtchecks -- EMM
:- doc(property_starterm/1,"This type defines a second,
   compact format in which properties can be expressed in the bodies
   of assertions. A @pred{property_starterm/1} is a term whose main
   functor is @op{*/2} and, when it appears in an assertion, the
   number of terms joined by @op{*/2} is exactly the arity of the
   predicate it refers to. A similar series of properties as in
   @pred{property_conjunction/1} appears, but the arity of each
   property is one less: the argument position to which they refer
   (first argument) is left out and determined by the position of the
   property in the @pred{property_starterm/1}. The idea is that each
   element of the @op{*/2} term corresponds to a head argument
   position. Several properties can be assigned to each argument
   position by grouping them in curly brackets. The following is an
   example of a complex property in this format:

   @begin{itemize} 

   @item @tt{ integer * list(integer)}: the first argument of the
   procedure (or function, or ...) has the property @pred{integer/1}
   and the second one has the property @pred{list/2}, with second
   argument @tt{integer}.

   @item @tt{ @{integer,var@} * list(integer)}: the first argument of
   the procedure (or function, or ...) has the properties
   @pred{integer/1} and @pred{var/1} and the second one has the
   property @pred{list/2}, with second argument @tt{integer}.

   @end{itemize}

   ").

:- prop property_starterm(Props) + regtype
   # "@var{Props} is either a term or several terms separated by
     @op{*/2}. The main functor of each of those terms corresponds to
     that of the definition of a property, and the arity should be one
     less than in the definition of such property. All arguments of
     each such term are ground.".

property_starterm(AP) :-
	abridged_property(AP).
property_starterm(AP1*AP2) :-
	abridged_property(AP1),
	abridged_property(AP2).

%:- prop property(P) : nonvar(P).

property(P) :-
	nonvar(P),
	!,
	P =.. [_|Args],
	contains_var(Args).
property(_).

contains_var([A|_As]):- var(A), !.
contains_var([_A|As]):- contains_var(As).

%:- prop abridged_property(P) : nonvar(P).

abridged_property(P) :-
	nonvar(P),
	!,
	P =.. [_|Args],
	ground(Args).
abridged_property(_).

:- doc(complex_goal_property(Props),
     "@var{Props} is a (possibly empty) @concept{complex goal
     property}. Such properties can be either a term or a @em{conjunction}
     of terms. The main functor and arity of each of those terms corresponds
     to the definition of a property. Such properties apply to all
     executions of all goals of the predicate which comply with the
     assertion in which the @var{Props} appear.

     The arguments of the terms in @var{Props} are implicitely augmented
     with a first argument which corresponds to a goal of the predicate
     of the assertion in which the @var{Props} appear.
     For example, the assertion
     @begin{verbatim}
     :- comp var(A) + not_further_inst(A).
     @end{verbatim}
     has property @pred{not_further_inst/1} as goal property, and 
     establishes that in all executions of @tt{var(A)} it should hold
     that @tt{not_further_inst(var(A),A)}.

     ").

:- prop complex_goal_property(Props) + regtype
   # "@var{Props} is either a term or a @em{conjunction} of terms. The
     main functor and arity of each of those terms corresponds to the
     definition of a property. A first implicit argument in such terms
     identifies goals to which the properties apply.".

% imprecise...
complex_goal_property(CP) :- 
	property_conjunction(CP).


:- prop nabody(ABody)
	# "@var{ABody} is a normalized assertion body.".

nabody((Pred::Compat:Call=>Succ+Comp#Comm)):-
	head_pattern(Pred),
	list(Compat,property),
	list(Call,property),
	list(Succ,property),
	list(Comp,property),
	docstring(Comm).

:- regtype dictionary(D)
	# "@var{D} is a dictionary of variable names.".

dictionary([]).
dictionary([N=V|D]):-
	string(N),
	var(V),
	dictionary(D).

:- prop c_assrt_body(X) + regtype
   # "@var{X} is a call assertion body.".

:- doc(c_assrt_body/1,"

   @cindex{assertion body syntax} This predicate defines the different
   types of syntax admissible in the bodies of @pred{call/1},
   @pred{entry/1}, etc. assertions. The following are admissible:

     @begin{verbatim}
      Pr : CP [# CO]
     @end{verbatim}

     where (fields between [...] are optional):
   
     @begin{itemize}

     @item @var{CP} is a (possibly empty) @concept{complex argument property}
     (@pred{complex_arg_property/1}) which applies to the @em{calls} to the
     predicate.

     @item @var{CO} is a @concept{comment string}
     (@pred{docstring/1}). This comment only applies if the (possibly
     empty) properties given for calls in the assertion hold. The
     usual formatting commands that are applicable in comment strings
     can be used (see @pred{stringcommand/1}).

     @end{itemize} 

     The format of the different parts of the assertion body are given
     by @pred{n_assrt_body/5} and its auxiliary types.

").

c_assrt_body((Pr :CP #CO)):-      n_assrt_body(nabody(Pr,CP,true,true,CO)).
c_assrt_body((Pr :CP)):-          n_assrt_body(nabody(Pr,CP,true,true,true)).

:- prop s_assrt_body(X) + regtype
   # "@var{X} is a predicate assertion body.".

:- doc(s_assrt_body/1,"

   @cindex{assertion body syntax} This predicate defines the different
   types of syntax admissible in the bodies of @pred{pred/1},
   @pred{func/1}, etc. assertions. The following are admissible:

     @begin{verbatim}
      Pr : CP => AP # CO       
      Pr : CP => AP            
      Pr => AP # CO            
      Pr => AP                 
     @end{verbatim}

     where:
   
     @begin{itemize}

     @item @var{Pr} is a @concept{head pattern}
     (@pred{head_pattern/1}) which describes the predicate or property
     and possibly gives some implicit call/answer information.

     @item @var{CP} is a (possibly empty) @concept{complex argument
     property} (@pred{complex_arg_property/1}) which applies to the
     @em{calls} to the predicate.

     @item @var{AP} is a (possibly empty) @concept{complex argument
     property} (@pred{complex_arg_property/1}) which applies to the
     @em{answers} to the predicate (if the predicate succeeds). These
     only apply if the (possibly empty) properties given for calls in
     the assertion hold.

     @item @var{CO} is a @concept{comment string}
     (@pred{docstring/1}). This comment only applies if the (possibly
     empty) properties given for calls in the assertion hold. The
     usual formatting commands that are applicable in comment strings
     can be used (see @pred{stringcommand/1}).

     @end{itemize} 

     The format of the different parts of the assertion body are given
     by @pred{n_assrt_body/5} and its auxiliary types.

").

s_assrt_body((Pr :CP=>AP#CO)):-   n_assrt_body(nabody(Pr,CP,AP,true,CO)).
s_assrt_body((Pr :CP=>AP)):-      n_assrt_body(nabody(Pr,CP,AP,true,true)).
s_assrt_body((Pr=>AP #CO)):-      n_assrt_body(nabody(Pr,true,AP,true,CO)).
s_assrt_body((Pr=>AP )):-         n_assrt_body(nabody(Pr,true,AP,true,true)).

:- prop g_assrt_body(X) + regtype
   # "@var{X} is a comp assertion body.".

:- doc(g_assrt_body/1,"

   @cindex{assertion body syntax} This predicate defines the different
   types of syntax admissible in the bodies of @pred{comp/1}
   assertions. The following are admissible:

     @begin{verbatim}
      Pr : CP + GP # CO        
      Pr : CP + GP             
      Pr + GP # CO             
      Pr + GP                  
     @end{verbatim}

     where:
   
     @begin{itemize}

     @item @var{Pr} is a @concept{head pattern} (@pred{head_pattern/1})
     which describes the predicate or property and possibly gives
     some implicit call/answer information.

     @item @var{CP} is a (possibly empty) @concept{complex argument property}
     (@pred{complex_arg_property/1}) which applies to the @em{calls} to the
     predicate.

     @item @var{GP} contains (possibly empty) @concept{complex goal property}
     (@pred{complex_goal_property/1}) which applies to the @em{whole
     execution} of a call to the predicate. These only apply if the
     (possibly empty) properties given for calls in the assertion
     hold.

     @item @var{CO} is a @concept{comment string}
     (@pred{docstring/1}). This comment only applies if the (possibly
     empty) properties given for calls in the assertion hold. The
     usual formatting commands that are applicable in comment strings
     can be used (see @pred{stringcommand/1}).

     @end{itemize} 

     The format of the different parts of the assertion body are given
     by @pred{n_assrt_body/5} and its auxiliary types.

").

g_assrt_body((Pr :CP +GP#CO)):-   n_assrt_body(nabody(Pr,CP,true,GP,CO)).
g_assrt_body((Pr :CP +GP)):-      n_assrt_body(nabody(Pr,CP,true,GP,true)).
g_assrt_body((Pr +GP #CO)):-      n_assrt_body(nabody(Pr,true,true,GP,CO)).
g_assrt_body((Pr +GP)):-          n_assrt_body(nabody(Pr,true,true,GP,true)).

%% %% Still too advanced for the automatic documenter...
%% :- regtype n_assrt_body(B) : 
%%    ( B = nabody(Pr,CP,AP,GP,CO),
%%      head_pattern(Pr),
%%      complex_arg_property(CP),
%%      complex_arg_property(AP),
%%      goal_properties(GP),
%%      string(CO)
%%    ) 
%% 
%%    # "This is an auxiliary type definition which defines the types of
%%      the args that may appear in the bodies of assertions.".

%% Left out so that we see the warning...
%% :- regtype n_assrt_body(B) # 
%%    # "This is an auxiliary type definition which defines the types of
%%      the args that may appear in the bodies of assertions.".
	

n_assrt_body(nabody(Pr,CP,AP,GP,CO)) :- 
	head_pattern(Pr),
	complex_arg_property(CP),
	complex_arg_property(AP),
	complex_goal_property(GP),
	docstring(CO).

:- doc(assrt_status/1,"The types of assertion status. They have the
	same meaning as the program-point assertions, and are as follows:
        @includedef{assrt_status/1}").
:- prop assrt_status(X) + regtype
   # "@var{X} is an acceptable status for an assertion.".

assrt_status(true).
assrt_status(false).
assrt_status(check).
assrt_status(checked).
assrt_status(trust).

:- doc(assrt_type/1,"The admissible kinds of assertions:
        @includedef{assrt_type/1}").
:- prop assrt_type(X) + regtype
   # "@var{X} is an admissible kind of assertion.".

assrt_type(pred).
assrt_type(prop).
assrt_type(decl).
assrt_type(func).
% These are actually quite different:
%% assrt_type(compat). %% Not using these any more.
assrt_type(calls).
assrt_type(success).
assrt_type(comp).
assrt_type(entry).
% DTM: New Type
assrt_type(exit).
% EMM: used for testing
assrt_type(test).
assrt_type(texec).

%% assrt_type(trust). 
% As well as this one!
%% If this is not here then modedefs are not accepted by normalization pass one
assrt_type(modedef).


:- prop predfunctor(X) + regtype
   # "@var{X} is a type of assertion which defines a predicate.".

predfunctor(pred).
predfunctor(prop).
predfunctor(decl). %% ??
predfunctor(func). %% ??
predfunctor(modedef).

:- prop propfunctor(X) + regtype
   # "@var{X} is a type of assertion which defines a @em{property}.".

propfunctor(prop).

:- prop docstring(String)
   # "@var{String} is a text comment with admissible documentation commands.
     The usual formatting commands that are applicable in comment strings
     are defined by @pred{stringcommand/1}.
     See the @apl{lpdoc} manual for documentation on comments.
     ".

% imprecise...
docstring(String):- string(String).
