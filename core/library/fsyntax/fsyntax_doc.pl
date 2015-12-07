:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Functional notation").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Amadeo Casas").
:- doc(author,"Manuel Hermenegildo").
% Fixed behaviour with predicate abstractions and added support for function abstractions
% todo: document!
:- doc(author,"Jose F. Morales").

:- doc(module, "This library package allows the use of functional
   notation in a Ciao module/program. It supports function
   application, predefined evaluable functors, functional definitions,
   quoting, and (combined with the @lib{lazy} library) lazy
   evaluation. The extensions implemented by this library are also
   composable with higher-order features and can be combined with
   other Ciao packages such as constraints, assertions, etc.

   The package provides @em{syntactic sugar} for defining and using
   predicates as if they were functions. However, they can still
   retain the power of predicates. Any function definition written
   using this package is in fact defining a predicate, and any
   predicate can be used as a function.

   The predicate associated with a function has the same name and one
   more argument, meant as the place holder for the ``result'' of the
   function. In fact, this argument is just the one that will be
   syntactically connected to the surrounding goal or function, but it
   does not necessarily imply any directionality, i.e., it does not
   necessarily mean that this argument is an output or an input.  This
   argument is by default added to the right, i.e., it is the last
   argument, but can be changed by using a declaration, as explained
   below.

   @section{Function applications} 
   @cindex{Function applications} 

   Any term preceded by the @op{~ /1} operator is a function
   application, as can be seen in the goal @tt{write(~arg(1,T))},
   which is strictly equivalent to the sequence @tt{arg(1,T,A),
   write(A)}. The declaration @decl{fun_return/1} allows using a
   predicate argument other than the last as the return argument. For
   example with @tt{:- fun_return functor(~,_,_)} the expression
   @tt{~functor(f,2)} will be evaluated to the term @tt{f(_,_)}.  This
   definition of the return argument can also be done on the fly in
   each invocation in the following way: @tt{~functor(~,f,2)}.

   Functors can be declared as evaluable by using the declaration
   @decl{fun_eval/1}.  This allows avoiding the need to use the @tt{~}
   operator.  Thus, @tt{:- fun_eval arg/2} allows writing
   @tt{write(arg(1,T))} instead of @tt{write(~arg(1,T))} as above.  This
   declaration can be combined with the previous one:
   @tt{:- fun_eval functor(~,_,_)}.  

   @section{Predefined evaluable functors} 
   @cindex{Predefined evaluable functors} 

   By using the declaration
   @tt{:- fun_eval arith(true)}, all the functors understood by
   @pred{is/2} will be also evaluated.  This is active from the
   declaration downwards until a @tt{:- fun_eval arith(false)}
   declaration or the end of the module is reached.  Beware that
   arithmetic functors are used in some cases for other purposes than
   arithmetic: e.g. @tt{abolish(p/2)}.  But this is not so disturbing
   as it may appear because this package is not active in
   declarations, except for the goal-including declarations
   @decl{initialization/1} and @decl{on_abort/1}.  Note that all the
   declarations introduced by this package, as is customary in Ciao,
   are local to the module where they are included.

   In addition to functors declared with the declaration
   @pred{fun_eval/1}, the package defines as evaluable the functors used
   for disjunctive and conditional expressions: @op{| /2} and
   @op{? /2} (defined as operators).  A disjunctive expression has the
   form @tt{(V1|V2)}, and its value when first evaluated is @tt{V1}, and
   on backtracking @tt{V2}.  A conditional expression has the form
   @tt{(Cond ? V1)}, or more commonly @tt{(Cond ? V1 | V2)}, and its
   value, if the execution of @tt{Cond} as a goal succeeds, is @tt{V1},
   otherwise in the first form it causes backtracking, and on the second
   form its value is @tt{V2}.  Note that due to the operator
   precedences, these expressions normally need to be surrounded by
   parenthesis. Also, a nested expression: @tt{(Cond1 ? V1 | Cond2 ? V2 | V3)} 
   is evaluated as @tt{(Cond1 ? V1 | (Cond2 ? V2 | V3))}.


   @section{Functional definitions} 
   @cindex{Functional definitions} 

   A functional definition is composed
   of one or more functional clauses. A functional clause is written
   using the binary operator @op{:= /2}, as in:
@begin{verbatim}
opposite(red) := green.
@end{verbatim}
@noindent
   which is equivalent to @tt{opposite(red,green).} or 
@begin{verbatim}
addlast(X,L) := ~append(L,[X]).
@end{verbatim}
@noindent
   which is equivalent to @tt{addlast(X,L,R) :- append(L,[X],R).}

   Functional clauses can also have a body, which is executed before the
   result value is computed.  It can serve as a guard for the clause or to
   provide the equivalent of where-clauses in functional languages:
@begin{verbatim}
fact(0) := 1.
fact(N) := N * ~fact(--N) :- N > 0.
@end{verbatim}
   Note that guards can often be defined more compactly using conditional
   expressions:
@begin{verbatim}
fact(N) := N = 0 ? 1
         | N > 0 ? N * ~fact(--N).
@end{verbatim}
   The declaration @tt{:- fun_eval defined(true)} allows to locally
   define as evaluable functions being defined, so that the @tt{~}
   operator does not need to be used within a functional definition for
   the functor being defined. For example, for the @tt{fact} invocations
   in the previous definitions, which can now be written as, e.g. 
   (we provide the full module definition):
@begin{verbatim}
@includeverbatim{fsyntax/examples/factf_fsyntax.pl}
@end{verbatim}


@noindent
   This behaviour is reverted using 
   @tt{:- fun_eval defined(false)}.

   The translation of functional clauses has the following properties:
   @begin{itemize} 
     @item The translation produces @em{steadfast} predicates, that is,
       output arguments are unified after possible cuts.
     @item Defining recursive predicates in functional style maintains the
       tail recursion of the original predicate, thus allowing the usual
       compiler optimizations.
   @end{itemize}

   Some implementation details and a discussion of the recent
   combination of this library (which dates from Ciao version 0.2)
   with the lazy evaluation library can be found in
   @cite{functional-lazy-notation-flops2006}.

   @section{Quoting functors} 
   @cindex{Quoting functors} 

   Functors (either in functional or
   predicate clauses) can be prevented from being evaluated by using
   the @op{^ /1} prefix operator (read as ``quote''), as in
@begin{verbatim}
:- fun_eval arith(true).
pair(A,B) := ^(A-B).
@end{verbatim}
   Note that this just prevents the evaluation of the principal functor
   of the enclosed term, not the possible occurrences of other evaluable
   functors inside.

   @section{Some scoping issues} 
   @cindex{Some scoping issues} 

   When using function applications inside the goal
   arguments of meta-predicates, there is an ambiguity as they could
   be evaluated either in the scope of the outer execution or the in
   the scope of the inner execution.  The chosen behavior is by
   default to evaluate function applications in the scope of the outer
   execution. If they should be evaluated in the inner scope, the goal
   containing the function application needs to be escaped with the
   @op{^^ /1} prefix operator, as in @tt{findall(X, (d(Y), ^^(X =
   ~f(Y)+1)), L)} (which could also be written as @tt{findall(X, ^^
   (d(Y), X = ~f(Y)+1), L)}) and which expands into @tt{findall(X,
   (d(Y),f(Y,Z),T is Z+1,X=T), L)}. With no escaping the function
   application is evaluated in the scope of the outer execution, i.e.,
   it expands to @tt{f(Y,Z), T is Z+1, findall(X, (d(Y),X=T), L)}.

   @section{Other functionality} 
   @cindex{Other functionality} 

   In addition to the basic package @file{fsyntax}, a package
   @lib{functional} is also provided, to allow programming with a more
   functional-flavored style.  That package activates the declarations
   @tt{:- fun_eval arith(true)} and @tt{:- fun_eval defined(true)},
   and defines the @op{. /2} operator for use in lists (but be
   careful: this period cannot be followed by a whitespace!) and the
   operator @op{++ /2} as a function for appending lists. The
   factorial example above can be written as follows using the
   @lib{functional} package:
@begin{verbatim}
@includeverbatim{fsyntax/examples/factf.pl}
@end{verbatim}

   @noindent
   Which is equivalent to:
@begin{verbatim}
@includeverbatim{fsyntax/examples/factf_fsyntax.pl}
@end{verbatim}


   See the end of this chapter for additional examples.

   @section{Combining with higher order} 

   Ciao provides in its standard library the @tt{hiord} package, which
   supports a form of higher-order untyped logic programming with
   predicate abstractions @cite{ciao-hiord-tr,daniel-phd,ciao-hiord}.
   Predicate abstractions are Ciao's translation to logic programming
   of the lambda expressions of functional programming: they define
   unnamed predicates which will be ultimately executed by a
   higher-order call, unifying its arguments appropriately. A function
   abstraction is provided as functional syntactic sugar for predicate
   abstractions:

   Predicate abstraction: @tt{''(X,Y) :- p(X,Z), q(Z,Y)}.

   Function abstraction: @tt{''(X) := ~q(~p(X))}.

   @noindent
   and function application is syntactic sugar over predicate application: 

   Predicate application: @tt{..., P(X,Y), ...}
   Function application: @tt{..., Y = ~P(X), ...}

   The combination of this @tt{hiord} package with the @tt{fsyntax}
   and @tt{lazy} packages (and, optionally, the type inference and
   checking provided by the Ciao preprocessor
   @cite{ciaopp-sas03-journal-scp}) basically provide the
   functionality present in modern functional languages (currying is
   not @em{syntactically} implemented, but its results can be obtained
   by deriving higher-order data from any other higher-order data (see
   @cite{daniel-phd}), as well as some of the functionality of full 
   higher-order logic programming.

   At this moment, it is necessary to specify the @tt{:- fun_eval
   hiord(true)} option to enable correct handling of function
   abstractions.
   ").

%%  \item[Laziness:] Lazy evaluation is a program evaluation technique
%%    used particularly in functional languages.  When using lazy
%%    evaluation, an expression is not evaluated as soon as it is
%%    assigned, but rather when the evaluator is forced to produce the
%%    value of the expression.  The \texttt{when}, \texttt{freeze}, or
%%    \texttt{block} control primitives present in many modern logic
%%    programming systems are more powerful operationally than lazy evaluation.
%%    However, they lack the simplicity of use and cleaner semantics of
%%    functional lazy evaluation. In our design, a function (or
%%    predicate) can be declared as lazy via the declarations:
%% 
%%    \verb@:- @\verb@lazy fun_eval function_name/N.@
%% 
%%    (or, equivalently in predicate version, ``\verb@:- lazy pred_name/M.@'',
%%    where $M=N+1$).
%%    In order to achieve the intended behavior, the execution of each
%%    function declared as lazy is suspended until the return value of the
%%    function is needed. Thus, lazy evaluation allows dealing with infinite
%%    data structures and also evaluating function arguments only when needed.
%% 
%%  \item[Definition of real functions:] In the previous scheme, functions
%%    are (at least by default) not forced to provide a single solution
%%    for their result, and, furthermore, they can be partial, producing
%%    a failure when no solution can be found.  A predicate defined as a
%%    function can be declared to behave as a real function using the
%%    declaration ``\verb@:- @\verb@funct name/N.@''.  Such predicates
%%    are then converted automatically to real functions by adding
%%    pruning operators and a number of Ciao
%%    assertions~\cite{assert-lang-disciplbook} which pose
%%    (and check) additional restrictions such as determinacy,
%%    modedness, etc., so that the semantics will be the same as in
%%    traditional functional programming.

%%%%% See additional examples of 'real' functions in slides

:- doc(appendix, "

  @section{Some examples using functional syntax} 

  We now illustrate some of the uses of the package through examples.  The
  following example defines a simple unary function @tt{der(X)} which
  returns the derivative of a polynomial arithmetic expression:

@begin{verbatim}
der(x)      := 1.
der(C)      := 0                 :- number(C).
der(A + B)  := der(A) + der(B).
der(C * A)  := C * der(A)        :- number(C).
der(x ** N) := N * x ** ~(N - 1) :- integer(N), N > 0.
@end{verbatim}
   

  Note that if we include the directive mentioned before which makes
  arithmetic functors evaluable then we would have to write the
  program in the following (clearly, less pleasant and more
  obfuscated) way:

@begin{verbatim}
:- fun_eval(arith(true)).
der(x)         := 1.
der(C)         := 0                      :- number(C).
der(^(A + B))  := ^(der(A) + der(B)).
der(^(C * A))  := ^(C * der(A))          :- number(C).
der(^(x ** N)) := ^(N * ^(x ** (N - 1))) :- integer(N), N > 0.
@end{verbatim}

Both of the previous code fragments translate to the following code:

@begin{verbatim}
der(x, 1).
der(C, 0) :-
          number(C).
der(A + B, X + Y) :-
          der(A, X),
          der(B, Y).
der(C * A, C * X) :-
          number(C),
          der(A, X).
der(x ** N, N * x ** N1) :-
          integer(N),
          N > 0,
          N1 is N - 1.
@end{verbatim}

  Functional notation interacts well with other Ciao language
  features.  For example, it provides compact and familiar notation
  for regular types and other properties:

@begin{verbatim}
@includeverbatim{fsyntax/examples/propsf.pl}
@end{verbatim}

@noindent
where the functional clauses expand to (note the use of higher-order
in the third example):

@begin{verbatim}
color(red). color(blue). color(green).
list([]).
list([_|T]) :- list(T).
list_of(_, []).
list_of(T, [X|Xs]) :- T(X), list_of(T, Xs).
@end{verbatim}

@noindent
Such types and properties are then admissible in the usual way in
assertions, e.g.:

@begin{verbatim}
:- pred append/3 :: list * list * list.
:- pred color_value/2 :: list(color) * int.
@end{verbatim}

  The combination of functional syntax and user-defined operators
  brings significant flexibility, as can be seen in the following
  definition of a list concatenation (@tt{append}) operator (note that
  these are the definitions mentioned before which are active by
  default in the @lib{functional} package):

@begin{verbatim}
:- op(600, xfy, (.)).
:- op(650, xfy, (++)).
:- fun_eval (++)/2.
[]   ++ L := L.
X.Xs ++ L := X.(Xs ++ L).
@end{verbatim}

@noindent
This definition will be compiled exactly to the standard definition of
@tt{append} (and, thus, will be reversible).  The functional syntax
and user-defined operators allow writing for example @tt{Space = ' ',
write(\"Hello\" ++ Space ++ \"world!\")}  instead of the equivalent forms
@tt{Space = ' ', write( append(\"Hello\", append(Space, \"world!\")))} (if
@tt{append/2} is defined as evaluable) or @tt{Space = ' ',
append(Space, \"world!\", T1),} @tt{append(\"Hello\", T1, T2),}
@tt{write(T2)}.

As another example, we can define an array indexing operator for
fixed-size, multi-dimensional arrays. Assume that arrays are built
using nested structures whose main functor is @tt{a} and whose arities
are determined by the specified dimensions, i.e., a two-dimensional
array @em{A} of dimensions @em{[N,M]} will be represented by the
nested structure @tt{a(a(@em{A11},...,@em{A1M}),
a(@em{A21},..,@em{A2M}), ..., a(@em{AN1},...,} @tt{@em{ANM}))}, where
@tt{@em{A11},... @em{ANM}} may be arbitrary terms (we ignore for
simplicity arity limitations, solved in any case typically by further
nesting with logarithmic access time). The following recursive
definition defines the property @tt{fixed_array/2} and also the array
access operator @tt{@@}:

@begin{verbatim}
fixed_array([N|Ms],A):-
	functor(A,a,N),
	rows(N,Ms,A).
fixed_array([N],A):-
	functor(A,a,N).

rows(0,_,_).
rows(N,Ms,A) :-
        N > 0,
        arg(N,A,Arg),
        array(Ms,Arg),
        rows(N-1,Ms,A).

:- pred @@(Array,Index,Elem) :: array * list(int) * int
   # \"@@var@{Elem@} is the @@var@{Index@}-th element of @@var@{Array@}.\".

:- op(55, xfx, '@@').
:- fun_eval (@@)/2.
V@@[I]    := ~arg(I,V).       %% Or: V@@[] := V. 
V@@[I|Js] := ~arg(I,V)@@Js.
@end{verbatim}

This allows writing, e.g., @tt{M = fixed_array([2,2]), M@@[2,1] = 3}
(which could also be expressed as @tt{fixed_array([2,2])@@[2,1] = 3}),
where the call to the @tt{fixed_array} property generates an empty @em{2
x 2} array @em{M} and @tt{M@@[2,1] = 3} puts @em{3} in @em{M[2,1]}. 
This can be done in the top level:
@begin{verbatim}
?- M = ~fixed_array([2,2]), M@@[2,1] = 3.
@end{verbatim}
@noindent
provided the @tt{op} and @tt{function} declarations are 
loaded into the top level also. 
Another example of use is: @tt{A3@@[N+1,M] = A1@@[N-1,M] + A2@@[N,M+2]}.

Such functionality can be grouped into a @em{package} as follows. The
package main file (@tt{arrays.pl}) might be:
@begin{verbatim}
@includeverbatim{fsyntax/examples/arrays.pl} 
@end{verbatim}
@noindent where file @tt{arrays_ops.pl} may contain:
@begin{verbatim}
@includeverbatim{fsyntax/examples/arrays_ops.pl} 
@end{verbatim}
@noindent The main file is @tt{arrays_rt.pl} which would contain for example 
(note that it also uses @tt{arrays_ops.pl}, and that is why the contents of 
@tt{arrays_ops.pl} were not put directly in @tt{arrays.pl}):
@begin{verbatim}
@includeverbatim{fsyntax/examples/arrays_rt.pl} 
@end{verbatim}
A file using this package would be:
@begin{verbatim}
@includeverbatim{fsyntax/examples/arrays_test.pl} 
@end{verbatim}



@section{Examples of combining with higher order} 

The following @tt{map} and @tt{foldl} definitions (from the @lib{hiordlib} 
library) illustrate the combination of functional syntax and higher-order 
logic programming: 
@begin{verbatim}
:- fun_eval map/2.
:- meta_predicate map(_,pred(2),_).
map([], _)     := [].
map([X|Xs], P) := [P(X) | map(Xs, P)].

:- fun_eval foldl/3.
:- meta_predicate foldl(_,_,pred(3),_).
foldl([], Seed, _Op) := Seed.
foldl([X|Xs], Seed, Op) := ~Op(X,~foldl(Xs,Seed,Op)).
@end{verbatim}

With this definition:
@begin{verbatim}
?- L = ~map([1,2,3], ( _(X,Y):- Y = f(X) ) ).

L = [f(1),f(2),f(3)] ? 

?- [f(1),f(2),f(3)] = ~map(L, ( _(X,f(X)) :- true ) ).  

L = [1,2,3] ?
@end{verbatim}

Also, after running:
@begin{verbatim}
?- [\"helloworld\", \"byeworld\"] = map([\"hello\", \"bye\"], ++(X)).
@end{verbatim}

@noindent
(where @tt{(++)/2} corresponds to the above definition of
@tt{append}) @tt{X} will be bound to @tt{\"world\"}, which
is the only solution to the equation.  

And when calling:
@begin{verbatim}
map(L, ++(X), [\"hello.\", \"bye.\"]).
@end{verbatim}

@noindent
several values for @tt{L} and @tt{X} are returned through
backtracking:

@begin{verbatim}
L = [\"hello\",\"bye\"],   X = \".\" ? ;
L = [\"hello.\",\"bye.\"], X = [] ?
@end{verbatim}

@noindent (remember to set the flag @tt{write_strings} to on in these
examples so that the top level prints strings as strings of characters
instead of lists of ASCII codes).

@section{Some additional examples using functional syntax} 

A definition of the Fibonacci function, written in functional notation: 

@begin{verbatim}
@includeverbatim{fsyntax/examples/fib_fun.pl}
@end{verbatim}

This is the factorial example, written in functional notation and
including some assertions:

@begin{verbatim}
@includeverbatim{fsyntax/examples/factf_assrt.pl}
@end{verbatim}

And, the same example written using @lib{clpq} constraints:
@begin{verbatim}
@includeverbatim{fsyntax/examples/factf_clpq_assrt.pl}
@end{verbatim}
@noindent
which allows for example calling it ``backwards:''
@begin{verbatim}
?- 24 = ~fact(X).

X = 4 ? 
@end{verbatim}


A very simple example using lazy evaluation:

@begin{verbatim}
@includeverbatim{fsyntax/examples/lazy_simple.pl}
@end{verbatim}

A naive reverse example, using functional notation:

@begin{verbatim}
@includeverbatim{fsyntax/examples/revf.pl}
@end{verbatim}

And the same example using some assertions:

@begin{verbatim}
@includeverbatim{fsyntax/examples/revf_assrt_new.pl}
@end{verbatim}

Finally, a simple stream creation example where assertions are used to
define a safety policy (that no file outside @tt{/tmp} should be
opened):

@begin{verbatim}
@includeverbatim{fsyntax/examples/open_file_f_wide.pl}
@end{verbatim}

   ").

%% As a simple example of the use of @em{lazy evaluation} consider the
%% following definition of a function which returns the (potentially)
%% infinite list of integers starting with a given one:
%% 
%% @begin{verbatim}
%% :- lazy fun_eval nums_from/1.
%% nums_from(X) := [ X | nums_from(X+1) ].
%% @end{verbatim}

:- doc(bug, "Assumes that @pred{is/2} is imported.").
:- doc(bug, "Lazy functions declarations require translation priorities
   to move it to the lazy package.").
:- doc(bug, "Detect automatically when hiord is being used, deprecate @tt{eval_hiord}."). % who: jfmc
:- doc(bug, "I am not sure if shared variables are working for predicate abstractions."). % who: jfmc
:- doc(bug, "Find out if predicate abstractions are being fully translated at compile time (see output for hiordfun example)."). % who: jfmc
