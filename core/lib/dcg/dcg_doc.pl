:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Definite Clause Grammars").

:- doc(author, "The CLIP Group").

%% see @cite{Les Grammaires de Metamorphos} by A.Colmerauer,
%% Technical Report, Groupe d'Intelligence Artificielle,
%% Marseille-Luminy, November, 1975, and @cite{Definite clause grammars for
%% language analysis---a survey of the formalism and a comparison with
%% augmented transition networks} by F.C.N. Pereira and D.H.D. Warren, in
%% @cite{Artificial Intelligence} 13:231-278, 1980. 

:- doc(module,"This library package allows the use of DCGs (Definite
   Clause Grammars) @cite{Colmerauer78,PereiraWarren80} in a Ciao
   module/program.

Definite clause grammars are an extension of the well-known
context-free grammars. Prolog's grammar rules provide a convenient
notation for expressing definite clause grammars. A DCG rule in
Prolog takes the general form

@begin{verbatim}
@var{head} --> @var{body}.
@end{verbatim}

@noindent
meaning ``a possible form for @var{head} is @var{body}''.  Both
@var{body} and @var{head} are sequences of one or more items linked by the
standard Prolog conjunction operator ""@tt{,}"". 

@bf{Note:} support for @pred{phrase/2} and @pred{phrase/3} is
offered by the @lib{dcg/dcg_phrase} package. Those predicates may
perform code translations at runtime, which in some cases is not
desired feature (e.g., make precision of static analysis worse or
increasing size of static executables). Thus, we separate by design
the static and dynamic behaviours.

Definite clause grammars extend context-free grammars in the following ways:

@begin{enumerate}

@item
A non-terminal symbol may be any Prolog term (other than a variable or
number). 

@item
A terminal symbol may be any Prolog term.  To distinguish terminals from
non-terminals, a sequence of one or more terminal symbols is written within
a grammar rule as a Prolog list.  An empty sequence is written as the empty
list @tt{[]}.  If the terminal symbols are ASCII character codes, such
lists can be written (as elsewhere) as strings.  An empty sequence is
written as the empty list, @tt{[]} or @tt{""""}. 

@item
Extra conditions, in the form of Prolog procedure calls, may be included in
the right-hand side of a grammar rule.  Such procedure calls are written
enclosed in @tt{@{@}} brackets. 

@item
The left-hand side of a grammar rule consists of a non-terminal, optionally
followed by a sequence of terminals (again written as a Prolog list). 

@item
Alternatives may be stated explicitly in the right-hand side of a
grammar rule, using the disjunction operator @tt{;}, or, also, as
traditionally in Prolog, using @tt{|} (which is treated specially when this
package is loaded).

@item
The cut symbol may be included in the right-hand side of a grammar rule, as
in a Prolog clause.  The cut symbol does not need to be enclosed in
@tt{@{@}} brackets. 
@end{enumerate}

As an example, here is a simple grammar which parses an arithmetic
expression (made up of digits and operators) and computes its value.

@begin{verbatim}
expr(Z) --> term(X), ""+"", expr(Y), @{Z is X + Y@}.
expr(Z) --> term(X), ""-"", expr(Y), @{Z is X - Y@}.
expr(X) --> term(X).

term(Z) --> number(X), ""*"", term(Y), @{Z is X * Y@}.
term(Z) --> number(X), ""/"", term(Y), @{Z is X / Y@}.
term(Z) --> number(Z).

number(C) --> ""+"", number(C).
number(C) --> ""-"", number(X), @{C is -X@}.
number(X) --> [C], @{0'0=<C, C=<0'9, X is C - 0'0@}.
@end{verbatim}

In the last rule, @var{C} is the ASCII code of some digit.

The query

@begin{verbatim}
?- expr(Z, ""-2+3*5+1"", []).
@end{verbatim}

@noindent
will compute @var{Z}=14.  The two extra arguments are explained below.

Now, in fact, grammar rules are merely a convenient ``syntactic sugar'' for
ordinary Prolog clauses.  Each grammar rule takes an input string, analyses
some initial portion, and produces the remaining portion (possibly
enlarged) as output for further analysis.  The arguments required for the
input and output strings are not written explicitly in a grammar rule, but
the syntax implicitly defines them.  We now show how to translate grammar
rules into ordinary clauses by making explicit the extra arguments.

A rule such as

@begin{verbatim}
p(X) --> q(X).
@end{verbatim}

@noindent
translates into

@begin{verbatim}
p(X, S0, S) :- q(X, S0, S).
@end{verbatim}

If there is more than one non-terminal on the right-hand side, as in

@begin{verbatim}
p(X, Y) --> 
        q(X), 
        r(X, Y),
        s(Y).
@end{verbatim}

@noindent
then corresponding input and output arguments are identified, as in

@begin{verbatim}
p(X, Y, S0, S) :- 
        q(X, S0, S1), 
        r(X, Y, S1, S2), 
        r(Y, S2, S).
@end{verbatim}

Terminals are translated using the built-in predicate
@tt{'C'/3} (this predicate is not normally useful in itself;
it has been given the name @tt{'C'} simply to avoid using up a more 
useful name). 
Then, for instance

@begin{verbatim}
p(X) --> [go,to], q(X), [stop].
@end{verbatim}

@noindent
is translated by

@begin{verbatim}
p(X, S0, S) :-
        'C'(S0, go, S1), 
        'C'(S1, to, S2), 
        q(X, S2, S3), 
        'C'(S3, stop, S).
@end{verbatim}

Extra conditions expressed as explicit procedure calls naturally translate
as themselves, e.g.

@begin{verbatim}
p(X) --> [X], @{integer(X), X>0@}, q(X).
@end{verbatim}

@noindent
translates to

@begin{verbatim}
p(X, S0, S) :- 
        'C'(S0, X, S1), 
        integer(X), 
        X>0, 
        q(X, S1, S).
@end{verbatim}

Similarly, a cut is translated literally.

Terminals on the left-hand side of a rule translate into an explicit list
in the output argument of the main non-terminal, e.g.

@begin{verbatim}
is(N), [not] --> [aint].
@end{verbatim}

@noindent
becomes

@begin{verbatim}
is(N, S0, [not|S]) :- 'C'(S0, aint, S).
@end{verbatim}

Disjunction has a fairly obvious translation, e.g.

@begin{verbatim}
args(X, Y) --> 
        (   dir(X), [to], indir(Y)
        ;   indir(Y), dir(X)
        ).
@end{verbatim}

@noindent
translates to

@begin{verbatim}
args(X, Y, S0, S) :-
        (   dir(X, S0, S1), 
            'C'(S1, to, S2), 
            indir(Y, S2, S)
        ;   indir(Y, S0, S1), 
            dir(X, S1, S)
        ).
@end{verbatim}

").
