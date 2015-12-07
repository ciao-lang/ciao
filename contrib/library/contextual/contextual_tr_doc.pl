:- use_package( assertions).
:- use_package( regtypes).



:- doc( title, "Simple Notation For Implicit Context Passing").

:- doc( author, "Dragan Ivanovi@'{c} <idragan@@clip.dia.fi.upm.es>").

:- doc( summary, "This package defines a simple Prolog source code
transformation for generalized or multiple DCG (with DCG as a subset),
data abstraction, typing and object-oriented programming.  ").

:- doc( module, 

"
@section{Overview}

This package defines a simple Prolog source code transformation for
generalized or multiple DCG (with DCG as a subset), data abstraction,
typing and object-oriented programming.

@concept{contextual notation}@em{Conceptual notation} (@index{CN})
defined in this module is enabled in sources that use the package
@lib{contextual}, and this implementation exploits Ciao Prolog's
source transformation mechanism and integrates with its module
system.@footnote{For more information, see the Ciao documentation.}

@section{Related Work}

This package has been designed and developed independently by the
author.  However, some of its features, such as the ability to handle
multiple contextual variables grouped into the same argument of the
translated predicate, as well as its reliance on the DCG extension
concepts developed earlier, have a lot in common with the ideas that
have been previously developed by Jos@'{e} Francisco Morales
<@email{jfran@@clip.dia.fi.upm.es}> in his work in progress on using
scoped global variables in the optimizing compiler branch of the Ciao
system. 

The author is also thankful to R@'{e}my H@ae{}mmerl@'{e}
<@email{remy@@clip.dia.fi.upm.es}> for his help on how to program code
transformation using the powerful syntax-extension feature of the Ciao
system.

The module transforms standard DCG clauses in form (see @pred{context/5}):
@begin{verbatim}
Goal --> Body.
@end{verbatim}
and CN clauses in form:
@begin{verbatim}
Goal / Context :- Body.
@end{verbatim}
or @em{method definitions}
@begin{verbatim}
Goal // Context :- Body.
@end{verbatim}
").

% ======================================================================

:- regtype context_domain( IO) # "@var{IO} is a context
interpretation domain ('i' or 'o')".

:- doc( context_domain/1, "Defined as: @includedef{context_domain/1}").

context_domain( i).
context_domain( o).

:- export( context_domain/1).

% ----------------------------------------------------------------------

:- regtype context_var( CVar) # "@var{CVar} is a contextual variable".

:- doc( context_var/1, "Defined as: @includedef{context_var/1}").

context_var( CVar) :- atm( CVar).
context_var( Prefix^CVar):- atm( Prefix), context_var( CVar).

:- export( context_var/1).

% ----------------------------------------------------------------------

:- regtype context_var_typing( CVT) # "@var{CVT} is a contextual
variable typing".

:- doc( context_var_typing/1, "Defined as:
@includedef{context_var_typing/1}").

context_var_typing( CVar/Typing) :-
	context_var( CVar),
	term( Typing).

:- export( context_var_typing/1).

% ......................................................................

:- regtype context_var_mapping( CVT) # "@var{CVT} is a contextual
variable mapping".

:- doc( context_var_mapping/1, "Defined as:
@includedef{context_var_mapping/1}").

context_var_mapping( CVar=Var) :-
	context_var( CVar),
	var( Var).

:- export( context_var_mapping/1).

% ......................................................................

:- pred context( IO, Context, Interpretation, Mapping, Typing) :
	(nonvar( IO), context_domain( IO), term( Context)) => (
	nonvar( Interpretation), list( Interpretation), nonvar(
	Mapping), list( Mapping, context_var_mapping), nonvar(
	Typing), list( Typing, context_var_typing)) # "Interpret
	@var{Context} in domain @var{IO}, producing interpretation
	list @var{Interpretation} with context variable mappings in
	@var{Mapping} and typing in @var{Typing}".

:- doc( context/5, "A @index{context} is a specification for
	(additional) input and output arguments of a predicate.  A
	context has one input and one output @index{concept
	interpretation}, which is a list of terms, which may be empty,
	a singleton, or have more than one elements.

Context is defined using the @index{contextual notation} (@index{CN}),
on top of the usual Prolog grammar for terms, which assigns special
meanings to individual constructs.

Here, we will define CN inductivelly, starting from the basic elements
of CN, and proceeding with compound constructs.

The basic elements of CN include:

@begin{itemize}

@item Atom @tt{*}, which stands for the @index{empty context},
interpretation of which is the empty list.

@item Atom @tt{[]} stands for itself (i.e. its interpretation consists
of the atom).

@item Atom @tt{?} stands for @index{anything}, and its interpretation
consists of a fresh logical variable for each occurence.

@item Atom other than @tt{*}, @tt{[]}, and @tt{?} is either a name of
a @index{context macro}, or an unqualified name of a @index{contextual
variable}.  In the former case, interpretation of the atom is the
interpretation of the macro expansion.  In the latter case,
interpretation of a contextual variable consists of a logical
variable, such that (i) each occurence of the same contextual variable
in the context corresponds to the same logical variable, and (ii)
different contextual variables are always interpreted as different
logical variables.

@item Atomic terms other than atoms (e.g. integers and floating-point
numbers) stand for themselves.

@item Logical variables stand for themselves.

@end{itemize} 

The compound CN constructs include:

@begin{itemize}

@item @bf{Macro expansion}

If a term @tt{T}, which is an atom or a structured term, has been
globally or locally defined as a @index{context macro}, its occurence
in a context will be replaced with its expansion.

@item @bf{Sequencing and local macros}.

A construct of form @tt{(X,Y)} is a sequence of contexts.  Its
interpretation is obtained by concatenating interpretations of @tt{X}
and @tt{Y}.  

A special case is when @tt{X} is a @index{local macro definition} of
form @tt{A=E}, where @tt{A} is an atom (or an extension, see below),
and @tt{E} is its expansion.  The local macro definition overrides any
global or an earlier local macro definition for the same atom, and any
appearance of @tt{A} in @tt{Y} will be replaced by @tt{E}.

@item @bf{Term building and functor escaping}

To construct an arbitrary term, or to remove any special meaning in
interpretation from the functor of a structure or an atom, the
construct @tt{@{X@}} can be used.  If @tt{X} is an atom, the
interpretation of @tt{@{X@}} will be @tt{X} itself. If @tt{X} is a
structure, the interpretation will be a structure with the same
functor, while its arguments will correspond to the interpretation of
all arguments to @tt{X}, taken as a sequence.

To escape the entire term in a context, one needs to escape both the
functor, and, recursivelly, all of its arguments.

@item @bf{List construction}

The construct @tt{[X|Y]} is a @index{list constructor} that differs
from @tt{@{[X|Y]@}} in that it smartly expands and contracts depending
on interpretations of @tt{X} and @tt{Y}.

For instance, if the interpretation of @tt{X} is empty, @tt{[X|Y]}
devolves to tail @tt{Y}.  Or, if the interpretation of @tt{X} has more
than one term, then the list is properly constructed with the same
number of head elements followed by the tail @tt{Y}.

Also, if the interpretation of @tt{Y} is empty, the tail is assumed an
empty list, and if the interpretation of @tt{Y} has more than one
element, only the first is taken into account as the tail of the list.

@item @bf{Extension and reduction}

When defining macros as extensible data structures or @index{classes},
it is often useful to use a special contextual variable inside a
context or a macro definition to denote an (optional) extension of the
structure.  For that purpose, CN provides the construct @tt{\\T},
where @tt{T} is a non-variable term, called the @index{extension} of
@tt{T}.

@tt{\\T} is replaced by an atom that is uniquely determined by the
functor and the arity of @tt{T}.  These atoms are interpreted in the
above mentioned manner, for atoms other than @tt{*}, @tt{[]}, and
@tt{?}.  Extensions are seldom used directly, and are implicit in
class definitions.

Construct @tt{[](X)} is a @index{reduction} of @tt{X}, and forces
@tt{X} to be interpreted under assumption that its extension is equal
to @tt{[]}.  @tt{[](X)} is roughly equivalent to the sequence
@tt{(\X=[], X)}.

@item @bf{Input-output splitting}

There are several ways to separate input and output interpretations of
a context.  Starting from the most general, these are: 

@begin{itemize}

@item @tt{X-Y} gives the interpretation of @tt{X} for input, and the
interpretation of @tt{Y} for output.

@item @tt{+X} gives the interpretation of @tt{X} for input, and the
empty interpretation for output (equivalent to @tt{X-(*)}).

@item @tt{-Y} gives the empty interpretation for input, and the
interpretation of @tt{X} for output (equivalent to @tt{(*)-Y}).

@item @tt{(X->Y)} gives the interpretation of @tt{X} for input, and the
reduced interpretation of @tt{Y} for output (equivalent to @tt{X-[](Y)}.

@end{itemize}

@item @bf{Prefixing}

To disambiguate context variables denoted by the same atom in
different parts of a context, the construct @ttr{P^X} can be used,
where @tt{P} is an atom @tt{variable prefix} that qualifies all
context variable names occuring in @tt{X}.

@item @bf{Typing}

Contextual variables can be typed@concept{variable typing} using the
@tt{A::T} construct, where V is a context variable, and @tt{T} is a
sequence of one or more of its properties.  Assuming that @tt{V} is
mapped to a logical variable @tt{L}, the property @tt{S} from the
sequence can be:

@begin{itemize}

@item a variable, in which case the typing is translated to @tt{call(
S, L)};

@item a non-variable, callable term, in which case the typing is
translated to the function call with the same functor as in @tt{S},
with first argument @tt{L}, and the rest of arguments equal to
arguments of @tt{S}.  For instance, @tt{int} is translated to @tt{int(
L)}, and @tt{list( atom)} is translated to @tt{list( L, atom)}.

@end{itemize}

@end{itemize}

").
