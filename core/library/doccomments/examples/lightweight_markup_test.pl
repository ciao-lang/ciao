:- module(lightweight_markup_test, [], [assertions,regtypes,isomodes,fsyntax]).
:- use_package(doccomments).
:- use_package(expander).

%!
% @title  Lightweight mark-up test 
%
% @author Jose F. Morales 
%
% @summary This is a test file for the /lightweight/ mark-up language of
%   @apl{LPdoc}. See @tt{markup_test.pl} for the long mark-up version.

%!
% @module
% 
% @begin{alert}
%   - Define `@@verb` (which escapes).
%   - For @tt{`}, do not use `@@tt` but `@@verb`.
%   - Define the `@@verb` with `@@begin` and `@@end`.
%   - Write how to escape characters propertly in wiki syntax.
% @end{alert}
%
% * Simple mark-up
% 
%   @begin{alert} Add support for decorations spanning several lines
%   @end{alert}
%
%   Text can be /emphasized/ or *bold*.
%
% * Paragraphs
%
% Begin of
% a paragraph.
% This paragraph ends here.
%
% Begin of a new paragraph.
% This paragraph ends here.
%
% The last new paragraph. This paragraph ends here.
%
% * Lists
%
% ** Simple lists
%
% Here is another list:
%  - with some item
%  - other item
%  - more items
%
% ** Simple lists (alternative syntaxes - wiki only)
%
% Here is a list:
%
%  - with some item
%
%  - other item
%
%  - more items
%
% Here is a list:
%
%  - with some item
%  - other item
%
%  - more items
%
%  - even more items
%
% Here is yet another list:
% - with some item
% - other item
% - more items
% 
% *** Simple lists (without any heading - wiki only)
% 
% - with some item
% - other item
% - more items
% 
% ** Nested lists
%
% A list with nested items:
%   - item 1
%     - subitem 1-1
%     - subitem 1-2
%       - subitem 1-2-1
%     - subitem 1-3
%   - item 2
%     - subitem 2-1
%   - item 3
%
% ** Enumerated lists
% 
% Enumerated list with automatic numbering:
%   -# First
%   -# Second
%   -# Third
%
% Enumerated list with explicit numbering:
%   1. First
%   2. Second
%   3. Third
%
% Enumerated list with non-consecutive explicit numbering:
%   2. First
%   4. Second
%   6. Third
%
% @begin{alert}
%   The following only works in some backends.
% @end{alert}
%
% Enumerated list mixing all above:
%
%  2. First
%  4. Second
%  -# Third
%  -# Fourth
%  10. Fifth
%  -# Sixth
%
% ** Description lists
%
% Description lists:
%   - opt :: First
%   - foo :: Second
%   - bar :: Third
% 
% ** Description lists (more complex cases)
%
% Description lists with richer items:
%  
%   - `atom` :: an atom.
%   - `append/3` :: a predicate or functor name.
%   - `f(X0,...,Xn)` :: some term with variables `X0`, ..., `Xn`.
%   - `X` :: a variable.
%   - $x^2$ :: some math.
%   - $\bigwedge_j f_j(x_0, \ldots, x_n)$ :: some complex math.
%
% * Sections and subsections
% 
% Text for the section.
% 
% ** Subsection
%
% Text for the subsection.
%
% *** Subsubsection
% 
% Text for the subsubsection.
%
% * Links
%
% A link to [[http://ciao-lang.org]] showing its URL.
%
% A link to [[http://ciao-lang.org][Ciao]] hiding its URL.
%
% A link to [[http://ciao-lang.org][The *Ciao* System]] hiding its URL
% with a complex string.
%
% * Anchors, labels, references, bibliographical citations
% 
% A reference to the first section in this document @ref{Paragraphs}.
% 
% @begin{alert}
%   Implement symbolic labels. Fix the @tt{texinfo} backend by resolving
%   them to the section title.
% @end{alert}
% 
% * Other elements
% 
%   We will not include lightweight mark-up syntax for anything else
%   not described in this document (e.g., images).
% 
% * Syntax for code
% 
% ** Code spans
% 
% This is a predicate name `append/3`, a variable name `X`, an
% atom name `foo`, a quoted atom name `'foo'`.
% 
% ** Blocks of code
% 
% Text that is 4-char indented is recognized as code:
% 
%     list([]).
%     list([X|Xs]) :- list(Xs)
% 
% Code itself can have comments:
% 
%     % definition for lists
%     list([]). % see append/3
%     list([X|Xs]) :- list(Xs)
% 
% Code itself can have documentation comments:
% 
%     %! definition for lists
%     list([]). %< see `append/3`
%     list([X|Xs]) :- list(Xs)
% 
% ** Blocks of other code
% 
% @begin{alert} no way to specify source language yet @end{alert}
%
% This is a piece of C code:
%
%     #include <stdio.h>
%     int main() { return 0; }
%
% @begin{alert} newlines are lost in verbatim blocks @end{alert}
% 
% @begin{verbatim}
% #include <stdio.h>
% int main() { return 0; }
% @end{verbatim}
%
% ** More complex combinations
%
% (Check it from `doccomments_tests.pl`)

% ===========================================================================
% (for wiki only)

%! @bug Adding more sections not working yet. That is:
%    @begin{verbatim}
%      /*!
%      @tt{*} Same section
%     
%      Text for the section.
%      */
%    @end{verbatim}
% 
%  @bug Adding more sections not working yet. That is:
%    @begin{verbatim}
%      % ! * Same section
%      % 
%      %   Text for the section.
%    @end{verbatim}

%! @doinclude [foo/0, foo2/0, append/3, bar/2]

%! foo/0: Testing itemizes inside the description of a predicate. Make
%     sure that they are not interpreted as code verbatims:
%
%      - nothing
%      - even nothing
%
%    Not even in this case:
%
%      - nothing
%      - even nothing
% 
%    This block is not code:
%
%       foo :- example.
%
%    But this code is:
%
%        foo :- example.
foo.

%! foo2/0: Another predicate documentation with a
%          code block:
%    
%              foo :- bar.
foo2.

%! append/3: This is a another predicate
%    that does some things.
%    foobar/3: This is not a predicate doc since it does not start at 
%    column 0.
append(_,_,_).

%! bar(+X,-Y): This is a another predicate using `X` and `Y`,
%    which does even more things.
bar(a,b).

%! @doinclude [bar2/2]

%! bar2(X,Y): This is a another predicate using `X` and `Y`,
%    which does even more things.
bar2(a,b).

% ===========================================================================
%! * Documentation comments in programs
%
%  Syntax for documentation comments is @tt{%!} and @tt{%<} for
%  documenting the preceding program element.
%
%  The following are some programs with comments:

% %! @doinclude [color/1, weekday/1]
:- export([color/1, weekday/1]).

%! color/1: The `color/1` regular type.
%    @includedef{color/1}
:- regtype color/1.
color := (red   %< First case. See `bar/0`.
     ) | (blue  %< Second case
     ).

%! weekday/1: The `weekday/1` regular type.
%    @includedef{weekday/1}
:- regtype weekday/1.
weekday := (monday    /*< lunae dies. */)
	 | (tuesday   /*< dies Martis. */)
         | (wednesday /*< dies Mercurii. */)
         | (thursday  /*< Thor's day. */)
         | (friday    /*< day of Frigg. */).

%! @bug Fix ugly parenthesis around doccomments

% ===========================================================================
%! * Documentation of assertions in programs
%  ** Complex assertions

% %! @doinclude [len/2]
:- export([len/2]).

%! Complex assertions can be included in special @tt{@assertions}
%  blocks. The compiler can optionally read them if necessary (they
%  are not merely documentation):

:- true comp len(A,B) + native.
:- true pred len(L,N) : list * var => list * int
   # "Computes the length of @var{L}".
:- true pred len(L,N) : var * int => list * int
   # "Outputs @var{L} of length @var{N}".
:- true pred len(L,N) : list * int => list * int
   # "Checks that @var{L} is of length @var{N}".

len(L, N) :- var(N), !, llen(L, 0, N).
len(L, N) :- dlen(L, 0, N).

llen(_,_,_). % incomplete
dlen(_,_,_). % incomplete

%!
% @bug We need more compact ways of writing assertions (not only
%   syntactic sugar, we need semantics for that). E.g.
%
%       :- true pred length(L,N):
%          ( list * var => list * int  %< Computes the length of `L`
%          | var  * int => list * int  %< Outputs `L` of length `N`
%          | list * int => list * int  %< Checks that `L` is of length `N`
%          ).
%
%       :- true pred length(L,N) =>
%          ( list * int  %< Computes the length of `L`
%          | list * int  %< Outputs `L` of length `N`
%          | list * int  %< Checks that `L` is of length `N`
%          ).

%! ** Simple assertions

:- export([
	len_0/2,
	len_v/2, len_m/2, len_t/2,
	len_vm/2, len_vt/2, len_mt/2, len_vmt/2]).

%! len_0/2: Computes in the second argument the length of the first
%    argument (*showing*: no vars, no types, no modes).
len_0(_,_).

%! len_v(L,N): Computes the `N` length of the `L` argument (*showing*:
%    vars).
len_v(_,_).

%! len_m(+,-): Computes in the second argument the length of the first
%    argument (*showing*: modes).
len_m(_,_).

%! len_t(list,int): Computes in the second argument the length of
%    the first argument (*showing*: types).
len_t(_,_).

%! len_vm(+L,-N): Computes the `N` length of the `L` argument
%    (*showing*: vars and modes).
len_vm(_,_).

:- pred len_vt(L,N) :: list * int # "Computes the @var{N} length of
   the @var{L} argument (@bf{showing}: vars and types).".
len_vt(_,_).

%! len_mt(+list,-int): Computes the length of the first
%    argument (*showing*: modes and types).
len_mt(_,_).

:- pred len_vmt(+L,-N) :: list * int # "Computes the @var{N} length of
   the @var{L} argument (@bf{showing}: vars, modes, and types).".
len_vmt(_,_).

%! ** Combination of types and modes

:- export([
	sim_a/1, sim_a0/1, sim_a1/1, sim_a2/1, sim_a3/1,
	sim_b/1, sim_b0/1,
	sim_c/1]).

:- pred sim_a(?list).
sim_a(_).
:- pred sim_a0(X) : list(X) => list(X).
sim_a0(_).
:- pred sim_a1(X) :: list(X) => list(X).
sim_a1(_).
:- pred sim_a2(X) :: list(X).
sim_a2(_).
:- pred sim_a3(list). % Not working
sim_a3(_).
:- pred sim_b(-list).
sim_b(_).
:- pred sim_b0(X) :: list(X) : var(X) => list(X).
sim_b0(_).
:- pred sim_c(+list).
sim_c(_).

%% %! * Simpler assertions
%% %
%% %  Syntax for simpler assertions.
%% %
%% % TODO: Choose one of them
%% 
%% %@ pred length(+list,-int).
%% %< Computes the length of the first argument.
%% length(_,_).
%% 
%% %:- pred length(+list,-int).
%% %< Computes the length of the first argument.
%% length(_,_).
%% 
%% %- pred length(+list,-int).
%% %< Computes the length of the first argument.
%% length(_,_).
%% 
%% % TODO: Is it worth for very complex assertions?
%% 
%% %- true comp length(A,B) + native.
%% %- true pred length(L,N) : list * var => list * int.
%%              %< Computes the length of `L`
%% %- true pred length(L,N) : var * int => list * int.
%%              %< Outputs `L` of length `N`
%% %- true pred length(L,N) : list * int => list * int.
%%              %< Checks that `L` is of length `N`
%% 
%% length(L, N) :- var(N), !, llength(L, 0, N).
%% length(L, N) :- dlength(L, 0, N).
%% 
%% %! * Combining assertions with assertion-related predicates
%% 
%% %! This is the definition of a regular type `foo` and its code.
%% 
%% %- regtype foo/1.
%% %< Defines foo.
%% 
%% %@ foo := a
%% %      |  b
%% %      |  c.
%% %  foo := ~number.
%% 
%% %! It can be described more compactly as:
%% 
%% %- regtype foo/1. %< Defines foo.
%% 
%% %@ foo := a
%% %      |  b
%% %      |  c.
%% %  foo := ~number.
%% 
%% %! It can be described even more compactly as:
%% 
%% %! Defines foo
%% %- regtype foo :=
%% %      a
%% %    | b
%% %    | c.
%% %    | ~number.

% ===========================================================================

%! 
% @bug Collect @tt{%! @@bug TEXT} and @tt{%< @@bug TEXT} doccomments
%   that appear between the predicates.
%
% @bug We cannot add assertions in @tt{.lpdoc} files.
%
% @bug We cannot add @tt{:- if} directives in @tt{.lpdoc} files
%
% @bug Add labels.
% @bug LPdoc does not support @tt{\} as escape character
% @bug Need a verbatim with escape sequence
% @bug The elements @tt{`word`} and @@tt@{word@} are different. Add wiki
%   syntax for it? (see Haddock).
%
% @bug How ` can be escaped? (is there any other option than
%   using the long mark-up language?

