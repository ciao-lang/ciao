:-module(markup_test,[],[assertions, isomodes, regtypes, fsyntax]).

:- doc(title, "Mark-up test").
:- doc(author, "Jose F. Morales").

:- doc(summary,"This is test file for the mark-up language of LPdoc,
  using long mark-up. See @tt{markdown_test.pl} for the lightweight
  version.").

:- doc(module,"
@include{../../markdown/examples/markup_simple.lpdoc}
").

% ===========================================================================
:- doc(section, "Documentation comments in programs").

:- doc(text, "Syntax for documentation comments is @tt{%!} and @tt{%<}
for documenting the preceding program element.

The following are some programs with comments:").

% :- doc(doinclude, [color/1, weekday/1]).
:- export([color/1, weekday/1]).

:- regtype color/1 # "The @pred{color/1} regular type.
   @includedef{color/1}".
color := red   %< First case. See `bar/0`.
       | blue. %< Second case

:- regtype weekday/1 # "The @pred{weekday/1} regular type.
   @includedef{weekday/1}".
weekday := monday    /*< lunae dies. */
         | tuesday   /*< dies Martis. */
         | wednesday /*< dies Mercurii. */
         | thursday  /*< Thor's day. */
         | friday    /*< day of Frigg. */.

% ===========================================================================
:- doc(section, "Documentation of assertions in programs").
:- doc(subsection, "Complex assertions").

% :- doc(doinclude, [len/2]).
:- export([len/2]).

%  Complex assertions can be included in special @tt{@assertions}
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

% TODO: We need more compact ways of writing assertions (not only
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

:- doc(subsection, "Simple assertions").

:- export([
	len_0/2,
	len_v/2, len_m/2, len_t/2, 
	len_vm/2, len_vt/2, len_mt/2, len_vmt/2]).

:- pred len_0/2 # "Computes in the second argument the length of the
   first argument (@bf{showing}: no vars, no types, no modes).".
len_0(_,_).

:- pred len_v(L,N) # "Computes the @var{N} length of the @var{L}
   argument (@bf{showing}: vars).".
len_v(_,_).

:- pred len_m(+,-) # "Computes in the second argument the length of
   the first argument (@bf{showing}: modes).".
len_m(_,_).

:- pred len_t(list,int) # "Computes in the second argument the length of
   the first argument (@bf{showing}: types).".
len_t(_,_).

:- pred len_vm(+L,-N) # "Computes the @var{N} length of
   the @var{L} argument (@bf{showing}: vars and modes).".
len_vm(_,_).

:- pred len_vt(L,N) :: list * int # "Computes the @var{N} length of
   the @var{L} argument (@bf{showing}: vars and types).".
len_vt(_,_).

:- pred len_mt(+list,-int) # "Computes the length of the first
   argument (@bf{showing}: modes and types).".
len_mt(_,_).

:- pred len_vmt(+L,-N) :: list * int # "Computes the @var{N} length of
   the @var{L} argument (@bf{showing}: vars, modes, and types).".
len_vmt(_,_).

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

:- doc(bug, "We cannot add assertions in @tt{.lpdoc} files").
:- doc(bug, "We cannot add @tt{:- if} directives in @tt{.lpdoc} files").
:- doc(bug, "Add labels").
:- doc(bug, "LPdoc does not support @tt{\\\\} as escape character").
:- doc(bug, "Need a verbatim with escape sequence").
:- doc(bug, "The elements `word` and @@tt@{word@} are different. Add wiki
   syntax for it? (see Haddock)").
:- doc(bug, "How ` can be escaped? (is there any other option than
   using the long mark-up language?").

