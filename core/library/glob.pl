:- module(glob, [], [assertions, regtypes, isomodes, hiord]).

:- doc(title, "Shell-style pathname pattern expansion").

:- doc(author, "Manuel Hermenegildo (original @tt{file_find.pl})").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides file searching predicates to
   locate @concept{pathnames} matching a
   @href{http://en.wikipedia.org/wiki/Glob_(programming)}{@em{globbing}}
   pattern (shell wildcard expansion).
   ").

:- use_module(library(regexp/regexp_code), [match_term/2]).
:- use_module(library(system),
	[file_exists/1, file_property/2, directory_files/2]).
:- use_module(library(pathnames),
	[pathname/1, path_concat/3, path_split_list/2,
	 path_is_root/1]).

:- export(glob_pattern/1).

:- regtype glob_pattern(Pattern) # "@var{Pattern} is a pathname
   pattern whose components may contain shell-style wildcards.".

glob_pattern(A) :- atm(A).

:- use_module(library(aggregates), [findall/3]).

:- export(glob/3).
:- pred glob(Directory, Pattern, FileList)
	:: pathname * pathname * list(pathname)
   # "@var{FileList} is the list of pathnames matching the specified
      pathname pattern @var{Pattern}, relative to @var{Directory}.".

:- doc(glob(Directory, Pattern, FileList), 
   "Search the list of pathnames @var{FileList} matching the
    specified pathname pattern @var{Pattern}. If @var{Pattern} is an
    absolute path, @var{Directory} is ignored. Otherwise, all
    matches are relative to @var{Directory}. If @var{Directory} does
    not exist @var{FileList} is empty. The shortest version
    @pred{glob/2} can be used when @var{Directory} is @tt{'.'} (the
    current directory).

    For example, @pred{glob/2} and @pred{glob/3} will give the
    following results in a typical Unix installation:

@begin{verbatim}
?- use_module(library(glob)).

yes
?- cd('/bin').

yes
?- glob('e*', F).

F = [expr,ed,echo] ? 

yes
?- glob('/', 'bin/e*', F).

F = ['bin/expr','bin/ed','bin/echo'] ? 

yes
?- glob('/tmp', '../bin/e*', F).

F = ['../bin/expr','../bin/ed','../bin/echo'] ? 

yes
?- cd('/tmp').

yes
?- glob('/bin/e*', F).

F = ['/bin/expr','/bin/ed','/bin/echo'] ? 

yes
@end{verbatim}
").

glob(Base, Pattern, Fs) :-
	% Split Pattern path in components (each can be a pattern itself)
	path_split_list(Pattern, Patterns1),
	% Find matching patterns
	( Patterns1 = [Root|Rest], path_is_root(Root) ->
	    Base2 = Root, Patterns2 = Rest,
	    findall(F, matching_path(Patterns2, '', Root, F), Fs)
	; Base2 = Base, Patterns2 = Patterns1,
	  findall(F, matching_path(Patterns2, Base2, '', F), Fs)
	).

matching_path([Pattern|Patterns], Base, Dir, F) :-
	path_concat(Base, Dir, BaseDir),
	file_exists(BaseDir),
	directory_files(BaseDir, Files),
	member(F0, Files),
	match_term(Pattern, F0),
	path_concat(Dir, F0, F1),
	( Patterns = [] ->
	    F = F1
	; path_concat(Base, F1, BaseF1),
	  file_property(BaseF1, type(directory)),
	  matching_path(Patterns, Base, F1, F)
	).

:- export(glob/2).
:- pred glob(Pattern, FileList)
	:: pathname * list(pathname)
   # "Like @pred{glob/3}, relative to the current directory
     (equivalent to @tt{glob('.', Pattern, FileList)})".

glob(Pattern, FileList) :-
	glob('.', Pattern, FileList).


