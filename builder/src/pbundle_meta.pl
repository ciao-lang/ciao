:- module(pbundle_meta, [], [assertions, basicmodes,
            nativeprops, fsyntax, hiord, regtypes]).

:- use_module(engine(stream_basic)).
:- use_module(library(read)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(glob), [glob/3]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).

:- doc(module, "Handling of meta-information files for pbundle"). 

% TODO: *********************************************************************
% TODO: * If the desc.tmpl format changes, older files need to be modified! *
% TODO: *********************************************************************

:- regtype pbundle_meta(M)
   # "@var{M} is the description of a @concept{packaged bundle}".
pbundle_meta(_). % TODO: define

% ===========================================================================
% Normalizing pbundle description reader

% TODO: Not really templates, do not use 'tmpl' name. They are plain
%   var/value lists.

:- export(pbundle_meta_load/2).
:- pred pbundle_meta_load(File, Meta) :: atm * pbundle_meta 
   # "@var{Meta} is the result of loading @var{File}".
pbundle_meta_load(AbsFile) := PMeta :-
    open(AbsFile, read, Stream),
    read_tmpl(Stream, PMeta0),
    close(Stream),
    ( path_split(AbsFile, BaseDir, 'desc.tmpl') ->
        PMeta = [basedir = BaseDir|PMeta0]
    ; PMeta = PMeta0
    ).

read_tmpl(Stream, Out) :-
    read(Stream, R),
    !,
    ( R = end_of_file ->
        Out = []
    ; Out = [R|Rs],
      read_tmpl(Stream, Rs)
    ).

% ===========================================================================

:- export(pbundle_meta_attr/3).
pbundle_meta_attr(PMeta, A, V) :-
    member((A=V), PMeta),
    !.

:- export(pbundle_meta_has_name/2).
% (Name must be an atom)
pbundle_meta_has_name(PMeta, Name) :- atom(Name), !,
    BaseDir = ~pbundle_meta_attr(PMeta, basedir),
    path_split(BaseDir, _, Name).

% ---------------------------------------------------------------------------

:- use_module(library(sort)).
:- use_module(library(datetime)).

% Sort a list of PMeta by its timestamp (decreasing)
:- export(sort_pbundle_metas_by_timestamp/2).
sort_pbundle_metas_by_timestamp(PMetas) := SortedPMetas :-
    KTs = ~add_timestamp_key(PMetas),
    KTs2 = ~sort(KTs),
    KTs3 = ~reverse(KTs2),
    SortedPMetas = ~map_second(KTs3).

add_timestamp_key([], []).
add_timestamp_key([T|Ts], [(Timestamp,T)|KTs]) :-
    Date = ~pbundle_meta_attr(T, commit_date),
    date_iso8601_to_timestamp(Date, Timestamp),
    add_timestamp_key(Ts, KTs).

map_second([], []).
map_second([(_,X)|Xs], [X|Ys]) :- map_second(Xs, Ys).

% ---------------------------------------------------------------------------

:- doc(section, "Operations on Collections of pbundle_meta").

:- export(load_pbundle_metas/3).
% Load all the packaged bundle metadata found in a given directory
% @var{PDir} for a branch @var{Branch}
load_pbundle_metas(Branch, PDir0) := AllPMetas :-
    PDir = ~path_concat(PDir0, Branch),
    list_meta_tmpl(PDir, Fs),
    AllPMetas = ~load_pbundle_metas_(Fs, Branch).

load_pbundle_metas_([], _Branch, []).
load_pbundle_metas_([F|Fs], Branch, [V|Vs]) :-
    % TODO: is there a better way to add branch metadata?
    V = ~append(~pbundle_meta_load(F), [branch = Branch]),
    load_pbundle_metas_(Fs, Branch, Vs).

list_meta_tmpl(PDir, Fs) :-
    glob(PDir, '*', Dirs),
    findall(F, list_meta_tmpl_(PDir, Dirs, F), Fs).

list_meta_tmpl_(PDir, Dirs, F) :-
    member(Dir, Dirs),
    % TODO: get 'current' name from a single definition
    \+ Dir = 'current', % avoid it, its a symbolic link
    F = ~path_concat(~path_concat(PDir, Dir), 'desc.tmpl'),
    file_exists(F).

% ---------------------------------------------------------------------------

:- export(newest_pbundle_meta/2).
newest_pbundle_meta(AllPMetas) := PMeta :-
    SortedPMetas = ~sort_pbundle_metas_by_timestamp(AllPMetas),
    SortedPMetas = [PMeta|_].

% ---------------------------------------------------------------------------

% Lookup a pbundle_meta
:- export(lookup_pbundle_meta/3).
lookup_pbundle_meta(Rev, AllPMetas) := PMeta :-
    % Find a pbundle_meta whose 'basedir' param is Rev
    member(PMeta, AllPMetas),
    pbundle_meta_has_name(PMeta, Rev),
    !.
