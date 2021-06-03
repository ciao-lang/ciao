:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title, "Analyze grade").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module contains command definitions for the
   @tt{analyze} grade, which performs static code analysis.").

:- include(ciaobld(cmd_hooks)).

% ---------------------------------------------------------------------------

% Any target requires a bin build of ciaopp
'grade.requires'(analyze, bin, 'ciaopp').

% ---------------------------------------------------------------------------
% analyze

'grade.cmd'(analyze, analyze, analyze_).

'cmd.comment'(analyze_, ["analyzing", "analyzed"]).
'cmd.grade'(analyze_, analyze).
'cmd.needs_update_builder'(analyze_).
'cmd.needs_rescan'(analyze_).
'cmd.needs_config'(analyze_).
'cmd.recursive'(analyze_, forward).
'cmd.do_before.decl'(analyze_).
'cmd.do_before'(analyze_, _).

% ---------------------------------------------------------------------------
% Primitive targets for analyze grade

:- use_module(library(system), [working_directory/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(pathnames), [path_split/3]).
:- use_module(ciaobld(messages_aux), [normal_message/2]).
:- use_module(ciaobld(manifest_compiler), [main_file_path/3]).
:- use_module(ciaobld(ciaopp_aux), [
    invoke_ciaopp/1,
    invoke_ciaopp_batch/1,
    invoke_ciaopp_dump/1]).

'grade.prim_kind'(analyze, bin) :- !.
'grade.prim_do'(analyze, Prim, Bundle, Cmd) :- !,
    prim(Prim, Bundle, Cmd).
    %normal_message("ana ~w", [p(Prim,Bundle,Cmd)]).

prim(cmd(Path), Bundle, Cmd) :- atom(Path), !,
    % TODO: share
    path_split(Path, _, Name0),
    ( atom_concat(Name, '.pl', Name0) -> true
    ; Name = Name0
    ),
    prim(cmd(Name, [main=Path]), Bundle, Cmd).
prim(cmd(Name, Opts), Bundle, analyze) :-
    AbsPath = ~main_file_path(Bundle, Opts),
    ( member(libexec, Opts) ->
        normal_message("analyzing ~w (libexec)", [Name])
    ; normal_message("analyzing ~w (command)", [Name])
    ),
    !,
    analyze_cmd(AbsPath).
prim(lib(Path), Bundle, analyze) :- !,
    analyze_lib(~bundle_path(Bundle, Path)).
prim(_Prim, _Bundle, _Cmd) :-
    % (ignore others)
    % display(prim(_Prim, _Bundle, _Cmd)), nl.
    true.

analyze_cmd(Path) :-
    % ensure that it ends in .pl
    ( atom_concat(_, '.pl', Path) ->
        Path = Path2
    ; atom_concat(Path, '.pl', Path2)
    ),
    Dump = ~atom_concat(Path2, '.dump'),
    %
    path_split(Path, Dir, Base),
    working_directory(ThisDir, ThisDir),
    working_directory(_, Dir), % TODO: this should not be required
    invoke_ciaopp(['-A', Base, '-ftypes=none', '-fmodes=pdb', '-fintermod=on', '-fdump=incremental', '-foutput=off']),
    invoke_ciaopp_dump([report, reach, Dump]),
    working_directory(_, ThisDir).

analyze_lib(Path) :-
    invoke_ciaopp_batch(['pdb', Path]).

