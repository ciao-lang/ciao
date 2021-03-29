:- module(_, [], [assertions, doccomments, fsyntax, datafacts]).

%! \title Extended git interface
%  \author Jose F. Morales
%
%  With support for Src-commit annotations and custom tree projections

:- use_module(library(process)).
:- use_module(engine(messages_basic)).
:- use_module(library(pathnames)).
:- use_module(library(lists)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(stream_utils), [string_to_file/2]).

% ---------------------------------------------------------------------------

git_call(GitDir, Args, Opts) :-
    process_call(path(git), [~atom_concat('--git-dir=', GitDir)|Args], Opts).

% ---------------------------------------------------------------------------

:- export(git_check_remote/2).
% Check that git clone at GitDir has GitRemote as remote
git_check_remote(GitDir, GitRemote) :-
    git_call(GitDir, ['remote', '-v'], [status(0), stdout(string(Out))]),
    RemoteCs = ~atom_codes(GitRemote),
    append(_, ~append(RemoteCs, _), Out), % RemoteCs sublist of Out
    !.

% ---------------------------------------------------------------------------

:- export(git_commit_info/3).
git_commit_info(GitDir, Id) := info(AuthorStr, DateStr, RawBody) :-
    AuthorStr = ~git_commit_info_(GitDir, Id, '--format=%aN <%aE>'),
    DateStr = ~git_commit_info_(GitDir, Id, '--format=%ad'),
    RawBody = ~git_commit_info_(GitDir, Id, '--format=%B').

git_commit_info_(GitDir, Id, Format) := Str :-
    git_call(GitDir, ['log', Format, '-n', '1', '-r', Id], [stdout(line(Str))]).

% ---------------------------------------------------------------------------

:- export(git_latest_commit/2).
% Get Id for latest commit
git_latest_commit(GitDir) := Id :-
    git_call(GitDir, ['log', '--format=%H', '-n', '1'], [stdout(line(Out))]),
    atom_codes(Id, Out).

% ---------------------------------------------------------------------------

:- data f_all/1.
:- data f_allpub/1.
:- data f_nodist/1.

:- export(git_public_files/4).
% Obtain all public files Fs from Subdir. If Subdir='' then skip the 'bndls' directory.
git_public_files(GitDir, Subdir, Id) := DistFs :-
    cleanup_f,
    % All files, pruning nodist at firts level (some optimization) 
    Fs = ~git_ls_tree_prune1(GitDir, Id, Subdir),
    % Files by subdir prefix
    ( Subdir = '' -> Prefix = ''
    ; Prefix = ~atom_concat(Subdir, '/')
    ),
    ( member(F, Fs),
        atom_prefix(Prefix, F),
        assertz_fact(f_all(F)),
        fail
    ; true
    ),
    % TODO: No good indexing! Optimize with tries?
    % Collect no dist directories (with trailing '/')
    ( f_all(F),
        atom_concat(NoDist, '/NODISTRIBUTE', F),
        % display(nodist(NoDist)), nl,
        assertz_fact(f_nodist(~atom_concat(NoDist, '/'))),
        fail
    ; true
    ),
    % Distributable files (skip files under no dist)
    ( f_all(F),
        ( f_nodist(NoDist), % (trailing '/' included)
          atom_prefix(NoDist, F) ->
            fail % skip
        ; true
        ),
        \+ nopub_file(F),
        % display(F), nl,
        assertz_fact(f_allpub(F)),
        fail
    ; true
    ),
    % Collect all files, cleanup, and unify output
    DistFs0 = ~findall(F, f_allpub(F)),
    cleanup_f,
    DistFs = DistFs0.

cleanup_f :-
    retractall_fact(f_all(_)),
    retractall_fact(f_allpub(_)),
    retractall_fact(f_nodist(_)).

% Specialized ls-tree that prunes NODISTRIBUTE when Subdir='' (faster)
git_ls_tree_prune1(GitDir, Id, Subdir) := Fs :- Subdir = '', !,
    % L0: files at root level
    git_call(GitDir, ['ls-tree', '--name-only', Id, '.'], [stdout(atmlist(L0))]),
    % NoDist: root level so that .../NODISTRIBUTE exists 
    NoDist0 = ~concat_nodist(L0),
    git_call(GitDir, ['ls-tree', '--name-only', Id|NoDist0], [stdout(atmlist(NoDist1))]),
    NoDist = ~dirnames(NoDist1),
    % DistL0: L0 minus NoDist
    DistL0 = ~remove_nodist(L0, NoDist),
    %display(fs(NoDist,DistL0)), nl,
    % Now really list the files
    git_call(GitDir, ['ls-tree', '--name-only', '-r', Id|DistL0], [stdout(atmlist(Fs))]).
git_ls_tree_prune1(GitDir, Id, Subdir) := Fs :-
    git_call(GitDir, ['ls-tree', '--name-only', '-r', Id, Subdir], [stdout(atmlist(Fs))]).

concat_nodist([]) := [].
concat_nodist([F|Fs]) := [~path_concat(F,'NODISTRIBUTE')| ~concat_nodist(Fs)].

dirnames([]) := [].
dirnames([F|Fs]) := [~path_dirname(F)| ~dirnames(Fs)].

remove_nodist([],_) := [].
remove_nodist([F|Fs],NoDistFs) := Ds :-
    ( member(F,NoDistFs) -> Ds = Ds0
    ; Ds = [F|Ds0]
    ),
    Ds0 = ~remove_nodist(Fs,NoDistFs).

% Other non-distributable files
nopub_file('.arcconfig').
nopub_file('.gitmodules').

% Prefix is a prefix of Atom (without creating new atoms)
atom_prefix(Prefix, Atom) :-
    atom_length(Prefix, Len),
    sub_atom(Atom, 0, Len, Prefix).

% ---------------------------------------------------------------------------

:- export(git_latest_commit_src_id/2).
% Match "Str-commit: ..." in the latest commit from GitDir
git_latest_commit_src_id(GitDir) := Id :-
    git_call(GitDir, ['log', '--format=%B', '-n', '1'], [stdout(string(Out))]),
    append(_, "\n"||"Src-commit: "||Rest, Out),
    % remove trailing nl
    ( append(IdStr, "\n"||_, Rest) ->
        true
    ; IdStr = Rest
    ),
    !,
    atom_codes(Id, IdStr).

% ---------------------------------------------------------------------------

:- export(git_commits_until/3).
% Enumerate all commits until any of Ids
% (in chronological order)
git_commits_until(GitDir, StopIds) := Ids :-
    git_call(GitDir, [
        'log',
        '--format=%H',
        '--reverse',
        'HEAD'| ~until_ids(StopIds)
    ], [stdout(atmlist(Ids))]).

until_ids([]) := [].
until_ids([Id|Ids]) := [~atom_concat('^',Id)| ~until_ids(Ids)].

% ---------------------------------------------------------------------------

:- export(git_checkout_tree/4).
% Checkout files Fs from commit Id, striping Subdir
git_checkout_tree(GitDir, Id, Fs, Subdir) :-
    % Strip option for tar (remove sub dir path components)
    StripOpts = ~tar_strip_comps_opts(Subdir),
    % NOTE: using xargs seems fine here, `getconf ARG_MAX` is 262144
    process_pipe([
        process_call(path(xargs), [
            'git',
            ~atom_concat('--git-dir=', GitDir),
            'archive',
            Id]),
        process_call(path(tar), [
            '-xf',
            '-'|StripOpts])
    ], [stdin(atmlist(Fs))]).

% Tar options to strip components (if needed)
tar_strip_comps_opts(Subdir) := StripOpts :-
    ( Subdir = '' -> StripOpts = []
    ; path_split_list(Subdir, SubdirComps),
      length(SubdirComps, N),
      number_codes(N, NCs),
      atom_codes(N2, NCs),
      StripOpts = [~atom_concat('--strip-components=', N2)]
    ).

% ---------------------------------------------------------------------------

:- export(git_add_tree_has_changes/1).
% Check if adding the current tree (cwd) to GitDir introduces changes
git_add_tree_has_changes(GitDir) :-
    git_call(GitDir, [
        'add',
        '--dry-run',
        '-A'
    ], [status(0), stdout(string(DryRunOut))]),
    \+ DryRunOut = "". % Not empty means that changes are expected

% ---------------------------------------------------------------------------

:- export(git_add_tree/1).
% Add the current tree (cwd) to GitDir
git_add_tree(GitDir) :-
    git_call(GitDir, [
        'add',
        '-A'
    ], [stdout(string(Out))]),
    show_gitprefix(Out).

% ---------------------------------------------------------------------------

:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).

:- export(git_commit_annot_src_id/3).
% Commit using commit info Info annotated with "Src-commit:"
git_commit_annot_src_id(GitDir, Id, Info) :-
    Info = info(AuthorStr, DateStr, RawBody),
    atom_codes(AuthorAtm, AuthorStr),
    atom_codes(DateAtm, DateStr),
    mktemp_in_tmp('tmpXXXXXX', MsgFile),
    string_to_file(~msg_annot_src_id(Id, RawBody), MsgFile),
    git_call(GitDir, [
        'commit',
        ~atom_concat('--author=', AuthorAtm),
        ~atom_concat('--date=', DateAtm),
        '-F', MsgFile
    ], [stdout(string(Out))]),
    show_gitprefix(Out),
    del_file_nofail(MsgFile).

% Annotate "Src-commit:" in commit message
% TODO: add option to disable RawBody?
msg_annot_src_id(Id,RawBody,Msg) :-
    append(RawBody, "\n"||"Src-commit: "||Msg0, Msg),
    append(~atom_codes(Id), "\n", Msg0).

% ---------------------------------------------------------------------------

% Check if remote exists
:- export(git_ping_remote/1).
git_ping_remote(Remote) :-
    process_call(path(git), [
        'ls-remote',
        Remote
    ], [status(0), stdout(null), stderr(null)]).

% ---------------------------------------------------------------------------

:- export(git_cmd_atwd/3).
% Git command moving to the parent of Gitdir before
git_cmd_atwd(Gitdir, Args, Opts) :-
    D = ~path_dirname(Gitdir),
    process_call(path(git), Args, [cwd(D)|Opts]).

:- export(git_cmd_atwd_q/2).
% Git command moving to the parent of Gitdir before
git_cmd_atwd_q(Gitdir, Args) :-
    git_cmd_atwd(Gitdir, Args, [stdout(string(Out))]),
    show_gitprefix(Out).

% ---------------------------------------------------------------------------

% Show output prefixing each line with "[git] "
show_gitprefix(Out) :-
    Out2 = ~prefix_lines(Out, "[git] "),
    lformat([$$(Out2)]).

% TODO: copied from builder messages_aux.pl

% Add @var{Prefix} to each line in @var{String0}, put result in @var{String}
prefix_lines(String0, Prefix, String) :-
    append(Prefix, String1, String),
    prefix_lines_(String0, Prefix, String1).

prefix_lines_([],       _,      []).
prefix_lines_([0'\n|R], Prefix, NR) :- !,
    NR = [0'\n|NR1],
    append(Prefix, NR0, NR1),
    prefix_lines_(R, Prefix, NR0).
prefix_lines_([C|R], Prefix, [C|NR]) :- !,
    prefix_lines_(R, Prefix, NR).

