:- module(_, [], [assertions, fsyntax, datafacts]).

:- doc(title, "Publish from a mono-repository").
:- doc(author, "Jose F. Morales").

:- doc(module, "
   This tool allows publishing pristine views of an @em{annotated} git
   mono-repository. An annotated git repository contains
   @tt{NODISTRIBUTE} empty files that mark directories that must not
   be visible in a pristine copy.

   @begin{alert}
   This tool is tailored for the @tt{ciao-devel} mono-repository but
   it could be easily generalized for other repositories.
   @end{alert}
   
   @section{Usage}
   
   Use @tt{ciao publish list} to list publishable bundles from the
   Ciao devel repository (either the main @tt{ciao} or some bundles at
   @tt{bndls/}). E.g.:
@begin{verbatim}
$ ciao publish list
@end{verbatim}
   
   Publishing code is performed by the @tt{commit} and @tt{push}
   operations. Use the @tt{help} option to view other possible commmands.
   
   Example for publishing the bundle at the current directory (when
   run at the Ciao root directory the special @tt{ciao} bundle is
   selected, which contains @tt{core} and @tt{builder}):
@begin{verbatim}
$ ciao publish pull
$ ciao publish commit
<< (please review commits) >>
$ ciao publish push
@end{verbatim}
   
   A command may be followed by a target bundle name to specify a
   different bundle. Example for publishing @tt{ciao_emacs}:
@begin{verbatim}
$ ciao publish pull ciao_emacs
$ ciao publish commit ciao_emacs
<< (please review commits) >>
$ ciao publish push ciao_emacs
@end{verbatim}
   
   The first time that @tt{commit} is performed, it will require a first
   commit number. This is not needed for @tt{squash}.

   You can use the @tt{--all} option to perform a command on all
   publishable bundles (see @tt{--help} for more info), or use shell
   scripts.
   
   Examples on all bundles:
@begin{verbatim}
<< Consult the status of all publishable bundles >>
$ ciao publish status --all

<< Pull all public repositories (previous to publishing) >>
$ ciao publish pull --all

<< Commit all publishable changes >>
$ ciao publish commit --all

<< Push to the public repository >>
$ ciao publish push --all
@end{verbatim}
   
   @begin{itemize}
   @item The source is fetched from the repository (not from the
     checkout).
   @item Clones of publishing repos are kept at
     @tt{$HOME/REPOS-ciao-publish} (with some metadata to speedup
     detection of unpublished commits).
   @end{itemize}
   
   @section{Additional files for publishing}

   Other directories:   
@begin{verbatim}
ci-scripts/      Scripts for CI
ci-status/       Scripts for checking the status of CI
etc/ciao-download-stats.sh
                 Download statistics
@end{verbatim}
   
   @section{Contribution from public repositories}
   
   Assume that Bob maintains the private Ciao repository and Alice wants
   to contribute to it. This is the typical workflow for contributing to
   the project preserving authorship:
   @begin{itemize}
   @item Alice: clone public Git repository (@tt{github.com/ciao-lang/ciao})
   @item Alice: make some changes and commit them
   @item Alice: format patches using @tt{git format-patch} (never push them!)
@begin{verbatim}
$ git format-patch HEAD^n # where n is the number of commits not in master
@end{verbatim}
   
   @item Alice: send the @tt{????-*.patch} files via email to Bob
   @item Bob: apply back the changes in the private repository with
   
@begin{verbatim}
$ git am < 0001-*.patch
$ git am < 0002-*.patch
$ ...
@end{verbatim}
   
   @item Bob: check with @tt{git log} that the code is pushed correctly
   @item Bob: do @tt{ciao publish pull}, @tt{ciao publish commit},
     and @tt{ciao publish push}

   @item Alice: do @tt{git pull} and @tt{git rebase -i}. The first command
     will create a merge commit. The second command will break. After
     some @tt{git rebase --continue} it will end. If everything is fine
     there will be no unpushed changes to master and all new commits
     will be authored as Alice.
   @end{itemize}
").

% ===========================================================================

show_help :-
    write_string("Usage: ciao publish CMD [OPTS] [Bundle]

This command perform bundle publishing operations.

Options:

  --id Id          Select source commit Id
  --all            Select all bundles
  --verbose        Verbose mode
  --pubrepos DIR   Select directory containing the publishing repository
                   clones (~/REPOS-ciao-publish by default)
  --dry            Do not commit (only for 'squash')

Help:

  help             Show this message

Commands for querying information:

  list             Show publishable bundles
  info             Show publishing info about a bundle

Commands for adding commits:

  status           Show status (unpublished commits)
  commit           Create commits from publishable changes
  squash           Like commit, but squash into a single commit
                   (more efficient than rebase but loses history)
                   If '--dry', the tree is left in a temporary dir
                   (no commit is done) 

Commands on the publishing repo:

  init             Initialize
  pull             Pull
  push             Push
  rebase -- [args] Rebase (with of rest args)

If no Bundle is specified, it is automatically detected from the
working directory (this assumes that we have an updated checkout).
The root directory corresponds to the special 'ciao' bundle identifier
(containing core, builder, etc.)

Use --all to work on all publishable bundles (only for pull, commit,
push, status).

").

% ===========================================================================

:- use_module(library(lists)).

:- use_module(engine(messages_basic)).
:- use_module(library(streams)).

:- use_module(library(port_reify)).
:- use_module(library(process)).
:- use_module(library(stream_utils), [write_string/1, string_to_file/2, file_to_string/2]).
:- use_module(library(system_extra), [file_to_line/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(system)).
:- use_module(library(pathnames)).

:- use_module(ciaobld(git_extra)).

:- export(main/1).
main([H]) :-
    ( H = help ; H = '-h' ; H = '--help' ),
    !,
    show_help.
main(Args) :-
    Args = [Cmd|Args1],
    parse_opts(Args1, Args2, Opts),
    cmd_decl(Cmd),
    !,
    %
    run_cmd(Cmd, Opts, Args2).
main(_) :-
    message(error, [
        'Invalid usage. See \'ciao publish help\'.']),
    halt(1).

parse_opts(['--id', Id|Args], Args2, [srcid(Id)|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(['--all'|Args], Args2, [allbundles|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(['--verbose'|Args], Args2, [verbose|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(['--dry'|Args], Args2, [dryrun|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(['--pubrepos', Dir|Args], Args2, [pubrepos(Dir)|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(['--'|Args], Args2, [restargs(Args)]) :- !,
    Args2 = []. % consume rest of args unchanged
parse_opts([Arg|Args], [Arg|Args2], Opts) :- !, % (argument)
    parse_opts(Args, Args2, Opts).
parse_opts([], [], []).

% ---------------------------------------------------------------------------
%! # Run command

:- data opt/1.

run_cmd(list, _, _) :- !, % show publishable bundles
    ( % (failure-driven loop)
      list_bundles(B),
        display(B), nl,
        fail
    ; true
    ).
run_cmd(Cmd, Opts, Args) :-
    ( member(allbundles, Opts) ->
        AllBundles = yes
    ; AllBundles = no
    ),
    % Run command
    retractall_fact(opt(_)),
    ( member(dryrun, Opts) -> assertz_fact(opt(dryrun)) ; true ),
    ( member(verbose, Opts) -> assertz_fact(opt(verbose)) ; true ),
    ( AllBundles = yes, Args = [] ->
        run_cmd_all(Cmd, Opts)
    ; ( Args = [SrcBundle] -> true
      ; Args = [] -> SrcBundle = ~detect_bundle
      ; message(error, ['Incorrect input']),
        halt(1)
      ),
      run_cmd_one(Cmd, SrcBundle, Opts)
    ).

% Run on all bundles
run_cmd_all(Cmd, Opts) :-
    ( cmd_allow_all(Cmd) -> true
    ; message(error, ['Invalid command \'', Cmd, '\' with \'--all\', see help message']),
      halt(1)
    ),
    \+ (list_bundles(B),
        \+ run_cmd_one(Cmd, B, Opts)).

% Run on a single bundle
run_cmd_one(Cmd, SrcBundle, Opts) :-
    ( member(pubrepos(Dir), Opts) ->
        PubRepos = Dir
    ; PubRepos = ~path_concat(~get_home, 'REPOS-ciao-publish')
    ),
    ( file_exists(PubRepos) -> true
    ; message(error, [
        'Directory for publishing repository clones does not exist or is not readable:\n',
        '  ', PubRepos]),
      halt(1)
    ),
    %
    reset_cfg,
    init_cfg(SrcBundle, PubRepos),
    %
    ( cmd_needs_check_repos(Cmd) -> check_repos ; true ),
    init_tmpdir,
    cmd_run(Cmd, Opts),
    cleanup_tmpdir.

:- discontiguous(cmd_decl/1).
:- discontiguous(cmd_allow_all/1).
:- discontiguous(cmd_needs_check_repos/1).
:- discontiguous(cmd_run/2).

% ---------------------------------------------------------------------------
%! # List of bundles

% (handled separatedly)
cmd_decl(list).

% ---------------------------------------------------------------------------
%! # Info for publishing

cmd_decl(info).
cmd_run(info, _Opts) :- !, config_summary.

% ---------------------------------------------------------------------------
%! # Temporary file storage

cleanup_tmp_subdir(Dir) :-
    atom_concat(~atom_concat(~tmpdir, '/'), _, Dir), % sanity check
    process_call(path(rm), ['-rf', Dir], [status(0)]).

tmpdir := '/tmp/tmp-ciaopublish'.
treedir := '/tmp/tmp-ciaopublish/treedir'.
drytreedir := '/tmp/tmp-ciaopublish/dry-treedir'.

init_tmpdir :- mkdir_p(~tmpdir).

cleanup_tmpdir :-
    true. % TODO: really cleanup
    %process_call(path(rmdir), [~tmpdir], []).

% ---------------------------------------------------------------------------
%! # Status of dst repo

cmd_decl(status).
cmd_allow_all(status).
cmd_needs_check_repos(status).
cmd_run(status, _Opts) :- !, run_status.

run_status :-
    UIds = ~check_unpublished_commits, % (report some summary)
    ( opt(verbose) ->
        % More details
        Id = ~git_latest_commit(~srcgit),
        lformat([
            '  Latest source commit: ', Id, '\n',
            '  Latest published commit: ', ~dst_id, '\n'
        ]),
        ( NoPubId = ~get_dstgit_last_nopub_id ->
            lformat(['  Last commit without public changes for this bundle: ', NoPubId, '\n'])
        ; true
        ),
        ( UIds = [FirstId|_] ->
            lformat(['  First unpublished commit: ', FirstId, '\n'])
        ; true
        )
    ; true
    ).

check_unpublished_commits := UIds :-
    lformat(['=> ', ~srcbundle, ':']),
    ( DstId = ~dst_id, \+ DstId = '' ->
        true
    ; lformat([' unknown publishing status!\n']),
      lformat([
          % ___________________________________________________________________________
          'ERROR: Could not find any \'Src-commit\' mark in the publishing repository.\n',
          'Please specify the first commit Id in \'commit\' or \'squash\'.\n'
      ]),
      halt(1)
    ),
    UIds = ~unpublished_commits,
    length(UIds, UIdsN),
    ( UIdsN = 0 ->
        lformat([' no unpublished commits\n'])
    ; lformat([' ', UIdsN, ' unseen publishable commits\n'])
    ).

% ---------------------------------------------------------------------------
%! # Update dst repo

:- data did_commit/0.
:- data commit_n/1.
:- data delay_nl/0.
maybe_nl :- ( retract_fact(delay_nl) -> lformat(['\n']) ; true ).

%! ## Update commits
cmd_decl(commit).
cmd_allow_all(commit).
cmd_needs_check_repos(commit).
cmd_run(commit, Opts) :- !, run_commit(Opts, nosquash).

%! ## Squash
cmd_decl(squash).
cmd_needs_check_repos(squash).
cmd_run(squash, Opts) :- !, run_commit(Opts, squash).

run_commit(Opts, Squash) :- !,
    retractall_fact(did_commit),
    ( member(srcid(Id0), Opts) -> TargetId = Id0
    ; TargetId = ~git_latest_commit(~srcgit)
    ),
    run_commit_(Squash, TargetId),
    ( did_commit -> after_commit_help ; true ).

run_commit_(nosquash, TargetId) :- % Add all commits
    UIds = ~check_unpublished_commits,
    length(UIds, UIdsN),
    set_fact(commit_n(0)),
    ( % (failure-driven loop)
      member(UId, UIds),
        Fs = ~git_public_files(~srcgit, ~srcsubdir, UId),
        Info = ~git_commit_info(~srcgit, UId),
        prepare_tree(UId, Fs),
        %
        ( commit_n(UIdN0) -> true ; fail ),
        UIdN is UIdN0+1,
        set_fact(commit_n(UIdN)),
        lformat(['\rChecking commit ', UId, ' (', UIdN, '/', UIdsN, ')']),
        set_fact(delay_nl),
        %
        update_tree(UId, Info),
        ( UId = TargetId ->
            ! % (break)
        ; fail % (loop)
        )
    ; true
    ),
    maybe_nl.
run_commit_(squash, TargetId) :- % Squash single commit
    Fs = ~git_public_files(~srcgit, ~srcsubdir, TargetId),
    Info = ~squash_commit_info(TargetId),
    prepare_tree(TargetId, Fs),
    lformat(['Squasing commit ', TargetId, '\n']),
    ( opt(dryrun) ->
        keep_treedir
    ; update_tree(TargetId, Info)
    ).

% Commit info for squash % TODO: this can be improved
squash_commit_info(UId) := Info :-
    Info0 = ~git_commit_info(~srcgit, UId),
    ( DstId = ~dst_id, \+ DstId = '' ->
        Info = Info0
    ; % Replace message commit
      Info0 = info(AuthorStr, DateStr, _),
      RawBody = "Initial commit", % TODO: customize
      Info = info(AuthorStr, DateStr, RawBody)
    ).

keep_treedir :-
    mvdir(~treedir, ~drytreedir),
    lformat([
        '\n',
        'Running in DRY-RUN mode. No commit has been made.\n',
        'You can inspect the tree at:\n',
        '  ', ~drytreedir, '\n',
        '\n']).

prepare_tree(Id, Fs) :-
    % lformat(['Getting pristine view for commit: ', Id, '\n']), % TODO: debug
    checkout_tree(Id, Fs),
    patch_tree.

% Do a checkout of the public files
checkout_tree(Id, Fs) :-
    cleanup_tmp_subdir(~treedir),
    mkdir_p(~treedir),
    with_cwd(~treedir, checkout_tree_(Id, Fs)).

checkout_tree_(Id, Fs) :-
    git_checkout_tree(~srcgit, Id, Fs, ~srcsubdir).

% Update index (add new files, update modified files, remove missing
% files) to match the working tree.
update_tree(Id, Info) :-
    with_cwd(~treedir, update_tree_(Id, Info)).

update_tree_(Id, Info) :-
    % First try a dry run
    ( \+ git_add_tree_has_changes(~dstgit) ->
        % Cache Id as the latest commit without public changes
        string_to_file(~atom_codes(Id), ~dstgit_last_nopub_id)
    ; maybe_nl,
      git_add_tree(~dstgit),
      git_commit_annot_src_id(~dstgit, Id, Info),
      % Mark that we did a commit
      ( did_commit -> true ; assertz_fact(did_commit) )
    ).

% ---------------------------------------------------------------------------
%! # Initialization of dst repo

cmd_decl(init).
cmd_run(init, _Opts) :- !, run_init_dstgit.

% Create dstgit repository (if it does not exists)
run_init_dstgit :-
    % Abort if dstgit is already initialized
    ( file_exists(~dstgit) ->
        lformat([
            % ___________________________________________________________________________
            'ERROR: A local clone for publishing seems to exists at:\n',
            '  ', ~dstgit, '\n',
            '\n',
            'Aborting repository initialization.\n',
            '\n']),
        halt(1)
    ; true
    ),
    % Check that dstremote exist
    ( \+ git_ping_remote(~dstremote) -> % remote does not exist?
        % TODO: add support to GitHub API if needed?
        lformat([
            % ___________________________________________________________________________
            'NOTE: Remote repository must be created manually.\n',
            '\n',
            'Please login to https://github.com and create a new empty\n',
            'repository for ', ~dstremote, '.\n',
            '\n',
            'Then try again \'ciao publish init\'.\n',
            '\n']),
        halt(1)
    ; true
    ),
    % Actual initialization
    lformat([
        'Initializing a publishing repository at:\n',
        '  ', ~dstremote, '\n'
    ]),
    DstGitdir = ~path_dirname(~dstgit), % without .git
    mkdir_p(DstGitdir),
    with_cwd(DstGitdir, run_init_dstgit_),
    after_init_help.

run_init_dstgit_ :-
    git_cmd_atwd_q(~dstgit, ['init']),
    string_to_file("(empty)", 'README'),
    git_cmd_atwd_q(~dstgit, ['add', '.']),
    git_cmd_atwd_q(~dstgit, ['commit', '-m', 'First commit']),
    git_cmd_atwd_q(~dstgit, ['remote', 'add', 'origin', ~dstremote]),
    git_cmd_atwd_q(~dstgit, ['remote', '-v']),
    git_cmd_atwd_q(~dstgit, ['push', '-u', 'origin', 'master']).

% ---------------------------------------------------------------------------
%! # Pull and push on published repo

%! ## Pull
cmd_decl(pull).
cmd_allow_all(pull).
cmd_needs_check_repos(pull).
cmd_run(pull, _Opts) :- !, run_pull_dstgit.

% Pull to make sure that dstgit contains latest commits
run_pull_dstgit :-
    lformat(['=> ', ~srcbundle, ': pulling remote\n']),
    git_cmd_atwd_q(~dstgit, ['pull']).

%! ## Push
cmd_decl(push).
cmd_allow_all(push).
cmd_needs_check_repos(push).
cmd_run(push, _Opts) :- !, run_push_dstgit.

run_push_dstgit :-
    lformat(['=> ', ~srcbundle, ': pushing remote\n']),
    % TODO: make check for pending commits optional? (it is faster
    % than simply 'git push' but assumes remote is OK)
    git_cmd_atwd(~dstgit, ['log', 'origin/master..master'], [stdout(string(Out))]),
    ( Out = "" -> % No pending commits
        lformat(['No remaining commits to push\n'])
    ; git_cmd_atwd_q(~dstgit, ['push']),
      after_push_help 
    ).

%! ## Rebase
cmd_decl(rebase).
cmd_needs_check_repos(rebase).
cmd_run(rebase, Opts) :- !, run_rebase_dstgit(Opts).

run_rebase_dstgit(Opts) :-
    lformat(['=> ', ~srcbundle, ': rebasing remote\n']),
    ( member(restargs(Args), Opts) -> true
    ; Args = []
    ),
    git_cmd_atwd(~dstgit, ['rebase'|Args], []).

% ---------------------------------------------------------------------------
%! # Help on next steps

% Help after init
after_init_help :-
    lformat([
        % ___________________________________________________________________________
        '\n',
        'Destination repository created, you can publish your commits.\n',
        '\n',
        'Use \'ciao publish squash\' to publish the current version, or\n',
        '    \'ciao publish squash Id\' to select the first commit.\n',
        'Amending the commit message is recommended.\n',
        '\n']).

% Help after commit
after_commit_help :-
    lformat([
        % ___________________________________________________________________________
        '\n',
        'Use \'ciao publish rebase -- -i\' to rebase commits if needed.\n',
        'Use \'ciao publish push\' to send commits to remote.\n']).

% Help after push
% TODO: configure per bundle
after_push_help :- ~srcbundle = ciao, !,
    lformat([
        % ___________________________________________________________________________
        '\n',
        'REMINDERS:\n',
        '\n',
        '  - Check the status of github actions\n',
        '  - Trigger build at https://github.com/ciao-lang/docker-ciao\n',
        '    (Output: https://hub.docker.com/r/ciaolang/ciao/)\n',
        '  - For new releases, remember to create and push tags:\n',
        '    E.g.,\n',
        '      cd ', ~path_dirname(~dstgit), '\n',
        % TODO: instructions to delete local and remote tag
        %   git tag -d vMAJOR.MINOR.PATCH
        %   git push --delete origin vMAJOR.MINOR.PATCH
        '      git tag vMAJOR.MINOR.PATCH\n',
        '      git push origin vMAJOR.MINOR.PATCH\n',
        '\n',
        '  - Remember to change default_vers_bin=MAJOR.MINOR.PATCH in ciao-boot.sh\n',
        '\n']).
after_push_help.

% ---------------------------------------------------------------------------
%! # Patch a tree before publishing

patch_tree :-
    with_cwd(~treedir, patch_tree_).

patch_tree_ :-
    % Patch github specific (depends on bundle)
    ( ~srcbundle = 'ciao' ->
        rmfiles(['COPYRIGHT', 'LGPL', 'GPL']) % TODO: move?
        % patch_readme,
        % patch_ci_scripts
    ; % Remove ACTIVATE mark
      ( file_exists('ACTIVATE') ->
          rmfiles(['ACTIVATE'])
      ; true
      )
    ),
    patch_license.

% patch_readme :-
%     ( file_exists('README.md') ->
%         file_to_string('README.md', PrevText)
%     ; PrevText = ""
%     ),
%     append(
%         "[![Build Status](https://travis-ci.org/ciao-lang/ciao.svg)](https://travis-ci.org/ciao-lang/ciao)\n"||
%         "[![Build Status](https://ci.appveyor.com/api/projects/status/fu2eb23je22xc228?svg=true)](https://ci.appveyor.com/project/jfmc/ciao)\n"||
%         "\n",
%         Text0, Text),
%     append(PrevText, Text1, Text0),
%     Text1 = ("\n"||
%       "---\n"||
%       "**NOTE**: Repository automatically projected from the Ciao monorepo.\n"),
%     string_to_file(Text, 'README.md').

% patch_ci_scripts :-
%     bundle_path('builder', 'dist/ci-scripts', Dir),
%     cpfile(~path_concat(Dir, 'travis.yml'), '.travis.yml'),
%     cpfile(~path_concat(Dir, 'appveyor.yml'), '.appveyor.yml').

patch_license :-
    bundle_path('builder', 'dist/licenses', Dir),
    License = ~srclicense,
    ( License = 'LGPL' -> cpfile(~path_concat(Dir, 'LGPL'), 'LGPL')
    ; License = 'GPL' ->  cpfile(~path_concat(Dir, 'GPL'), 'GPL')
    ; true % none
    ).

% ---------------------------------------------------------------------------
%! # Bundles

:- use_module(engine(internals), [ciao_root/1]).

root_bundle(ciao). % Minimum system (not really a bundle!)

src_ciao_root := ~ciao_root. % TODO: customize?

% List publishable bundles (nondet)
% (Note: even if they are not ACTIVATEd!)
list_bundles(Bundle) :-
    Bundle = ~root_bundle.
list_bundles(Bundle) :-
    BndlsDir = ~path_concat(~src_ciao_root, 'bndls'),
    directory_files(BndlsDir, Fs),
    member(Bundle, Fs), % (nondet)
    F = ~path_concat(BndlsDir, Bundle),
    ( ( file_exists(~path_concat(F, 'Manifest'))
      ; file_exists(~path_concat(F, 'Manifest.pl'))
      ) -> true
    ; fail
    ),
    \+ file_exists(~path_concat(F, 'NODISTRIBUTE')).

detect_bundle(Bundle) :-
    BndlsDir = ~path_concat(~src_ciao_root, 'bndls'),
    % Check if we are in root or any bndls/ subdirectory
    working_directory(Tmp, Tmp),
    ( path_get_relative(BndlsDir, Tmp, Curr) ->
        % inside a bndls/ subdir, get first component
        path_split_list(Curr, [Bundle|_])
    ; Bundle = ~root_bundle
    ).

% ---------------------------------------------------------------------------
%! # Configuration of source and published repos

:- data srcbundle/1.
:- data srcgit/1.
:- data srcsubdir/1.
:- data srclicense/1.
:- data srcremote/1.
:- data dsturl/1.
:- data dstremote/1.
:- data dstgit/1.
:- data dstgit_last_nopub_id/1.

reset_cfg :-
    retractall_fact(srcbundle(_)),
    retractall_fact(srcgit(_)),
    retractall_fact(srcsubdir(_)),
    retractall_fact(srclicense(_)),
    retractall_fact(srcremote(_)),
    retractall_fact(dstgit(_)),
    retractall_fact(dstremote(_)).

% src and dst config
init_cfg(SrcBundle, PubRepos) :-
    % Source bundle
    set_fact(srcbundle(SrcBundle)),
    % srcgit=`git rev-parse --git-dir`
    % srcgit=`realpath "$srcgit"`
    % Path to source git clone .git directory
    set_fact(srcgit(~path_concat(~src_ciao_root, '.git'))),
    % Expected remote
    set_fact(srcremote('git@gitlab.software.imdea.org:ciao-lang/ciao-devel.git')),
    % Bundle subdirectory in source repository
    ( SrcBundle = ciao ->
        SrcSubdir = ''
    ; % TODO: customize?
      SrcSubdir = ~path_concat('bndls', SrcBundle) % (Prefix from src)
    ),
    set_fact(srcsubdir(SrcSubdir)),
    % License
    set_fact(srclicense('LGPL')), % TODO: customize, extract from Manifest.pl
    % Destination git
    set_fact(dsturl(~path_concat('github.com/ciao-lang', SrcBundle))),
    set_fact(dstremote(~path_concat('git@github.com:ciao-lang', SrcBundle))),
    set_fact(dstgit(~path_concat(~path_concat(PubRepos, ~dsturl), '.git'))),
    set_fact(dstgit_last_nopub_id(~path_concat(PubRepos, ~atom_concat(~dsturl, '.last_nopub_id')))).

config_summary :-
    lformat([
        % ___________________________________________________________________________
        '[Bundle \'', ~srcbundle, '\' as ', ~dstremote, ']\n',
        '  srcgit: ', ~srcgit, '\n',
        '  srcsubdir: ', ~srcsubdir, '\n',
        '  srclicense: ', ~srclicense, '\n',
        '  srcremote: ', ~srcremote, '\n',
        ' ===>\n',
        '  dstgit: ', ~dstgit, '\n',
        '  dstgit_last_nopub_id: ', ~dstgit_last_nopub_id, '\n',
        '\n']).

config_short_summary :-
    lformat([
        '[Bundle \'', ~srcbundle, '\' as ', ~dstremote, ']\n'
    ]).

% ---------------------------------------------------------------------------
%! # Some src/dst repo sanity checks

check_repos :-
    check_srcgit,
    check_dstgit,
    ( opt(verbose) -> config_short_summary ; true ).

check_srcgit :-
    ( git_check_remote(~srcgit, ~srcremote) ->
        true
    ; config_summary,
      lformat([
          % ___________________________________________________________________________
          'ERROR: Source directory does not seem the right clone.\n',
          '\n',
          '  Make sure that directory:\n',
          '    ', ~srcgit, '\n',
          '  contains a clone of:\n',
          '    ', ~srcremote, '\n',
          '\n'
      ])
    ).

check_dstgit :-
    ( file_exists(~dstgit),
      git_check_remote(~dstgit, ~dstremote) ->
        true
    ; config_summary,
      lformat([
          % ___________________________________________________________________________
          'ERROR: Could not find a valid local clone for publishing.\n',
          '\n',
          '  Make sure that this directory exists:\n',
          '    ', ~dstgit, '\n',
          '  and contains a clone of:\n',
          '    ', ~dstremote, '\n',
          '\n',
          'If the repository for publishing does not exists yet, you can use the\n',
          '\'init\' command of this script to create a new empty repository.\n',
          '\n'
      ])
    ).

% ---------------------------------------------------------------------------

% Commit (from srcgit) at dstgit
dst_id := ~git_latest_commit_src_id(~dstgit).

% Cached Id of latest checked commit (without publishable changes)
get_dstgit_last_nopub_id := NoPubId :-
    file_exists(~dstgit_last_nopub_id),
    NoPubId0 = ~file_to_line(~dstgit_last_nopub_id),
    !,
    atom_codes(NoPubId, NoPubId0).

% Get all commits which have not been checked for publishing yet
% (all commits until ~dst_id or ~get_dstgit_last_nopub_id)
% (in chronological order)
unpublished_commits := Ids :-
    StopIds = [~dst_id|StopIds0],
    ( NoPubId = ~get_dstgit_last_nopub_id -> StopIds0 = [NoPubId]
    ; StopIds0 = []
    ),
    Ids = ~git_commits_until(~srcgit, StopIds).

% ---------------------------------------------------------------------------
%! # Helper predicates

% Execute Goal under Dir
:- meta_predicate with_cwd(?, goal).
with_cwd(Dir, Goal) :-
    working_directory(OldDir, Dir),
    once_port_reify(Goal, Port),
    working_directory(_, OldDir),
    port_call(Port).

mkdir_p(Dir) :- process_call(path(mkdir), ['-p', Dir], []).
mvdir(Dir1,Dir2) :- process_call(path(mv), [Dir1, Dir2], []).
rmfiles(Files) :- process_call(path(rm), ['-f'|Files], []).
cpfile(From,To) :- process_call(path(cp), [From,To], []).

