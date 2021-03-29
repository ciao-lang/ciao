:- module(_, [], [assertions, doccomments, fsyntax, datafacts]).

%! \title Ciao publish from mono-repository tool
%  \author Jose F. Morales
%
%  \module
%  This is a tool to publish pristine views of an "annotated" git
%  mono-repository.
%  
%  An annotated git repository contains NODISTRIBUTE empty files that mark
%  directories that must not be visible in a pristine copy.
%  
%  This script does not need a checkout (it works directly on the git
%  repository).
%  
%  This script is tailored for Ciao but it could be easily generalized:
%   - assumes the `ciao-devel` structure
%   - keeps clones of publishing repos at `$HOME/REPOS-ciao-publish`
%  
%  ## Publishing code at Github
%  
%  Use `ciao-publish list` to enumerate publishable bundles from the
%  Ciao devel repository (either the main `ciao` or some bundles at
%  `bndls/`). E.g.:
%  ```
%  $ ciao-publish list
%  ```
%  
%  Publishing code is performed by the `publish` and `push`
%  operations. Use the `help` option to view other possible
%  commmands. Use the `--bundle BNDL` option to select the bundle to
%  publish (`ciao` by default).
%  
%  Example for publishing `ciao` (the special root bundle that
%  contains containing `core`, `builder`, etc.):
%  ```
%  $ ciao-publish pull
%  $ ciao-publish publish
%  << (please review commits) >>
%  $ ciao-publish push
%  ```
%  
%  Example for publishing `ciao_emacs`:
%  ```
%  $ ciao-publish --bundle ciao_emacs pull
%  $ ciao-publish --bundle ciao_emacs publish
%  << (please review commits) >>
%  $ ciao-publish --bundle ciao_emacs push
%  ```
%  
%  The first time that `publish` is performed, it may complain with a `sed:
%  first RE may not be empty` error. Fix it by specifying a first commit
%  number, or do `squash` instead of `publish`.
%  
%  ## Working on multiple bundles
%  
%  You can use the `--all` option to perform a command on all publishable
%  bundles (see `--help` for more info), or use shell scripts.
%  
%  Examples on all bundles:
%  ```
%  << Consult the status of all publishable bundles >>
%  $ ciao-publish --all status
%  
%  << Pull all public repositories (previous to publishing) >>
%  $ ciao-publish --all pull
%  
%  << Publish all publishable bundles >>
%  $ ciao-publish --all publish
%  
%  << Push to the public repository >>
%  $ ciao-publish --all push
%  ```
%  
%  Examples of scripts:
%  ```
%  << Consult the status of all publishable bundles >>
%  $ for i in `ciao-publish list`; do ciao-publish --bundle $i status; echo; done
%  
%  << Pull all public repositories (previous to publishing) >>
%  $ for i in `ciao-publish list`; do ciao-publish --bundle $i pull; done
%  
%  << Publish all publishable bundles >>
%  $ for i in `ciao-publish list`; do echo "PUBLISH $i? (C-c to abort)"; read; ciao-publish --bundle $i publish; done
%  
%  << Push to the public repository >>
%  $ for i in `ciao-publish list`; do ciao-publish --bundle $i push; done
%  ```
%  
%  ## Additional files for publishing
%  
%  Scripts for CI are located at:
%  ```
%  ci-scripts/
%  ```
%  
%  Scripts for checking the status of CI are located at:
%  ```
%  ci-status/
%  ```
%  
%  ## Consulting statistics
%  
%  For download statistics:
%  ```
%  $ ./ciao-download-stats.sh
%  ```
%  
%  ## Contribution from public repositories
%  
%  Assume that Bob maintains the private Ciao repository and Alice wants
%  to contribute to it. This is the typical workflow for contributing to
%  the project preserving authorship:
%  
%    - Alice: clone public Git repository (`github.com/ciao-lang/ciao`)
%    - Alice: make some changes and commit them
%    - Alice: format patches using `git format-patch` (never push them!)
%  ```
%  $ git format-patch HEAD^n # where n is the number of commits not in master
%  ```
%  
%    - Alice: send the `????-*.patch` files via email to Bob
%    - Bob: apply back the changes in the private repository with
%  
%  ```
%  $ git am < 0001-*.patch
%  $ git am < 0002-*.patch
%  $ ...
%  ```
%  
%    - Bob: check with `git log` that the code is pushed correctly
%    - Bob: do `ciao-publish pull`, `ciao-publish publish`,
%      and `ciao-publish push`
%  
%    - Alice: do `git pull` and `git rebase -i`. The first command will
%      create a merge commit. The second command will break. After some
%      `git rebase --continue` it will end. If everything is fine there
%      will be no unpushed changes to master and all new commits will be
%      authored as Alice.
%  
%  ## Pointers to code and distribution files
%  
%  Source repositories:
%  
%    - **Local repositories** at gitlab: @href{https://gitlab.software.imdea.org/ciao-lang}
%    - **Public repositories** at github: @href{https://github.com/ciao-lang}
%  
%  Binaries:
%  
%    - **Continuous integration** on Travis-CI and AppVeyor:
%       - @href{https://github.com/ciao-lang/ciao}
%       - @href{https://github.com/ciao-lang/ciao.CD}
%       - @href{https://ci.appveyor.com/project/jfmc/ciao}
%       - @href{https://ci.appveyor.com/project/jfmc/ciao.CD}
%  
%    - **Binaries** at Bintray: [log in with your Github account]:
%       - @href{https://bintray.com/ciao-lang/builds/ciao}
%       - @href{https://dl.bintray.com/ciao-lang/builds}
%  
%    - **Homebrew** formula (macOS): @href{https://github.com/ciao-lang/homebrew-ciao}
%  
%    - **Docker** image (with pointers to docker hub account): @href{https://github.com/ciao-lang/docker-ciao}
%  
%  Other binaries (not ready):
%        
%    - **Launchpad (Ubuntu)** account for Ciao team: @href{https://launchpad.net/~ciao-lang}
%      [Created with `jfmc` user, ask for membership]

% ===========================================================================

show_help :-
    write_string("Usage: ciao-publish [OPTS] CMD

Publish the current git repository through another (public) repository.

Options:

  --bundle Bundle
               Select bundle Bundle. Use 'ciao' as the special root
               bundle (containing core, builder, etc.)
  --all            Select all bundles
  --pubrepos DIR   Select directory containing the publishing repository
               clones (~/REPOS-ciao-publish by default)
  --dry        Do not commit (only for 'squash')

Available commands:

  help             Show this message
  list             Show publishable bundles
  init             Create the remote repository for publishing
  info             Show publishing info about a bundle
  pull             Pull (public repo)
  publish [Id]     Update (git add -A) from the latest or given commit
                   (for each unpublished commit)
  squash [Id]      Like publish, but squash into a single commit
                   (more efficient than rebase but loses history)
                   If '--dry', the tree is left in a temporary dir
                   (no commit is done) 
  status           Show status (unpublished commits)
  push             Push (public repo)
  rebase [args]    Rebase (public repo)

Use the --bundle option to select a bundle. If not specified, the
bundle is automatically detected from the working directory (this
assumes that we have an updated checkout).

Use --all to work on all publishable bundles (only for pull, publish,
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

:- use_module(.(git_extra)).

:- export(main/1).
main([H]) :-
    ( H = help ; H = '-h' ; H = '--help' ),
    !,
    show_help.
main(Args) :-
    parse_opts(Args, Args1, Opts),
    Args1 = [Cmd|Args2],
    !,
    %
    run_cmd(Cmd, Opts, Args2).
main(_) :-
    message(error, [
        'Invalid usage, see help message']),
    halt(1).

parse_opts(['--bundle', Bundle|Args], Args2, [srcbundle(Bundle)|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(['--all'|Args], Args2, [allbundles|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(['--dry'|Args], Args2, [dryrun|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(['--pubrepos', Dir|Args], Args2, [pubrepos(Dir)|Opts]) :- !,
    parse_opts(Args, Args2, Opts).
parse_opts(Args, Args, []).

% ---------------------------------------------------------------------------
%! # Run command

:- data dryrun/0.

run_cmd(list, _, _) :- !, % show publishable bundles
    ( % (failure-driven loop)
      list_bundles(B),
        display(B), nl,
        fail
    ; true
    ).
run_cmd(Cmd, Opts, Args2) :-
    ( member(allbundles, Opts) ->
        AllBundles = yes
    ; AllBundles = no
    ),
    % Run command
    retractall_fact(dryrun),
    ( member(dryrun, Opts) -> set_fact(dryrun) ; true ),
    ( AllBundles = yes ->
        run_cmd_all(Cmd, Opts, Args2)
    ; ( member(srcbundle(Bundle), Opts) ->
          SrcBundle = Bundle
      ; SrcBundle = ~detect_bundle
      ),
      run_cmd_one(Cmd, SrcBundle, Opts, Args2)
    ).

% Run on all bundles
run_cmd_all(Cmd, Opts, Args) :-
    ( cmd_allow_all(Cmd) -> true
    ; message(error, ['Invalid command \'', Cmd, '\' with \'--all\', see help message']),
      halt(1)
    ),
    \+ (list_bundles(B),
        \+ run_cmd_one(Cmd, B, Opts, Args)).

% Run on a single bundle
run_cmd_one(Cmd, SrcBundle, Opts, Args) :-
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
    cmd_run(Cmd, Args),
    cleanup_tmpdir.

:- discontiguous(cmd_allow_all/1).
:- discontiguous(cmd_needs_check_repos/1).
:- discontiguous(cmd_run/2).

% ---------------------------------------------------------------------------
%! # Info for publishing

cmd_run(info, _Args) :- !, config_summary.

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

cmd_allow_all(status).
cmd_needs_check_repos(status).
cmd_run(status, _Args) :- !, run_status.

run_status :-
    Id = ~git_latest_commit(~srcgit),
    DstId = ~dst_id,
    lformat([
        '  Latest commit: ', Id, '\n',
        '  Commit at dstgit: ', DstId, '\n'
    ]),
    ( NoPubId = ~get_dstgit_last_nopub_id ->
        lformat(['  Last known commit without public changes: ', NoPubId, '\n'])
    ; true
    ),
    check_dst_id, % (needed for unpublished commits)
    UIds = ~unpublished_commits,
    ( UIds = [FirstId|_] ->
        length(UIds, UIdsN),
        lformat(['  ', UIdsN, ' unpublished commits since: ', FirstId, '\n'])
    ; lformat(['  No unpublished commits\n'])
    ).

% ---------------------------------------------------------------------------
%! # Update dst repo

:- data did_commit/0.

%! ## Publish
cmd_allow_all(publish).
cmd_needs_check_repos(publish).
cmd_run(publish, Args) :- !, run_publish_commits(Args).

run_publish_commits(Args) :-
    retractall_fact(did_commit),
    ( Args = [Id0] -> TargetId = Id0
    ; TargetId = ~git_latest_commit(~srcgit)
    ),
    check_dst_id, % (needed for unpublished commits)
    ( % (failure-driven loop)
      UIds = ~unpublished_commits,
      member(UId, UIds),
        Fs = ~git_public_files(~srcgit, ~srcsubdir, UId),
        Info = ~git_commit_info(~srcgit, UId),
        prepare_tree(UId, Fs),
        update_tree(UId, Info),
        ( UId = TargetId ->
            ! % (break)
        ; fail % (loop)
        )
    ; true
    ),
    ( did_commit -> after_commit_help ; true ).

%! ## Squash
cmd_needs_check_repos(squash).
cmd_run(squash, Args) :- !, run_squash_commit(Args).

run_squash_commit(Args) :-
    retractall_fact(did_commit),
    ( Args = [Id0] -> UId = Id0
    ; UId = ~git_latest_commit(~srcgit)
    ),
    Fs = ~git_public_files(~srcgit, ~srcsubdir, UId),
    squash_commit_info(UId, Info),
    prepare_tree(UId, Fs),
    ( dryrun ->
        keep_treedir
    ; update_tree(UId, Info)
    ),
    ( did_commit -> after_commit_help ; true ).

% Commit info for squash % TODO: this can be improved
squash_commit_info(UId, Info) :-
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

% ---------------------------------------------------------------------------
%! # Initialization of dst repo

cmd_run(init, _Args) :- !, run_init_dstgit.

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
            'Then try again the \'init\' command.\n',
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
cmd_allow_all(pull).
cmd_needs_check_repos(pull).
cmd_run(pull, _Args) :- !, run_pull_dstgit.

% Pull to make sure that dstgit contains latest commits
run_pull_dstgit :-
    git_cmd_atwd_q(~dstgit, ['pull']).

%! ## Push
cmd_allow_all(push).
cmd_needs_check_repos(push).
cmd_run(push, _Args) :- !, run_push_dstgit.

run_push_dstgit :-
    % TODO: make check for pending commits optional? (it is faster
    % than simply 'git push' but assumes remote is OK)
    git_cmd_atwd(~dstgit, ['log', 'origin/master..master'], [stdout(string(Out))]),
    ( Out = "" -> % No pending commits
        lformat(['No remaining commits to push\n'])
    ; git_cmd_atwd_q(~dstgit, ['push']),
      after_push_help 
    ).

%! ## Rebase
cmd_needs_check_repos(rebase).
cmd_run(rebase, Args) :- !, run_rebase_dstgit(Args).

run_rebase_dstgit(Args) :-
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
        'Use the \'squash\' command to publish the current version, or select the\n',
        'first commit with \'squash Id\'. Amending the commit message is\n',
        'recommended.\n',
        '\n']).

% Help after publish
after_commit_help :-
    lformat([
        % ___________________________________________________________________________
        '\n',
        'Use \'rebase -i\' rebase commits if needed.\n',
        'Use \'push\' to send commits to remote.\n']).

% Help after push
% TODO: configure per bundle
after_push_help :- ~srcbundle = ciao, !,
    lformat([
        % ___________________________________________________________________________
        '\n',
        'NOTES:\n',
        '\n',
        '  - Make sure that Travis-CI and AppVeyor builds the source correctly\n',
        '    (go to https://github.com/ciao-lang/ciao)\n',
        '\n',
        '  - Remember to trigger build at https://github.com/ciao-lang/docker-ciao\n',
        '\n',
        '    Output: https://hub.docker.com/r/ciaolang/ciao/\n',
        '\n',
        '  - Remember to trigger build at https://github.com/ciao-lang/ciao.CD\n',
        '\n',
        '    Output: https://bintray.com/ciao-lang/builds/ciao/latest/view\n',
        '\n',
        '  - Remember to tag and push tags (if needed):\n',
        '    E.g.,\n',
        '      cd ', ~path_dirname(~dstgit), '\n',
        '      git tag vMAJOR.MINOR.PATCH\n',
        '      git push origin vMAJOR.MINOR.PATCH\n',
        '\n',
        '  - Remember to change default_vers_bin=MAJOR.MINOR.PATCH in ciao-boot.sh\n',
        '\n']).
after_push_help.

% ---------------------------------------------------------------------------
%! # Checkout a tree

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
    % lformat(['Adding commit ', Id, '\n']),
    % First try a dry run
    ( \+ git_add_tree_has_changes(~dstgit) ->
        lformat(['Commit ', Id, ' does not contain public changes\n']),
        % Cache Id as the latest commit without public changes
        string_to_file(~atom_codes(Id), ~dstgit_last_nopub_id)
    ; lformat(['Commit ', Id, ' contains public changes\n']),
      git_add_tree(~dstgit),
      git_commit_annot_src_id(~dstgit, Id, Info),
      % Mark that we did a commit
      ( did_commit -> true ; assertz_fact(did_commit) )
    ).

% ---------------------------------------------------------------------------
%! # Patch a tree before publishing

patch_tree :-
    with_cwd(~treedir, patch_tree_).

patch_tree_ :-
    % Patch github specific (depends on bundle)
    ( ~srcbundle = 'ciao' ->
        rmfiles(['COPYRIGHT', 'LGPL', 'GPL']), % Outdated...
        patch_readme,
        patch_ci_scripts
    ; % Remove ACTIVATE mark
      ( file_exists('ACTIVATE') ->
          rmfiles(['ACTIVATE'])
      ; true
      )
    ),
    patch_license.

patch_readme :-
    ( file_exists('README.md') ->
        file_to_string('README.md', PrevText)
    ; PrevText = ""
    ),
    append(
        "[![Build Status](https://travis-ci.org/ciao-lang/ciao.svg)](https://travis-ci.org/ciao-lang/ciao)\n"||
        "[![Build Status](https://ci.appveyor.com/api/projects/status/fu2eb23je22xc228?svg=true)](https://ci.appveyor.com/project/jfmc/ciao)\n"||
        "\n",
        Text0, Text),
    append(PrevText, Text1, Text0),
    % Text1 = ("\n"||
    %   "---\n"||
    %   "**NOTE**: Repository automatically projected from the Ciao monorepo.\n"),
    Text1 = "",
    string_to_file(Text, 'README.md').

patch_ci_scripts :-
    bundle_path('builder', 'distro/ci-scripts', Dir),
    cpfile(~path_concat(Dir, 'travis.yml'), '.travis.yml'),
    cpfile(~path_concat(Dir, 'appveyor.yml'), '.appveyor.yml').

patch_license :-
    bundle_path('builder', 'distro/licenses', Dir),
    License = ~srclicense,
    ( License = 'LGPL' -> cpfile(~path_concat(Dir, 'LGPL'), 'LGPL')
    ; License = 'GPL' ->  cpfile(~path_concat(Dir, 'GPL'), 'GPL')
    ; true % none
    ).

% ---------------------------------------------------------------------------
%! # Bundles

%:- use_module(engine(runtime_control), [statistics/0]).
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
    config_short_summary.

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

check_dst_id :-
    ( DstId = ~dst_id, \+ DstId = '' ->
        true
    ; lformat([
          % ___________________________________________________________________________
          'ERROR: Could not find any \'Src-commit\' mark in the publishing repository.\n',
          'Please specify the first commit Id in \'publish\' or \'squash\'.\n'
      ]),
      halt(1)
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

