:- module(get_ci_status, [], [fsyntax]).

:- use_module(library(process)).
:- use_module(library(pillow/json)).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(write), [writeq/1]).
:- use_module(library(system), [getenvstr/2]).

% ---------------------------------------------------------------------------

:- export(travis_status/0).
travis_status :-
    Accept = 'Accept: application/vnd.travis-ci.2+json',
    URL = 'https://api.travis-ci.org/repos/ciao-lang/ciao/builds',
    process_call(path(curl), ['-s', '-H', Accept, URL], [stdout(string(Str))]),
    JSON = ~string_to_json(Str),
    member(B, ~json_get(JSON, builds)),
    string(State) = ~json_get(B, state),
    !,
    ( Commits = ~json_get(JSON, commits),
      member(Commit, Commits),
        string(Author) = ~json_get(Commit, author_name),
        string(Email) = ~json_get(Commit, author_email),
        string(Date) = ~json_get(Commit, committed_at),
        string(SHA) = ~json_get(Commit, sha),
        format("~s ~s ~s <~s>~n", [SHA, Date, Author, Email]),
        fail
    ; true
    ),
%       writeq(Commits), nl,
    format("state: ~s~n", [State]).

% ---------------------------------------------------------------------------

:- export(gitlab_pipeline_status/0).
gitlab_pipeline_status :-
    Token = ~get_gitlab_api_private_token,
    git_check_repo,
    Remote = origin, % TODO: customize
    Project = ~gitlab_get_project(Remote),
    Branch = ~git_branch,
    SHA = ~git_sha(Remote, Branch),
    format("Status of last pipeline for ~s on ~w/~w (SHA:~s):~n", [Project, Remote, Branch, SHA]),
    JSON = ~curl_pipeline_status(Token, Project, SHA),
    ( string(Status) = ~json_get(~json_get(JSON, last_pipeline), status) ->
        true
    ; Status = "unknown?"
    ),
    format("~s~n", [Status]).

% :- pred get_gitlab_api_private_token(X) => atm(X).
get_gitlab_api_private_token := Token :-
    ( Token0 = ~getenvstr('GITLAB_API_PRIVATE_TOKEN') -> atom_codes(Token, Token0)
    ; format("ERROR: missing GITLAB_API_PRIVATE_TOKEN key~n"||
             "  Obtain yours from https://gitlab.software.imdea.org/profile/personal_access_tokens~n"||
             "  and include it in your environment (.bashrc, etc.) with:~n"||
             "    export GITLAB_API_PRIVATE_TOKEN=<KEY>~n"||
             "  and your .emacs.d/init.d with:~n"||
             "    (setenv \"GITLAB_API_PRIVATE_TOKEN\" \"<KEY>\"~n~n", []),
      fail
    ).

gitlab_get_project(Remote) := Project :-
    URL = ~git_get_url(Remote),
    ( append(_, ":"||URL0, URL),
      append(Project, ".git"||_, URL0) ->
        true
    ; format("ERROR: unknown URL: ~s~n", [URL])
    ).
    
url_encode([]) := [].
url_encode([0'/|Xs]) := "%2F"||(~url_encode(Xs)) :- !.
url_encode([X|Xs]) := [X| ~url_encode(Xs)].

curl_pipeline_status(Token, Project, SHA) := JSON :-
    PT = ~atom_concat('PRIVATE-TOKEN: ', Token),
    URL0 = ~append("https://gitlab.software.imdea.org/api/v4/projects/",
             ~append(~url_encode(Project),
               ~append("/repository/commits/", SHA))),
    atom_codes(URL, URL0),
    process_call(path(curl), ['-s', '--header', PT, URL], [stdout(string(Str))]),
    JSON = ~string_to_json(Str).

% ---------------------------------------------------------------------------

git_check_repo :-
    process_call(path(git), [status], [stdout(null), stderr(null), status(Status)]),
    ( Status = 0 -> true
    ; format("ERROR: current directory is not a git repository~n", []),
      fail
    ).

git_get_url(Remote) := URL :-
    process_call(path(git), [remote, 'get-url', Remote], [stdout(line(URL))]).
    
git_branch := Branch :-
    process_call(path(git), ['rev-parse', '--abbrev-ref', 'HEAD'], [stdout(line(Branch0))]),
    atom_codes(Branch, Branch0).

git_sha(Remote, Branch) := SHA :-
    RemoteBranch = ~atom_concat(Remote, ~atom_concat('/', Branch)),
    process_call(path(git), ['rev-parse', RemoteBranch], [stdout(line(SHA))]).
