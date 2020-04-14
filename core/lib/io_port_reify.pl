:- module(_, [], [assertions, regtypes, isomodes, hiord]).

:- doc(title, "Call goals with reified IO and (exit) ports").
:- doc(author, "Jose F. Morales").

:- doc(module, "Predicates to delay the execution of the (exit) port
   of a goal and capture the OS standard output/error streams
   (stdout,stderr).").

:- doc(bug, "An interface similar to @pred{process_call/3} would be
   simpler. Try to merge some code.").

:- use_module(library(system), [mktemp_in_tmp/2, fd_dup/2, fd_close/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(engine(stream_basic)).
:- use_module(library(stream_utils), [file_to_string/2]).
:- use_module(library(port_reify), [once_port_reify/2]).
:- use_module(library(io_alias_redirection), [set_stream/3]).

:- export(io_once_port_reify/3).
:- pred io_once_port_reify(Goal, Port, OutString)
    => (string(OutString))
    #"Executes @var{Goal}, @var{Port} is the state when the
      predicate finishes (true, fail). Its stdout is
      stored in @var{OutString}.".
:- meta_predicate io_once_port_reify(goal, ?, ?).
io_once_port_reify(Goal, Port, OutString) :-
    open_std_redirect(stdout, string(OutString), OutRedirect),
    once_port_reify(Goal, Port0),
    close_std_redirect(OutRedirect),
    Port = Port0.

:- export(io_once_port_reify/4).
:- pred io_once_port_reify(Goal, Port, OutString, ErrString)
    => (string(OutString), string(ErrString))
    #"Executes @var{Goal}, @var{Port} is the state when the
      predicate finishes (true, fail). Its stdout and stderr are
      stored in @var{OutString} and @var{ErrString} respectively.".
:- meta_predicate io_once_port_reify(goal, ?, ?, ?).
io_once_port_reify(Goal, Port, OutString, ErrString) :-
    open_std_redirect(stdout, string(OutString), OutRedirect),
    open_std_redirect(stderr, string(ErrString), ErrRedirect),
    once_port_reify(Goal, Port0),
    close_std_redirect(OutRedirect),
    close_std_redirect(ErrRedirect),
    Port = Port0.

% ---------------------------------------------------------------------------
% Generic channel redirections to standard file descriptors

% TODO: merge with process_channel.pl code

:- discontiguous open_std_redirect_/3.
:- discontiguous close_std_redirect_/1.

:- export(open_std_redirect/3).
open_std_redirect(Std, Channel, Redirect) :-
    open_std_redirect_(Channel, Std, Redirect).

:- export(close_std_redirect/1).
close_std_redirect(Redirect) :-
    close_std_redirect_(Redirect).

% ---------------------------------------------------------------------------
% Stream redirections
%
% Setting stdout/stderr restores the OS file descriptors and also
% resets streams for the corresponding aliases (user_output or
% user_error) so that they point to the default streams (with file
% descriptors 1 or 2).
%
% TODO: Additionally stdout resets the current output (e.g.,
% display(foo), display(user_output, foo)).

open_std_redirect_(stream(NewS), stdout, out_redirect(OldCurrS, OldS, SavedFD)) :-
    % reset default current output and user_output
    % TODO: is there a simpler way to do it? (JF)
    stream_code(StdS, 1),
    current_output(OldCurrS), set_output(StdS),
    set_stream(user_output, StdS, OldS),
    % redirect standard file descriptor to the new stream
    stream_code(NewS, NewFD), push_fd(1, NewFD, SavedFD).
open_std_redirect_(stream(NewS), stderr, err_redirect(OldS, SavedFD)) :-
    stream_code(StdS, 2),
    set_stream(user_error, StdS, OldS),
    % redirect standard file descriptor to the new stream
    stream_code(NewS, NewFD), push_fd(2, NewFD, SavedFD).

close_std_redirect_(out_redirect(OldCurrS, OldS, SavedFD)) :-
    pop_fd(1, SavedFD),
    set_output(OldCurrS),
    set_stream(user_output, OldS, _).
close_std_redirect_(err_redirect(OldS, SavedFD)) :-
    pop_fd(2, SavedFD),
    set_stream(user_error, OldS, _).

% ---------------------------------------------------------------------------
% File redirections

open_std_redirect_(file(File), Std, file_redirect(S, Redirect)) :-
    open(File, write, S),
    open_std_redirect(Std, stream(S), Redirect).

close_std_redirect_(file_redirect(S, Redirect)) :-
    close_std_redirect(Redirect),
    close(S).

% ---------------------------------------------------------------------------
% String redirections

open_std_redirect_(string(String), Std, string_redirect(String, File, Redirect)) :-
    mktemp_in_tmp('tmpXXXXXX', File),
    open_std_redirect(Std, file(File), Redirect).

close_std_redirect_(string_redirect(String, File, Redirect)) :-
    close_std_redirect(Redirect),
    file_to_string(File, String),
    del_file_nofail(File).

% ---------------------------------------------------------------------------
% Stdout redirection (only for stderr)

open_std_redirect_(stdout, stderr, stdout_redirect(Redirect)) :-
    stream_code(StdOut, 1),
    open_std_redirect(stderr, stream(StdOut), Redirect).

close_std_redirect_(stdout_redirect(Redirect)) :-
    close_std_redirect(Redirect).

% ---------------------------------------------------------------------------
% Auxiliary FD operations

% Save and redirect FD to NewFD
push_fd(FD, NewFD, SavedFD) :-
    fd_flush(FD),
    fd_dup(FD, SavedFD),
    fd_dup(NewFD, FD).

% Restore PD to saved descriptor (which is closed)
pop_fd(FD, SavedFD) :-
    fd_flush(FD),
    fd_dup(SavedFD, FD),
    fd_close(SavedFD).

fd_flush(FD) :- stream_code(S, FD), flush_output(S).
