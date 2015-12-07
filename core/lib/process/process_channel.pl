:- module(_, [], [assertions, regtypes, isomodes, hiord, dcg]).

:- doc(title, "Process Channels").

:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides an abstraction for communication
   channels between processes (@lib{process}) based on standard
   input/output/error streams.").

:- doc(bug, "(feature) Generalize channel marshalling").
%
%   - IO format (decode/encode), merge with library(remote)
%   - merge with optim_comp superfast term IO and bytecode serialization?
%   - merge with getopts (for argument serialization)
%

:- doc(bug, "(feature) Asynchronous data transfer is not really
   asynchronous").

% ---------------------------------------------------------------------------

:- use_module(library(lists), [append/3]).
:- use_module(library(port_reify)).

:- use_module(library(system),
	[file_exists/1,
	 mktemp_in_tmp/2,
	 delete_file/1]).
:- use_module(library(read), [read_term/3]).
:- use_module(library(write), [write/1, write_canonical/2]).
:- use_module(library(strings), [write_string/2, get_line/2]).
:- use_module(library(file_utils), [stream_to_string/2]).

% ===========================================================================

:- doc(section, "Channel bindings (for sync/async data transfer)").
% TODO: async transfer is not really async

:- export(process_channel/1).
:- regtype process_channel(Channel) # "A communication channel for
   standard file descriptors".
:- doc(process_channel/1, "
   @begin{description}
   @item @tt{default}: the default descriptor
   @item @tt{null}: the null stream (@tt{/dev/null} file in POSIX, @tt{nul} in Windows)
   @item @tt{pipe(-Stream)}: a pipe (@var{Stream} unified with the
     parent's read/write end of the pipe)
   @item @tt{file(+File)}: a file @var{File}
   @item @tt{stream(+Stream)}: a stream @var{Stream} (must be opened
     with the right mode)
   @item @tt{file_append(+File)}: a file @var{File}, where writes are
     appended to the end
   @item @tt{string(?String)}: a string (list of codes)
   @item @tt{line(?Line)}: a string, ignoring trailing new line character
   @item @tt{atmlist(?Xs)}: a list of atoms (for each line)
   @item @tt{terms(?Xs)}: terms, ended in full-stop (using
     @pred{read_term/3} and @pred{write_canonical/2})
   @item @tt{stdout}: (only valid for @tt{stderr(_)}) redirect to the same
     channel than stdout (useful for redirecting both standard output
     and standard error).
   @end{description}
").
process_channel(default).
process_channel(null).
process_channel(pipe(Stream)) :- stream(Stream).
process_channel(stream(Stream)) :- stream(Stream).
process_channel(file(_File)).
process_channel(file_append(_File)).
process_channel(string(String)) :- string(String).
process_channel(line(Line)) :- string(Line).
process_channel(atmlist(Xs)) :- list(atm, Xs).
process_channel(terms(Xs)) :- list(Xs).
process_channel(stdout).

% A channel that read or written as a term.
% TODO: rename by 'memory channel'? (vs filesystem channel)
term_channel(string(_)).
term_channel(line(_)).
term_channel(atmlist(_)).
term_channel(terms(_)).

% ---------------------------------------------------------------------------

:- export(channel_bindings/2).
:- pred channel_bindings(Channels, ChannelBinds) 
   # "Create channel bindings (taking into account pipes)".
channel_bindings(Channels, ChannelBinds) :-
	count_pipes(Channels, Pipes0),
	channel_bindings_(Channels, ChannelBinds, Pipes0, _Pipes).

channel_bindings_([], [], Pipes, Pipes).
channel_bindings_([Channel|Channels], [ChannelBind|ChannelBinds], Pipes0, Pipes) :-
	channel_binding(Channel, ChannelBind, Pipes0, PipesN),
	Pipes1 is Pipes0 + PipesN,
	channel_bindings_(Channels, ChannelBinds, Pipes1, Pipes).

% Pipes is the number of pipe(_) channels in Channels
count_pipes(Channels, Pipes) :-
	count_pipes_(Channels, 0, Pipes).

count_pipes_([], Pipes, Pipes).
count_pipes_([Channel|Channels], Pipes0, Pipes) :- Channel = pipe(_), !,
	Pipes1 is Pipes0 + 1,
	count_pipes_(Channels, Pipes1, Pipes).
count_pipes_([_|Channels], Pipes0, Pipes) :-
	count_pipes_(Channels, Pipes0, Pipes).

% ---------------------------------------------------------------------------

% NOTE: Files or terms are 'atomic data', that can be read or
%   written as a whole. Pipes are message queues for bytes,
%   and only send/receive operations are meaningful.
% 
%   Files or terms can be transmited using pipes (e.g., for
%   efficiency); terms can be written to temporary files; but
%   pipes (when used as message queues) CANNOT be written to
%   temporary files without introducing changes in semantics.

% Create an appropriate transmission channel for each user channel.
% Temporary files may be used to avoid deadlocks (see the
% corresponding bug).
%
% The pair of channels is called a channel binding, described as:
%
%   '$binding'(Channel, Channel2, Tmp, TrMode, Res) where
%   
%     Channel: original channel
%     Channel2: target channel
%     Tmp: Channel2 is a temporary file (that must be deleted)
%     Res: reified exit port of transmission (once_port_reify/2)
%
%     TrMode: automatic transmission mode (w.r.t. the parent process)
%
%       @begin{itemize}
%       @item @tt{TrMode=async}: asynchronous transmission (pipe-based)
%         (send) after the child process starts
%         (receive) before the child process terminates
%       @item @tt{TrMode=sync}: synchronous transmission (file-based)
%         (send) before the child process starts
%         (receive) after the child process terminates
%       @item @tt{TrMode=none}: no automatic transmission from the
%         parent process
%       @end{itemize}

channel_binding(Channel, ChannelB, CurrPipes, PipesN) :-
	term_channel(Channel),
	!,
	( CurrPipes = 0 -> % no pipe is being used yet
	    % Select a pipe channel
	    Channel2 = pipe(_),
	    Tmp = no,
	    ChannelB = '$binding'(Channel, Channel2, Tmp, async, _Res),
	    PipesN = 1
	; % Use a temporary file
	  % TODO: check umask?
	  mktemp_in_tmp('ciao-channel-XXXXXX', File), Tmp = yes,
	  Channel2 = file(File),
	  ChannelB = '$binding'(Channel, Channel2, Tmp, sync, _Res),
	  PipesN = 0
	).
channel_binding(Channel, ChannelB, _CurrPipes, 0) :-
	Res = success, % (no transfer)
	ChannelB = '$binding'(Channel, Channel, no, none, Res).

:- export(cleanup_binding/1).
:- pred cleanup_binding(ChannelB)
   # "Cleanup temporaries due to channel file-based bindings.".
cleanup_binding('$binding'(_, file(File), yes, _, _)) :- !,
	delete_file(File).
cleanup_binding(_).

:- export(binding_port_call/1).
:- pred binding_port_call(ChannelB)
   # "Do port_call/1 on the result of channel transfer (send or receive).".
binding_port_call('$binding'(_Channel, _Channel2, _Tmp, _TrMode, Res)) :-
	port_call(Res).

% ---------------------------------------------------------------------------

:- export(send_input/2).
:- pred send_input(Mode, ChannelBinding)
   # "Send input through channel binding
     @var{ChannelBinding}. Transfer status is internally stored (see
     @pred{binding_port_call/1}).".

send_input(Mode, '$binding'(Channel, Channel2, _, Mode, Res)) :- !,
	( Channel2 = pipe(Stream) ->
	    once_port_reify(write_channel(Stream, Channel), Res)
	; Channel2 = file(File) ->
	    once_port_reify(write_channel_to_file(File, Channel), Res)
	; Res = success
	).
send_input(_, _).

:- export(receive_output/2).
:- pred receive_output(Mode, ChannelBinding)
   # "Receive output from channel binding
     @var{ChannelBinding}. Transfer status is internally stored (see
     @pred{binding_port_call/1}).".

receive_output(Mode, '$binding'(Channel, Channel2, _, Mode, Res)) :- !,
	( Channel2 = pipe(Stream) ->
	    once_port_reify(read_channel(Stream, Channel), Res)
	; Channel2 = file(File) ->
	    once_port_reify(read_channel_from_file(File, Channel), Res)
	; Res = success
	).
receive_output(_, _).

% ---------------------------------------------------------------------------

:- pred read_channel(Stream, Channel) 
   # "Read channel @var{Channel} contents from a stream @var{Stream},
      atomically.

      This predicate will throw an exception if channel data cannot be
      parsed correctly. If the channel term cannot be unified, it will
      fail. In any case, the predicate will ensure that all available
      data of the stream has been read and that the stream is closed
      afterwards (independently on the result).".

read_channel(Stream, Channel) :-
	catch(read_channel_(Channel, Stream), E, read_exception(Stream, E)).

read_exception(Stream, E) :-
	discard_stream_data(Stream),
	throw(E).

% Note: ensure that all stream codes have been read.
read_channel_(string(Term), Stream) :- !,
	stream_to_string(Stream, Term0),
	% close(Stream), (closed in stream_to_string)
	Term = Term0.
read_channel_(line(Term), Stream) :- !,
	stream_to_string(Stream, String0),
	% close(Stream), (closed in stream_to_string)
	no_tr_nl(String0, Term).
read_channel_(Channel, Stream) :-
	( read_channel__(Channel, Stream),
	  close(Stream) ->
	    true
	; discard_stream_data(Stream),
	  fail
	).

% (May throw parsing errors)
read_channel__(atmlist(Term), Stream) :- !,
	read_lines(Stream, Term).
read_channel__(terms(Term), Stream) :- !,
	read_terms(Stream, Term).
read_channel__(Channel, _Stream) :-
	throw(error(bad_channel(Channel), read_channel/2)).

% Read the rest of the stream code and close the stream
% TODO: this can be done more efficiently
discard_stream_data(Stream) :-
	\+ \+ stream_to_string(Stream, _).
	% close(Stream). (closed in stream_to_string)

:- pred read_channel_from_file(File, Channel) 
   # "Read the contents of channel @var{Channel} from file @var{File}".
read_channel_from_file(File, Channel) :-
	open(File, read, Stream),
	read_channel(Stream, Channel).

:- pred write_channel(Stream, Channel)
   # "Write the contents of channel @var{Channel} to stream @var{Stream}".
write_channel(Stream, Channel) :-
	( write_channel_(Channel, Stream) ->
	    close(Stream)
	; close(Stream),
	  fail
	).

write_channel_(string(Term), Stream) :- !,
	write_string(Stream, Term).
write_channel_(line(Term), Stream) :- !,
	write_string(Stream, Term),
	nl(Stream).
write_channel_(atmlist(Term), Stream) :- !,
	write_lines(Stream, Term).
write_channel_(terms(Term), Stream) :- !,
	write_terms(Stream, Term).
write_channel_(Channel, _) :-
	throw(error(bad_channel(Channel), write_channel/2)).

:- pred write_channel_to_file(File, Channel) 
   # "Write the contents of channel @var{Channel} to file @var{File}".
write_channel_to_file(File, Channel) :-
	open(File, write, Stream),
	write_channel(Stream, Channel).

% ---------------------------------------------------------------------------

no_tr_nl(L, NL) :-
	append(NL, [0'\n], L),
	!.
no_tr_nl(L, L).

% read each line as individual atoms
read_lines(Stream, Xs) :-
	get_line(Stream, L),
	!,
	( L = end_of_file ->
	    Xs = []
	; atom_codes(X, L),
	  Xs = [X|Xs0],
	  read_lines(Stream, Xs0)
	).
read_lines(_, []).

% write individual atoms as lines
write_lines(_, []) :- !.
write_lines(Stream, [X|Xs0]) :-
	display(Stream, X), nl(Stream),
	write_lines(Stream, Xs0).

% read terms (ended in full-stop)
read_terms(Stream, Xs) :-
	read_term(Stream, X, []),
	!,
	( X = end_of_file ->
	    Xs = []
	; Xs = [X|Xs0],
	  read_terms(Stream, Xs0)
	).
read_terms(_, []).

% write terms (ended in full-stop)
write_terms(_, []) :- !.
write_terms(Stream, [X|Xs0]) :-
	write_canonical(Stream, X), display(Stream, ' .'), nl(Stream),
	write_terms(Stream, Xs0).

% ---------------------------------------------------------------------------

:- export(open_redirect/3).
:- pred open_redirect(ChannelB, Mode, S)
   # "Open stream file redirections (for internals:'$exec'/9).".
%   - @var{Mode} is the open mode
%   - @var{S} is the redirection for internals:'$exec'/9 (see
%     os_utils.c for details)

open_redirect('$binding'(_Channel, Channel2, _Tmp, _TrMode, _Res), Mode, S) :-
	open_redirect_(Channel2, Mode, S).

open_redirect_(file(File), read, S) :- !,
	open(File, read, S).
open_redirect_(file(File), write, S) :- !,
	open(File, write, S).
open_redirect_(file_append(File), write, S) :- !,
	open(File, append, S).
open_redirect_(pipe(Stream), _, S) :- !, S = Stream.
open_redirect_(stream(Stream), _, S) :- !, S = Stream.
open_redirect_(stdout, _, S) :- !, S = stdout.
open_redirect_(_, _, []).

:- export(close_redirect/2).
:- pred close_redirect(ChannelB, S)
   # "Close stream file redirections (for internals:'$exec'/9).".
close_redirect('$binding'(_Channel, Channel2, _Tmp, _TrMode, _Res), S) :-
	( Channel2 = file(_)
	; Channel2 = file_append(_)
	),
	!,
	close(S).
close_redirect(_, _).



