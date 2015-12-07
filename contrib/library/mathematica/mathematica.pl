:-  module(mathematica,
	[evaluate/2, close_link/0],
	[foreign_interface, define_flag]).

:- doc(author, "Remy Haemmerle").
:- doc(author, "Maximiliano Klemen").

:- doc(title, "Simple Interface to Mathematica").
:- doc(module, 
	"This library allows communication with Mathematica."
      ).

:- use_module(library(lists), [append/3]).

% Uncomment to use the C interface (deprecated).
%:- compilation_fact(mathematica__use_c_infertace).

:- include(library(mathematica/mathematica_decl_auto)).

:- pred evaluate(Query, Result) : string(Query)	=> string(Result)
   # "Sends expression @var{Query} to Mathematica and returns the
      response in @var{Result}.".
 
:- doc(evaluate/2, "
@begin{alert}
For the sake of efficiency, the first call to @pred{evaluate/2}
starts an instance of the Mathematica kernel in the background and keeps it
alive. This instance will be used for subsequent calls to  @pred{evaluate/2}.
The instance can be explicitly killed using @pred{close_link/0}.
@end{alert}
").

:- pred close_link # "Closes any instance of the Mathematica kernel
	running in the background.".

% ----------------------------------------------------------
% communication through C interface

:- if(defined(mathematica__use_c_infertace)).

:- true pred mathematica_init(in(Kernel),go(Status)) :: (atm*int)
	+ (foreign(mathematica_init), returns(Status)).

:- true pred evaluateInMathematica(in(Query), go(Result)) :: (string * string)
	+ (foreign(evaluateInMathematica), returns(Result)).

:- true pred close_link + (foreign(closelink)).
 
evaluate(Query,Result):-
	    mathematica_kernel(Kernel),
	    (
		(mathematica_init(Kernel,S),S=0)
	    -> 
		evaluateInMathematica(Query,Result)
	    ;
		error_code(S,E),
		throw(error(E, evaluate/2))
	    ).

error_code(1,mathematica_initialization).
error_code(2,mathematica_opening_link).
error_code(3,mathematica_broken_link).

:- use_foreign_source(library(mathematica/mathematica_conn)).

:- else.

% ----------------------------------------------------------
% communication through pipe

:- use_package(dcg).

:- use_module(library(llists), [append/2]).
:- use_module(library(process), [process_call/3, process_join/1, process_send_signal/2]).
:- use_module(library(strings), [get_line/2, write_string/2]).
:- use_module(library(system), [time/1]).
:- use_module(library(terms), [atom_concat/2]).

:- data kernel_handler_fact/5.

% Uncomment the following line to activate low level tracing.
% :- compilation_fact(low_level_mathematica_debug).

:- if(defined(low_level_mathematica_debug)).

:- use_module(library(messages), [debug_message/1]).
:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
issue_debug_messages(mathematica).

:- endif.

:- doc(define_flag/3,"Defines a flag @term{mathematica_kernel} as follows:
        @includedef{define_flag/3}
        (See @ref{Changing system behaviour and various flags}).

         The flag specifies the URL of the Mathematica kernel executable. 
         This URL is an atom that can be either:
@begin{itemize} 

@item The empty atom. This value means that the Mathematica kernel is not
   defined.  When the flag is set to such a value, any call to evaluate/2
   throws a @throw{mathemtica_kernel_path_undefined} error.

@item A standard file name path. 
   This value specifies the absolute path of the Mathematica kernel
   that will be executed locally.  

@item An ssh URL. This specifies the remote url of the Mathematica
   kernel that will be executed remotely using ssh. The ssh URL must be of
   the form:
@begin{verbatim}
@tt{ssh://[User@@]HostName[:Port]/Path}
@end{verbatim}

   where @tt{Host} is the host name, @tt{Path} is the absolute path of
   the Mathematica kernel on the remote host, @tt{User} is the
   optional user name, and @tt{Port} is the optional port to connect
   on the remote host.  
@end{itemize}

@begin{alert}
Note that the mathematica kernel must be closed (using
@pred{close_link/0}) prior to any change of the
@term{mathematica_kernel} flag.
@end{alert}
 ").

define_flag(mathematica_kernel, atom, Path):-
	mathematica_kernel_path(Path).

kernel_handler(Process, Input, Output, Loc):-
	current_fact(kernel_handler_fact(Process, Input_, Output_, Type, StartingTime)), !,
	( Type = ssh, time(Time), Time > StartingTime + 600 ->
	    close_link, kernel_handler(Process, Input, Output, Loc)
	;
	    Input = Input_, Output = Output_
	).
:- if(defined(low_level_mathematica_debug)).
kernel_handler(_Process, _Input, _Output, _Loc):- 
	debug_message("*** start_link ***"),
	fail.
:- endif.
kernel_handler(Process, Input, Output, Loc):-
	Options_ = ['-noprompt'],
	current_ciao_flag(mathematica_kernel,  URL),
	(
	    URL = ''
	->
	    throw(error(mathemtica_kernel_path_undefined, Loc))
	;
	    atom_concat('ssh://', SSHURL, URL)
	->
	    Cmd = path(ssh), Type = ssh,
	    (
		atom_concat([Host, ':', Port, '/', Tail],  SSHURL)
	    ->
	        Options = [Host, '-p', Port, Path | Options_]
	    ;
		atom_concat([Host, '/', Tail], SSHURL)
	    ->
		Options = [Host, Path | Options_]
	    ),
	    atom_concat('/', Tail, Path)
	; Cmd = URL, Options = Options_, Type = normal
	),
	time(StartingTime),
	process_call(Cmd, Options,
	    [ background(Process),
	      stdin(stream(Input)), stdout(stream(Output)) ]),
	assertz_fact(kernel_handler_fact(Process, Input, Output, Type, StartingTime)),
	% simple test to ensure connection is working
	write_line(Input, ["$VersionNumber"]),
	read_line(Output, Result),
	(
	    Result = [_,_|_],
	    looks_like_a_version_number(Result, []) 
	->
	    true
	;
	    close(Input),
	    catch(process_join(Process), _, true),
	    close(Output),
	    throw(error(mathematica_opening_link, Loc))
	).

looks_like_a_version_number --> [].
looks_like_a_version_number --> ".",
	looks_like_a_version_number.
looks_like_a_version_number --> [C],
	{0'0 =< C, C =< 0'9 }, looks_like_a_version_number.

evaluate("", ""):-!.
evaluate(Query, Result):-
	kernel_handler(_Process, Input, Output, mathematica:evaluate/2),
	write_line(Input,["Quiet[", Query, "]"]),
	write_line(Input,["$ack"]),
	read_line(Output, Result),
	( get_ack(Output, Err), Err = [_|_] ->
            throw(error(mathematica_error(Err), evaluate/2-1))
	;   true
	).

get_ack(Stream, L):-
	read_line(Stream, Str), 
	( Str = "$ack" ->
	    L = []
	; Str = "" ->
	    get_ack(Stream, L)
	;   L = [Str|T],
	    get_ack(Stream, T)
	).
     
:- if(defined(low_level_mathematica_debug)).
close_link:- 
	debug_message("*** close_link ***"),
	fail.
:- endif.
close_link:-
	retract_fact(kernel_handler_fact(Process, Input, Output, _, _)), !,
	close(Input),
	catch(process_join(Process), _, true),
	close(Output).
close_link.
 

:- pred write_line/2 : (stream * list(string)).

:- if(defined(low_level_mathematica_debug)).	
write_line(_Stream, List):-
	append(List, String),
	debug_message(" > " || String),
	fail.
:- endif.
write_line(Stream, List):-
	append(List, String),
	write_string(Stream, String),
	nl(Stream),
	flush_output(Stream).

:- pred read_line/2 : (stream * var) => (stream * string).

:- if(defined(low_level_mathematica_debug)).
read_line(Stream, Line):-
	get_line(Stream, Line),
	debug_message(" < " || Line).
:- else.
read_line(Stream, Line):-
	get_line(Stream, Line).
:- endif.


:- endif.

