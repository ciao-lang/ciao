:- module(zeromq, 
	[
	    zmq_init/0,
	    zmq_term/0,
	    zmq_socket/2,
	    zmq_close/1,
	    zmq_bind/2,
	    zmq_connect/2,
	    zmq_subscribe/3,
	    zmq_unsubscribe/3,
	    zmq_send/4,
	    zmq_recv/5,
	    zmq_multipart_pending/2,
	    zmq_poll/3,
	    zmq_device/3,

	    zmq_error_check/1,
	    zmq_errors/1,

	    zmq_send_multipart/3,
	    zmq_recv_multipart/4,

	    zmq_send_terms/3,
	    zmq_recv_terms/4,
	    
	    demo_responder/0,
	    demo_requester/1,
	    demo_requester/2
	], 
	[assertions,foreign_interface]).

:- use_module(library(lists)).

:- use_module(library(write), [writeq/1]).

:- use_module('./term_ser').

:- use_foreign_library([zmq]).
:- use_foreign_source(ciao_zmq_ll).

:- include(compiler_options).

% -- Documentation -------------------------------------------------

:- comment(title, "Ciao bindings for ZeroMQ messaging library").

:- comment(subtitle, "Access to the basic ZMQ functionality from Ciao Prolog").

:- comment(author, "Dragan Ivanovic (idragan@@clip.dia.fi.upm.es)").

:- comment(module, "Access to the basic
   @href{http://www.zeromq.org/}{ZMQ} functionality from Ciao
   Prolog").

:- comment(summary, "This module provides Ciao Prolog bindings for the
	ZeroMQ (ZMQ) in-process/inter-process/networked messaging
	library.

        ZermoMQ is a free software, licenced under GPL.  It is
        available for many platforms (including Linux, Windows and Mac
        OS X), and has bindings for many programming languages,
        including C (the core API), C++, Common Lisp, Erlang, Java,
        Lua, Perl, PHP, Python, Ruby, Ada, Basic, Go, Haskell,
        Objective-C and ooc.

        @bf{Note:} These bindings support ZeroMQ version 2.2.0.  Newer
        versions have somewhat changed APIs, and (among other things)
        their include files and library executables are incompatible.
        We recommend making a local installation.  Please refer to the
        @tt{ZMQ-INSTALL.txt} file in @lib{zeromq} for
        further details.

        For more information on ZeroMQ, reference manuals, user
        guides, downloads, etc., please refer to:

        @href{http://www.zeromq.org}

        The key ZMQ API calls are supported, while some more complex
	calls (such as for setting or getting socket options), may be
	implemented in the future. The major differences to the ZMQ C
	API library are the following:

      @begin{itemize} 

      @item The Ciao binding predicates use prefix @tt{zmq_},
      while the rest of predicate names is either the same as in the
      ZMQ C API, or are chosen to resemble the functionality.  The
      predicates @pred{zmq_send_multipart/3} and
      @pred{zmq_recv_multipart/4} are ``higher level'' operations
      that automate operations on multipart messages, and are
      implemented in Prolog.

      @item Contexts are always thread-specific, and are automatically
      created when necessary.  Therefore, calling
      @pred{zmq_init/0} explicitly is not necessary.

      @item Atoms are used to name sockets.  Sockets and their names
      are thread-specific, so the user does not need to worry about
      thread safety there.

      @item @pred{zmq_term/0} tries to close all thread-specific
      sockets before terminating the thread-specific context.

      @item Only a subset of @tt{zmq_setsockopt} and
      @tt{zmq_getsockopt} operations is supported: to check whether
      another part of a multipart message is pending for reading, or
      to add or remove subscriptions.

      @item Errors are not reported by the individual ZMQ binding
      predicates.  Instead, errors are remembered and can be checked
      or retrieved using @pred{zmq_error_check/1} and
      @pred{zmq_errors/1}.

      @end{itemize}").

:- comment(copyright, "").

% -- Interface to C code -------------------------------------------

:- true pred zmq_init + foreign(ciao_zmq_init_) # "Initializes the ZMQ
	context for the current thread.  This is done automatically on
	any call to ZMQ routines that require the context.".

:- true pred zmq_term + foreign(ciao_zmq_term_) # "Terminates the ZMQ
	context for the current thread.  Previously attempts to close
	all open sockets.".

:- true pred zmq_socket(in(SocketAtom), in(TypeAtom)) :: atm * atm +
	foreign(ciao_zmq_socket) # "Creates a new socket named by atom
	@var{SocketAtom}.  @var{TypeAtom} corresponds to a ZMQ socket
	type.  It can be one of: @tt{req}, @tt{rep}, @tt{pub},
	@tt{sub}, @tt{push}, @tt{pull}, @tt{pair}, @tt{router},
	@tt{dealer}.  No other socket must be associated with
	@var{SocketAtom} at the time of calling.".

:- true pred zmq_close(in(SocketAtom)) :: atm +
	foreign(ciao_zmq_close) # "Closes socket identified with the
	atom @var{SocketAtom}.".

:- true pred zmq_bind(in(SocketAtom), in(Endpoint)) :: atm * string +
	foreign(ciao_zmq_bind) # "Binds socket @var{SocketAtom} to the
	given @var{Endpoint} address.  Addresses are strings of the
	form ``tcp://HOST:PORT'', or ``ipc://ENDPOINT'', or
	``inproc://IDENTIFIER''.  See ZMQ documentation for more
	details.  A socket can be simultaneously bound to several
	endpoints.".

:- true pred zmq_connect(in(SocketAtom), in(Endpoint)) :: atm * string
	+ foreign(ciao_zmq_connect) # "Connects socket to the given
	endpoint.  See @pred{zmq_bind/2} for more details. A socket
	can be simultaneously connected to several endpoints.".

:- true pred zmq_subscribe(in(SocketAtom), in(Len), in(Prefix)) :: atm
* c_int * any_term + (foreign(ciao_zmq_subscribe)) # "Subscribes socket
@var{SocketAtom} of type @tt{sub} to listen for messages that start
with the given @var{Prefix} byte list of size @var{Len}.  A @tt{sub}
socket @em{must} be subscribed to receive messages, even if the prefix
is an empty list.  If @var{Size}<0, the actual size will be the length
of @var{Prefix}, otherwise at most @var{Size} bytes from @var{Prefix}
will be used.  If @var{Size}>@tt{length(}@var{Prefix}@tt{)}, the
remaining bytes will be zeroes.".

:- true pred zmq_unsubscribe(in(SocketAtom), in(Len), in(Prefix)) ::
	atm * c_int * any_term + (foreign(ciao_zmq_unsubscribe)) #
	"Removes subscription previously established with
	@pred{zmq_subscribe/3}.  If @var{Size}<0, the actual size will
	be the length of @var{Prefix}, otherwise at most @var{Size}
	bytes from @var{Prefix} will be usedP.  If
	@var{Size}>@tt{length(}@var{Prefix}@tt{)}, the remaining bytes
	will be zeroes.".

% TODO: Size type should be c_size (and size_t in the C part)

:- true pred zmq_send(in(SocketAtom), in(Size), in(ByteList),
	in(Options)) :: atm * c_int * any_term * any_term +
	(foreign(ciao_zmq_send)) # "Sends a list of bytes
	@var{ByteList} of size @var{Size} over socket @var{SocketAtom}
	using the list of options @var{Options}.  Possible options
	are: @tt{noblock} to send in background, and @tt{sndmore} to
	signify that more message parts will follow.  If @var{Size}<0,
	the actual size will be the length of @var{ByteList},
	otherwise at most @var{Size} bytes from @var{ByteList} will be
	used.  If @var{Size}>@tt{length(}@var{ByteList}@tt{)}, the
	remaining bytes will be zeroes.".

:- true pred zmq_recv(in(SocketAtom), go(Maybe), go(Size),
	go(ByteList),in(Options)) :: atm * any_term * c_size * c_uint8_list
	* any_term + (foreign(ciao_zmq_recv), returns(Maybe),
	size_of(ByteList,Size)) # "Reads a message (or a message part)
	from socket @var{SocketAtom} using list of options
	@var{Options}.  The only valid option is currently
	@tt{noblock}, which returns without waiting for a message to
	arrive.  On exit, @var{Maybe} is set to either @tt{none} (if
	no message was read), or to @tt{some} if some message (part)
	was read.  @var{Size} tells the number of bytes read, and
	@var{ByteList} is the list of bytes read.".

:- true pred zmq_multipart_pending(in(SocketName), go(Result)) :: atm
	* any_term + (foreign(ciao_zmq_multipart_pending),
	returns(Result)) # "Checks whether there are more parts from
	the same multipart message waiting to be read on the socket
	@var{SocketAtom}.  @var{Result} is set to either @tt{some} or
	@tt{none}.".

:- true pred zmq_poll(in(SocketList), in(Timeout), go(Result)) ::
	any_term * c_int * any_term + (foreign(ciao_zmq_poll),
	returns(Result)) # "Polls sockets from the list
	@var{SocketList} for incoming messages.  @var{Timeout} defines
	how long to wait in microseconds: 0 means immediate return,
	and -1 means indefinite waiting.  @var{Result} is a sublist of
	sockets from @var{SocketList} that have pending messages.".

:- true pred zmq_device(in(DevType), in(Frontend), in(Backend)) :: atm
	* atm * atm + (foreign(ciao_zmq_device)) # "Starts a ZMQ ``device'' of
	type @var{DevType} with front-end socket @var{Frontend} and back-end
	socket @var{Backend}.  @var{DevType} must be @tt{queue},
	@tt{forwarder} or @tt{streamer}.  The front-end and the back-end
	sockets must be previously set up with correct socket types and
	bound/connected to their respective endpoints.".

:- true pred zmq_error_check(go(Maybe)) :: any_term +
	(foreign(ciao_zmq_error_check), returns(Maybe)) # "Checks
	whether errors ocurred in the previous @tt{zmq_XXX}
	operations.  @var{Maybe} is set either to @tt{none} or to
	@tt{some}".

:- true pred zmq_errors(go(ErrorList)) :: any_term +
	(foreign(ciao_zmq_errors), returns(ErrorList)) # "Retrieves
	and clears the list of errors (latest first) that have
	accumulated as the result of the previous @tt{zmq_XXX} calls.
	Each list element has the form @tt{error(Errno, Reason,
	Socket)}, where @var{Errno} is a numeric error code,
	@var{Reason} is an atom describing the context of the error,
	and @var{Socket} is the name of socket in relation to which
	the error has occured.  For an error that has ocurred in
	@pred{zmq_poll/3} @var{Socket} is @tt{''}.".


% -- Multipart send/receive ----------------------------------------

:- pred zmq_send_multipart(SocketAtom, Parts, Options) :: atm *
	list * list(atm) # "Sends a multipart message over socket
	@var{SocketAtom} using option list @var{Options} (the only
	usable option is currently @tt{noblock}).  @var{Parts} is a
	(ground, proper) list of elements of the form
	@tt{Size:Bytelist} or @tt{ByteList}.  In the latter case, the
	size is calculated using @pred{length/2}.".

zmq_send_multipart(SocketAtom, [Part], Options):- !,
	opts_wo_sndmore(Options, OptionsWOSM),
	part_size(Part, Size, Bytes),
	zmq_send(SocketAtom, Size, Bytes, OptionsWOSM).
zmq_send_multipart(SocketAtom, [Part|Parts], Options):-
	opts_wo_sndmore(Options, OptionsWOSM),
	part_size(Part, Size, Bytes),
	zmq_send(SocketAtom, Size, Bytes, [sndmore|OptionsWOSM]),
	!,
	zmq_send_multipart(SocketAtom, Parts, Options).

:- pred zmq_recv_multipart(SocketAtom, Maybe, Parts, Options) ::
	atm * atm * list * list # "Receives a multipart message over
	socket @var{SocketAtom} using option list @var{Option} (the
	only usable option is currently @tt{noblock}).  @var{Maybe} is
	set either to @tt{none} (no message parts read) or to
	@tt{some} (some message parts read).  @var{Parts} contains the
	message parts in the format @tt{Size:ByteList}".

zmq_recv_multipart(SocketAtom, Maybe, Parts, Options):-
	zmq_recv(SocketAtom, Maybe, Size, Bytes, Options),
	(  Maybe=some
	-> Parts=[Size:Bytes|OtherParts],
	   recv_multipart_1(SocketAtom, OtherParts, Options)
	;  Parts=[]
	).

recv_multipart_1(SocketAtom, Parts, Options):-
	(  zmq_multipart_pending(SocketAtom, some)
	-> zmq_recv(SocketAtom, Maybe, Size, Bytes, Options),
	   (  Maybe=none
	   -> Parts= []
	   ;  Parts= [Size:Bytes|OtherParts],
	      recv_multipart_1(SocketAtom, OtherParts, Options)
	   )
	;  Parts=[]
	).

part_size(Size:Bytes, Size, Bytes):- !.
part_size(Bytes, -1, Bytes).

opts_wo_sndmore([], []):- !.
opts_wo_sndmore(Opts, WOSM):-
	(  select(sndmore, Opts, Opts1)
	-> opts_wo_sndmore(Opts1, WOSM)
	;  WOSM= Opts
	).

% -- Send and receive lists of terms -------------------------------

:- comment(zmq_send_terms/3, "Sends a list of terms over the given sockets.  The terms
	are serialized before sending.").

:- pred zmq_send_terms(SocketAtom, Terms, Options) :: atm * list *
	list(atm) # "Send list of terms @var{Terms} over socket
	@var{SocketAtom} using options @var{Options}.  The meaning of
	@var{Options} is the same as in @pred{zmq_send_multipart/3}.".

zmq_send_terms(SocketAtom, Terms, Options):-
	uncook(Terms, Parts),
	zmq_send_multipart(SocketAtom, Parts, Options).

uncook([], []).
uncook([TH|TT], [H|T]):-
	serialize_term(TH, H),
	uncook(TT, T).

% ..................................................................

:- comment(zmq_recv_terms/4, "Receives a list of terms from the given
	socket sent using @pred{zmq_send_terms/3}").

:- pred zmq_recv_terms(SocketAtom, Maybe, Terms, Options) :: atm * atm
* list * list # "Receive list of terms @var{Terms} over socket
@var{SocketAtom}.  The meaning of @var{Maybe} and @var{Options} is the
same as in @pred{zmq_send_multipart/3}.  Each list item of @var{Terms}
is constructed using a disjoint set of free variables.".

zmq_recv_terms(SocketAtom, Maybe, Terms, Options):-
	zmq_recv_multipart(SocketAtom, Maybe, Parts, Options),
	(  Maybe=some
	-> cook(Parts, Terms)
	;  Terms=[]
	).

cook([], []).
cook([_:H|T], [TH|TT]):-
	(  deserialize_term(TH, H)
	-> true
	;  TH= ?
	),
	cook(T, TT).


% -- Demo responders and requesters --------------------------------

:- pred demo_responder # "Starts a responder (@tt{resp} type) socket
	that reads messages from TCP port 64321 on the local machine.
	While waiting for inbound messages, every five seconds prints
	a reminder on the screen.  Replies with ``Ok'' to each received
	message.  Stops and closes the socket after receiving a
	message of length zero.".

demo_responder :-
	zmq_errors(_),
	zmq_socket(responder, rep),
	zmq_bind(responder, "tcp://*:64321"),
	zmq_errors([]), !,
        display_string("Prolog-ZMQ binding: a demo responder.\n"),
        display_string("Listening at the TCP port 64321\n"),
        display_string("Stop me by sending an empty message (length 0).\n"),
	resp_loop,
	zmq_close(responder).

resp_loop:-
	zmq_poll([responder], 5000000, Q),
	(  Q=[]
	-> display_string("tik-tak...\n"),
	   resp_loop
	;  zmq_recv(responder, _, Size, Bytes, []),
	   zmq_errors(E),
	   (  E=[]
	   -> true
	   ;  display(E), nl
	   ),
	   display('['), display(Size), display(']'), tab(1),
	   display_string(Bytes), nl,
	   (  Bytes = [ 38 | Bytes0 ],
	      deserialize_term( T, Bytes0, [])
	   -> display('--> term: '),
	      writeq( T),
	      nl
	   ; true
	   ),
	   zmq_send(responder, 2, "Ok", []),
	   !,
	   (  Size>0
	   -> resp_loop
	   ;  display('Finished...')
	   )
	).

:- pred demo_requester(ByteList) # "Same as @pred{demo_requester/2},
	except that it connects specifically to
	@tt{tcp://locahost:64321}.".

demo_requester(Bytes) :-
	demo_requester("tcp://localhost:64321", Bytes).

:- pred demo_requester(Endpoint, ByteList) # "Sends a message
	@var{ByteList} to a responder socket listening on
	@var{Endpoint}, then waits for the response, prints it and
	finishes. ".

demo_requester(Endpoint, Msg) :-
	zmq_socket(requester, req),
	zmq_connect(requester, Endpoint),
	(  Msg = term(T)
	-> serialize_term( T, Bytes0, []),
	   Bytes = [ 38 | Bytes0 ]
	;  Bytes = Msg
	),
	zmq_send(requester, -1, Bytes, []),
	zmq_recv(requester, _, _, Response, []),
	display_string(Response),
	zmq_close(requester).
