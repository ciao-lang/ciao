:- module(javart, [ 
        java_start/0,
        java_start/1,
        java_start/2,
	java_stop/0,
	java_connect/2,
        java_disconnect/0,
        machine_name/1,
	java_constructor/1,
	java_object/1,
	java_event/1,
	prolog_goal/1,
	java_field/1,
	java_use_module/1,
        java_create_object/2,
        java_delete_object/1, 
        java_invoke_method/2, 
	java_method/1,
        java_get_value/2,
        java_set_value/2,
        java_add_listener/3,
        java_remove_listener/3
	],
	[assertions,regtypes,isomodes]).

:- doc(title,"Prolog to Java interface").

:- doc(author,"Jes@'{u}s Correas").

:- doc(module,"

This module defines the Ciao Prolog to Java interface. This interface
allows a Prolog program to start a Java process, create Java objects,
invoke methods, set/get attributes (fields), and handle Java events.

This interface only works with JDK version 1.2 or higher.

Although the Java side interface is explained in Javadoc format (it is
available at @tt{library/javall/javadoc/} in your Ciao installation),
the general interface structure is detailed here.

@section{Prolog to Java Interface Structure}
@cindex{Prolog to Java Interface Structure}
This interface is made up of two parts: a Prolog
part and a Java part, running in separate processes. The Prolog part 
receives requests from a Prolog program and sends them to the Java part
through a socket. The Java part receives requests from the socket and
performs the actions included in the requests.

If an event is thrown in the Java side, an asynchronous message must
be sent away to the Prolog side, in order to launch a Prolog goal to
handle the event. This asynchronous communication is performed using a
separate socket. The nature of this communication needs the use of
threads both in Java and Prolog: to deal with the 'sequential program
flow,' and other threads for event handling.

In both sides the threads are automatically created by the context of
the objects we use. The user must be aware that different requests to the 
other side of the interface could run concurrently.


@subsection{Prolog side of the Java interface} @cindex{Prolog to Java
Interface Structure. Prolog side} The Prolog side receives the actions
to do in the Java side from the user program, and sends them to the
Java process through the socket connection.  When the action is done
in the Java side, the result is returned to the user Prolog program,
or the action fails if there is any problem in the Java side.

Prolog data representation of Java elements is very simple in this
interface. Java primitive types such as integers and characters are
translated into the Prolog corresponding terms, and even some Java
objects are translated in the same way (e. g. Java strings). Java
objects are represented in Prolog as compound terms with a reference
id to identify the corresponding Java object. Data conversion is made
automatically when the interface is used, so the Prolog user programs
do not have to deal with the complexity of these tasks.

@subsection{Java side}
@cindex{Prolog to Java Interface Structure. Java side}
The Java side of this layer is more complex than the Prolog side. The tasks
this part has to deal with are the following:

@begin{itemize}
  
@item Wait for requests from the Prolog side.

@item Translate the Prolog terms received in the Prolog 'serialized'
	form to a more useful Java representation (see the Java
	interface documentation available at
	@tt{library/javall/javadoc/} in your Ciao installation for
	details regarding Java representation of Prolog terms).

@item Interpret the requests received from the Prolog side, and execute them. 

@item Handle the set of objects created by or derived from the requests
        received from the prolog side.

@item Handle the events raised in the Java side, and launch the listeners
        added in the prolog side.

@item Handle the exceptions raised in the Java side, and send them to
	the Prolog side.

@end{itemize}

In the implementation of the Java side, two items must be carefully
designed: the handling of Java objects, and the representation of
prolog data structures. The last item is specially important because
all the interactions between Prolog and Java are made using Prolog
structures, an easy way to standardize the different data management
in both sides. Even the requests themselves are encapsulated using
Prolog structures.  The overload of this encapsulation is not
significant in terms of socket traffic, due to the optimal
implementation of the prolog serialized term.

The java side must handle the objects created from the Prolog side
dinamically, and these objects must be accessed as fast as possible from
the set of objects. The Java API provides a powerful implementation of Hash
tables that achieves all the requirements of our implementation.

On the other hand, the java representation of prolog terms is made using
the inheritance of java classes. In the java side there exists a representation
of a generic prolog term, implemented as an abstract class in
java. Variables, atoms, compound terms, lists, and numeric terms are
classes in the java side which inherit from the term class. Java objects can
be seen also under the prolog representation as compound terms, where the
single argument corresponds to the Hash key of the actual java object in
the Hash table referred to before. This behaviour makes the handling of mixed
java and prolog elements easy. Prolog goals are represented in the java
side as objects which contain a prolog compound term with the term
representing the goal. This case will be seen more in depth in next chapter,
where the java to prolog interface is explained. 

@section{Java event handling from Prolog}
@cindex{Java event handling from Prolog}
Java event handling is based on a delegation model since version
1.1.x. This approach to event handling is very powerful and elegant, but a
user program cannot handle all the events that can arise on a given object:
for each kind of event, a listener must be implemented and added
specifically. However, the Java 2 API includes a special listener
(@tt{AWTEventListener}) that can manage the internal java event queue.

The prolog to java interface has been designed to emulate the java
event handler, and is also based on event objects and listeners. The
prolog to java interface implements its own event manager, to handle
those events that have prolog listeners associated to the object that
raises the event. From the prolog side can be added listeners to
objects for specific events. The java side includes a list of goals to
launch from the object and event type.

Due to the events nature, the event handler must work in a separate thread
to manage the events asynchronously. The java side has its own mechanisms
to work this way. The prolog side must be implemented specially for event
handling using threads. The communication between java and prolog is also
asynchronous, and an additional socket stream is used to avoid interferences
with the main socket stream. The event stream will work in this
implementation only in one way: from java to prolog. If an event handler
needs to send back requests to java, it will use the main socket stream, just
like the requests sent directly from a prolog program.

The internal process of register a Prolog event handler to a Java event is
shown in the next figure:

@image{javall/autofig/ip2jbn-events-pl-reg}

When an event raises, the Prolog to Java interface has to send to
the Prolog user program the goal to evaluate. Graphically, the complete
process takes the tasks involved in the following figure: 

@image{javall/autofig/ip2jbn-events-pl-fire}


@section{Java exception handling from Prolog}
@cindex{Java exception handling from Prolog}
Java exception handling is very similar to the peer prolog handling:
it includes some specific statements to trap exceptions from user code. In the
java side, the exceptions can be originated from an incorrect request, or can
be originated in the code called from the request. Both exception types will be
sent to prolog using the main socket stream, allowing the prolog
program manage the exception. However, the first kind of exceptions are
prefixed, so the user program can distinguish them from the
second type of exceptions.

In order to handle exceptions properly using the prolog to java and java to 
prolog interfaces simultaneously, in both sides of the interface those 
exceptions coming from their own side will be filtered: this avoids an
endless loop of exceptions bouncing from one side to another.

").

:- use_module(library(concurrency)).
:- use_module(library(iso_char)).
:- use_module(library(lists)).
:- use_module(library(read), [read/2]).
:- use_module(library(write), [write/1]).
:- use_module(library(javall/javasock)).
:- use_module(library(process)).
:- use_module(library(system)).


% core/engine/basic_props.pl
%% -----------------------------------------------------------------------
%% REGTYPES
%% -----------------------------------------------------------------------

:- regtype machine_name(X) # "@var{X} is the network name of a machine.".

machine_name(X) :- atm(X).

:- regtype java_object(X) # "@var{X} is a java object (a structure with functor
	'$java_object', and argument an integer given by the java side).".

java_object('$java_object'(X)) :- int(X).

:- regtype java_field(X) # "@var{X} is a java field (structure on which
	the functor name is the field name, and the single argument is 
        the field value).".

java_field(X) :- struct(X).

:- regtype java_method(X) # "@var{X} is a java method (structure with
	functor as method name, and arguments as method ones, plus a result
	argument. This result argument is unified with the atom 'Yes' if
	the java method returns void).".

java_method(X) :- struct(X).

:- regtype java_constructor(X) # "@var{X} is a java constructor (structure
	with functor as constructor full name, and
	arguments as constructor arguments).".

java_constructor(X) :- struct(X).

:- regtype java_event(X) # "@var{X} is a java event represented as an atom
	with the full event constructor name (e.g.,
	'java.awt.event.ActionListener').".

java_event(X) :- atm(X).

:- regtype prolog_goal(X) # "@var{X} is a prolog predicate. Prolog term
	that represents the goal that must be invoked when the event
	raises on the object. The predicate arguments can be java objects,
	or even the result of java methods. These java objects will be
	evaluated when the event raises (instead of when the listener is
	added). The arguments that represent java objects must be
	instantiated to already created objects. The variables will be kept
	uninstantiated when the event raises and the predicate is
	called.".

prolog_goal(X) :- callable(X).

%% -----------------------------------------------------------------------
:- pred java_start
	# "Starts the Java server on the local machine,
	connects to it, and starts the event handling thread.".
%% -----------------------------------------------------------------------

java_start :-
	java_start("").

%% -----------------------------------------------------------------------
:- pred java_start(+Classpath)
	:: string # "Starts the Java server on the local machine,
	connects to it, and starts the event handling thread. The Java
        server is started using the classpath received as argument.".
%% -----------------------------------------------------------------------

java_start(Cp) :-
	(is_connected_to_java ->
% exceptions are not handled properly with multiple threads.
	 %display(java_exception('Java connection already active')),nl,
	 throw(java_exception('Java connection already active')),
	 fail
	;
	 true
	), 
%jcf%	compound_classpath(Cp, Cp2),
	set_classpath(Cp),
	Args = ['CiaoJava.PLJavaServer'],
%	Args = ['-Xdebug', '-Xnoagent', '-Djava.compiler=NONE', '-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005', 'CiaoJava.PLJavaServer'],
%	name(Command,Command1),
%jcf%	append("java -classpath ", Cp2, CommandS1),
%jcf%   append(CommandS1," CiaoJava.PLJavaServer", CommandS2),
%jcf%   name(Command, CommandS2),
	!,
        process_call(path(java), Args, [stdout(stream(Stream)), background(P)]),
	get_port(Stream,Port),
	java_connect_stream(localhost,Port,Stream),
	set_fact(java_process(P)),
	!.

:- pred java_process(Process) :: process 
   # "Stores the Java process handler".
:- data java_process/1.

%% -----------------------------------------------------------------------
:- pred java_start(+Machine_name,+Classpath)
	:: machine_name * string # "Starts the Java server in
	@var{machine_name} (using rsh!), connects to it, and starts the event
        handling thread. The Java server is started using the @var{Classpath}
        received as argument.". 
%% -----------------------------------------------------------------------

java_start(Node,Cp):-
	(is_connected_to_java ->
% exceptions are not handled properly with multiple threads.
%	 display(java_exception('Java connection already active')),nl,
	 throw(java_exception('Java connection already active')),
	 fail
	;
	 true
	), 
	!,
	compound_classpath(Cp,Cp2),
	atom_codes(ClassPath, Cp2),
        Args = ['-n',
	        Node,
		'java',
		'-classpath', ClassPath,
		'java_daemon'],
        process_call(path(rsh), Args, [stdout(stream(Stream)), background(P)]),
	get_port(Stream,Port),
	java_connect_stream(Node,Port,Stream),
	process_join(P).

%% -----------------------------------------------------------------------
:- pred java_stop/0 # "Stops the interface terminating the threads
	that handle the socket connection, and finishing the Java
        interface server if it was started using java_start/n.".
%% -----------------------------------------------------------------------
java_stop :-
	is_connected_to_java,
	!,
	stop_socket_interface,
	try_join_java_process.

try_join_java_process :-
	( retract_fact(java_process(P)) ->
	    process_join(P)
	; true
	).

%% -----------------------------------------------------------------------
:- pred java_connect(+Machine_name,+Port_number) 
	:: machine_name * int # "Connects to an existing
	Java interface server running in @var{Machine_name} and listening at
        port @var{port_number}. To connect to a Java server located in the
        local machine, use 'localhost' as machine_name.". 
%% -----------------------------------------------------------------------
java_connect(Node,Port) :-
	start_socket_interface(Node:Port,_).

%% -----------------------------------------------------------------------
:- pred java_connect_stream(+Machine_name,+Port_number,+Stream) 
	:: machine_name * int * stream # "Connects to an existing
	Java interface server running in @var{Machine_name}, listening at port
        @var{port_number}, and with std. input stream @var{Stream}. To connect
        to a Java server located in the local machine, use 'localhost' as
        @var{Machine_name}.". 
%% -----------------------------------------------------------------------
java_connect_stream(Node,Port,Stream) :-
	start_socket_interface(Node:Port,Stream).

%% -----------------------------------------------------------------------
:- pred java_disconnect/0
        # "Closes the connection with the java process, terminating the
        threads that handle the connection to Java. This predicate does
        not terminate the Java process (this is the disconnection 
        procedure for Java servers not started from Prolog). This 
        predicate should be used when the communication is established
        with java_connect/2.".
%% -----------------------------------------------------------------------
java_disconnect :-
	is_connected_to_java,
	!,
	stop_socket_interface.

%% -----------------------------------------------------------------------
:- pred set_classpath(+User_classpath) :: string # "Assigns
	the CLASSPATH environment variable needed by Java to run the
	Java server and user classes.".
%% -----------------------------------------------------------------------

set_classpath(UserClasspath) :-
	compound_classpath(UserClasspath,CPath),
	setenvstr('CLASSPATH',CPath).

%% -----------------------------------------------------------------------
:- pred compound_classpath(+User_classpath,-New_classpath) ::
	string * string # "Compounds a string with the
	classpath needed by Java to run the Java server and user
	classes.".
%% -----------------------------------------------------------------------

compound_classpath(UserClasspath,NewClasspath) :-
        absolute_file_name(library(javall/javart),AbsFileName),
        name(AbsFileName,AbsFileNameS),
        append(UClasspath,"/javart.pl",AbsFileNameS),
	correct_win_path(UClasspath,CiaoClasspath,System),
	addPath(CiaoClasspath,UserClasspath,System,NewClasspath).

addPath(Cp,"",_,Cp).

addPath("",Cp,_,Cp).

addPath(Cp1,Cp2,windows,Cp) :-
	change_slashes(Cp1,Cp1s),
	change_slashes(Cp2,Cp2s),
	append(Cp1s,[0';|Cp2s],Cp).
%

addPath(Cp1,Cp2,other,Cp) :-
	append(Cp1,[0':|Cp2],Cp).

correct_win_path([0'/,0'c,0'y,0'g,0'd,0'r,0'i,0'v,0'e,0'/,L,0'/|Upath],[L,0':,Bs|Wpath],windows):-
	!,
	char_code('\\',Bs),
	member(L,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
	change_slashes(Upath, Wpath).

correct_win_path([0'/,0'/,L,0'/|Upath], [L,0':,Bs|Wpath],windows):-
	char_code('\\',Bs),
	member(L,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
	change_slashes(Upath, Wpath).

correct_win_path(X,X,other).
	
change_slashes([],[]).

change_slashes([0'/|Upath],[0'\\|Wpath]):-
	!,
	change_slashes(Upath,Wpath).

change_slashes([L|Upath],[L|Wpath]):-
	change_slashes(Upath,Wpath).

%% -----------------------------------------------------------------------
:- pred get_port(+Stream,-Port)
	:: stream * atom # "Gets the @var{Port} number to connect to Java
	server, reading it from the @var{Stream} received as argument.".
%% -----------------------------------------------------------------------

get_port(Stream,Port):-
        read(Stream, Port).

%% -----------------------------------------------------------------------
:- pred java_use_module(+Module)
	:: term # "Loads a module and makes it available from Java.".
	
%% -----------------------------------------------------------------------
java_use_module(Module):-
	assertz_fact(prolog_query(0,internal_use_module(Module))),
	!.

%% -----------------------------------------------------------------------
:- pred java_create_object(+java_constructor,-java_object) 
	:: java_constructor * java_object # "New java object creation. The
	constructor must be a compound term as defined by its type, with
	the full class name as functor (e.g., 'java.lang.String'), and the
	parameters passed to the constructor as arguments of the
	structure.".
%% -----------------------------------------------------------------------

java_create_object(Constructor, Object):-
	is_connected_to_java,
	nonvar(Constructor),
	var(Object),
	Constructor =.. [Name | ArgList],
	eng_goal_id(Id),
	assertz_fact(java_query(Id,'$java_create_object'(Name,ArgList))),
	retract_fact(java_response(Id,Object)),
	!,
	check_error(Object).

%% -----------------------------------------------------------------------
:- pred java_delete_object(+java_object) 
	:: java_object # "Java object deletion. It removes the object given
	as argument from the Java object table.".
%% -----------------------------------------------------------------------

java_delete_object(Object) :-
	is_connected_to_java,
	nonvar(Object),
	eng_goal_id(Id),
	assertz_fact(java_query(Id,'$java_delete_object'(Object))),
	retract_fact(java_response(Id,T)),
	!,
	check_error(T).

%% -----------------------------------------------------------------------
:- pred java_get_value(+java_object,+java_field)
	:: java_object * java_field # "Gets the value of a field. Given a
	Java object as first argument, it instantiates the variable given
        as second argument. This field must be 	uninstantiated in the
        java_field functor, or this predicate will fail.".
%% -----------------------------------------------------------------------

java_get_value(Object,Field) :-
	is_connected_to_java,
        nonvar(Object),
        Field =.. [Name, Value],
	eng_goal_id(Id),
	assertz_fact(java_query(Id,'$java_get_value'(Object,Name))),
	retract_fact(java_response(Id,Value)),
	!,
        check_error(Value).

%% -----------------------------------------------------------------------
:- pred java_set_value(+java_object,+java_field) 
	:: java_object * java_field # "Sets the value of a Java object
	field. Given a Java object reference, it assigns the value included
	in the java_field compound term. The field value in the java_field
	structure must be instantiated.".
%% -----------------------------------------------------------------------

java_set_value(Object,Field) :-
	is_connected_to_java,
        nonvar(Object),
        Field =.. [Name, Value],
	eng_goal_id(Id),
	assertz_fact(java_query(Id,'$java_set_value'(Object,Name,Value))),
	retract_fact(java_response(Id,T)),
	!,
        check_error(T).

%% -----------------------------------------------------------------------
:- pred java_invoke_method(+java_object,+java_method) 
	:: java_object * java_method # "Invokes a java method on an
	object. Given a Java object reference, invokes the method
	represented with the second argument. ".
%% -----------------------------------------------------------------------

java_invoke_method(Object,Method) :-
	is_connected_to_java,
        nonvar(Object),
        nonvar(Method),
        Method =.. [Name | ArgList],
        append(Args,[Result],ArgList),
	eng_goal_id(Id),
	assertz_fact(java_query(Id,'$java_invoke_method'(Object,Name,Args))),
	retract_fact(java_response(Id,Result)),
	!,
	check_error(Result).

%% -----------------------------------------------------------------------
:- pred java_add_listener(+java_object, +java_event, +prolog_goal) 
	:: java_object * java_event * prolog_goal # "Adds a listener to an
	event on an object. Given a Java object reference, it registers the
	goal received as third argument to be launched when the Java event
	raises.".
%% -----------------------------------------------------------------------

:- meta_predicate java_add_listener(?,?,goal).

java_add_listener(Object, Event, Goal) :-
	is_connected_to_java,
        nonvar(Object),
        nonvar(Event),
        nonvar(Goal),
	eng_goal_id(Id),
	assertz_fact(java_query(Id,'$java_add_listener'(Object, Event, Goal))),
	retract_fact(java_response(Id,T)),
	!,
        check_error(T).

%% -----------------------------------------------------------------------
:- pred java_remove_listener(+java_object, +java_event, +prolog_goal) 
	:: java_object * java_event * prolog_goal # "It removes a listener
	from an object event queue. Given a Java object reference, goal
	registered for the given event is removed.".
%% -----------------------------------------------------------------------

java_remove_listener(Object, Event, Goal) :-
	is_connected_to_java,
        nonvar(Object),
        nonvar(Event),
        nonvar(Goal),
	eng_goal_id(Id),
	assertz_fact(java_query(Id,'$java_remove_listener'(Object, Event, Goal))),
	retract_fact(java_response(Id,T)),
	!,
        check_error(T).

%% -----------------------------------------------------------------------
:- pred check_error(+term)
	:: term
	# "Checks if a term received from java is the '$fail' term, or an
        exception has occured in Java.".
%% -----------------------------------------------------------------------
check_error('$fail') :- 
        !,
        fail.

check_error('$java_exception'(E)):-
	!,
% exceptions are not handled properly with multiple threads.
%	display(java_exception(E)),nl,
	throw(java_exception(E)),
	fail.

check_error(_).
