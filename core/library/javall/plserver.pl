% Prolog server used in the Java to Prolog interface


:- use_module(library(javall/jtopl)).
:- use_module(library(format), [format/2]).

main([]):-
	!,
	prolog_server.

main(['--server',Port]):-
	atom_codes(Port,Ports),
	number_codes(Portn,Ports),
	!,
	prolog_server(Portn).

main(['--java',Node:Port]):-
	atom_codes(Port,Ports),
	number_codes(Portn,Ports),
	!,
	prolog_server(Node,Portn).

main(_):-
	format("Usage:~n",[]),
	format("plserver ~n",[]),
	format("     Starts a Prolog server that expect from the standard~n",[]),
	format("     input the port number to which it has to connect to.~n",[]),
	format("plserver --server Port~n",[]),
	format("     Starts a Prolog server in 'server mode': it waits for~n",[]),
	format("     Java connections at port given in the command line.~n",[]),
	format("plserver --java Node:Port~n",[]),
	format("     Starts a Prolog server that will connect to Node:Port~n",[]),
	format("     Java process (which is waiting for the Prolog server~n",[]),
	format("     connection).~n",[]).
