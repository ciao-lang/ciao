:- module(http_server_ll,[http_receive_header/3,http_receive_content/4]).

:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(sockets), [socket_recv/2]).

http_receive_header(Stream,Str,Tail):-
	http_receive_header_(Stream,Data,[],Tail),
	flatten(Data,Str).

http_receive_header_(S,[Chunk|Cont],LastThree,Tail):-
	socket_recv(S,Chunk),
	append(LastThree,Chunk,Ch1),
	miniparse(Ch1,Tail,StopReading,LastThree2),
	(StopReading = yes ->
	 Cont = []
	;
	 http_receive_header_(S,Cont,LastThree2,Tail)
	).

%% traverses the chunk just read looking for cr lf cr lf.
miniparse([A],_,no,[A]):- !.
miniparse([10,10|Tail],Tail,yes,[10,10]):- !.
miniparse([A,B],_,no,[A,B]):- !.
miniparse([A,B,C],_,no,[A,B,C]):- !.
miniparse([13,10,13,10|Tail],Tail,yes,[10,13,10]):- !.
miniparse([_|Xs],Tail,Stop,LastThree):-
	miniparse(Xs,Tail,Stop,LastThree).
	

% depth 2 list flattening.
flatten([],[]).
flatten([[]|Xs],Ys):-
	flatten(Xs,Ys).
flatten([[X|Xs]|Ys],Zs):-
	flatten(Ys,Z1s),
	append([X|Xs],Z1s,Zs).


http_receive_content(Stream,Length,AlreadyRead,Data):-
	length(AlreadyRead,N),
	Length1 is Length - N,
	http_receive_content_(Stream,Length1,Data1),
	(Data1 = [] ->
	 trim(AlreadyRead,Length,Data)
	;
	 append(AlreadyRead,Data1,Data2),
	 trim(Data2,Length,Data)
	).

http_receive_content_(_,N,[]):-
	N =< 0, !.
http_receive_content_(Stream,Length,Data):-
	socket_recv(Stream,Chunk),
	length(Chunk,N),
	Length1 is Length - N,
	http_receive_content_(Stream,Length1,Data1),
	append(Chunk,Data1,Data).
	
trim(_,0,[]).
trim([X|Xs],N,[X|Ys]):-
	N1 is N-1,
	trim(Xs,N1,Ys).
