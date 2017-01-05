%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ground_vars([],Index,Index):- !.
ground_vars([Var|Tail],Index,IndexOut):-
   var(Var), !,
   number_codes(Index,AtomIndex),
   atom_codes(Var,[86,65,82|AtomIndex]),
   NextIndex is Index + 1,
   ground_vars(Tail,NextIndex,IndexOut).
ground_vars([Atom|Tail],Index,IndexOut):-
   atomic(Atom), !,
   ground_vars(Tail,Index,IndexOut).
ground_vars([Compound|Tail],Index,IndexOut):-
   Compound=..CompoundList,
   ground_vars(CompoundList,Index,NextIndex),
   ground_vars(Tail,NextIndex,IndexOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

window_size(2).
seq(4).
fixed(fix).

%config(WS,Fix) :- 
%	retractall(window_size(_)),
%	retractall(seq(_)),
%	retractall(fixed(_)),
%	S is 2*WS,
%	assert(window_size(WS)),
%	assert(seq(S)),
%	assert(fixed(Fix)).

pdef([iproto(WinSize,Fix),[21,1,21,20]],
	[pref([config(WinSize,Fix),[22,2,22,21]],[imain,[23,2,23,6]]),[22,2,23,6]],
	[(Fix,'Fix'), (WinSize,'WinSize')]).

pdef([imain,[25,1,25,5]],
	[par([sender(RLout,LRin,s(1,0)),[26,4,26,30]],[par([medium(LRin,LRout),[27,4,27,22]],[par([medium(RLin,RLout),[28,4,28,22]],[receiver(LRout,RLin,r(1,0,0,0,0,0)),[29,4,29,40]]),[28,4,29,40]]),[27,4,29,40]]),[26,4,29,40]],
	[(RLin,'RLin'), (LRout,'LRout'), (LRin,'LRin'), (RLout,'RLout')]).


% :- datatype boolean.
boolean(true).
boolean(false).

% :- datatype packettype.
packettype(type_DATA).
packettype(type_ACK).
packettype(type_NAK).

pdef([medium(In,Out),[45,1,45,15]],
	[pref([in(In,X),[46,2,46,7]],[pref([choice([out(Out,recvpak(true,true,X)),[48,6,48,33]],[choice([action(drop),[49,6,49,17]],[pref([action(corrupt),[50,6,50,20]],[choice([out(Out,recvpak(true,false,X)),[51,8,51,36]],[choice([out(Out,recvpak(false,false,X)),[52,8,52,37]],[out(Out,recvpak(false,true,X)),[53,8,53,36]]),[52,8,53,36]]),[51,6,54,6]]),[50,6,54,6]]),[49,6,54,6]]),[47,2,55,2]],[medium(In,Out),[56,2,56,16]]),[47,2,56,16]]),[46,2,56,16]],
	[(X,'X'), (Out,'Out'), (In,'In')]).

pdef([sender(In,Out,Old),[69,1,69,20]],
	[pref([choice([s_sendmsg(In,Out,Old,New),[70,6,70,33]],[choice([s_getpkt(In,Out,Old,New),[71,6,71,32]],[pref([action(progress),[72,6,72,21]],[s_timeout(In,Out,Old,New),[72,24,72,51]]),[72,6,72,51]]),[71,6,72,51]]),[70,2,73,2]],[sender(In,Out,New),[74,2,74,21]]),[70,2,74,21]],
	[(New,'New'), (Old,'Old'), (Out,'Out'), (In,'In')]).

pdef([s_sendmsg(In,Out,Old,New),[76,1,76,28]],
	[pref([action(user_send),[77,2,77,18]],[pref([s_sendmsg_while1(In,Out,Old,Mid),[78,2,78,36]],[pref([(Mid = s(SendSeq,Rack)),[79,2,79,23]],[pref([out(Out,packet(type_DATA,SendSeq,0)),[80,2,80,47]],[pref([eval((NextSend is (SendSeq + 1))),[81,2,81,28]],[(New = s(NextSend,Rack)),[82,2,82,24]]),[81,2,82,24]]),[80,2,82,24]]),[79,2,82,24]]),[78,2,82,24]]),[77,2,82,24]],
	[(NextSend,'NextSend'), (Rack,'Rack'), (SendSeq,'SendSeq'), (Mid,'Mid'), (New,'New'), (Old,'Old'), (Out,'Out'), (In,'In')]).

pdef([s_sendmsg_while1(In,Out,Old,New),[84,1,84,35]],
	[pref([(Old = s(SendSeq,Rack)),[85,2,85,23]],[if([(eval(((SendSeq - Rack) > window_size)) ; (SendSeq == Rack)),[86,5,86,5]],[pref([choice([s_getpkt(In,Out,Old,Mid),[88,5,88,31]],[pref([action(timeout),[89,5,89,19]],[s_timeout(In,Out,Old,Mid),[89,22,89,49]]),[89,5,89,49]]),[88,3,90,3]],[s_sendmsg_while1(In,Out,Mid,New),[91,3,91,37]]),[87,11,92,6]],[(New = Old),[93,11,93,19]]),[86,2,93,19]]),[85,2,93,19]],
	[(Mid,'Mid'), (Rack,'Rack'), (SendSeq,'SendSeq'), (New,'New'), (Old,'Old'), (Out,'Out'), (In,'In')]).

pdef([s_getpkt(In,Out,Old,New),[95,1,95,27]],
	[pref([in(In,RecvPak),[96,2,96,13]],[pref([(RecvPak = recvpak(Hck,_Dck,Pak)),[97,2,97,34]],[if([(Hck == true),[98,5,98,15]],[pref([(Pak = packet(PakType,_Seq,Ack)),[100,3,100,34]],[pref([s_handle_ack(Ack,Old,Mid),[101,3,101,29]],[if([(PakType == type_NAK),[102,6,102,24]],[s_handle_nak(Out,Mid,New,Pak),[103,12,103,43]],[(New = Mid),[104,12,104,20]]),[102,3,104,20]]),[101,3,104,20]]),[99,11,105,6]],[(New = Old),[106,11,106,19]]),[98,2,106,19]]),[97,2,106,19]]),[96,2,106,19]],
	[(Mid,'Mid'), (Ack,'Ack'), (_Seq,'_Seq'), (PakType,'PakType'), (Pak,'Pak'), (_Dck,'_Dck'), (Hck,'Hck'), (RecvPak,'RecvPak'), (New,'New'), (Old,'Old'), (Out,'Out'), (In,'In')]).


s_handle_ack(Ack, Old, New) :-
	Old = s(SendSeq, Rack),
	(in_interval(Ack, open(SendSeq, Rack))
		-> New = s(SendSeq, /* Rack = */ Ack)
		;  New = Old
	).

pdef([s_handle_nak(Out,Old,Old,Pak),[117,1,117,32]],
	[pref([(Pak = packet(_PakType,Seq,_Ack)),[118,2,118,34]],[pref([(Old = s(SendSeq,Rack)),[119,2,119,23]],[if([in_interval(Seq,open(SendSeq,Rack)),[120,5,120,41]],[out(Out,packet(type_DATA,Seq,0)),[121,11,121,52]]),[120,2,121,52]]),[119,2,121,52]]),[118,2,121,52]],
	[(Rack,'Rack'), (SendSeq,'SendSeq'), (_Ack,'_Ack'), (Seq,'Seq'), (_PakType,'_PakType'), (Pak,'Pak'), (Old,'Old'), (Out,'Out')]).

pdef([s_timeout(In,Out,Old,New),[123,1,123,28]],
	[pref([(Old = s(SendSeq,Rack)),[124,2,124,23]],[choice([s_getpkt(In,Out,Old,New),[126,6,126,32]],[pref([out(Out,packet(type_NAK,1,0)),[129,3,129,52]],[pref([if([not(eval((SendSeq == (Rack + 1)))),[130,6,130,6]],[pref([eval((PakSeq1 is (Rack + 1))),[132,4,132,28]],[out(Out,packet(type_DATA,PakSeq1,0)),[133,4,133,49]]),[131,12,134,7]]),[130,3,134,7]],[(New = Old),[135,3,135,11]]),[130,3,135,11]]),[128,6,136,6]]),[125,2,137,2]]),[124,2,137,2]],
	[(PakSeq1,'PakSeq1'), (Rack,'Rack'), (SendSeq,'SendSeq'), (New,'New'), (Old,'Old'), (Out,'Out'), (In,'In')]).

pdef([receiver(In,Out,Old),[148,1,148,22]],
	[pref([choice([r_getpkt(In,Out,Old,New),[150,6,150,32]],[pref([action(progress),[151,6,151,21]],[r_timeout(In,Out,Old,New),[151,24,151,51]]),[151,6,151,51]]),[149,2,152,2]],[receiver(In,Out,New),[153,2,153,23]]),[149,2,153,23]],
	[(New,'New'), (Old,'Old'), (Out,'Out'), (In,'In')]).

pdef([r_getpkt(In,Out,Old,New),[155,1,155,27]],
	[pref([in(In,RecvPak),[156,2,156,13]],[pref([(RecvPak = recvpak(Hck,Dck,Pak)),[157,2,157,33]],[if([(Hck == true),[158,5,158,15]],[pref([(Pak = packet(PakType,_Seq,_Ack)),[160,3,160,35]],[if([(PakType == type_DATA),[161,6,161,25]],[r_handle_data(Out,Old,New,Pak,Dck),[162,12,162,49]],[if([(PakType == type_NAK),[164,7,164,25]],[r_handle_nak(Out,Old,New,Pak),[165,13,165,44]],[(New = Old),[166,13,166,21]]),[164,4,166,21]]),[161,3,166,21]]),[159,11,167,3]],[(New = Old),[168,11,168,19]]),[158,2,168,19]]),[157,2,168,19]]),[156,2,168,19]],
	[(_Ack,'_Ack'), (_Seq,'_Seq'), (PakType,'PakType'), (Pak,'Pak'), (Dck,'Dck'), (Hck,'Hck'), (RecvPak,'RecvPak'), (New,'New'), (Old,'Old'), (Out,'Out'), (In,'In')]).

pdef([r_handle_data(Out,Old,New,Pak,Dck),[170,1,170,38]],
	[pref([(Old = r(_SendSeq,_RecSeq,Lack,_Rack,_Nakd,_RecBuf)),[171,2,171,56]],[pref([(Pak = packet(_PakType,Seq,_Ack)),[172,2,172,34]],[if([(not(eval(((Seq - Lack) > window_size))) , (Seq \== Lack)),[173,5,173,5]],[r_handle_data_correct(Out,Old,New,Pak,Dck),[174,11,174,56]],[(New = Old),[175,11,175,19]]),[173,2,175,19]]),[172,2,175,19]]),[171,2,175,19]],
	[(_Ack,'_Ack'), (Seq,'Seq'), (_PakType,'_PakType'), (_RecBuf,'_RecBuf'), (_Nakd,'_Nakd'), (_Rack,'_Rack'), (Lack,'Lack'), (_RecSeq,'_RecSeq'), (_SendSeq,'_SendSeq'), (Dck,'Dck'), (Pak,'Pak'), (New,'New'), (Old,'Old'), (Out,'Out')]).

pdef([r_handle_data_correct(Out,Old,New,Pak,Dck),[177,1,177,46]],
	[pref([(Old = r(SendSeq,RecSeq,Lack,Rack,Nakd,RecBuf)),[178,2,178,51]],[pref([(Pak = packet(_PakType,Seq,_Ack)),[179,2,179,34]],[if([(Dck == true),[180,5,180,15]],[pref([remove_from_set(Seq,Nakd,MidNakd),[182,3,182,37]],[pref([(Mid = r(SendSeq,RecSeq,Lack,Rack,MidNakd,RecBuf)),[183,3,183,55]],[if([eval((Seq == (RecSeq + 1))),[184,6,184,28]],[r_handle_data_expected_seqno(Out,Mid,New,Pak),[185,12,185,59]],[r_handle_data_unexpected_seqno(Out,Mid,New,Pak),[186,12,186,61]]),[184,3,186,61]]),[183,3,186,61]]),[181,11,187,6]],[if([(((Seq \== RecSeq) ; in_set(Seq,RecBuf)) ; in_set(Seq,Nakd)),[189,6,189,6]],[pref([out(Out,packet(type_NAK,Seq,RecSeq)),[193,4,193,38]],[pref([add_to_set(Seq,Nakd,NewNakd),[194,4,194,33]],[(New = r(SendSeq,RecSeq,RecSeq,Rack,NewNakd,RecBuf)),[195,4,195,58]]),[194,4,195,58]]),[192,12,196,7]],[(New = Old),[197,12,197,20]]),[189,3,197,20]]),[180,2,197,20]]),[179,2,197,20]]),[178,2,197,20]],
	[(NewNakd,'NewNakd'), (Mid,'Mid'), (MidNakd,'MidNakd'), (_Ack,'_Ack'), (Seq,'Seq'), (_PakType,'_PakType'), (RecBuf,'RecBuf'), (Nakd,'Nakd'), (Rack,'Rack'), (Lack,'Lack'), (RecSeq,'RecSeq'), (SendSeq,'SendSeq'), (Dck,'Dck'), (Pak,'Pak'), (New,'New'), (Old,'Old'), (Out,'Out')]).

pdef([r_handle_data_expected_seqno(Out,Old,New,Pak),[199,1,199,48]],
	[pref([(Old = r(SendSeq,RecSeq,Lack,Rack,Nakd,RecBuf)),[200,2,200,51]],[pref([(Pak = packet(_PakType,_Seq,_Ack)),[201,2,201,35]],[pref([eval((MidRecSeq is (RecSeq + 1))),[202,2,202,30]],[pref([action(user_recv),[203,2,203,18]],[pref([eval((Tmp is (MidRecSeq + 1))),[204,2,204,27]],[pref([r_supply_to_user(Out,Tmp,MidRecSeq,RecBuf,NewRecSeq,NewRecBuf),[205,2,205,68]],[pref([if([eval(((NewRecSeq - Lack) >= (window_size // 2))),[206,5,206,44]],[pref([out(Out,packet(type_ACK,NewRecSeq,NewRecSeq)),[208,3,208,46]],[(NewLack = NewRecSeq),[209,3,209,21]]),[207,11,210,6]],[(NewLack = Lack),[211,11,211,24]]),[206,2,211,24]],[(New = r(SendSeq,NewRecSeq,NewLack,Rack,Nakd,NewRecBuf)),[212,2,212,60]]),[206,2,212,60]]),[205,2,212,60]]),[204,2,212,60]]),[203,2,212,60]]),[202,2,212,60]]),[201,2,212,60]]),[200,2,212,60]],
	[(NewLack,'NewLack'), (NewRecBuf,'NewRecBuf'), (NewRecSeq,'NewRecSeq'), (Tmp,'Tmp'), (MidRecSeq,'MidRecSeq'), (_Ack,'_Ack'), (_Seq,'_Seq'), (_PakType,'_PakType'), (RecBuf,'RecBuf'), (Nakd,'Nakd'), (Rack,'Rack'), (Lack,'Lack'), (RecSeq,'RecSeq'), (SendSeq,'SendSeq'), (Pak,'Pak'), (New,'New'), (Old,'Old'), (Out,'Out')]).

pdef([r_supply_to_user(Out,Tmp,RecSeq,RecBuf,NewRecSeq,NewRecBuf),[215,1,215,64]],
	[if([in_set(Tmp,RecBuf),[216,5,216,23]],[pref([action(user_recv),[218,3,218,19]],[pref([remove_from_set(Tmp,RecBuf,MidRecBuf),[219,3,219,41]],[pref([eval((Next is (Tmp + 1))),[220,3,220,23]],[r_supply_to_user(Out,Next,Tmp,MidRecBuf,NewRecSeq,NewRecBuf),[221,3,222,29]]),[220,3,222,29]]),[219,3,222,29]]),[217,11,223,6]],[pref([(NewRecBuf = RecBuf),[224,12,224,29]],[(NewRecSeq = RecSeq),[224,32,224,49]]),[224,11,224,50]]),[216,2,224,50]],
	[(Next,'Next'), (MidRecBuf,'MidRecBuf'), (NewRecBuf,'NewRecBuf'), (NewRecSeq,'NewRecSeq'), (RecBuf,'RecBuf'), (RecSeq,'RecSeq'), (Tmp,'Tmp'), (Out,'Out')]).

pdef([r_handle_data_unexpected_seqno(Out,Old,New,Pak),[226,1,226,50]],
	[pref([(Old = r(SendSeq,RecSeq,Lack,Rack,Nakd,RecBuf)),[227,2,227,51]],[pref([(Pak = packet(_PakType,Seq,_Ack)),[228,2,228,34]],[if([((Seq \== RecSeq) , not(in_set(Seq,RecBuf))),[229,5,229,5]],[pref([add_to_set(Seq,RecBuf,NewRecBuf),[231,3,231,36]],[pref([eval((Tmp is (RecSeq + 1))),[232,3,232,25]],[pref([r_send_naks(Out,Tmp,Seq,RecSeq,Nakd,NewRecBuf,Lack,NewNakd,NewLack),[233,3,234,41]],[(New = r(SendSeq,RecSeq,NewLack,Rack,NewNakd,NewRecBuf)),[235,3,235,61]]),[233,3,235,61]]),[232,3,235,61]]),[230,11,236,6]],[(New = Old),[237,11,237,19]]),[229,2,237,19]]),[228,2,237,19]]),[227,2,237,19]],
	[(NewLack,'NewLack'), (NewNakd,'NewNakd'), (Tmp,'Tmp'), (NewRecBuf,'NewRecBuf'), (_Ack,'_Ack'), (Seq,'Seq'), (_PakType,'_PakType'), (RecBuf,'RecBuf'), (Nakd,'Nakd'), (Rack,'Rack'), (Lack,'Lack'), (RecSeq,'RecSeq'), (SendSeq,'SendSeq'), (Pak,'Pak'), (New,'New'), (Old,'Old'), (Out,'Out')]).

pdef([r_send_naks(Out,Tmp,Seq,RecSeq,Nakd,RecBuf,Lack,NewNakd,NewLack),[239,1,239,72]],
	[if([(Tmp \== Seq),[240,5,240,5]],[pref([if([(not(in_set(Tmp,Nakd)) , not(in_set(Tmp,RecBuf))),[242,6,242,6]],[pref([out(Out,packet(type_NAK,Tmp,RecSeq)),[244,4,244,38]],[pref([add_to_set(Tmp,Nakd,MidNakd),[245,4,245,33]],[(MidLack = RecSeq),[246,4,246,19]]),[245,4,246,19]]),[243,12,247,7]],[pref([(MidNakd = Nakd),[248,13,248,26]],[(MidLack = Lack),[248,29,248,42]]),[248,12,248,43]]),[242,3,248,43]],[pref([eval((Next is (Tmp + 1))),[249,3,249,23]],[r_send_naks(Out,Next,Seq,RecSeq,MidNakd,RecBuf,MidLack,NewNakd,NewLack),[250,3,251,39]]),[249,3,251,39]]),[241,11,252,6]],[pref([(NewNakd = Nakd),[253,12,253,25]],[(NewLack = Lack),[253,28,253,41]]),[253,11,253,42]]),[240,2,253,42]],
	[(Next,'Next'), (MidLack,'MidLack'), (MidNakd,'MidNakd'), (NewLack,'NewLack'), (NewNakd,'NewNakd'), (Lack,'Lack'), (RecBuf,'RecBuf'), (Nakd,'Nakd'), (RecSeq,'RecSeq'), (Seq,'Seq'), (Tmp,'Tmp'), (Out,'Out')]).


r_handle_ack(Ack, Old, New) :-
	Old = r(SendSeq, RecSeq, Lack, Rack, Nakd, RecBuf),
	(in_interval(Ack, open(SendSeq, Rack))
		-> New = r(SendSeq, RecSeq, Lack, /* Rack = */ Ack, 
				Nakd, RecBuf)
		;  New = Old
	).

pdef([r_handle_nak(Out,Old,New,Pak),[266,1,266,32]],
	[if([fixed(fix),[267,5,267,14]],[pref([(Pak = packet(_PakType,_Seq,_Ack)),[269,3,269,36]],[pref([(Old = r(SendSeq,RecSeq,_Lack,Rack,Nakd,RecBuf)),[270,3,270,53]],[pref([out(Out,packet(type_ACK,RecSeq,RecSeq)),[271,3,271,40]],[(New = r(SendSeq,RecSeq,RecSeq,Rack,Nakd,RecBuf)),[272,3,272,54]]),[271,3,272,54]]),[270,3,272,54]]),[268,11,273,6]],[(New = Old),[275,3,275,11]]),[267,2,275,11]],
	[(RecBuf,'RecBuf'), (Nakd,'Nakd'), (Rack,'Rack'), (_Lack,'_Lack'), (RecSeq,'RecSeq'), (SendSeq,'SendSeq'), (_Ack,'_Ack'), (_Seq,'_Seq'), (_PakType,'_PakType'), (Pak,'Pak'), (New,'New'), (Old,'Old'), (Out,'Out')]).

pdef([r_timeout(In,Out,Old,New),[277,1,277,28]],
	[pref([(Old = r(SendSeq,RecSeq,Lack,Rack,_Nakd,RecBuf)),[278,2,278,52]],[pref([(Mid = r(SendSeq,RecSeq,Lack,Rack,0,RecBuf)),[279,2,279,48]],[choice([r_getpkt(In,Out,Mid,New),[281,6,281,32]],[pref([eval((PakSeq is (RecSeq + 1))),[283,6,283,31]],[pref([add_to_set(PakSeq,0,NewNakd),[284,6,284,35]],[pref([out(Out,packet(type_NAK,PakSeq,RecSeq)),[285,6,285,43]],[(New = r(SendSeq,RecSeq,RecSeq,Rack,NewNakd,RecBuf)),[286,6,287,23]]),[285,6,287,23]]),[284,6,287,23]]),[283,6,287,23]]),[280,2,288,2]]),[279,2,288,2]]),[278,2,288,2]],
	[(NewNakd,'NewNakd'), (PakSeq,'PakSeq'), (Mid,'Mid'), (RecBuf,'RecBuf'), (_Nakd,'_Nakd'), (Rack,'Rack'), (Lack,'Lack'), (RecSeq,'RecSeq'), (SendSeq,'SendSeq'), (New,'New'), (Old,'Old'), (Out,'Out'), (In,'In')]).

fdef(livelock, or(form(ll2),diamAll(form(livelock)))).
fneg(livelock, neg_form(livelock)).
fdef(ll2, neg_form(neg_ll2)).
fdef(neg_ll2, boxSet([action(timeout),nop,tau],neg(ll2))).
fneg(ll2, form(neg_ll2)).

% :- import ground/1 from basics.

eval(X = Y)  :- !, X = Y.
eval(X == Y) :- !, eval(X, U), eval(Y, V), modulus(U, Ua), modulus(V, Va), Ua == Va.
eval(X is Y) :- !, eval(Y, U), modulus(U, X).
eval(X > Y)  :- !, eval(X, U), eval(Y, V), modulus(U, Ua), modulus(V, Va), Ua > Va.
eval(X < Y)  :- !, eval(X, U), eval(Y, V), modulus(U, Ua), modulus(V, Va), Ua < Va.
eval(X >= Y) :- !, eval(X, U), eval(Y, V), modulus(U, Ua), modulus(V, Va), Ua >= Va.
eval(X =< Y) :- !, eval(X, U), eval(Y, V), modulus(U, Ua), modulus(V, Va), Ua =< Va.
eval(not(E)) :- !, not(eval(E)).

eval(A+B, V) :- !, eval(A, Va), eval(B, Vb), V is Va+Vb.
eval(A-B, V) :- !, eval(A, Va), eval(B, Vb), V is Va-Vb.
eval(A*B, V) :- !, eval(A, Va), eval(B, Vb), V is Va*Vb.
eval(A/B, V) :- !, eval(A, Va), eval(B, Vb), V is Va/Vb.
eval(A//B, V) :- !, eval(A, Va), eval(B, Vb), V is Va//Vb.
eval(window_size, V) :- !, window_size(V).
eval(E, V) :- V is E.

modulus(E, V) :- seq(Seq), X is E mod Seq, abs(X, V).
abs(X, V) :- ( X < 0 -> (seq(Seq), V is X+Seq) ; V = X).

%% Now, some interval stuff

in_interval(X, open(L,U)) :- 
	X \== L,
	eval((L-X) =< window_size),
	eval((X-U) =< window_size).

in_open_interval(X, L,U) :- 
	X \== L,
	eval((L-X) =< window_size),
	eval((X-U) =< window_size).

in_interval(X, upper_closed(L, U)) :-
	(L > U -> (X > L; X =< U)
		; (X > L, X =< U)).
in_interval(X, closed(L, U)) :-
	(L > U -> (X >= L; X =< U)
		; (X >= L, X =< U)).

%% Finally, some set stuff

% implement set as bit-string

in_set(Element, Set) :-
	1 is ((Set >> Element) mod 2).

add_to_set(Element, Set, NewSet) :- 
	in_set(Element, Set)
	->	NewSet = Set
	;	NewSet is Set + (1 << Element).

remove_from_set(Element, Set, NewSet) :-
	in_set(Element, Set)
	->	NewSet is Set - (1 << Element)
	;	NewSet = Set.

% ----------------------------------------------------------

trans(par(A,end,end,B),nop,B).
trans(par(A,B,C,D),E,par(A,F,G,D)) :-
    (partrans(A,E,B,C,F,G)
     ;
     partrans(A,E,C,B,G,F)
    ).
% trans(iproto_0(A,B,C),nop,imain_0(C)) :- config(A,B).
trans(iproto_0(A,B,C),nop,imain_0(C)).
trans(s_timeout_0(A,B,s(C,D),E,F),out(B,packet(type_NAK,1,0)),s_timeout_6(E,s(C,D),B,G,F)) :-
    not eval(C == D + 1),
    eval(G is D + 1).
trans(s_timeout_0(A,B,s(C,D),E,F),nop,s_getpkt_0(A,B,s(C,D),E,F)).
trans(s_timeout_6(s(A,B),s(A,B),C,D,E),out(C,packet(type_DATA,D,0)),E).
trans(s_timeout_0(A,B,s(C,D),s(C,D),E),out(B,packet(type_NAK,1,0)),E) :-
    eval(C == D + 1).
trans(s_handle_nak_0(A,s(B,C),s(B,C),packet(D,E,F),G),out(A,packet(type_DATA,E,0)),G) :-
    in_interval(E,open(B,C)).
trans(s_handle_nak_0(A,s(B,C),s(B,C),packet(D,E,F),G),nop,G) :-
    not in_interval(E,open(B,C)).
trans(s_getpkt_0(A,B,C,D,E),in(A,recvpak(F,G,packet(H,I,J))),s_handle_nak_0(B,K,D,packet(H,I,J),E)) :-
    F == true,
    s_handle_ack(J,C,K),
    H == type_NAK.
trans(s_getpkt_0(A,B,C,D,E),in(A,recvpak(F,G,packet(H,I,J))),E) :-
    F == true,
    s_handle_ack(J,C,D),
    not H == type_NAK.
trans(s_getpkt_0(A,B,C,C,D),in(A,recvpak(E,F,packet(G,H,I))),D) :-
    not E == true.
trans(s_sendmsg_while1_0(A,B,s(C,D),E,F),nop,s_getpkt_0(A,B,s(C,D),G,s_sendmsg_while1_0(A,B,G,E,F))) :-
    (eval(C - D > window_size)
     ;
     C == D
    ).
trans(s_sendmsg_while1_0(A,B,s(C,D),E,F),action(timeout),s_timeout_0(A,B,s(C,D),G,s_sendmsg_while1_0(A,B,G,E,F))) :-
    (eval(C - D > window_size)
     ;
     C == D
    ).
trans(s_sendmsg_while1_0(A,B,s(C,D),s(C,D),E),nop,E) :-
    not (eval(C - D > window_size)  ';'  C == D).
trans(s_sendmsg_0(A,B,C,D,E),action(user_send),s_sendmsg_while1_0(A,B,C,F,s_sendmsg_2(D,B,F,E))).
trans(s_sendmsg_2(s(A,B),C,s(D,B),E),out(C,packet(type_DATA,D,0)),E) :-
    eval(A is D + 1).
trans(sender_0(A,B,C,D),nop,s_sendmsg_0(A,B,C,E,sender_0(A,B,E,D))).
trans(sender_0(A,B,C,D),nop,s_getpkt_0(A,B,C,E,sender_0(A,B,E,D))).
trans(sender_0(A,B,C,D),action(progress),s_timeout_0(A,B,C,E,sender_0(A,B,E,D))).
trans(medium_0(A,B,C),in(A,D),medium_1(B,A,D,C)).
trans(medium_1(A,B,C,D),action(corrupt),medium_3(A,B,C,D)).
trans(medium_1(A,B,C,D),out(A,recvpak(true,true,C)),medium_0(B,A,D)).
trans(medium_1(A,B,C,D),action(drop),medium_0(B,A,D)).
trans(medium_3(A,B,C,D),out(A,recvpak(true,false,C)),medium_0(B,A,D)).
trans(medium_3(A,B,C,D),out(A,recvpak(false,false,C)),medium_0(B,A,D)).
trans(medium_3(A,B,C,D),out(A,recvpak(false,true,C)),medium_0(B,A,D)).
trans(r_send_naks_0(A,B,C,D,E,F,G,H,I,J),out(A,packet(type_NAK,B,D)),r_send_naks_0(A,K,C,D,L,F,D,H,I,J)) :-
    B \== C,
    not in_set(B,E),
    not in_set(B,F),
    add_to_set(B,E,L),
    eval(K is B + 1).
trans(r_send_naks_0(A,B,C,D,E,F,D,G,H,I),nop,r_send_naks_0(A,J,C,D,E,F,D,G,H,I)) :-
    B \== C,
    (in_set(B,E)
     ;
     in_set(B,F)
    ),
    eval(J is B + 1).
trans(r_send_naks_0(A,B,C,D,E,F,D,E,D,G),nop,G) :-
    not B \== C.
trans(r_handle_data_unexpected_seqno_0(A,r(B,C,D,E,F,G),H,packet(I,J,K),L),nop,r_send_naks_0(A,M,J,C,F,N,D,O,P,r_handle_data_unexpected_seqno_7(H,B,E,C,N,O,P,L))) :-
    J \== C,
    not in_set(J,G),
    add_to_set(J,G,N),
    eval(M is C + 1).
trans(r_handle_data_unexpected_seqno_7(r(A,B,C,D,E,F),A,D,B,F,E,C,G),nop,G).
trans(r_handle_data_unexpected_seqno_0(A,r(B,C,D,E,F,G),r(B,C,D,E,F,G),packet(H,I,J),K),nop,K) :-
    not (I \== C  ','  not in_set(I,G)).
trans(r_handle_data_0(A,r(B,C,D,E,F,G),H,packet(I,J,K),L,M),nop,r_handle_data_correct_0(A,r(B,C,D,E,F,G),H,packet(I,J,K),L,M)) :-
    not eval(J - D > window_size),
    J \== D.
trans(r_handle_data_0(A,r(B,C,D,E,F,G),r(B,C,D,E,F,G),packet(H,I,J),K,L),nop,L) :-
    not (not eval(I - D > window_size)  ','  I \== D).
trans(r_getpkt_0(A,B,C,D,E),in(A,recvpak(F,G,packet(H,I,J))),r_handle_data_0(B,C,D,packet(H,I,J),G,E)) :-
    F == true,
    H == type_DATA.
trans(r_getpkt_0(A,B,C,D,E),in(A,recvpak(F,G,packet(H,I,J))),r_handle_nak_0(B,C,D,packet(H,I,J),E)) :-
    F == true,
    not H == type_DATA,
    H == type_NAK.
trans(r_getpkt_0(A,B,C,C,D),in(A,recvpak(E,F,packet(G,H,I))),D) :-
    E == true,
    not G == type_DATA,
    not G == type_NAK.
trans(r_getpkt_0(A,B,C,C,D),in(A,recvpak(E,F,packet(G,H,I))),D) :-
    not E == true.
trans(r_timeout_0(A,B,r(C,D,E,F,G,H),I,J),nop,r_getpkt_0(A,B,r(C,D,E,F,0,H),I,J)).
trans(r_timeout_0(A,B,r(C,D,E,F,G,H),r(C,D,D,F,I,H),J),out(B,packet(type_NAK,K,D)),J) :-
    eval(K is D + 1),
    add_to_set(K,0,I).
trans(receiver_0(A,B,C,D),nop,r_getpkt_0(A,B,C,E,receiver_0(A,B,E,D))).
trans(receiver_0(A,B,C,D),action(progress),r_timeout_0(A,B,C,E,receiver_0(A,B,E,D))).
trans(r_handle_nak_0(A,r(B,C,D,E,F,G),r(B,C,C,E,F,G),packet(H,I,J),K),out(A,packet(type_ACK,C,C)),K) :-
    fixed(fix).
trans(r_handle_nak_0(A,r(B,C,C,D,E,F),r(B,C,C,D,E,F),packet(G,H,I),J),nop,J) :-
    not fixed(fix).
trans(r_supply_to_user_0(A,B,C,D,E,F,G),action(user_recv),r_supply_to_user_0(A,H,B,I,E,F,G)) :-
    in_set(B,D),
    remove_from_set(B,D,I),
    eval(H is B + 1).
trans(r_supply_to_user_0(A,B,C,D,C,D,E),nop,E) :-
    not in_set(B,D).
trans(r_handle_data_correct_0(A,r(B,C,D,E,F,G),H,packet(I,J,K),L,M),nop,r_handle_data_expected_seqno_0(A,r(B,C,D,E,N,G),H,packet(I,J,K),M)) :-
    L == true,
    remove_from_set(J,F,N),
    eval(J == C + 1).
trans(r_handle_data_correct_0(A,r(B,C,D,E,F,G),H,packet(I,J,K),L,M),nop,r_handle_data_unexpected_seqno_0(A,r(B,C,D,E,N,G),H,packet(I,J,K),M)) :-
    L == true,
    remove_from_set(J,F,N),
    not eval(J == C + 1).
trans(r_handle_data_correct_0(A,r(B,C,D,E,F,G),r(B,C,C,E,H,G),packet(I,J,K),L,M),out(A,packet(type_NAK,J,C)),M) :-
    not L == true,
    ((J \== C
      ;
      in_set(J,G)
     )
     ;
     in_set(J,F)
    ),
    add_to_set(J,F,H).
trans(r_handle_data_correct_0(A,r(B,C,C,D,E,F),r(B,C,C,D,E,F),packet(G,H,I),J,K),nop,K) :-
    not J == true,
    not ((H \== C  ';'  in_set(H,F))  ';'  in_set(H,E)).
trans(r_handle_data_expected_seqno_0(A,r(B,C,D,E,F,G),H,packet(I,J,K),L),action(user_recv),r_supply_to_user_0(A,M,N,G,O,P,r_handle_data_expected_seqno_6(H,B,D,E,F,A,O,P,L))) :-
    eval(N is C + 1),
    eval(M is N + 1).
trans(r_handle_data_expected_seqno_0(A,r(B,C,D,E,F,G),H,packet(I,J,K),L),action(user_recv),r_supply_to_user_0(A,M,N,G,O,P,r_handle_data_expected_seqno_6(H,B,D,E,F,A,O,P,L))) :-
    eval(N is C + 1),
    eval(M is N + 1).
trans(r_handle_data_expected_seqno_6(r(A,B,B,C,D,E),A,F,C,D,G,B,E,H),out(G,packet(type_ACK,B,B)),H) :-
    eval(B - F >= window_size // 2).
trans(r_handle_data_expected_seqno_6(r(A,B,B,C,D,E),A,B,C,D,F,B,E,G),nop,G) :-
    not eval(B - B >= window_size // 2).
trans(imain_7_0(s_timeout_0(A,B,s(C,D),E,F),G,H,I,J),nop,imain_7_0(s_getpkt_0(A,B,s(C,D),E,F),G,H,I,J)).
trans(imain_7_0(s_handle_nak_0(A,s(B,C),s(B,C),packet(D,E,F),G),H,I,J,K),nop,imain_7_0(G,H,I,J,K)) :-
    not in_interval(E,open(B,C)).
trans(imain_7_0(s_sendmsg_while1_0(A,B,s(C,D),E,F),G,H,I,J),nop,imain_7_0(s_getpkt_0(A,B,s(C,D),K,s_sendmsg_while1_0(A,B,K,E,F)),G,H,I,J)) :-
    (eval(C - D > window_size)
     ;
     C == D
    ).
trans(imain_7_0(s_sendmsg_while1_0(A,B,s(C,D),E,F),G,H,I,J),action(timeout),imain_7_0(s_timeout_0(A,B,s(C,D),K,s_sendmsg_while1_0(A,B,K,E,F)),G,H,I,J)) :-
    (eval(C - D > window_size)
     ;
     C == D
    ).
trans(imain_7_0(s_sendmsg_while1_0(A,B,s(C,D),s(C,D),E),F,G,H,I),nop,imain_7_0(E,F,G,H,I)) :-
    not (eval(C - D > window_size)  ';'  C == D).
trans(imain_7_0(s_sendmsg_0(A,B,C,D,E),F,G,H,I),action(user_send),imain_7_0(s_sendmsg_while1_0(A,B,C,J,s_sendmsg_2(D,B,J,E)),F,G,H,I)).
trans(imain_7_0(sender_0(A,B,C,D),E,F,G,H),nop,imain_7_0(s_sendmsg_0(A,B,C,I,sender_0(A,B,I,D)),E,F,G,H)).
trans(imain_7_0(sender_0(A,B,C,D),E,F,G,H),nop,imain_7_0(s_getpkt_0(A,B,C,I,sender_0(A,B,I,D)),E,F,G,H)).
trans(imain_7_0(sender_0(A,B,C,D),E,F,G,H),action(progress),imain_7_0(s_timeout_0(A,B,C,I,sender_0(A,B,I,D)),E,F,G,H)).
trans(imain_7_0(sender_0(A,B,s(1,0),C),D,E,F,G),nop,imain_7_0(s_sendmsg_0(A,B,s(1,0),H,sender_0(A,B,H,C)),D,E,F,G)).
trans(imain_7_0(sender_0(A,B,s(1,0),C),D,E,F,G),nop,imain_7_0(s_getpkt_0(A,B,s(1,0),H,sender_0(A,B,H,C)),D,E,F,G)).
trans(imain_7_0(sender_0(A,B,s(1,0),C),D,E,F,G),action(progress),imain_7_0(s_timeout_0(A,B,s(1,0),H,sender_0(A,B,H,C)),D,E,F,G)).
trans(imain_7_0(A,medium_1(B,C,D,E),F,G,H),action(corrupt),imain_7_0(A,medium_3(B,C,D,E),F,G,H)).
trans(imain_7_0(A,medium_1(B,C,D,E),F,G,H),action(drop),imain_7_0(A,medium_0(C,B,E),F,G,H)).
trans(imain_7_0(A,B,medium_1(C,D,E,F),G,H),action(corrupt),imain_7_0(A,B,medium_3(C,D,E,F),G,H)).
trans(imain_7_0(A,B,medium_1(C,D,E,F),G,H),action(drop),imain_7_0(A,B,medium_0(D,C,F),G,H)).
trans(imain_7_0(A,B,C,r_send_naks_0(D,E,F,G,H,I,G,J,K,L),M),nop,imain_7_0(A,B,C,r_send_naks_0(D,N,F,G,H,I,G,J,K,L),M)) :-
    E \== F,
    (in_set(E,H)
     ;
     in_set(E,I)
    ),
    eval(N is E + 1).
trans(imain_7_0(A,B,C,r_send_naks_0(D,E,F,G,H,I,G,H,G,J),K),nop,imain_7_0(A,B,C,J,K)) :-
    not E \== F.
trans(imain_7_0(A,B,C,r_handle_data_unexpected_seqno_0(D,r(E,F,G,H,I,J),K,packet(L,M,N),O),P),nop,imain_7_0(A,B,C,r_send_naks_0(D,Q,M,F,I,R,G,S,T,r_handle_data_unexpected_seqno_7(K,E,H,F,R,S,T,O)),P)) :-
    M \== F,
    not in_set(M,J),
    add_to_set(M,J,R),
    eval(Q is F + 1).
trans(imain_7_0(A,B,C,r_handle_data_unexpected_seqno_7(r(D,E,F,G,H,I),D,G,E,I,H,F,J),K),nop,imain_7_0(A,B,C,J,K)).
trans(imain_7_0(A,B,C,r_handle_data_unexpected_seqno_0(D,r(E,F,G,H,I,J),r(E,F,G,H,I,J),packet(K,L,M),N),O),nop,imain_7_0(A,B,C,N,O)) :-
    not (L \== F  ','  not in_set(L,J)).
trans(imain_7_0(A,B,C,r_handle_data_0(D,r(E,F,G,H,I,J),K,packet(L,M,N),O,P),Q),nop,imain_7_0(A,B,C,r_handle_data_correct_0(D,r(E,F,G,H,I,J),K,packet(L,M,N),O,P),Q)) :-
    not eval(M - G > window_size),
    M \== G.
trans(imain_7_0(A,B,C,r_handle_data_0(D,r(E,F,G,H,I,J),r(E,F,G,H,I,J),packet(K,L,M),N,O),P),nop,imain_7_0(A,B,C,O,P)) :-
    not (not eval(L - G > window_size)  ','  L \== G).
trans(imain_7_0(A,B,C,r_timeout_0(D,E,r(F,G,H,I,J,K),L,M),N),nop,imain_7_0(A,B,C,r_getpkt_0(D,E,r(F,G,H,I,0,K),L,M),N)).
trans(imain_7_0(A,B,C,receiver_0(D,E,F,G),H),nop,imain_7_0(A,B,C,r_getpkt_0(D,E,F,I,receiver_0(D,E,I,G)),H)).
trans(imain_7_0(A,B,C,receiver_0(D,E,F,G),H),action(progress),imain_7_0(A,B,C,r_timeout_0(D,E,F,I,receiver_0(D,E,I,G)),H)).
trans(imain_7_0(A,B,C,receiver_0(D,E,r(1,0,0,0,0,0),F),G),nop,imain_7_0(A,B,C,r_getpkt_0(D,E,r(1,0,0,0,0,0),H,receiver_0(D,E,H,F)),G)).
trans(imain_7_0(A,B,C,receiver_0(D,E,r(1,0,0,0,0,0),F),G),action(progress),imain_7_0(A,B,C,r_timeout_0(D,E,r(1,0,0,0,0,0),H,receiver_0(D,E,H,F)),G)).
trans(imain_7_0(A,B,C,r_handle_nak_0(D,r(E,F,F,G,H,I),r(E,F,F,G,H,I),packet(J,K,L),M),N),nop,imain_7_0(A,B,C,M,N)) :-
    not fixed(fix).
trans(imain_7_0(A,B,C,r_supply_to_user_0(D,E,F,G,H,I,J),K),action(user_recv),imain_7_0(A,B,C,r_supply_to_user_0(D,L,E,M,H,I,J),K)) :-
    in_set(E,G),
    remove_from_set(E,G,M),
    eval(L is E + 1).
trans(imain_7_0(A,B,C,r_supply_to_user_0(D,E,F,G,F,G,H),I),nop,imain_7_0(A,B,C,H,I)) :-
    not in_set(E,G).
trans(imain_7_0(A,B,C,r_handle_data_correct_0(D,r(E,F,G,H,I,J),K,packet(L,M,N),O,P),Q),nop,imain_7_0(A,B,C,r_handle_data_expected_seqno_0(D,r(E,F,G,H,R,J),K,packet(L,M,N),P),Q)) :-
    O == true,
    remove_from_set(M,I,R),
    eval(M == F + 1).
trans(imain_7_0(A,B,C,r_handle_data_correct_0(D,r(E,F,G,H,I,J),K,packet(L,M,N),O,P),Q),nop,imain_7_0(A,B,C,r_handle_data_unexpected_seqno_0(D,r(E,F,G,H,R,J),K,packet(L,M,N),P),Q)) :-
    O == true,
    remove_from_set(M,I,R),
    not eval(M == F + 1).
trans(imain_7_0(A,B,C,r_handle_data_correct_0(D,r(E,F,F,G,H,I),r(E,F,F,G,H,I),packet(J,K,L),M,N),O),nop,imain_7_0(A,B,C,N,O)) :-
    not M == true,
    not ((K \== F  ';'  in_set(K,I))  ';'  in_set(K,H)).
trans(imain_7_0(A,B,C,r_handle_data_expected_seqno_0(D,r(E,F,G,H,I,J),K,packet(L,M,N),O),P),action(user_recv),imain_7_0(A,B,C,r_supply_to_user_0(D,Q,R,J,S,T,r_handle_data_expected_seqno_6(K,E,G,H,I,D,S,T,O)),P)) :-
    eval(R is F + 1),
    eval(Q is R + 1).
trans(imain_7_0(A,B,C,r_handle_data_expected_seqno_0(D,r(E,F,G,H,I,J),K,packet(L,M,N),O),P),action(user_recv),imain_7_0(A,B,C,r_supply_to_user_0(D,Q,R,J,S,T,r_handle_data_expected_seqno_6(K,E,G,H,I,D,S,T,O)),P)) :-
    eval(R is F + 1),
    eval(Q is R + 1).
trans(imain_7_0(A,B,C,r_handle_data_expected_seqno_6(r(D,E,E,F,G,H),D,E,F,G,I,E,H,J),K),nop,imain_7_0(A,B,C,J,K)) :-
    not eval(E - E >= window_size // 2).
trans(imain_7_0(s_timeout_0(A,B,s(C,D),E,F),medium_0(B,G,H),I,J,K),tau,imain_7_0(s_timeout_6(E,s(C,D),B,L,F),medium_1(G,B,packet(type_NAK,1,0),H),I,J,K)) :-
    not eval(C == D + 1),
    eval(L is D + 1).
trans(imain_7_0(s_timeout_6(s(A,B),s(A,B),C,D,E),medium_0(C,F,G),H,I,J),tau,imain_7_0(E,medium_1(F,C,packet(type_DATA,D,0),G),H,I,J)).
trans(imain_7_0(s_timeout_0(A,B,s(C,D),s(C,D),E),medium_0(B,F,G),H,I,J),tau,imain_7_0(E,medium_1(F,B,packet(type_NAK,1,0),G),H,I,J)) :-
    eval(C == D + 1).
trans(imain_7_0(s_handle_nak_0(A,s(B,C),s(B,C),packet(D,E,F),G),medium_0(A,H,I),J,K,L),tau,imain_7_0(G,medium_1(H,A,packet(type_DATA,E,0),I),J,K,L)) :-
    in_interval(E,open(B,C)).
trans(imain_7_0(s_getpkt_0(A,B,C,D,E),F,medium_1(A,G,packet(H,I,J),K),L,M),tau,imain_7_0(s_handle_nak_0(B,N,D,packet(H,I,J),E),F,medium_0(G,A,K),L,M)) :-
    s_handle_ack(J,C,N),
    H == type_NAK.
trans(imain_7_0(s_getpkt_0(A,B,C,D,E),F,medium_3(A,G,packet(H,I,J),K),L,M),tau,imain_7_0(s_handle_nak_0(B,N,D,packet(H,I,J),E),F,medium_0(G,A,K),L,M)) :-
    s_handle_ack(J,C,N),
    H == type_NAK.
trans(imain_7_0(s_getpkt_0(A,B,C,D,E),F,medium_1(A,G,packet(H,I,J),K),L,M),tau,imain_7_0(E,F,medium_0(G,A,K),L,M)) :-
    s_handle_ack(J,C,D),
    not H == type_NAK.
trans(imain_7_0(s_getpkt_0(A,B,C,D,E),F,medium_3(A,G,packet(H,I,J),K),L,M),tau,imain_7_0(E,F,medium_0(G,A,K),L,M)) :-
    s_handle_ack(J,C,D),
    not H == type_NAK.
trans(imain_7_0(s_getpkt_0(A,B,C,C,D),E,medium_3(A,F,packet(G,H,I),J),K,L),tau,imain_7_0(D,E,medium_0(F,A,J),K,L)).
trans(imain_7_0(s_getpkt_0(A,B,C,C,D),E,medium_3(A,F,packet(G,H,I),J),K,L),tau,imain_7_0(D,E,medium_0(F,A,J),K,L)).
trans(imain_7_0(s_sendmsg_2(s(A,B),C,s(D,B),E),medium_0(C,F,G),H,I,J),tau,imain_7_0(E,medium_1(F,C,packet(type_DATA,D,0),G),H,I,J)) :-
    eval(A is D + 1).
trans(imain_7_0(A,medium_1(B,C,packet(D,E,F),G),H,r_getpkt_0(B,I,J,K,L),M),tau,imain_7_0(A,medium_0(C,B,G),H,r_handle_data_0(I,J,K,packet(D,E,F),true,L),M)) :-
    D == type_DATA.
trans(imain_7_0(A,medium_1(B,C,packet(D,E,F),G),H,r_getpkt_0(B,I,J,K,L),M),tau,imain_7_0(A,medium_0(C,B,G),H,r_handle_nak_0(I,J,K,packet(D,E,F),L),M)) :-
    not D == type_DATA,
    D == type_NAK.
trans(imain_7_0(A,medium_1(B,C,packet(D,E,F),G),H,r_getpkt_0(B,I,J,J,K),L),tau,imain_7_0(A,medium_0(C,B,G),H,K,L)) :-
    not D == type_DATA,
    not D == type_NAK.
trans(imain_7_0(A,medium_3(B,C,packet(D,E,F),G),H,r_getpkt_0(B,I,J,K,L),M),tau,imain_7_0(A,medium_0(C,B,G),H,r_handle_data_0(I,J,K,packet(D,E,F),false,L),M)) :-
    D == type_DATA.
trans(imain_7_0(A,medium_3(B,C,packet(D,E,F),G),H,r_getpkt_0(B,I,J,K,L),M),tau,imain_7_0(A,medium_0(C,B,G),H,r_handle_nak_0(I,J,K,packet(D,E,F),L),M)) :-
    not D == type_DATA,
    D == type_NAK.
trans(imain_7_0(A,medium_3(B,C,packet(D,E,F),G),H,r_getpkt_0(B,I,J,J,K),L),tau,imain_7_0(A,medium_0(C,B,G),H,K,L)) :-
    not D == type_DATA,
    not D == type_NAK.
trans(imain_7_0(A,medium_3(B,C,packet(D,E,F),G),H,r_getpkt_0(B,I,J,J,K),L),tau,imain_7_0(A,medium_0(C,B,G),H,K,L)).
trans(imain_7_0(A,medium_3(B,C,packet(D,E,F),G),H,r_getpkt_0(B,I,J,J,K),L),tau,imain_7_0(A,medium_0(C,B,G),H,K,L)).
trans(imain_7_0(A,B,medium_0(C,D,E),r_send_naks_0(C,F,G,H,I,J,K,L,M,N),O),tau,imain_7_0(A,B,medium_1(D,C,packet(type_NAK,F,H),E),r_send_naks_0(C,P,G,H,Q,J,H,L,M,N),O)) :-
    F \== G,
    not in_set(F,I),
    not in_set(F,J),
    add_to_set(F,I,Q),
    eval(P is F + 1).
trans(imain_7_0(A,B,medium_0(C,D,E),r_timeout_0(F,C,r(G,H,I,J,K,L),r(G,H,H,J,M,L),N),O),tau,imain_7_0(A,B,medium_1(D,C,packet(type_NAK,P,H),E),N,O)) :-
    eval(P is H + 1),
    add_to_set(P,0,M).
trans(imain_7_0(A,B,medium_0(C,D,E),r_handle_nak_0(C,r(F,G,H,I,J,K),r(F,G,G,I,J,K),packet(L,M,N),O),P),tau,imain_7_0(A,B,medium_1(D,C,packet(type_ACK,G,G),E),O,P)) :-
    fixed(fix).
trans(imain_7_0(A,B,medium_0(C,D,E),r_handle_data_correct_0(C,r(F,G,H,I,J,K),r(F,G,G,I,L,K),packet(M,N,O),P,Q),R),tau,imain_7_0(A,B,medium_1(D,C,packet(type_NAK,N,G),E),Q,R)) :-
    not P == true,
    ((N \== G
      ;
      in_set(N,K)
     )
     ;
     in_set(N,J)
    ),
    add_to_set(N,J,L).
trans(imain_7_0(A,B,medium_0(C,D,E),r_handle_data_expected_seqno_6(r(F,G,G,H,I,J),F,K,H,I,C,G,J,L),M),tau,imain_7_0(A,B,medium_1(D,C,packet(type_ACK,G,G),E),L,M)) :-
    eval(G - K >= window_size // 2).
trans(imain_0(A),nop,imain_7_0(sender_0(B,C,s(1,0),end),medium_0(C,D,end),medium_0(E,B,end),r_getpkt_0(D,E,r(1,0,0,0,0,0),F,receiver_0(D,E,F,end)),A)) :-
    handlechan([],[B,C],G),
    handlechan([B,C],[D],H),
    handlechan([B,D],[E],I).
trans(imain_7_0(end,end,end,end,A),nop,A).

partrans(A,B,C,D,E,F) :-
    ((var(B)
      ;
      B == tau
     )
     -> (B = G,
         F = D,
         (G = action(H)
          ;
          (G = nop
           ;
           G = tau
          )
         ),
         trans(C,G,E)
         ;
         G = out(I,J),
         trans(C,G,E),
         (I > A
          -> B = tau,
             trans(D,in(I,J),F)
          ;  (B = G,
              F = D
              ;
              B = tau,
              trans(D,in(I,J),F)
             )
         )
        )
     ;  F = D,
        trans(C,B,E),
        ((B = out(I,K)
          ;
          B = in(I,L)
         )
         -> I =< A
         ;  true
        )
    ).

handlechan(A,B,C) :-
    maxchan(A,C),
    enumlocal(B,C).

maxchan(A,B) :-
    max(A,0,B).

max([],A,A).
max([A|B],C,D) :-
    (A > C
     -> E = A
     ;  E = C
    ),
    max(B,E,D).

enumlocal([],A).
enumlocal([A|B],C) :-
    A  is  C + 1,
    enumlocal(B,A).

startstate(iproto(A,B),iproto_0(A,B,end)).
startstate(imain,imain_0(end)).
startstate(s_timeout(A,B,C,D),s_timeout_0(A,B,C,D,end)).
startstate(s_handle_nak(A,B,B,C),s_handle_nak_0(A,B,B,C,end)).
startstate(s_getpkt(A,B,C,D),s_getpkt_0(A,B,C,D,end)).
startstate(s_sendmsg_while1(A,B,C,D),s_sendmsg_while1_0(A,B,C,D,end)).
startstate(s_sendmsg(A,B,C,D),s_sendmsg_0(A,B,C,D,end)).
startstate(sender(A,B,C),sender_0(A,B,C,end)).
startstate(medium(A,B),medium_0(A,B,end)).
startstate(r_send_naks(A,B,C,D,E,F,G,H,I),r_send_naks_0(A,B,C,D,E,F,G,H,I,end)).
startstate(r_handle_data_unexpected_seqno(A,B,C,D),r_handle_data_unexpected_seqno_0(A,B,C,D,end)).
startstate(r_handle_data(A,B,C,D,E),r_handle_data_0(A,B,C,D,E,end)).
startstate(r_getpkt(A,B,C,D),r_getpkt_0(A,B,C,D,end)).
startstate(r_timeout(A,B,C,D),r_timeout_0(A,B,C,D,end)).
startstate(receiver(A,B,C),receiver_0(A,B,C,end)).
startstate(r_handle_nak(A,B,C,D),r_handle_nak_0(A,B,C,D,end)).
startstate(r_supply_to_user(A,B,C,D,E,F),r_supply_to_user_0(A,B,C,D,E,F,end)).
startstate(r_handle_data_correct(A,B,C,D,E),r_handle_data_correct_0(A,B,C,D,E,end)).
startstate(r_handle_data_expected_seqno(A,B,C,D),r_handle_data_expected_seqno_0(A,B,C,D,end)).

fDef(livelock,fOr(form(ll2),fDiamSetMinus([],form(livelock)))).
fDef(ll2,neg_form(neg_ll2)).
fDef(neg_ll2,fBoxSet([action(timeout),nop,tau],form(neg_ll2))).

% ----------------------------------------------------------
