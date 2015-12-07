:- module(det_hook_tr, [det_hook_trans/2], []).

det_hook_trans((A :- B), (A :- B2)) :-
	cut_trans(B, B2).

cut_trans((A0, B0), (A, B)) :- !,
	cut_trans(A0, A),
	cut_trans(B0, B).
% cut_trans((A0 -> B0), (A, ! ; B)) :-
%	cut_trans(A0, A),
%	cut_trans(B0, B).
% cut_trans(('$metacut'(C)), ('$metacut'(C), '$pending_cut_goals')).
cut_trans(('!!'), (!, '$pending_cut_goals')) :- !.
cut_trans(A, A).
