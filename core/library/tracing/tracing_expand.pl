
:- module(tracing_expand,[ expand_tracing/3 ],[ ]).

:- use_module(engine(internals), [module_concat/3]).

expand_tracing((:-D0),(:-D),M):-
	expand_directive(D0,M,D).
expand_tracing((H:-B0),(H:-B),M):-
	functor(H,F,A),
	module_concat(M,F,Fun),
	expand_body(B0,Fun,A,B).
expand_tracing(end_of_file,end_of_file,_M).

expand_directive(spy(F/A),M,initialization(traces:spy(M:F/A))):- !.
expand_directive(D,_M,D).

% TODO: It should be a goal translation
expand_body((A0,B0),F,Ar,(A,B)):- !,
	expand_body(A0,F,Ar,A),
	expand_body(B0,F,Ar,B).
expand_body((A0;B0),F,Ar,(A;B)):- !,
	expand_body(A0,F,Ar,A),
	expand_body(B0,F,Ar,B).
expand_body((A0->B0),F,Ar,(A->B)):- !,
	expand_body(A0,F,Ar,A),
	expand_body(B0,F,Ar,B).
expand_body(!,_F,_Ar,!):- !.
expand_body(A,F,Ar,traces:trace(A,F,Ar)).
