:- module(boyer, [test_boyer/1], []).

% Program: Boyer (toy theorem prover)
% Author:  E. Tick (after Lisp by R. Boyer)
% Date:    November 12 1985

test_boyer(R) :- wff(X), ( tautology(X) -> R = proved(X) ; R = not_proved(X) ).

wff(implies(and(implies(X,Y),
    and(implies(Y,Z),
        and(implies(Z,U),
        implies(U,W)))),
    implies(X,W))) :-
    X = f(plus(plus(a,b),plus(c,zero))),
    Y = f(times(times(a,b),plus(c,d))),
    Z = f(reverse(append(append(a,b),[]))),
    U = equal(plus(a,b),difference(x,y)),
    W = lessp(remainder(a,b),member(a,length(b))).

tautology(Wff) :-
    % display('rewriting...'),nl,
    rewrite(Wff,NewWff),
    % display('proving...'),nl,
    tautology_2(NewWff,[],[]).

tautology_2(Wff,Tlist,Flist) :-
    ( truep(Wff,Tlist) -> true
    ; falsep(Wff,Flist) -> fail
    ; Wff = if(If,Then,Else) ->
      ( truep(If,Tlist) -> tautology_2(Then,Tlist,Flist)
      ; falsep(If,Flist) -> tautology_2(Else,Tlist,Flist)
      ; tautology_2(Then,[If|Tlist],Flist), % both must hold
        tautology_2(Else,Tlist,[If|Flist])
      )
    ),
    !.

rewrite(Atom,Atom) :-
    atomic(Atom),!.
rewrite(Old,New) :-
    functor(Old,F,N),
    functor(Mid,F,N),
    rewrite_args(N,Old,Mid), 
    ( equal2(Mid,Next), % should be ->, but is compiler smart enough?
      rewrite(Next,New) % to generate cut for ->?
    ; New=Mid
    ),!.

rewrite_args(0,_,_) :- !.
rewrite_args(N,Old,Mid) :-
    arg(N,Old,OldArg),
    arg(N,Mid,MidArg),
    rewrite(OldArg,MidArg),
    N1 is N - 1,
    rewrite_args(N1,Old,Mid).

truep(t,_) :- !.
truep(Wff,Tlist) :- member2(Wff,Tlist).

falsep(f,_) :- !.
falsep(Wff,Flist) :- member2(Wff,Flist).

member2(X,[X|_]) :- !.
member2(X,[_|T]) :- member2(X,T).


equal2( and(P,Q), % 106 rules
    if(P,if(Q,t,f),f)
    ).
equal2( append(append(X,Y),Z),
    append(X,append(Y,Z))
    ).
equal2( assignment(X,append(A,B)),
    if(assignedp(X,A),
       assignment(X,A),
       assignment(X,B))
    ).
equal2( assume_false(Var,Alist),
    cons(cons(Var,f),Alist)
    ).
equal2( assume_true(Var,Alist),
    cons(cons(Var,t),Alist)
    ).
equal2( boolean(X),
    or(equal(X,t),equal(X,f))
    ).
equal2( car(gopher(X)),
    if(listp(X),
       car(flatten(X)),
       zero)
    ).
equal2( compile(Form),
    reverse(codegen(optimize(Form),[]))
    ).
equal2( count_list(Z,sort_lp(X,Y)),
    plus(count_list(Z,X),
     count_list(Z,Y))
    ).
equal2( countps_(L,Pred),
      countps_loop(L,Pred,zero)
    ).
equal2( difference(A,B),
    C
    ) :- difference3(A,B,C).
equal2( divides(X,Y),
    zerop(remainder(Y,X))
    ).
equal2( dsort(X),
    sort2(X)
    ).
equal2( eqp(X,Y),
    equal(fix(X),fix(Y))
    ).
equal2( equal(A,B),
    C
    ) :- eq3(A,B,C).
equal2( even1(X),
    if(zerop(X),t,odd(decr(X)))
    ).
equal2( exec(append(X,Y),Pds,Envrn),
    exec(Y,exec(X,Pds,Envrn),Envrn)
    ).
equal2( exp(A,B),
    C
    ) :- exp3(A,B,C).
equal2( fact_(I),
    fact_loop(I,1)
    ).
equal2( falsify(X),
    falsify1(normalize(X),[])
    ).
equal2( fix(X),
    if(numberp(X),X,zero)
    ).
equal2( flatten(cdr(gopher(X))),
    if(listp(X),
       cdr(flatten(X)),
       cons(zero,[]))
    ).
equal2( gcd(A,B),
    C
    ) :- gcd3(A,B,C).
equal2( get(J,set(I,Val,Mem)),
    if(eqp(J,I),Val,get(J,Mem))
    ).
equal2( greatereqp(X,Y),
    not(lessp(X,Y))
    ).
equal2( greatereqpr(X,Y),
    not(lessp(X,Y))
    ).
equal2( greaterp(X,Y),
    lessp(Y,X)
    ).
equal2( if(if(A,B,C),D,E),
    if(A,if(B,D,E),if(C,D,E))
    ).
equal2( iff(X,Y),
    and(implies(X,Y),implies(Y,X))
    ).
equal2( implies(P,Q),
    if(P,if(Q,t,f),t)
    ).
equal2( last(append(A,B)),
    if(listp(B),
       last(B),
       if(listp(A),
      cons(car(last(A))),
      B))
    ).
equal2( length(A),
    B
    ) :- mylength(A,B).
equal2( lesseqp(X,Y),
    not(lessp(Y,X))
    ).
equal2( lessp(A,B),
    C
    ) :- lessp3(A,B,C).
equal2( listp(gopher(X)),
    listp(X)
    ).
equal2( mc_flatten(X,Y),
    append(flatten(X),Y)
    ).
equal2( meaning(A,B),
    C
    ) :- meaning3(A,B,C).
equal2( member(A,B),
    C
    ) :- mymember(A,B,C).
equal2( not(P),
    if(P,f,t)
    ).
equal2( nth(A,B),
    C
    ) :- nth3(A,B,C).
equal2( numberp(greatest_factor(X,Y)),
    not(and(or(zerop(Y),equal(Y,1)),
    not(numberp(X))))
    ).
equal2( or(P,Q),
    if(P,t,if(Q,t,f),f)
    ).
equal2( plus(A,B),
    C
    ) :- plus3(A,B,C).
equal2( power_eval(A,B),
    C
    ) :- power_eval3(A,B,C).
equal2( prime(X),
    and(not(zerop(X)),
    and(not(equal(X,add1(zero))),
        prime1(X,decr(X))))
    ).
equal2( prime_list(append(X,Y)),
    and(prime_list(X),prime_list(Y))
    ).
equal2( quotient(A,B),
    C
    ) :- quotient3(A,B,C).
equal2( remainder(A,B),
    C
    ) :- remainder3(A,B,C).
equal2( reverse_(X),
    reverse_loop(X,[])
    ).
equal2( reverse(append(A,B)),
    append(reverse(B),reverse(A))
    ).
equal2( reverse_loop(A,B),
    C
    ) :- reverse_loop3(A,B,C).
equal2( samefringe(X,Y),
    equal(flatten(X),flatten(Y))
    ).
equal2( sigma(zero,I),
    quotient(times(I,add1(I)),2)
    ).
equal2( sort2(delete(X,L)),
    delete(X,sort2(L))
    ).
equal2( tautology_checker(X),
    tautologyp(normalize(X),[])
    ).
equal2( times(A,B),
    C
    ) :- times3(A,B,C).
equal2( times_list(append(X,Y)),
    times(times_list(X),times_list(Y))
    ).
equal2( value(normalize(X),A),
    value(X,A)
    ).
equal2( zerop(X),
    or(equal(X,zero),not(numberp(X)))
    ).

difference3(X, X, zero ) :- !.
difference3(plus(X,Y), X, fix(Y) ) :- !.
difference3(plus(Y,X), X, fix(Y) ) :- !.
difference3(plus(X,Y), plus(X,Z), difference(Y,Z) ) :- !.
difference3(plus(B,plus(A,C)), A, plus(B,C) ) :- !.
difference3(add1(plus(Y,Z)), Z, add1(Y) ) :- !.
difference3(add1(add1(X)), 2, fix(X) ).

eq3(plus(A,B), zero, and(zerop(A),zerop(B)) ) :- !.
eq3(plus(A,B), plus(A,C), equal(fix(B),fix(C)) ) :- !.
eq3(zero, difference(X,Y),not(lessp(Y,X)) ) :- !.
eq3(X, difference(X,Y),and(numberp(X),
                    and(or(equal(X,zero),
                        zerop(Y)))) ) :- !.
eq3(times(X,Y), zero, or(zerop(X),zerop(Y)) ) :- !.
eq3(append(A,B), append(A,C), equal(B,C) ) :- !.
eq3(flatten(X), cons(Y,[]), and(nlistp(X),equal(X,Y)) ) :- !.
eq3(greatest_factor(X,Y),zero, and(or(zerop(Y),equal(Y,1)),
                    equal(X,zero)) ) :- !.
eq3(greatest_factor(X,_),1, equal(X,1) ) :- !.
eq3(Z, times(W,Z), and(numberp(Z),
                    or(equal(Z,zero),
                       equal(W,1))) ) :- !.
eq3(X, times(X,Y), or(equal(X,zero),
                   and(numberp(X),equal(Y,1))) ) :- !.
eq3(times(A,B), 1, and(not(equal(A,zero)),
                  and(not(equal(B,zero)),
                    and(numberp(A),
                      and(numberp(B),
                        and(equal(decr(A),zero),
                      equal(decr(B),zero))))))
                                ) :- !.
eq3(difference(X,Y), difference(Z,Y),if(lessp(X,Y),
                    not(lessp(Y,Z)),
                    if(lessp(Z,Y),
                        not(lessp(Y,X)),
                        equal(fix(X),fix(Z)))) ) :- !.
eq3(lessp(X,Y), Z, if(lessp(X,Y),
                   equal(t,Z),
                   equal(f,Z)) ).

exp3(I, plus(J,K), times(exp(I,J),exp(I,K))) :- !.
exp3(I, times(J,K), exp(exp(I,J),K) ).

gcd3(X, Y, gcd(Y,X) ) :- !.
gcd3(times(X,Z), times(Y,Z), times(Z,gcd(X,Y)) ).

mylength(reverse(X),length(X)).
mylength(cons(_,cons(_,cons(_,cons(_,cons(_,cons(_,X7)))))),
     plus(6,length(X7))).

lessp3(remainder(_,Y), Y, not(zerop(Y)) ) :- !.
lessp3(quotient(I,J), I, and(not(zerop(I)),
                or(zerop(J),
                   not(equal(J,1)))) ) :- !.
lessp3(remainder(X,Y), X, and(not(zerop(Y)),
                and(not(zerop(X)),
                      not(lessp(X,Y)))) ) :- !.
lessp3(plus(X,Y), plus(X,Z), lessp(Y,Z) ) :- !.
lessp3(times(X,Z), times(Y,Z), and(not(zerop(Z)),
                    lessp(X,Y)) ) :- !.
lessp3(Y, plus(X,Y), not(zerop(X)) ) :- !.
lessp3(length(delete(X,L)), length(L), member(X,L) ).

meaning3(plus_tree(append(X,Y)),A,
    plus(meaning(plus_tree(X),A),
     meaning(plus_tree(Y),A))
    ) :- !.
meaning3(plus_tree(plus_fringe(X)),A,
    fix(meaning(X,A))
    ) :- !.
meaning3(plus_tree(delete(X,Y)),A,
    if(member(X,Y),
       difference(meaning(plus_tree(Y),A),
          meaning(X,A)),
       meaning(plus_tree(Y),A))).

mymember(X,append(A,B),or(member(X,A),member(X,B))) :- !.
mymember(X,reverse(Y),member(X,Y)) :- !.
mymember(A,intersect(B,C),and(member(A,B),member(A,C))).

nth3(zero,_,zero).
nth3([],I,if(zerop(I),[],zero)).
nth3(append(A,B),I,append(nth(A,I),nth(B,difference(I,length(A))))).

plus3(plus(X,Y),Z,
    plus(X,plus(Y,Z))
    ) :- !.
plus3(remainder(X,Y),
     times(Y,quotient(X,Y)),
    fix(X)
    ) :- !.
plus3(X,add1(Y),
    if(numberp(Y),
       add1(plus(X,Y)),
       add1(X))
    ).

power_eval3(big_plus1(L,I,Base),Base,
    plus(power_eval(L,Base),I)
    ) :- !.
power_eval3(power_rep(I,Base),Base,
    fix(I)
    ) :- !.
power_eval3(big_plus(X,Y,I,Base),Base,
    plus(I,plus(power_eval(X,Base),
        power_eval(Y,Base)))
    ) :- !.
power_eval3(big_plus(power_rep(I,Base),
            power_rep(J,Base),
            zero,
            Base),
       Base,
    plus(I,J)
    ).

quotient3(plus(X,plus(X,Y)),2,plus(X,quotient(Y,2))).
quotient3(times(Y,X),Y,if(zerop(Y),zero,fix(X))).

remainder3(_, 1,zero) :- !.
remainder3(X, X,zero) :- !.
remainder3(times(_,Z), Z,zero) :- !.
remainder3(times(Y,_), Y,zero).

reverse_loop3(X,Y, append(reverse(X),Y) ) :- !.
reverse_loop3(X,[], reverse(X) ).

times3(X, plus(Y,Z), plus(times(X,Y),times(X,Z)) ) :- !.
times3(times(X,Y),Z, times(X,times(Y,Z)) ) :- !.
times3(X, difference(C,W),difference(times(C,X),times(W,X))) :- !.
times3(X, add1(Y), if(numberp(Y),
                plus(X,times(X,Y)),
                fix(X)) ).

