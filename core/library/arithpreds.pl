:- module(arithpreds,
        [(-)/2,(+)/2,(--)/2,(++)/2,(+)/3,(-)/3,(*)/3,(//)/3,(/)/3,rem/3,mod/3,
         abs/2,sign/2,float_integer_part/2,float_fractional_part/2,integer/2,
         truncate/2,float/2,floor/2,round/2,ceiling/2,(**)/3,(>>)/3,(<<)/3,
         (/\)/3,(\/)/3,(\)/2,(#)/3,exp/2,log/2,sqrt/2,sin/2,cos/2,atan/2,
         gcd/3,arithfunctor/1],
        []).

-(A,B) :-
        B is -(A).

+(A,B) :-
        B is +A.

--(A,B) :-
        B is --A.

++(A,B) :-
        B is ++A.

+(A,B,C) :-
        C is A+B.

-(A,B,C) :-
        C is A-B.

*(A,B,C) :-
        C is A*B.

//(A,B,C) :-
        C is A//B.

/(A,B,C) :-
        C is A/B.

rem(A,B,C) :-
        C is A rem B.

mod(A,B,C) :-
        C is A mod B.

abs(A,B) :-
        B is abs(A).

sign(A,B) :-
        B is sign(A).

float_integer_part(A,B) :-
        B is float_integer_part(A).

float_fractional_part(A,B) :-
        B is float_fractional_part(A).

integer(A,B) :-
        B is integer(A).

truncate(A,B) :-
        B is truncate(A).

float(A,B) :-
        B is float(A).

floor(A,B) :-
        B is floor(A).

round(A,B) :-
        B is round(A).

ceiling(A,B) :-
        B is ceiling(A).

**(A,B,C) :-
        C is A**B.

>>(A,B,C) :-
        C is A>>B.

<<(A,B,C) :-
        C is A<<B.

/\(A,B,C) :-
        C is A/\B.

\/(A,B,C) :-
        C is A\/B.

\(A,B) :-
        B is \A.

#(A,B,C) :-
        C is A#B.

exp(A,B) :-
        B is exp(A).

log(A,B) :-
        B is log(A).

sqrt(A,B) :-
        B is sqrt(A).

sin(A,B) :-
        B is sin(A).

cos(A,B) :-
        B is cos(A).

atan(A,B) :-
        B is atan(A).

gcd(A,B,C) :-
        C is gcd(A,B).

arithfunctor(-_) .
arithfunctor(+_) .
arithfunctor(--_) .
arithfunctor(++_) .
arithfunctor(_+_) .
arithfunctor(_-_) .
arithfunctor(_*_) .
arithfunctor(_//_) .
arithfunctor(_/_) .
arithfunctor(_ rem _) .
arithfunctor(_ mod _) .
arithfunctor(abs(_)) .
arithfunctor(sign(_)) .
arithfunctor(float_integer_part(_)) .
arithfunctor(float_fractional_part(_)) .
arithfunctor(integer(_)) .
arithfunctor(truncate(_)) .
arithfunctor(float(_)) .
arithfunctor(floor(_)) .
arithfunctor(round(_)) .
arithfunctor(ceiling(_)) .
arithfunctor(_**_) .
arithfunctor(_>>_) .
arithfunctor(_<<_) .
arithfunctor(_/\_) .
arithfunctor(_\/_) .
arithfunctor(\(_)) .
arithfunctor(_#_ ) .
arithfunctor(exp(_)) .
arithfunctor(log(_)) .
arithfunctor(sqrt(_)) .
arithfunctor(sin(_)) .
arithfunctor(cos(_)) .
arithfunctor(atan(_)) .
arithfunctor(gcd(_,_)) .

% 
% :- use_module(library(terms), [copy_args/3]).
% :- use_module(library(pretty_print)).
% wp :-
%         arithfunctor(E),
%         functor(E,F,A),
%         A1 is A+1,
%         functor(P,F,A1),
%         arg(A1, P, R),
%         copy_args(A, P, E),
%         pretty_print([(P :- R is E)],[]),
%         nl,
%         fail.
