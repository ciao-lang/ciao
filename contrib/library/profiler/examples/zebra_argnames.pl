:- module(zebra_argnames, [zebra/3], [assertions, regtypes]).



/*     There are five consecutive houses, each of a different
color and inhabited by men of different nationalities. They each
own a different pet, have a different favorite drink and drive a
different car.

1.   The Englishman lives in the red house.
2.   The Spaniard owns the dog.
3.   Coffee is drunk in the green house.
4.   The Ukrainian drinks tea.
5.   The green house is immediately to the right of the ivory
     house.
6.   The Porsche driver owns snails.
7.   The Masserati is driven by the man who lives in the yellow
     house.
8.   Milk is drunk in the middle house.
9.   The Norwegian lives in the first house on the left.
10.  The man who drives a Saab lives in the house next to the man
     with the fox.
11.  The Masserati is driven by the man in the house next to the
     house where the horse is kept.
12.  The Honda driver drinks orange juice.
13.  The Japanese drives a Jaguar.
14.  The Norwegian lives next to the blue house.

The problem is: Who owns the Zebra?  Who drinks water?
*/

:- use_package([argnames]).

:- argnames house(color, nation, pet, drink, car).

zebra(Owns_zebra, Drinks_water, Street) :-
        Street = [house${},house${},house${},house${},house${}],
        is_member(house${nation => Owns_zebra, pet => zebra}, Street),
        is_member(house${nation => Drinks_water, drink => water}, Street),
        is_member(house${nation => englishman, color => red}, Street),
        is_member(house${nation => spaniard, pet => dog}, Street),
        is_member(house${drink => coffee, color => green}, Street),
        is_member(house${nation => ukrainian, drink => tea}, Street),
        left_right(house${color => ivory}, house${color => green}, Street),
        is_member(house${car => porsche, pet => snails}, Street),
        is_member(house${car => masserati, color => yellow}, Street),
        Street = [_, _, house${drink => milk}, _, _],
        Street = [house${nation => norwegian}|_],
        next_to(house${car => saab}, house${pet => fox}, Street),
        next_to(house${car => masserati}, house${pet => horse}, Street),
        is_member(house${car => honda, drink => orange_juice}, Street),
        is_member(house${nation => japanese, car => jaguar}, Street),
        next_to(house${nation => norwegian}, house${color => blue}, Street).


is_member(X,[X|_]).
is_member(X,[_|Y]) :- is_member(X,Y).

left_right(L,R,[L,R|_]).
left_right(L,R,[_|T]) :- left_right(L,R,T).

next_to(X,Y,L) :- left_right(X,Y,L).
next_to(X,Y,L) :- left_right(Y,X,L).