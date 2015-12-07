
:- module(age,[],[mycin_rulebase]).

age(angel,25) cf 1.
age(uncle_tom,75) cf 1.

age(frank,60) cf -0.3.
age(frank,40) cf 0.3.
age(frank,16) cf -0.3.
age(frank,30) cf 0.7.

:- export(senior/1).

senior(Person) cf 1    :- age(Person,Age), Age > 50.
senior(Person) cf 0.5  :- age(Person,Age), Age > 40.
senior(Person) cf -1   :- age(Person,Age), Age < 25.
senior(Person) cf -0.8 :- age(Person,Age), Age < 45.
