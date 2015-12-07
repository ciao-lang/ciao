:- module(civil_registry, [profession/1, surname/1, sevillian/1,
		family_income/1, married/2, salary/2],
	    [assertions, isomodes, metatypes, hiord, nativeprops]).

:- doc(author, "Alvaro Sevilla San Mateo").

family_information(husband(name(antonio, garcia, fernandez),
		profession(architect), salary(300000)),
	    wife(name(ana, ruiz, lopez),
		profession(teacher), salary(120000)),
	    address(sevilla)).

family_information(husband(name(luis, alvarez, garcia),
		profession(architect), salary(400000)),
	    wife(name(ana, romero, soler),
		profession(housewife), salary(0)),
	    address(sevilla)).

family_information(husband(name(bernardo, bueno, martinez),
		profession(teacher), salary(120000)),
	    wife(name(laura, rodriguez, millan),
		profession(doctor), salary(250000)),
	    address(cuenca)).


family_information(husband(name(miguel, gonzalez, ruiz),
		profession(businessman), salary(400000)),
	    wife(name(belen, salguero, cuevas),
		profession(housewife), salary(0)),
	    address(dos_hermanas)).

profession(X) :- family_information(husband(_,profession(X),_),_,_).
profession(X) :- family_information(_,wife(_,profession(X),_),_).

:- test profession(X) : (X = doctor) + not_fails.
:- test profession(X) : (X = businessman) + not_fails.
:- test profession(X) : (X = computer_scientist) + fails.

surname(X) :-
	family_information(husband(name(_, X, _), _, _), _, _).
surname(X) :-
	family_information(_, wife(name(_, X, _), _, _), _).

:- test surname(X) : (X = alvarez) + not_fails.
:- test surname(X) : (X = salguero) + not_fails.
:- test surname(X) : (X = sanchez) + fails.

sevillian(X) :-
	family_information(husband(X, _, _), _, address(sevilla)).
sevillian(X) :-
	family_information(_, wife(X, _, _), address(sevilla)).

:- test sevillian(X) : (X = name(miguel, gonzalez, ruiz)) + fails.
:- test sevillian(X) : (X = name(belen, salguero, cuevas)) + fails.
:- test sevillian(X) : (X = name(pedro, sanchez, rodriguez)) + fails.
:- test sevillian(X) : (X = name(antonio, garcia, fernandez)) + not_fails.

family_income(X) :-
	family_information(
	    husband(_, _, salary(N1)),
	    wife(_, _, salary(N2)), _),
	X is N1+N2.

:- test family_income(X) : (X = 400000) + not_fails.
:- test family_income(X) : (X = 370000) + not_fails.
:- test family_income(X) : (X = 100) + fails.

married(X, Y) :-
	family_information(
	    husband(name(X, _, _), _, _),
	    wife(name(Y, _, _), _, _), _).

:- test married(X, Y) : (X = antonio, Y = ana) + not_fails.
:- test married(X, Y) : (X = miguel, Y = belen) + not_fails.
:- test married(X, Y) : (X = antonio, Y = maria) + fails.

salary(X, Y) :-
	family_information(husband(X, _, salary(Y)), _, _).
salary(X, Y) :-
	family_information(_, wife(X, _, salary(Y)), _).

:- test salary(X, Y) : (X = name(luis, alvarez, garcia), Y = 400000)
	+ not_fails.
:- test salary(X, Y) : (X = name(laura, rodriguez, millan), Y = 250000)
	+ not_fails.
:- test salary(X, Y) : (X = ana, Y = 100) + fails.
