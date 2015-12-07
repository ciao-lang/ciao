% TODO: See the 'todo' comments in this file
:- module(test, [delete_non_ground/3,foo/1], [assertions,regtypes,isomodes]).
:- use_package(doccomments).
:- use_package(expander).

%! title:     Operating system utilities
%! subtitle:  A useful guide for the programmer that needs the system
%
%! author:    Daniel Cabeza
%! author:    Manuel Carro
%! credits:   @bf{The Ciao Team}
%
%! summary:   
%
%  This module contains predicates for invoking services which
%  are typically provided by the operating system.  
%
%! module:
%
%  This @bf{module} contains predicates for invoking services which
%  are typically provided by the operating system.  Note that the
%  predicates which take names of files or directories as arguments in
%  this @tt{foo} module expect "atoms", not +path alias+es. I.e., 
%    - Hello 
%    - Boo
%    - Moo
%    - Too is =3+2=
%  generally these predicates will not call
%  absolute_file_name/2 on names of files or directories taken
%  as arguments. 
% 
%
%! hide: foo/1
%! nodoc: assertions
%! hide: [bar/1,too/2]
%! doinclude: eq/2
% 

:- pred foo/1.

foo(_).

% TODO: Does not work (see bug in process_comments/2)
%
% %! delete_non_ground(L1,E,L2): A nice predicate, L2 is ... .

%% Works: 
%! delete_non_ground/3: A nice predicate.

:- pred delete_non_ground(L1,_E,L2) => (list(L1), list(L2)).

% TODO: We would like it to be:
% %!   L2 is L1 without the ocurrences of E. E can be a nonground term so
% %   that all the elements in L1 it unifies with will be deleted.
% TODO: In order to do this, we must detect strings followed by : as 
% commands and other things as strings.

:- true comp delete_non_ground/3 + sideff(soft).
:- true comp delete_non_ground(L1,E,L2) : (ground(L1), ground(L2)) + eval.

delete_non_ground([A], _, [A]) :-
	var(A), !.
delete_non_ground([], _, []).
delete_non_ground([Head|Tail], Element, Rest) :-
	eq(Head,Element), !,
	delete_non_ground(Tail, Element, Rest).
delete_non_ground([Head|Tail], Element, [Head|Rest]) :-
	delete_non_ground(Tail, Element, Rest).


:- pred eq(@,@).

%! eq/2: Checks arguments unify without biding them.

eq(A, B):- \+ \+ A = B.

/*!
 title: Operating system utilities

 module:

 This module contains predicates for invoking services which
 are typically provided by the operating system.  Note that the
 predicates which take names of files or directories as arguments for
 Jan: this module expect atoms, not +path alias+es. I.e.,
  - Hello 
  - Boo
  - Too is =3+2=
 generally these predicates will not call
 absolute_file_name/2 on names of files or directories taken
 as arguments.

 author: Daniel Cabeza
 author: Manuel Carro
*/

