:- module(test, [delete_non_ground/3], [assertions, regtypes]).
:- include(.(doccomments)).
:- use_package(expander).

%% <module> Operating system utilities
%
%  This module contains predicates for invoking services which are
%  typically provided by the operating system.  Note that the
%  predicates which take names of files or directories as arguments in
%  this module expect atoms, not @concept{path alias}es. I.e.,
%  generally these predicates will not call
%  @pred{absolute_file_name/2} on names of files or directories taken
%  as arguments.").
%
%  @author Daniel Cabeza
%  @author Manuel Carro

:- doc(title,"Operating system utilities").

%! @title Operating system utilities
%
%  @module This module contains predicates for invoking services which are
%  typically provided by the operating system.  Note that the
%  predicates which take names of files or directories as arguments in
%  this module expect atoms, not @concept{path alias}es. I.e.,
%  generally these predicates will not call
%  @pred{absolute_file_name/2} on names of files or directories taken
%  as arguments.
%
%  @author Daniel Cabeza
%  @author Manuel Carro

/*!
 Title: Operating system utilities

 Module:

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

 Author: Daniel Cabeza
 Author: Manuel Carro
*/

%! Title: Operating system utilities
%
%! Module:
%
%  This module contains predicates for invoking services which
%  are typically provided by the operating system.  Note that the
%  predicates which take names of files or directories as arguments in
%  this @tt{foo} module expect "atoms", not +path alias+es. I.e., 
%    - Hello 
%    - Boo
%    - Too is =3+2=
%  generally these predicates will not call
%  absolute_file_name/2 on names of files or directories taken
%  as arguments.
%
%! Author: Daniel Cabeza
%! Author: Manuel Carro




%! Title: Operating system utilities
%

:- doc(title,"Operating system utilities").

%! Module:
%
%  This module contains predicates for invoking services which
%  are typically provided by the operating system.  Note that the
%  predicates which take names of files or directories as arguments in
%  this @tt{foo} module expect "atoms", not +path alias+es. I.e., 
%    - Hello 
%    - Boo
%    - Too is =3+2=
%  generally these predicates will not call
%  absolute_file_name/2 on names of files or directories taken
%  as arguments.

:- doc(module,"

  This module contains predicates for invoking services which
  are typically provided by the operating system.  Note that the
  predicates which take names of files or directories as arguments in
  this @tt{foo} module expect \"atoms\", not +path alias+es. I.e., 
    - Hello 
    - Boo
    - Too is =3+2=
  generally these predicates will not call
  absolute_file_name/2 on names of files or directories taken
  as arguments.").


%
%! Author: Daniel Cabeza
%! Author: Manuel Carro

:- pred delete_non_ground(L1,_E,L2) => (list(L1), list(L2)).

%!  L2 is L1 without the ocurrences of E. E can be a nonground term so
%   that all the elements in L1 it unifies with will be deleted.

:- true comp delete_non_ground/3 + sideff(true).
:- true comp delete_non_ground(L1,E,L2) : (ground(L1), ground(L2)) + eval.

delete_non_ground([A], _, [A]) :-
	var(A), !.
delete_non_ground([], _, []).
delete_non_ground([Head|Tail], Element, Rest) :-
	eq(Head,Element), !,
	delete_non_ground(Tail, Element, Rest).
delete_non_ground([Head|Tail], Element, [Head|Rest]) :-
	delete_non_ground(Tail, Element, Rest).

eq(A, B):- \+ \+ A = B.

