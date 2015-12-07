/*  $Id: find.pl,v 1.4 2005/11/17 07:37:02 toms Exp $

    Part of CHR (Constraint Handling Rules)

    Author:        Bart Demoen, Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U. Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(chr_find,
	[
		find_with_var_identity/4,
		forall/3,
		forsome/3
	], []).

:- use_module(library(lists)).
:- use_module(library(chr/aggregates_nat)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ciao begin
% :- meta_predicate
% 	find_with_var_identity(?, +, :, -),
% 	forall(-, +, :),
% 	forsome(-, +, :).

:- meta_predicate find_with_var_identity(?, ?, goal, ?).
:- meta_predicate forall(?, ?, goal).
:- meta_predicate forsome(?, ?, goal).
%% Ciao end

% find_with_var_identity(Template, IdVars, Goal, Answers) :-
%         Key = foo(IdVars),
%         findall(Key - Template, Goal, As),
%         smash(As,Key,Answers).

:- use_module(library(write)).

find_with_var_identity(Template, IdVars, Goal, Answers) :-
         Key = foo(IdVars),
 	 copy_term_nat(Template-Key-Goal,TemplateC-KeyC-GoalC),
%	 write( b4_cp( Key,tmpl(TemplateC),key(KeyC))),nl,nl,
         findall(KeyC - TemplateC, GoalC, As),
%	 write( answer( tmpl(TemplateC),key(Key),as(As))),nl,nl,
         smash(As,Key,Answers).
%	 write( answer( key(Key),as(As),result(Answers))),nl,nl,nl.


smash([],_,[]).
smash([Key-T|R],Key,[T|NR]) :- smash(R,Key,NR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forall(X,L,G) :-
	\+ (member(X,L), \+ call(G)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forsome(X,L,G) :-
	member(X,L),
	call(G), !.
