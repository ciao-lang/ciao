
:- module(polyhedra_amk_doc, [project/3, convex_hull/6, polyhedra_widen/5],[assertions, isomodes]).

:- doc(title,"Polyhedra library based on CLP(Q) and CLP(R)").

:- doc(author,"Florence Benoy").
:- doc(author,"Andy King").
:- doc(author,"Fred Mesnard").
:- doc(author,"Samir Genaim (porting to Ciao)").

:- doc(module,"

This library provides a set of predicates for manipulating @bf{closed}
polyhedrons. It uses the CLP(Q) and CLP(R) libraries of Ciao. The supported
operations are: Convex Hull, Projection, and (the classical Cousot and
Halbwachs) Widening. Note that the correctness is guaranteed only for closed
polyhedrons, so @em{do not} use @pred{.>.} and @pred{.<.}. Here are some examples:

@begin{verbatim}


?- use_module(library(polyhedra_amk_clpq)).

yes
?- project([A,B],[A .>=. C, C .>=. B],Cs).

Cs = [A.>=.B] ?

yes
?- convex_hull([A,B],[A.>=.1,A.=<.2,B.>=.1,B.=<.2],
               [C,D],[C.>=.4,C.=<.5,D.>=.4,D.=<.5],Vs,Cs).

Cs = [_B.>=. -1+_A,_A.=<.5,_A.>=.1,_B.>=.1,_B.=<.1+_A,_B.=<.5],
Vs = [_A,_B] ?

yes
?- polyhedra_widen([A,B],[A.>=.1,B.>=.5],[A.>=.2,B.>=.5],Vs,Cs).

Cs = [_A.>=.1,_B.>=.5],
Vs = [_A,_B] ?

yes


@end{verbatim}
").

:- doc(usage,"
@begin{itemize}
@item CLP(R): @tt{ :- use_module(library(polyhedra_amk_clpr)).}
@item CLP(Q): @tt{ :- use_module(library(polyhedra_amk_clpq)).}
@end{itemize}
").


:- pred project(+Vs,+Cvs,-ProjectCvs) #

        "@var{Vs} is a list of variables, @var{Cvs} is a list of contraints,
         @var{ProjectCvs} is the projection of the conjunction of the
         constraints of @var{Cvs} on the variables @var{Cs}.".

project(_,_,_).


:- pred convex_hull(+Xs, +Cxs, +Ys, +Cys, +Zs, +Czs) #

        "@var{Xs} and @var{Ys} are list of variables of the same length,
        @var{Cxs} and @var{Cys} are lists of constraints on the variables of
        @var{Xs} and @var{Ys} repressively, @var{Zs} is a list of new fresh
        variables of the same length as @var{Xs} and @var{Ys}, and @var{Czs} is
        a list of constrains over @var{Zs} which represents the convex-hull of
        @var{Cxs} and @var{Cys}.".

convex_hull(_,_,_,_,_,_).

:- pred polyhedra_widen(+Vars,+Clist1,+Clist2,-WidenVars,-ClistWiden) #

        "@var{Clist1}, @var{Clist2} are 2 lists of cosntraints on
         @var{Vars}. @var{WidenVars} are fresh variables and @var{ClistWiden}
         is the result of selecting the constraints from @var{Clist1} which are
         entailed by @var{Clist2}.".

polyhedra_widen(_,_,_,_,_).


/*
@Article{polyhedra:bkm:tplp,
        author = {F.~Benoy and A.~King and F.~Mesnard},
        title = {{C}omputing {C}onvex {H}ulls with a {L}inear {S}olver},
        month = {unknown},
        year = {2005},
        pages = {259-271},
        keywords = {convex hull, polyhedra, abstract interpretation, linear constraints},
        url = {http://www.cs.kent.ac.uk/pubs/2005/1734},
        publication_type = {article},
        submission_id = {22300_1066923833},
        journal = {Theory and Practice of Logic Programming},
        number = {1&2},
        publisher = {Cambridge University Press},
        volume = {5},
}
*/
