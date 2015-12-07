%%------------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% DOCUMENTATION 
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : FEBRAURY 2000
%%
%%------------------------------------------------------------------------
:- module(mycin_doc,_,[assertions]).

:- doc(nodoc,assertions).

:- doc(filetype,package).

:- doc(title, "Programming MYCIN rules").

:- doc(author,"Angel Fernandez Pineda").

:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(summary,
	"This library enables MYCIN-style inference using
         certainty factors on rules.").

:- doc(module,
	"MYCIN databases are declared as Prolog modules containing mycin
         rules. Those rules are given a @index{certainty factor} 
	 (@em{CF}) which denotates an expert's credibility on that rule:
         @begin{itemize}
	 
	 @item A value of -1 stands for @em{surely not}.
 
         @item A value of 1 stands for @em{certainly}.

         @item A value of 0 stands for @em{I don't know}.

         @end{itemize}

         Intermediate values are allowed.

         Mycin rules work on a different way as Prolog clauses: a rule 
         will never fail (in the Prolog sense), it will return a certainty
         value instead. As a consequence @bf{all} mycin rules will be explored
         during inference, so the order in which rules are written
         is not significant. For this reason, the usage of the Prolog
         @em{cut} (!) is discouraged.
        ").

:- doc(usage,
	"In order to declare a mycin database you must include the following
         declaration as the first one in your file:
@begin{verbatim} 
           :- mycin(MycinDataBaseName).
@end{verbatim}
        ").

%%------------------------------------------------------------------------
%%
%% MYCIN DECLARATION DOC
%%
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%%
%% EXPORTATION
%%
%%------------------------------------------------------------------------

:- doc(export/1,
	"This directive allows a given mycin predicate to be called
         from Prolog programs. The way in which mycin rules are called
         departs from Prolog ones. For instance, the followin mycin
         predicate:
@begin{verbatim}
:- export p/1.
@end{verbatim}

         must be called from Prolog Programs as: @tt{mycin(p(X),CF)},
         where CF will be binded to the resulting @concept{certainty factor}.
         Obviously, the variables on @em{P/1} may be instantiated as you
         wish. Since the Prolog predicate @em{mycin/2} may be imported
         from several mycin databases, it is recommended to fully qualify
         those predicate goals. For example : @tt{mydatabase:mycin(p(X),CF)}.
        ").

:- decl export(Spec) #
	"@var{Spec} will be a callable mycin predicate.".

%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% BUGS / TO IMPLEMENT IN THE FUTURE
%%------------------------------------------------------------------------

:- doc(bug,
	"Not fully implemented.").
:- doc(bug,
	"Dynamic mycin predicates not implemented: open question.").
:- doc(bug,
	"Importation of user-defined mycin predicates requires further
         design. This includes importation of mycin databases from 
         another mycin database.").
