%%------------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% DOCUMENTATION 
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : 2002
%%
%% Distributed under Ciao Prolog license terms
%%
%%------------------------------------------------------------------------

:- use_package(assertions).

:- doc(nodoc,assertions).

:- doc(filetype,package).

%:- use_module('../mycin/mycin_support').

:- doc(title, "Declaring MYCIN rulebases").

:- doc(author,"Angel Fernandez Pineda").

:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(summary,
	"This library enables MYCIN-style inference using
         certainty factors on rules.").

:- doc(module,
	"MYCIN rulebases are declared as Prolog modules containing mycin
         rules. Those rules are given a @concept{certainty factor} 
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
	"In order to declare a mycin rulebase you must include the following
         declaration as the first one in your file:
@begin{verbatim} 
           :- mycin_rulebase(MycinRulebaseName).
@end{verbatim}

         It is also possible to use the following one, instead:
@begin{verbatim} 
           :- module(MycinRulebaseName,[],[mycin_rulebase]).
@end{verbatim}
        ").

%%------------------------------------------------------------------------
%%
%% MYCIN DECLARATION DOC
%%
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% EXPORTATION
%%------------------------------------------------------------------------

:- doc(export/1,
	"This directive allows a given mycin predicate to be called
         from Prolog programs and other rulebases. 
         Exported mycin rules may be called @tt{entry points} in this
         context.

         The way in which mycin rules are called
         departs from Prolog ones. For instance, the followin mycin
         predicate:
@begin{verbatim}
:- export p/1.
@end{verbatim}

         should be called from Prolog as: @tt{p(X) cf CF},
         where CF will be binded to the resulting @concept{certainty factor}.
         See @pred{cf/2} for further reference.
         Obviously, the variables on @em{p/1} may be instantiated as you
         wish.
        ").

:- decl export(Spec) #
	"@var{Spec} will be a callable mycin predicate.".

%%------------------------------------------------------------------------
%% DOCUMENTATION ON HOW TO DECLARE MYCIN RULES
%%------------------------------------------------------------------------

:- doc(appendix,
	"
 This section elaborates on the way in which @concept{mycin rules}
 are declared.
 There are two kind of mycin rules: those traditional modus-ponens rules
 and @concept{metarules}, which are provided as a simple way to integrate
 Prolog and mycin inference. Mycin rules does not depart too much from
 Prolog clauses, they differ in clause heads, where mycin rules includes a 
 @concept{certainty factor} declaration.

 @subsection{Declaring Mycin rules}

 @index{Mycin rules} are declared as follows:
@begin{verbatim}
 Head cf CertaintyFactor :- Body.
@end{verbatim}

 The same considerations are applied as for @em{Head :- Body} Prolog clauses.
 @em{CertaintyFactor} @tt{must} be a number between -1 and 1 which declares
 the certainty factor for that rule. For example:
@begin{verbatim}
 risk(high) cf -0.3 :-
	market_status(defensive),
	imitation_time(short).
@end{verbatim}
 
 Rule bodies may contain a Prolog goal as well as a @concept{mycin goal}
 in any number, but those goals will @em{never fail} since mycin goals 
 always retrieve a certainty factor. For Prolog goals a certainty factor
 of -1 will be retrieved whenever they fail and +1 in other case.

 Mycin goals may be derived from other rules inside the same 
 source file or may be imported using a @decl{use_module/1} declaration,
 the same used to import Prolog predicates.

 Note that rule bodies may also contain logical connectives as 
 @em{;/2, ->/2 and \+/2} (if/3 is not allowed), however their usage is
 discouraged. Meta-programming practiques are also strongly discouraged.
 
 Some examples of mycin rulebases may be found along with this distribution
 at path mycin_rulebase/Examples.


 @subsection{Declaring Mycing metarules}

 
 A @index{mycin metarule} is just a mycin rule where its 
 @concept{certainty factor} is calculated @em{at runtime} via Prolog
 goals.

 Metarule heads differ from rule heads in the Certainty Factor, which
 must be a variable. Such variable should be further instantiated to a
 valid certainty factor at runtime. For example:
@begin{verbatim}
 weather(Forecast) cf Certainty :-
     forecasting:weather_forecast(Forecast,Probability),
     Certainty is (Probability * 2) - 1.
@end{verbatim}

 Metarule bodies differ from rule bodies because they are treated as Prolog
 clause bodies, so they may fail. In such case a certainty factor of 0 will
 be used for that rule. Calling mycin goal from metarule bodies is not
 possible unless @pred{cf/2} predicate is used.

 Metarules are supposed to retrieve a valid certainty factor, otherwise
 a certainty factor of 0 will be used, too. The programmer must take care
 of such constraint.

 The porpouse of metarules is to provide a simple way to perform mycin
 inference from data or facts that may be stored in databases or other
 external resources.
      ").
