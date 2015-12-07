:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Phrase Support for DCGs").

:- doc(author, "Jose F. Morales").
:- doc(author, "The CLIP Group").

%% see @cite{Les Grammaires de Metamorphos} by A.Colmerauer,
%% Technical Report, Groupe d'Intelligence Artificielle,
%% Marseille-Luminy, November, 1975, and @cite{Definite clause grammars for
%% language analysis---a survey of the formalism and a comparison with
%% augmented transition networks} by F.C.N. Pereira and D.H.D. Warren, in
%% @cite{Artificial Intelligence} 13:231-278, 1980. 
 
:- doc(module, "This library extends the DCG package (@lib{dcg}) with
   support for the @pred{phrase/2} and @pred{phrase/3} predicates.

   Those predicates allow the translation and execution of arbitrary
   terms as DCGs goals at runtime. Those features, are not always
   desirable, since arbitrary code execution can negatively affect the
   precision of static analysis and increasing the size of static
   executables. This package offers a method to include runtime
   support for DCGs only when necessary.
").

:- doc(bug, "Runtime support could be included automatically when
   required. However, detecting if @pred{phrase/2} or @pred{phrase/3}
   are necessary is harder. It could be implemented just by detecting
   if @pred{call/N} is visible and runtime expansions are required in
   the module.").

