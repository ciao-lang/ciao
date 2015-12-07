:- use_package(assertions).

:- doc(filetype, part).

:- doc(title,"PART III - The basic Ciao library (lib)").

:- doc(module,"
   This part documents the @concept{basic Ciao library}.
   The predicates in this library are part of the core Ciao
   system. Many of these predicates are part of the @concept{ISO
   Prolog standard}, while some others are Ciao extensions such as
   support for the assertion syntax (and definition of properties,
   types, etc.). A large subset of these predicates are typically
   @tt{built-ins} in classical Prolog systems. In Ciao, in order to be
   used in a given program file, they have to be loaded instead
   explicitly (see @ref{The module system}).

   The advantage is that this allows building @concept{fully static
   executables} (e.g., for standalone applications) of minimal size by
   not loading predicates that are not needed. This is in contrast
   with the comparatively large executables or saved states (specially
   for standalone applications) that are typically built with other
   approaches.
   ").

%% Note: the preprocessor will eventually be able to put the use_module 
%% decls in! (or we can write a browser).


