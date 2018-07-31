:- use_package([assertions,iso]).
:- doc(nodoc,assertions).

:- doc(filetype,package).

:- doc(title,"ISO-Prolog package").

:- doc(author, "The Ciao Development Team").

:- doc(module,"This package enables by default
   @concept{ISO-Prolog} predicates in Ciao programs.

   Compliance with ISO is still not complete: currently there are some
   minor deviations in, e.g., the treatment of characters, the syntax,
   some of the arithmetic functions, and part of the error system.
   Also, Ciao does not offer a strictly conforming mode which rejects
   uses of non-ISO features. 
").

% @comment{Given that the final version of the ISO standard has only
% been recently published,}

% @comment{On the other hand, Ciao has been reported by independent
% sources (members of the standarization body) to be one of the most
% conforming Prologs at the moment of this writing, and the first one
% to be able to compile all the standard-conforming test cases.}
   
