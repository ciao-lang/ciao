:- module(_,_,[make,assertions]).

main :- 
	make_toplevel(lpmake).

:- comment(title,"The Ciao lpmake scripting facility").

:- comment(author,"Manuel Hermenegildo").

:- comment(abstract,"@apl{lpmake} is a small Ciao application which
   uses the Ciao @lib{make} library to implement a dependency-driven
   scripts in a similar way to the Unix @apl{make} facility.").

:- comment(module,"

   @bf{Note:} @apl{lpmake} and the @lib{make} library are still under
   active development, and they may change substantially in future
   releases.

   @apl{lpmake} is a Ciao application which uses the Ciao @lib{make}
   library to implement a dependency-driven scripts in a similar way
   to the Unix @apl{make} facility.

   The original purpose of the Unix @apl{make} utility was to
   determine automatically which pieces of a large program needed to
   be recompiled, and issue the commands to recompile them.  In fact,
   @apl{make} is often used for many other purposes: it can be used to
   describe any task where some files must be updated automatically
   from others whenever the others change.

   To prepare to use @apl{lpmake}, you must write a file (typically
   called @file{Makefile.pl}) that describes the relationships among
   files in your program or application, and states the commands for
   updating each file.  In a program, typically the executable file is
   updated from object files, which are in turn made by compiling
   source files.  

   Once a suitable makefile exists, each time you change some source
   files, simply typing @tt{lpmake} suffices to perform all necessary
   recompilations.  The @apl{make} program uses the makefile data base
   and the last modification times of the files to decide which of the
   files need to be updated.  For each of those files, it issues the
   commands recorded in the data base. 

   @apl{lpmake} executes commands in the @file{Makefile.pl} to update
   one or more target @em{names}, where @em{name} is typically a
   program, but can also be a file to be generated or even a
   ``virtual'' target.  If no @tt{-l} or @tt{-m} options are present,
   @apl{lpmake} will look for the makefile @file{Makefile.pl}.

   @apl{lpmake} updates a target if it depends on prerequisite files
   that have been modified since the target was last modified, or if
   the target does not exist.

   You can provide command line arguments to @apl{lpmake} to control
   which files should be regenerated, or how.

    @bf{Note:} if you use @lib{make} and @lib{functions}, then
    @lib{make} should appear before @lib{functions} in the list of
    packages.

").

%%% Usage?

:- comment(acknowledgements,"Major parts of the documentation are
   taken from the documentation of GNU's @apl{gmake}. ").


