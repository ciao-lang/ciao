:- use_package(assertions).

:- doc(filetype, documentation).

:- doc(title,"Getting started on Unix machines").

:- doc(author,"Manuel Hermenegildo").

:- doc(module,"

This part guides you through some very basic first steps with Ciao on
a Unix system.  It assumes that Ciao is already installed
correctly on your Unix system. If this is not the case, then follow
the instructions in @ref{Installing Ciao from the source distribution}
first.

We start with by describing the basics of using Ciao from a normal
command shell such as @tt{sh/bash}, @tt{csh/tcsh}, etc. We strongly
recommend reading also @ref{An introduction to the Ciao emacs
environment (Unix)} for the basics on using Ciao under @apl{emacs},
which is a much simpler and much more powerful way of developing Ciao
programs, and has the advantage of offering an almost identical
environment under Unix and Windows.

@section{Testing your Ciao Unix installation}

@noindent It is a good idea to start by performing some tests to check
that Ciao is installed correctly on your system (these are the same
tests that you are instructed to do during installation, so you can
obviously skip them if you have done them already at that time). If
any of these tests do not succeed either your environment variables
are not set properly (see @ref{Unix user setup} for how to fix this):

@include{InstallTestUnix.lpdoc}

@section{Unix user setup}

@cindex{user setup}
@cindex{environment variables, setup}

If the tests above have succeeded, the system is probably installed
correctly and your environment variables have been set
already. In that case you can skip to the next section.

Otherwise, if you have not already done so, make the following
modifications in your startup scripts, so that these files are used
(@tt{<LIBROOT>} must be replaced with the appropriate value, i.e.,
where the Ciao library is installed):

@include{UserSetup.lpdoc}

If after following these steps things do not work properly, then the
installation was probably not completed properly and you may want to
try reinstalling the system.

@section{Using Ciao from a Unix command shell}

@subsection{Starting/exiting the top-level shell (Unix)}

@cindex{top-level shell, starting, unix}

      The basic methods for starting/exiting the top-level shell have
      been discussed above.  If upon typing @tt{ciao} you get a
      ``command not found'' error or you get a longer message from
      Ciao before starting, it means that either Ciao was not
      installed correctly or you environment variables are not set up
      properly. Follow the instructions on the message printed by Ciao
      or refer to the installation instructions regarding user-setup
      for details.

@subsection{Getting help (Unix)}

@cindex{help, unix}

      The basic methods for accessing the manual on-line have also
      been discussed above. Use the table of contents and the indices
      of @em{predicates}, @em{libraries}, @em{concepts}, etc. to find
      what you are looking for. @concept{Context-sensitive} help is
      available within the @apl{emacs} environment (see below).


@subsection{Compiling and running programs (Unix)}

@cindex{compiling programs}
@cindex{loading programs}
@cindex{running programs}

      Once the shell is started, you can compile and execute 
      modules inside the interactive top-level shell in the standard
      way. E.g., type @tt{use_module(@em{file}).},
      @tt{use_module(library(@em{file})).} for library modules,
      @tt{ensure_loaded(@em{file}).} for files which are not modules,
      and @tt{use_package(@em{file}).} for library packages (these are
      syntactic/semantic packages that extend the Ciao language
      in many different ways). Note that the use of @pred{compile/1}
      and @pred{consult/1} is discouraged in Ciao.

      For example, you may want to type @tt{use_package(iso)} to
      ensure Ciao has loaded all the ISO builtins (whether this is
      done by default or not depends on your @file{.ciaorc} file).  Do
      not worry about any ``module already in executable'' messages
      --these are normal and simply mean that a certain module is
      already pre-loaded in the top-level shell. At this point, typing
      @tt{write(hello).} should work.

      Note that some predicates that may be built-ins in typical
      Prolog implementations are available through libraries in Ciao.
      This facilitates making small executables.

      To change the working directory to, say, the @tt{examples}
      directory from the Ciao root directory, first do:

@begin{verbatim}
      ?- use_module(library(system)).
@end{verbatim}

      @noindent (loading the @lib{system} library makes a number of
      system-related predicates such as @pred{cd/1} accessible) and
      then:

@begin{verbatim}
      ?- cd('examples').  
@end{verbatim}

      For more information see @ref{The interactive top-level shell}.

@subsection{Generating executables (Unix)}

@cindex{executables, generating}
@cindex{compiling programs}

      Executables can be generated from the top-level shell (using
      @pred{make_exec/2}) or using the standalone compiler
      (@apl{ciaoc}). To be able to make an executable, the file should
      define the predicate @pred{main/1} (or @pred{main/0}), which
      will be called upon startup (see the corresponding manual
      section for details).  In its simplest use, given a top-level
      @em{foo}@tt{.pl} file for an application, the compilation
      process produces an executable @tt{foo}, automatically detecting
      which other files used by @tt{foo.pl} need recompilation.

      @noindent For example, within the @file{examples} directory, you
      can type:

@begin{verbatim}
    ?- make_exec(hw,_).
@end{verbatim}

      @noindent which should produce an executable. Typing @tt{hw} in
      a shell (or double-clicking on the icon from a graphical window)
      should execute it.

      For more information see @ref{The interactive top-level shell}
      and @ref{The standalone command-line compiler}.

@subsection{Running Ciao scripts (Unix)}

@cindex{scripts}
@cindex{compiling programs}
@cindex{running programs}

      Ciao allows writing @concept{scripts}. These are files
      containing Ciao source but which get executed without having
      to explicitly compile them (in the same way as, e.g., @tt{.bat}
      files or programs in scripting languages). As an example, you
      can run the file @file{hw} in the @file{examples} 
      directory of the Ciao distribution and look at the source with
      an editor. You can try changing the @tt{Hello world} message and
      running the program again (no need to recompile!).

      As you can see, the file should define the predicate
      @pred{main/1} (not @pred{main/0}), which will be called upon
      startup.  The two header lines are necessary in Unix in. In
      Windows you can leave them in or you can take them out, but you
      need to rename the script to @file{hw.pls}.  Leaving the lines
      in has the advantage that the script will also work in Unix
      without any change.

      For more information see @ref{The script interpreter}.

@subsection{The Ciao initialization file (Unix)}

@cindex{.ciaorc}
@cindex{initialization file} 

      The Ciao toplevel can be made to execute upon startup a number
      of commands (such as, e.g., loading certain files or setting
      certain Ciao flags) contained in an initialization file.  This
      file should be called @file{.ciaorc} and placed in your
      @em{home} directory (e.g., @tt{~}, the same in which the
      @file{.emacs} file is put). You may need to set the environment
      variable @tt{HOME} to the path of this directory for the Ciao
      toplevel shell to be able to locate this file on startup.

@section{An introduction to the Ciao emacs environment (Unix)}

@comment{--------------}
@include{EmacsUse.lpdoc}
@comment{--------------}

@section{Keeping up to date (Unix)}

You may want to read @ref{Beyond installation} for instructions on how
to sign up on the Ciao user's mailing list, receive announcements
regarding new versions, download new versions, report bugs, etc.

@comment{
@section{Some notes for seasoned Prolog users}
Ciao is a little different...
}

").

