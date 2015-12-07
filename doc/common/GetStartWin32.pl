:- use_package(assertions).

:- doc(filetype, documentation).

:- doc(title,"Getting started on Windows machines").

:- doc(author,"Manuel Hermenegildo").

:- doc(module,"

This part guides you through some very basic first steps with Ciao on
an MSWindows (``Win32'') system.  It assumes that Ciao is already
installed correctly on your Windows system. If this is not the case,
then follow the instructions in @ref{Installing Ciao from a Win32
binary distribution} (or @ref{Installing Ciao from the source
distribution}) first.

We start with by describing the basics of using Ciao from the Windows
explorer and/or a DOS command shell. We strongly recommend reading
also @ref{An introduction to the Ciao emacs environment (Win32)} for
the basics on using Ciao under @apl{emacs}, which is a much simpler
and much more powerful way of developing Ciao programs, and has the
advantage of offering an almost identical environment under Windows
and Unix.

@section{Testing your Ciao Win32 installation}

@noindent It is a good idea to start by performing some tests to check
that Ciao is installed correctly on your system (these are the same
tests that you are instructed to do during installation, so you can
obviously skip them if you have done them already at that time):

@include{InstallTestWin32bin.lpdoc}

@section{Using Ciao from the Windows explorer and command shell}

@subsection{Starting/exiting the top-level shell (Win32)}

@cindex{top-level shell, starting, windows}

      The basic methods for starting/exiting the top-level shell have
      been discussed above. The installation script also leaves a
      @tt{ciaosh}(@tt{.bat}) file inside the @file{shell} folder of the Ciao
      distribution which can be used to start the top-level shell from
      the command line in Windows systems.

@subsection{Getting help (Win32)}

@cindex{help, windows}

      The basic methods for accessing the manual on-line have also
      been discussed above. Use the table of contents and the indices
      of @em{predicates}, @em{libraries}, @em{concepts}, etc. to find
      what you are looking for. @concept{Context-sensitive} help is
      available within the @apl{emacs} environment (see below).


@subsection{Compiling and running programs (Win32)}

@cindex{compiling programs}
@cindex{loading programs}
@cindex{running programs}

      Once the shell is started, you can compile and execute Ciao 
      modules inside the interactive toplevel shell in the standard
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
      already pre-loaded in the toplevel shell. At this point, typing
      @tt{write(hello).} should work.

      Note that some predicates that may be built-ins in typical
      Prolog implementations are available through libraries in Ciao.
      This facilitates making small executables.

      To change the working directory to, say, the @tt{examples}
      directory from the Ciao source directory, first do:

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

@subsection{Generating executables (Win32)}

@cindex{executables, generating}
@cindex{compiling programs}

      Executables can be generated from the toplevel shell
      (using @pred{make_exec/2}) or using the standalone compiler
      (@apl{ciaoc}(@tt{.cpx}), located in the @tt{ciaoc} folder). To be able
      to make an executable, the file should define the predicate
      @pred{main/1} (or @pred{main/0}), which will be called upon
      startup (see the corresponding manual section for details).

      @noindent For example, within the @file{examples} directory, you
      can type:

@begin{verbatim}
    ?- make_exec(hw,_).
@end{verbatim}

      @noindent which should produce an executable. Double-clicking on
      this executable should execute it.

      Another way of creating Ciao executables from source files is by
      right-clicking on @tt{.pl} files and choosing ``make
      executable''. This uses the standalone compiler (this has the
      disadvantage, however, that it is sometimes difficult to see the
      error messages).

      For more information see @ref{The interactive top-level shell}
      and @ref{The standalone command-line compiler}.

@subsection{Running Ciao scripts (Win32)}

@cindex{scripts}
@cindex{compiling programs}
@cindex{running programs}

      Double-clicking on files ending in @tt{.pls},
      @em{Ciao scripts}, will also execute them. These are
      files containing Ciao source but which get executed without
      having to explicitly compile them (in the same way as, e.g.,
      @tt{.bat} files or programs in scripting languages). As an
      example, you can double-click on the file @file{hw.pls} in the
      @file{examples} folder and look at the source with an editor. You
      can try changing the @tt{Hello world} message and
      double-clicking again (no need to recompile!).

      As you can see, the file should define the predicate
      @pred{main/1} (not @pred{main/0}), which will be called upon
      startup.  The two header lines are only necessary in Unix. In
      Windows you can leave them in or you can take them out, but
      leaving them in has the advantage that the script will also work
      in Unix without any change.

      For more information see @ref{The script interpreter}.

@subsection{The Ciao initialization file (Win32)}

@cindex{.ciaorc}
@cindex{initialization file} 

      The Ciao toplevel can be made to execute upon startup a number
      of commands (such as, e.g., loading certain files or setting
      certain Ciao flags) contained in an initialization file.  This
      file should be called @file{.ciaorc} and placed in your
      @em{home} folder (e.g., the same in which the @file{.emacs} file
      is put). You may need to set the environment variable @tt{HOME}
      to the path of this folder for the Ciao toplevel shell to be
      able to locate this file on startup.

@section{An introduction to the Ciao emacs environment (Win32)}

@comment{--------------}
@include{EmacsUse.lpdoc}
@comment{--------------}

@section{Keeping up to date (Win32)}

You may want to read @ref{Beyond installation} for instructions on how
to sign up on the Ciao user's mailing list, receive announcements
regarding new versions, download new versions, report bugs, etc.

").
