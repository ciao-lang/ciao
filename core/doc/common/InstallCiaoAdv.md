# Network-based installation options

The network installer is delivered as an HTTPs redirection to
`https://raw.githubusercontent.com/ciao-lang/ciao/master/ciao-boot.sh`
(which contains the latest version of the `ciao-boot.sh` script). The
bootstrap script recognizes that it is being called without sources
automatically. Ciao can be installed from the network using a `curl`
pipe from `https://ciao-lang.org/boot` to `sh` in a terminal. This is
probably the fastest/easiest way to install Ciao in binary or source
form.

Examples:

```
# Interactive
curl https://ciao-lang.org/boot -sSfL | sh
# Minimal local installation with prebuilt binaries
curl https://ciao-lang.org/boot -sSfL | sh -s -- --prebuilt-bin local-install
# Full environment
curl https://ciao-lang.org/boot -sSfL | sh -s -- get devenv
# Show network installer help
curl https://ciao-lang.org/boot -sSfL | sh -s -- --help
```

# Manual installation from source

You can get the source code for the latest stable and development
versions of Ciao by cloning the [Ciao repository](https://github.com/ciao-lang/ciao):
```
git clone https://github.com/ciao-lang/ciao
```

Installation of Ciao from source is driven by the `ciao-boot.sh`
script (`ciao-boot.bat` in Windows), which bootstraps and invokes the
Ciao build system.

1. Type `./ciao-boot.sh configure`. This command will attempt to
   detect the configuration options for your system automatically and
   select reasonable defaults.

   Options can be configured interactively using `./ciao-boot.sh
   configure --interactive` or passed explicitly as arguments from the
   command-line (see @ref{Customize your installation} and
   `./ciao-boot.sh help` for more information). In particular, make
   sure the location of the @apl{emacs} executable is set correctly.

2. Type `./ciao-boot.sh fetch devenv` (for the full development
   environment, optional). This will fetch the source of other
   components required for the development environment. This step is
   not needed when compiling Ciao from the *monorepository*.

2. Type `./ciao-boot.sh build`. This will build executables, compile
   libraries, and generate the documentation.

3. Type `./ciao-boot.sh install`. This will install everything in
   the specified directories.

Alternatively, `ciao-boot.sh` provides shorthands to perform
configure, build, and install in a single command:

1. Type `./ciao-boot.sh local-install`. This will do a user-local
   installation (that will be accessible just for your user).

2. Type `./ciao-boot.sh global-install`. This will do a system-wide
   installation (e.g., as administrator or `root` user).

3. Type `./ciao-boot.sh get devenv` to fetch, configure, and install
   the full environment ([devenv](https://github.com/ciao-lang/devenv)
   bundle).

The system will include appropriate code at the end of your startup
scripts. This will make the documentation and executables accessible,
make sure the correct mode is set when opening Ciao source files in
@apl{emacs}, etc.

## Cleaning up the source directory

After a **global installation**, the source directory can be cleaned
up using `./ciao-boot.sh realclean`, leaves the distribution is its
original form, throwing away any intermediate files (as well as any
unneeded files left behind by the Ciao developers), while still
allowing recompilation.

## Uninstalling

Type `./ciao-boot.sh uninstall` in the top directory to *uninstall*
the system. To ensure that this process works, configuration should
have not changed since installation, so that the same directories are
cleaned.

# Customize your installation
@cindex{installation, configuration}

You can configure and customize your Ciao installation with the command:
```
./ciao-boot.sh configure
```

By default, it will perform a default configuration, where the system
will be configured to run from the sources directly, and configured in
the user's home directory (recommended for Ciao developers or users
without admin rights).

The option `--instype=global` will prepare Ciao to be installed as
the system administrator (`root`) in a standard directory available
for all users in the machine (e.g., `/usr/local`).

For 32-bit builds in 64-bit architectures use the `--core:m32=yes`
configuration flag.

In case you want to install elsewhere, or change any of the
installation options, you can use a customized configuration
procedure (see `./ciao-boot.sh help` for more information). The
meaning of some important options is as follows:

 - `--prefix`: prefix for the default values of installation
   directories (`<prefix>/bin`, `<prefix>/lib`, etc).

 - `--bindir`: directory where the Ciao commands will be
   installed. For example, if `--bindir` is set to `/usr/local/bin`,
   then the Ciao @concept{compiler} (@apl{ciaoc}) will be stored at
   `/usr/local/bin/ciaoc`. Actually, it will be a link to
   `ciaoc-`*VersionNumber*. This applies also to other executables
   below and is done so that several versions of Ciao can coexist on
   the same machine.  Note that the *version installed latest* will
   be the one started by default when typing `ciao`, `ciaoc`, etc.
   @cindex{binary directory}

 - The Ciao installation procedure will create a new subdirectory
   `ciao/<vers>` denoted `INSTALL_CIAOROOT` below `--prefix`. The
   `<vers>` directory indicate the Ciao system version. It allows
   having several Ciao versions installed simultaneously.

 - `--mandir`: directory where the @concept{manuals} in @apl{man}
   format will be installed.

 - `--infodir`: directory where the @concept{manuals} in @apl{info}
   format will be installed.

@comment{
2. **Build Ciao:** At the ciao top level directory type
   `./ciao-boot.sh build`.

   This will:

    - Build an @concept{engine}. The engine is the actual
      interpreter of the low level code into which Ciao programs
      are compiled.

    - Build a new Ciao @concept{standalone compiler}
      (@apl{ciaoc}), with the default paths set for your local
      configuration (nonetheless, these can be overridden by
      environment variables, as described below).

    - Compile a toplevel @concept{Ciao shell} and a shell for
      @concept{Ciao scripts}, @cindex{scripts} under the
      `<CIAOSRC>/shell` directory.

    - Compile auxiliary applications (documented in the part of
      the manual on 'Miscellaneous Standalone Utilities').

    - Precompile all the libraries using this compiler.

    - Generate the manuals.

   **Check compilation:** If the above steps have been satisfactorily
   finished, the compiler has compiled itself and all the distribution
   modules, and very probably everything is fine.
}

Once configured, the `./ciao-boot.sh build` command and
`./ciao-boot.sh install` command will build and install Ciao,
respectively, using the selected configuration options. This will:

 - Install the executables of the Ciao @concept{program development
   tools} (i.e., the general driver/top-level @apl{ciao}, the
   standalone compiler @apl{ciaoc}, the script interpreter
   @apl{ciao-shell}, miscellaneous utilities, etc.) in the selected
   binary directory. In order to use these tools, make sure that
   the `PATH` @cindex{PATH} @concept{environment variable} contains
   such path.

 - Install the Ciao libraries under `INSTALL_CIAOROOT` (these will
   be automatically found).

 - Install the Ciao manuals in several formats (such as GNU `info`,
   `html`, `pdf`, etc.) under `INSTALL_CIAOROOT` and other
   documentation-specific paths. In order for these manuals to be
   found when typing `M-x info` within @apl{emacs}, or by the
   standalone @apl{info} and @apl{man} commands, the `MANPATH`
   @cindex{MANPATH} and `INFOPATH` @cindex{INFOPATH}
   @concept{environment variables} of users both need to contain
   the path specified at `--mandir` and `--infodir`. Documentation
   in other formats can be accesed with the `ciao doc` command.

 - Install under `INSTALL_CIAOROOT` the Ciao interface with GNU
   @apl{emacs} (which provides an interactive interface to the Ciao
   program development tools, as well as some other auxiliary
   files).

Finally, the installation will modify the user startup files
automatically by default to **set up user environments**. If you
disable this option in the configuration or you wish to switch between
different installations, it would be necessary to modify the startup
files manually.

The installation process generates a @apl{ciao-env} command to update
the environments for @apl{sh}-like and @apl{csh}like shells. It also
generates a `INSTALL_CIAOROOT/ciao-mode-init` (for emacs) with
appropriate definitions which will take care of all needed
@concept{environment variable definitions} and @concept{emacs mode
setup}.

 - For users a @index{csh-compatible shell} (@apl{csh}, @apl{tcsh}, ...),
   add to @file{~/.cshrc}:
   ```
   if ( -x <prefix>/bin/ciao-env ) then
      eval `<prefix>/bin/ciao-env --csh`
   endif
   ```
 - For users of an @index{sh-compatible shell} (@apl{sh},
   @apl{bash}, ...), the installer will add to @file{~/.bashrc} the
   next lines:
   ```
   if [ -x <prefix>/bin/ciao-env ]; then
      eval "$(<prefix>/bin/ciao-env --sh)"
   fi
   ```
 - For users of an @apl{zsh} shell, the same lines will be added to
   @file{~/.zshrc}.

   This will set up things so that the Ciao executables are found
   and you can access the Ciao system manuals using the @apl{info}
   command. Note that, depending on your shell, *you may have to
   log out and back in* for the changes to take effect.

 - Also, if you use @apl{emacs} (highly recommended) the install
   will add the next line to your @file{~/.emacs} file:
   ```
   (load-file "INSTALL_CIAOROOT/ciao-site-file.el")
   (if (file-exists-p "INSTALL_CIAOROOT/ciao-site-file.el")
     (load-file "INSTALL_CIAOROOT/ciao-site-file.el")
   )
   ```

If you are installing Ciao globally in a multi-user machine, make
sure that you instruct all users that to do the same.  If you are
the system administrator, the previous steps can be done once and
for all, and globally for all users by including the lines above in
the central startup scripts (e.g., in Linux @file{/etc/bashrc},
@file{/etc/csh.login}, @file{/etc/csh.cshrc}, @file{/etc/skel},
@file{/usr/share/emacs/.../lisp/site-init.pl}, etc.).

# Checking for correct installation
@cindex{installation, checking the}

If you have any problems you may want to check @ref{Troubleshooting}.
Otherwise, if everything has gone well, several applications and tools
should be available to a normal user.  Try the following while logged
in as a *normal user* (important in order to check that permissions
are set up correctly):

@include{InstallTest.lpdoc}

Finally, if @apl{emacs} is installed, after starting it (typing
`emacs`) the following should work:

@include{EmacsTesting.lpdoc}

# Environment variables used by Ciao executables

The executables generated by the Ciao compiler (including the ciao
development tools themselves) locate automatically where the Ciao
engine and libraries have been installed, since those paths are stored
as defaults in the engine and compiler at installation time. Thus,
there is no need for setting any environment variables in order to
*run* Ciao executables (on a single architecture -- see
@ref{Multiarchitecture installation} for running on multiple
architectures).

However, the default paths can be overridden by using the environment
variable `CIAOROOT`, which tell the Ciao executables where to look
for engines and system libraries.

# Multiarchitecture installation
@cindex{multiarchitecture installation}

@comment{
    For network-based installations, @cindex{installation, network
    based} it is of *utmost importance* that the configured paths
    be reachable in all the networked machines.  Different machines
    with different architectures can share the same physical source
    directory during installation, since compilations for different
    architectures take place in dedicated subdirectories. Also,
    different machines/architectures can share the same
    `INSTALL_CIAOROOT` directory. This saves space since the
    architecture-independent libraries will be shared. See
    @ref{Multiarchitecture installation} below.

    Only the engine and some small parts of the libraries (those
    written in @concept{C}) differ from one architecture to the other.
    Standard Ciao code compiles into @concept{bytecode object files}
    (`.po`) and/or @concept{executables} which are portable among
    machines of different architecture, provided there is an
    executable engine accessible in every such machine.
}

In order to perform a multi-architecture installation, it is possible
to repeat successively the build and installation process for several
architectures from different machines sharing part of their
filesystem.

The Ciao build and installation process maintains separate name spaces
for platform- and architecture-dependant binaries (like the engine
binary, or `.so` or `.dll` gluecode files for the foreign
interface).  Portable Ciao applications (i.e., except self-contained
standalone executables) can run on several machines with different
architectures without any need for recompiling, provided the Ciao
engine and libraries are correctly installed.

@comment{Ciao applications (including the compiler and the top level)
can run on several machines with different architectures without any
need for recompiling, provided the Ciao engine (compiled for the
corresponding architecture) accessible in each machine. Also, the Ciao
libraries (installed in `INSTALL_CIAOROOT`, which contain also the
engines) and the actual binaries (installed in the directory specified
by `--bindir`) can themselves be shared on several machines with
different architectures.}  @comment{, saving disk space.}

@comment{
For example, assume that the compiler is installed as:

`/usr/local/share/bin/ciaoc`

and the libraries are installed under

`/usr/local/share/lib`

Assume also that the `/usr/local/share` directory is mounted on,
say, a number of (physical or virtual) @concept{Linux} and a number of
@concept{Solaris} boxes. In order for `ciaoc` to run correctly on
both types of machines, the following is needed:

 - Make sure you that have done `./ciao-boot.sh install` on one
   machine of each architecture (once for Linux and once for Solaris
   in our example).  This recompiles and installs a new engine and any
   architecture-dependent parts of the libraries for each
   architecture. The engines will have names such as
   `ciaoengine.<CIAOOS><CIAOARCH>`.

 - In multi-architecture environments it is even more important to
   make sure that users make the modifications to their startup
   scripts. The selection of the engine (and architecture-dependent
   parts of libraries) is done by setting the environment variables
   `CIAOOS` and `CIAOARCH`, using the values given by the
   `ciao_sysconf` command, which is installed automatically when
   installing Ciao.

   However, note that this is not strictly necessary if running on
   only one architecture: if `CIAOOS` and `CIAOARCH` are not set
   (i.e., undefined), the Ciao executables will look simply for
   `ciaoengine`, which is always a link to the latest engine installed
   in the libraries. But including the initialization files provided
   has the advantage of setting also paths for the manuals, etc.
}

@comment{Although Ciao
implements its own build system, the @concept{GNU} implementation of
the @concept{make} command is (still) used internally. If any of the
installation steps stop right away with @apl{make} error messages, you
probably need to install `gmake`.}
