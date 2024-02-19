This describes the installation procedure for the Ciao system,
including libraries and manuals:

1. Check the [**requirements and dependencies**](#Requirements%20and%20dependencies).
2. Begin the interactive [**network-based installation**](/ciao/build/doc/ciao.html/Install.html#Network-based%20installation%20options)
   typing the following *one-liner* in an `sh`-compatible terminal:
   ```
   curl https://ciao-lang.org/boot -sSfL | sh
   ```

Please report as [issues](https://github.com/ciao-lang/ciao/issues)
any problems found during the installation process.

You can explore additional components at the **[bundle
catalog](/bundles.html)** (including links to documentation and source
code repositories) and install them with the `ciao get BUNDLENAME`
command.

For *customized installations* or *developer builds*, please consult
the [**manual installation from
source**](/ciao/build/doc/ciao.html/Install.html#Manual%20installation%20from%20source)
instructions.

# Requirements and dependencies

Ciao supports many operating systems, including:

 - Linux distributions
 - macOS
 - Windows (with [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10))
 - Android (with [Termux](https://termux.com)).

Installing [**Emacs**](https://www.gnu.org/software/emacs/) or
[**Ciao support for VSCode**](https://marketplace.visualstudio.com/items?itemName=ciao-lang.ciao-prolog-vsc)
is highly recommended: the Ciao distribution includes a very powerful
*application development environment* which enables, e.g., syntax
coloring, source code formatting, embedded top-level, source-level
debugging, context-sensitive on-line help, etc.

In any case, it is easy to use Ciao from a terminal and edit code with
any editor of your choice. The top level includes some minimal editing
facilities when [rlwrap](https://github.com/hanslub42/rlwrap) is
available.

## Installing dependencies for your operating system

Depending on the operating system and package management tool, you may
need to install the following dependencies (using `sudo` or as
administrator or `root` user):

 - Debian/Ubuntu:
   ```
   $ apt-get install build-essential
   # (optional) for 32 bits compatibility mode (x86)
   $ apt-get install gcc-multilib libc6-i386 libc6-dev-i386 g++-multilib
   # (optional) for emacs-based IDE
   $ apt-get install emacs
   # (optional) for line edition from the terminal
   $ apt-get install rlwrap
   # (optional) for generating documentation in PDF format
   $ apt-get install texlive texinfo imagemagick
   ```
   (valid for both `x86` and `arm` architectures)
	
 - Fedora:
   ```
   $ dnf install gcc make which kernel-headers kernel-devel emacs
   # (optional) for emacs-based IDE
   $ dnf install emacs
   # (optional) for 32 bits compatibility mode (x86)
   $ dnf install glibc-devel.i686 glibc-devel libstdc++-devel.i686
   # (optional) for line edition from the terminal
   $ dnf install rlwrap
   # (optional) for generating documentation in PDF format
   $ dnf install texlive texinfo texinfo-tex ImageMagick
   ```
   (use `yum` instead of `dnf` above in older versions of Fedora)
	
 - Arch Linux:
   ```
   # Optional, upgrade if needed
   $ pacman -Syu
   # Dependencies for build and development environment
   # (base-devel: includes gcc, make, which)
   $ pacman -S base-devel linux-headers
   # (optional) for 32 bits compatibility mode (x86)
   # NOTE: Remember to enable multilib (https://wiki.archlinux.org/index.php/Multilib)
   $ pacman -S lib32-glibc lib32-libstdc++5
   # (optional) for emacs-based IDE
   $ pacman -S emacs
   # (optional) for line edition from the terminal
   $ pacman -S rlwrap
   # (optional) for generating documentation in PDF format
   $ pacman -S texlive-core texinfo imagemagick
   ```

 - macOS:
   - Install command line tools for Xcode (from the App store)
   - Install emacs and ImageMagick (and texinfo if needed, but recent
     versions of macOS include it). A software management tool like
     homebrew or macports is recommended, e.g. (as root/sudo):
     ```
     $ brew install emacs-mac imagemagick
     ```
   - Install TexLive. If using homebrew we recommend installing the
     MacTeX distribution, available from: `https://www.tug.org/mactex`.
   - Install the following packages:
     ```
     # (optional) for line edition from the terminal
     $ brew install rlwrap
     ```
	
 - FreeBSD:
   - Install GCC or clang and the following packages:
     ```
     $ pkg install gmake
     # (optional) for emacs-based IDE
     $ pkg install emacs ImageMagick expat
     # NOTE: use emacs-nox and ImageMagick-nox if X11 support is not needed
     # (optional) for line edition from the terminal
     $ pkg install rlwrap
     # (optional) for generating documentation in PDF format
     $ pkg install texinfo texlive-full
     ```

 - NetBSD:
   - Install GCC or clang and the following packages:
     ```
     $ pkgin install gmake
     # (optional) for emacs-based IDE
     $ pkgin install emacs ImageMagick expat
     # (optional) for line edition from the terminal
     $ pkgin install rlwrap
     # (optional) for generating documentation in PDF format
     $ pkgin install tex-texinfo texlive-collection-latex
     ```

 - Windows (using Windows Subsystem or Linux):
   - Install [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10)
   - Install some Linux distribution (e.g., Ubuntu) from the Windows Store.
   - Open a `bash` terminal and install the dependencies for the
     selected distribution (see points above), e.g., Debian/Ubuntu.

 - Windows (native, **experimental**)
   - Install [MSYS2](http://www.msys2.org/) and the following packages:
     ```
     pacman --noconfirm -S mingw-w64-x86_64-gcc
     ```
	
 - Android (using the Termux Linux environment):
   - Install [Termux](https://termux.com/)
   - Install the following packages:
     ```
     $ pkg install clang make
     # (optional) for emacs-based IDE
     $ pkg install emacs
     # (optional) for line edition from the terminal
     $ pkg install rlwrap
     ```
	
The dependencies above typically include a relatively modern C
compiler (GCC or clang), libraries, and build tools. Documentation
generation in PDF format requires TeX and ImageMagick.

**Additional dependencies:**

Some advanced libraries and components require an additional set of
software packages:

```
# Debian/Ubuntu:
$ apt-get install g++ libgsl0-dev libgsl0ldbl
$ apt-get install default-jdk ant ant-optional
 
# Fedora:
$ yum install gsl gsl-devel ant gcc-c++
```

To install the Java JDK on Fedora, please visit Sun Java website
(`http://java.sun.com/javase/downloads/index.jsp`) and follow the
installation instructions there.

# Upgrading or uninstalling components

Bundles installed via `ciao get` must be uninstalled and removed
explicitly. Currently, this needs to be done manually per bundle.
Upgrading a bundles requires uninstallation followed by
(re)installation.

E.g., uninstalling the **development environment** (if installed)
requires:
```
ciao uninstall ciao_emacs; ciao rm ciao_emacs; ciao rm devenv
```
Once all bundles have been removed, the core Ciao system can be
uninstalled running `./ciao-boot.sh uninstall` from the source
directory (then, remove the directory).

# Upgrading or uninstalling Ciao

If installed using `curl`, the system is installed in a
per-version subdirectory under `~/.ciaoroot/`. E.g., development
version is installed at `~/.ciaoroot/master`. Proper uninstallation
requires executing `./ciao-boot.sh uninstall` from that directory.
It can be automatized with a script like:
```sh
( cd ~/.ciaoroot/master; ./ciao-boot.sh uninstall )
rm -rf ~/.ciaoroot/master
rmdir ~/.ciaoroot > /dev/null 2>&1 || true
```
