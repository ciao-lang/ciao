This describes the installation procedure for the Ciao system,
including libraries and manuals:

1. Check / install the [**requirements and dependencies**](#Requirements%20and%20dependencies).
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
debugging, context-sensitive on-line help, generating documentation,
etc., etc.

In any case, it is easy to use Ciao from a terminal and edit code with
any editor of your choice. The top level includes some minimal editing
facilities when [rlwrap](https://github.com/hanslub42/rlwrap) is
available.

## Installing dependencies for your operating system

Depending on the operating system and package management tool, you may
need to install the following dependencies (using `sudo` or as
administrator or `root` user):

 - **Debian/Ubuntu**:
   ```sh
   $ apt-get install build-essential
   $ apt-get install emacs # optional
   $ apt-get install rlwrap # optional
   $ apt-get install texlive texinfo imagemagick # optional
   ```

 - Debian/Ubuntu (additional for **x86, 32-bits**):
   ```sh
   $ apt-get install gcc-multilib libc6-i386 libc6-dev-i386 g++-multilib
   ```
	
 - **Fedora**:
   ```sh
   $ dnf install gcc make which kernel-headers kernel-devel
   $ dnf install emacs # optional
   $ dnf install rlwrap # optional
   $ dnf install texlive texinfo texinfo-tex ImageMagick # optional
   ```

 - Fedora (additional for **x86, 32-bits**):
   ```sh
   $ dnf install glibc-devel.i686 glibc-devel libstdc++-devel.i686
   ```
	
 - **Arch Linux**:
   ```sh
   $ pacman -Syu # optional, upgrade if needed
   $ pacman -S base-devel linux-headers
   $ pacman -S emacs # optional
   $ pacman -S rlwrap # optional
   $ pacman -S texlive-core texinfo imagemagick # optional
   ```

 - Arch Linux (additional for **x86, 32-bits**):
   ```sh
   $ pacman -S lib32-glibc lib32-libstdc++5
   ```
   Remember to enable multilib (https://wiki.archlinux.org/index.php/Multilib).

 - **macOS**:
   - Install command line tools for Xcode (from the App store)
   - Install emacs and ImageMagick (and texinfo if needed, but recent
     versions of macOS include it). A software management tool like
     homebrew or macports is recommended, e.g. (as root/sudo):
     ```sh
     $ brew install emacs-mac imagemagick
     ```
   - Install TexLive. If using homebrew we recommend installing the
     MacTeX distribution, available from: `https://www.tug.org/mactex`.
   - Install the following packages:
     ```sh
     $ brew install rlwrap # optional
     ```
	
 - **FreeBSD**:
   - Install GCC or clang and the following packages:
     ```sh
     $ pkg install gmake
     $ pkg install emacs ImageMagick expat # optional
     $ pkg install rlwrap # optional
     $ pkg install texinfo texlive-full # optional
     ```

 - **NetBSD**:
   - Install GCC or clang and the following packages:
     ```sh
     $ pkgin install gmake
     $ pkgin install emacs ImageMagick expat # optional
     $ pkgin install rlwrap # optional
     $ pkgin install tex-texinfo texlive-collection-latex # optional
     ```

 - **Windows** (using Windows Subsystem or Linux):
   - Install [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10)
   - Install some Linux distribution (e.g., Ubuntu) from the Windows Store.
   - Open a `wsl` terminal and install the dependencies for the
     selected distribution (see points above), e.g., Debian/Ubuntu.

 - Windows (native, **experimental**)
   - Install [MSYS2](http://www.msys2.org/) and the following packages:
     ```sh
     pacman --noconfirm -S mingw-w64-x86_64-gcc
     ```
	
 - **Android** (using the Termux Linux environment):
   - Install [Termux](https://termux.com/)
   - Install the following packages:
     ```sh
     $ pkg install clang make
     $ pkg install emacs # optional
     $ pkg install rlwrap # optional
     ```
	
The dependencies above typically include a relatively modern C
compiler (GCC or clang), libraries, and build tools. The optional
dependencies are:

 - [emacs](https://www.gnu.org/software/emacs/): required for
   emacs-based IDE (use `emacs-nox` if no graphical interface is
   needed)
 - [ImageMagick](https://en.wikipedia.org/wiki/ImageMagick): used
   image conversion in `lpdoc` (use `ImageMagick-nox` if no graphical
   interface is needed)
 - [TeX](https://en.wikipedia.org/wiki/TeX): typesetting system used
   as backed to generate documentation in
   [info](https://www.gnu.org/software/texinfo/manual/texinfo/html_node/Info-Format-Specification.html)
   and PDF formats.
 - [rlwrap](https://github.com/hanslub42/rlwrap): readline wrapper
   utility that provides line edition from terminals.

# Upgrading or uninstalling components

Bundles installed via `ciao get` must be uninstalled and removed
explicitly. Currently, this needs to be done manually per bundle.
Upgrading a bundle requires uninstallation followed by
(re)installation.

E.g., uninstalling the **development environment** (if installed)
requires: 
```sh
ciao uninstall ciao_emacs; ciao rm ciao_emacs; ciao rm devenv
```

# Upgrading or uninstalling Ciao

For full cleanup, first remove all installed bundles (see above) .
Once all bundles have been removed, the core Ciao system can be
uninstalled running `./ciao-boot.sh uninstall` from the source
directory; then, remove the directory.

If installed using `curl`, the system is installed in a per-version
subdirectory under `~/.ciaoroot/<ciao_version>`. E.g., the development
version is installed at `~/.ciaoroot/master`. Proper uninstallation
requires executing `./ciao-boot.sh uninstall` from that directory.

It can be automated with a script such as:
```sh
( cd ~/.ciaoroot/master; ./ciao-boot.sh uninstall )
rm -rf ~/.ciaoroot/master
rmdir ~/.ciaoroot > /dev/null 2>&1 || true
```
