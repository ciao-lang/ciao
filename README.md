[![Build Status](https://travis-ci.org/ciao-lang/ciao.svg)](https://travis-ci.org/ciao-lang/ciao)
[![Build Status](https://ci.appveyor.com/api/projects/status/fu2eb23je22xc228?svg=true)](https://ci.appveyor.com/project/jfmc/ciao)

# The Ciao Programming Language

[Ciao](https://ciao-lang.org) is a programming language that builds up
from a logic-based simple kernel, and is designed to be extensible and
modular. Its supports:

 - **constraint** logic programming (and, in particular, **Prolog**)
 - different levels of modularity (from small to large scale)
   - **modules** as (analysis-friendly) compilation units
   - **bundles** as collections of modules
 - **packages** as modules implementing language extensions
   (syntactic definitions, compilation options, compiler plugins)
 - **assertions** (as a homogeneous framework that allows static and
   dynamic verification to work cooperatively in a unified way)
 - **multiparadigm** constructs (meta-programming, higher-order,
   mutables, concurrency, functions, etc.) and interfacing with
   **foreign** code

This repository contains the compiler and Ciao standard libraries.
Together with the `builder` bundle it can be used as a *minimal* Ciao
installation.

Other advanced features (such as global program analysis and
transformations, static debugging,
[documentation generation](https://github.com/ciao-lang/lpdoc),
[Emacs-based development environment](https://github.com/ciao-lang/devenv),
etc.) are provided in [separate bundles](https://github.com/ciao-lang).

## Installation

Please consult the [quick installation](https://ciao-lang.org/install.html)
instructions or refer to the [INSTALLATION](core/INSTALLATION) file.

---
**NOTE**: This repository is [automatically synchronized](https://github.com/ciao-lang/ciao-distro-tools) from the Ciao monorepo.
