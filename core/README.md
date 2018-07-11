# Ciao compiler and standard libraries

Ciao is a programming language that builds up from a logic-based
simple kernel, and is designed to be extensible and modular. Its
supports:

 - **constraint** logic programming (and, in particular, **Prolog**)
 - different levels of modularity (from small to large scale)
   - **modules** as (analysis-friendly) compilation units
   - **bundles** as collections of modules
 - **packages** as modules implementing language extensions
   (syntactic definitions, compilation options, compiler plugins)
 - **assertions** (as an homogeneous framework that allows static and
   dynamic verification to work cooperatively in a unified way)
 - **multiparadigm** constructs (meta-programming, higher-order,
   mutables, concurrency, functions, etc.) and interfacing with
   **foreign** code

This bundle contains the compiler and Ciao standard libraries.
Together with the `builder` bundle it can be used as a *minimal* Ciao
installation.

Other advanced features (such as global program analysis and
transformations, static debugging, documentation generation,
development environments, etc.) are provided in separate bundles.

See the `INSTALLATION` file for installation instructions.
