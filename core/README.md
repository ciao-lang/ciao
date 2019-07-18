# Ciao compiler and standard libraries

This bundle contains the compiler and Ciao standard libraries.
Together with the `builder` bundle, it can be used as a *minimal* Ciao
installation.

Code is organized as follows:

```
  bootstrap/    bootstrap compiler (bytecode and auto-generated C)
  ciaoc/        main module for the compiler command-line
  shell/        main module for the Ciao toplevel
  cmds/         other command-line tools
  engine/       Ciao modules and C code for the engine
  lib/          essential libraries
                (including the compiler and toplevel)
  library/      additional standard libraries
```

See the `INSTALLATION` file for detailed installation instructions.

