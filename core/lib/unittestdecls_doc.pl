:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(filetype,package).
:- new_declaration(load_test_module/1).
:- new_declaration(load_test_module/2).
:- new_declaration(load_test_package/1).

:- doc(title,"Loading auxiliary test-related code").

:- doc(module,"This package provides declarations that can be used to
   load modules and packages needed for testing but which one does not
   want to be part of the module being tested.").

:- doc(bug, "load_compilation_module, load_test_module and
             load_resource_module directives have similar behavior").

:- decl load_test_module(Module) : sourcename
# "Specifies an auxiliary module that must be loaded in order
   to execute the tests.".

:- decl load_test_module(Module, PredNames)
: ( sourcename(Module), list(PredNames) ) 

# "Specifies a module and the list of predicates that must be loaded
   in order to execute the tests".

:- decl load_test_package(Module) : sourcename
# "Specifies a package that must be used in order to execute 
   the tests.".
