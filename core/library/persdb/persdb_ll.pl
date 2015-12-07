:- package(persdb_ll).
% Like 'persdb', but do not redefine builtins (*_fact predicates).

:- use_package(library(persdb/persdb_decl)).

:- use_module(library(persdb/persdb_rt), [%donotredefinebuiltins
        passerta_fact/1, 
        passertz_fact/1, 
        pretract_fact/1,
        init_persdb/0, 
        initialize_db/0,
        make_persistent/2,
        update_files/0,
        update_files/1]).

% TODO: Do not use initialization here (at least, add priorities)
:- initialization(init_persdb).

:- load_compilation_module(library(persdb/persdb_tr)).
:- add_sentence_trans(persdb_tr:persistent_tr/2, 1110).
