:- module(_,[main/0],[]).

:- use_module(library(terms)).
%:- use_module(library(vndict)).                                               % it has wrong assertions and regtypes.
%:- use_module(library(compiler/srcdbg)).                                      % it uses c_itf
%:- use_module(library(compiler/c_itf)).                                       % it uses build_foreign_interface
:- use_module(library(compiler/translation)).
:- use_module(library(compiler/pl2wam)).
%:- use_module(library(compiler/build_foreign_interface)).            % it uses assrt_lib
:- use_module(library(messages)).
:- use_module(library(assertions/c_itf_props)).
%:- use_module(library(assertions/assrt_lib)).                                 % it uses assrt_write+assertions_props
%:- use_module(library(assertions/assrt_write)).                               % it uses assrt_lib+assertions_props+vndict!!
%:- use_module(library(assertions/assertions_props)).                          % it has wrong regtypes.
:- use_module(library(assertions/native_props)).
%:- use_module(library(andprolog/andprolog_rt)).
:- use_module(library(concurrency)).
%:- use_module(library(miscprops)).
:- use_module(library(dcg/dcg_tr)).
:- use_module(library(foreign_compilation)).
:- use_module(library(dynamic/dynamic_rt)).
:- use_module(library(ctrlcclean)).
:- use_module(library(errhandle)).
:- use_module(library(fastrw)).
:- use_module(library(formulae)).
:- use_module(engine(runtime_control)).
:- use_module(library(pathnames)).
:- use_module(library(idlists)).

:- use_module(library(terms_vars)).
:- use_module(library(sets)).
:- use_module(library(persdb/persdb_rt)).
:- use_module(library(persdb/persdb_cache)).
%:- use_module(library(pretty_print)).                                         % it uses vndict
:- use_module(library(file_locks)).
:- use_module(library(write_c)).
:- use_module(library(write_c/write_tokens)).
:- use_module(library(llists)).
:- use_module(library(sockets)).
%jcf%:- use_module(library(sockets/sockets_c)).
%pp%%:- use_module(library(ciaodeconfig)).
%pp%%:- use_module(library(runtime_ops)).
%pp%%:- use_module(library(ciaoconfig)).
:- use_module(library(lists)).
:- use_module(library(foreign_compilation)).
:- use_module(library(sort)).
%:- use_module(library(miscprops)).
:- use_module(library(llists)).
:- use_module(library(ttyout)).
:- use_module(library(libpaths)).
%pp%%:- use_module(library(prelude)).
:- use_module(library(stream_utils)).
%pp%%:- use_module(library(dcg)).
:- use_module(engine(io_basic)).
:- use_module(engine(stream_basic)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(strings)).
%pp%:- use_module(library(between)).
%pp%:- use_module(library(goal_trans)).
%pp%%% Warning: following module reexports other modules, and CiaoPP complains bitterly.
%pp%%kk%:- use_module(library(default_predicates)).
:- use_module(library(format)).
%pp%:- use_module(library(dict)).
:- use_module(library(dec10_io)).
%pp%%:- use_module(library(nodebug)).
%pp%%:- use_module(library(dcg/dcg_doc)).
%pp%%:- use_module(library(isomodes)).
%pp%:- use_module(library(dcg/dcg_tr)).
%pp%%:- use_module(library(runtime_ops_doc)).
%pp%%:- use_module(library(pure)).
%pp%%:- use_module(library(noprelude)).
:- use_module(library(read)).
:- use_module(library(aggregates)).
%pp%
%pp%
%pp%:- use_module(library(runtime_ops_tr)).
%pp%%:- use_module(library(isomodes_doc)).
%pp%%:- use_module(library(hiord)).
:- use_module(library(old_database)).
%pp%%:- use_module(library(pure_doc)).
:- use_module(library(tokenize)).
%pp%%:- use_module(library(debug)).
:- use_module(library(system)).
:- use_module(library(cyclic_terms)).
%pp%%:- use_module(library(basicmodes)).
:- use_module(library(odd)).
:- use_module(library(iso_misc)).
:- use_module(library(attrdump)).
%pp%% :- use_module(library(trace)).
%pp%% :- use_module(library(basicmodes_doc)).
%pp%% :- use_module(library(iso)).
%pp%% :- use_module(library(nonpure)).
%pp%% :- use_module(library(default)).
:- use_module(library(operators)).
:- use_module(library(iso_char)).
:- use_module(library(iso_incomplete)).
%pp%%:- use_module(library(default_for_ciaosh)).


main.

