% NOTE: This is part of old p_asr:do_cache/0. That cache mechanism was
% incomplete. We used gen_lib_sources/1 instead

%% ---------------------------------------------------------------------------
% CACHE
%% ---------------------------------------------------------------------------

ast_cache([
            library(aggregates),
            library(debugger),
            library(lists),
% hiord?
%       library(hiord_rt),
            library(sort),
            library(terms_check),
            %
            engine(term_basic),
            engine(arithmetic),
            engine(debugger_support),
            engine(mattr_global),
            engine(term_compare),
            engine(term_typing),
            engine(atomic_basic),
            engine(exceptions),
            engine(runtime_control),
            engine(attributes),
            engine(basic_props),
            engine(internals),
            engine(basiccontrol),
            engine(messages_basic),
            engine(stream_basic),
            library(datafacts/datafacts_rt),
            engine(io_basic),
            engine(system_info)
        ]
).

do_cache :-
    cleanup_pasr,
    removeall_assertion_read(_, _, _, _, _, _, _, _, _),
    ast_cache(Modules),
% this funcion assert 'related_files' in order to make
% related_files_closure work.
    transform_to_related_files(Modules, _Files, Names),
    related_files_closure(direct, quiet, []),
    open_asr_to_write(ciaopp_cache, Stream, CI),
    write_asr_header(Stream),
    set_fact(generate_asr_file(Stream)),
    (
% save assertions_of
        save_cache_assertions(Names, Stream),
% save prop_clauses
        save_prop_clauses(Names),
% save related_files
        save_related_files,
% save processed_files
        save_processed_files
    ->
        true
    ;
        message(error, ['There was an error generating cache.'])
    ),
% save processed_files  
    retractall_fact(generate_asr_file(Stream)),
    close_asr(Stream, CI).

transform_to_related_files([],     [],     []).
transform_to_related_files([M|Ms], [F|Fs], [N|Ns]) :-
    absolute_file_name(M, F),
    !,
    get_module_from_path(F, N),
    add_related_file(F),
    transform_to_related_files(Ms, Fs, Ns).
transform_to_related_files([_|Ms], F, N) :-
    transform_to_related_files(Ms, F, N),
    !.

save_cache_assertions([],     _).
save_cache_assertions([M|Ms], S) :-
    findall(As, get_mod_assertion(M, As), L),
    fast_write_assertions(L, S),
    save_cache_assertions(Ms, S).

get_mod_assertion(M, As) :-
    As = as${ module => M },
    get_assertion(_, As).

save_prop_clauses([]).
save_prop_clauses([M|Ms]) :-
    save_prop_clauses__(M),
    save_prop_clauses(Ms).

save_prop_clauses__(M) :-
% relevant_prop( M, Prop ),
% db_clause_of( Prop, _ , M,Head,Body,VarNames,Source,Line0,Line1),
    prop_clause_read(M, Head, Body, VarNames, Source, Line0, Line1),
    Fact = prop_clause_read(M, Head, Body, VarNames, Source, Line0, Line1),
    write_asr_fact(Fact),
    fail.
save_prop_clauses__(_).

save_related_files :-
    current_fact(related_file(F)),
    write_asr_fact(related_file(F)),
    fail.
save_related_files.

save_processed_files :-
    current_fact(processed_file(F)),
    write_asr_fact(processed_file(F)),
    fail.
save_processed_files.

