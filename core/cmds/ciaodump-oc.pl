:- module(_, [], [compiler(complang)]).

%! \title Dump Ciao compilation files (optim_comp)
%  \author Jose F. Morales

:- use_module(compiler(module_pli)).
:- use_module(compiler(module_deps)).
:- use_module(compiler(module_itf)).
:- use_module(compiler(module_exp)).
:- use_module(compiler(module_ideps)).
:- use_module(compiler(errlog)).
:- use_module(compiler(memoize)). % TODO: because it is required by some imports...
:- use_module(compiler(linker__bytecode)).
:- use_module(compiler(linker__bootstrap)).
:- use_module(compiler(store)).

% TODO: only for file types in 'store'?
:- use_module(compiler(frontend)).
% TODO: only for file types in 'store'
:- use_module(compiler(action__archcompile), []).

:- use_module(library(process), [process_call/3]).
:- use_module(library(terms), [atom_concat/2]).

% TODO: document and change names, 'What' is not a good name...

:- export(main/1).
main(Args) :-
    ( main__2(Args) ->
        true
    ; halt(-1)
    ).

main__2(['--resolve', Type, Spec0]) :- !,
    store:find_source(Spec0, relpath('.'), Spec),
    resolve(Type, Spec).
main__2(['--module', What, Spec0]) :- !,
    store:find_source(Spec0, relpath('.'), Spec),
    dump(What, Spec).
main__2(['--file', What, FileName]) :- !,
    % TODO: check that FileName exists
    dump__2(What, FileName).
% shorthands
main__2(['--dump', Spec]) :- !,
    main__2(['--module', compile__dump, Spec]).
main__2(['--disasm', Spec]) :- !,
    main__2(['--module', compile__emu, Spec]).

% ---------------------------------------------------------------------------
% Resolve the file name of a module
resolve(Type, Spec) :-
    functor(ActionG, Type, 1),
    arg(1, ActionG, Spec),
    store:addr(ActionG, Name),
    display(Name), nl.

% ---------------------------------------------------------------------------
% Dump contents (of element Type) of a module

:- use_module(engine(ql_inout)).
:- use_module(library(write)).

dump(What, Spec) :-
    what_type(What, Type),
    functor(ActionG, Type, 1),
    arg(1, ActionG, Spec),
    store:addr(ActionG, Name),
    dump__2(What, Name).

% ---------------------------------------------------------------------------
% Dump contents of a given type of a file

what_type(dectok, compile__emu) :- !.
what_type(X, X) :- !.

dump__2(What, Name) :- text_type(What), !,
    process_call(path(cat), [Name], [status(0)]). % TODO: requires external tool
dump__2(What, Name) :- archdump_type(What), !,
    archdump(Name).
dump__2(What, Name) :- ql_type(What), !,
    ql_dump(Name).
dump__2(What, Name) :- bytecode_type(What), !,
    bytecode_disassemble(Name).
dump__2(dectok, Name) :- !,
    bytecode_dectok(Name).
dump__2(What, _Name) :-
    write('error: unknown dump method for file type '),
    write(What), nl.

text_type(compile__dump).
text_type(compile__c).
text_type(compile__h).
text_type(prolog_source).
text_type(c_source).
text_type(s_object).
text_type(archcompile__s).
text_type(prolog_ptocdump).

archdump_type(archcompile__o).
archdump_type(archcompile__so).

ql_type(archcompile).
ql_type(compile).
ql_type(split).
ql_type(split__src).
ql_type(split__itf).
ql_type(expand__ideps).
ql_type(expand__sym).

bytecode_type(compile__emu).

% ---------------------------------------------------------------------------

ql_dump(Name) :-
    '$open'(Name, r, Stream),
    '$qread_begin'(Stream),
    repeat,
      ( '$qread'(Goal) ->
          writeq(Goal), nl,
          fail
      ; true
      ), !,
    '$qread_end',
    close(Stream).

% ---------------------------------------------------------------------------

bytecode_disassemble(Name) :-
    '$open'(Name, r, Stream),
    '$qread_begin'(Stream),
    repeat,
      ( bytecode_disassemble_1 -> fail % loop
      ; true % end
      ),
    !,
    '$qread_end',
    close(Stream).

bytecode_disassemble_1 :-
    '$qread'(Module),
    \+ Module = -1, % TODO: Wrong!!! qread must fail in this case
    write('module='), writeq(Module), nl,
    '$qread'(SpecKey),
    write('speckey='), writeq(SpecKey), nl,
    read_metadatas,
    '$qread'(Count),
    write('defs='), writeq(Count), nl,
    repeat,
      ( '$qread'(Goal), \+ Goal = e(_) ->
          ( Goal = b(Bytecode, f(Bits, Key)) ->
              write('bits='), writeq(Bits),
              ( functor(Key, N, A) ->
                  write(' key='), writeq(N/A)
              ; write(' nokey')
              ),
              write(' code='),
              nl,
              '$disasm'(Bytecode)
          ; Goal = d(PredName, Bits) ->
              writeq(PredName), write(':'),
              write(' def='),
              writeq(Bits),
              nl
          ; Goal = c(X) ->
              write('count='), writeq(X),
              nl
          ; Goal = f(X) ->
              writeq(X),
              nl
          ; write('unknown '), writeq(Goal), nl % TODO: incomplete...
          ),
          fail
      ; true
      ), !.

read_metadatas :-
    repeat,
      ( '$qread'(X) -> true ; X = c /* todo: error */ ),
      ( X = c -> true % end
      ; msg_metadata__c(X),
        '$qread'(N),
        ( repeat(N),
            read_metadata__c(X),
            fail
        ; true
        ),
        fail % loop
      ),
    !.

repeat(N) :- N < 1, !, fail.
repeat(1) :- !.
repeat(_).
repeat(N) :- N1 is N - 1, repeat(N1).

msg_metadata__c(u) :-
    write('uses='), nl.
msg_metadata__c(exports) :-
    write('exports='), nl.
msg_metadata__c(defines) :-
    write('defines='), nl.
msg_metadata__c(imports_all) :-
    write('imports_all='), nl.
msg_metadata__c(imports) :-
    write('imports= '), nl.
msg_metadata__c(meta_args) :-
    write('meta_args='), nl.
msg_metadata__c(context) :-
    write('context='), nl.

read_metadata__c(u) :-
    '$qread'(IM),
    write('  '),
    writeq(IM), nl.
read_metadata__c(exports) :-
    '$qread'(F),
    '$qread'(N),
    '$qread'(MF),
    write('  '),
    writeq(F), write('/'), writeq(N), write(' as '), writeq(MF), nl.
read_metadata__c(defines) :-
    '$qread'(F),
    '$qread'(N),
    '$qread'(MF),
    write('  '),
    writeq(F), write('/'), writeq(N), write(' as '), writeq(MF), nl.
read_metadata__c(imports_all) :-
    '$qread'(IM),
    write('  '),
    writeq(IM), nl.
read_metadata__c(imports) :-
    '$qread'(F),
    '$qread'(N),
    '$qread'(IM),
    '$qread'(MF),
    write('  '), writeq(IM), write(':'),
    writeq(F), write('/'), writeq(N),
    write(' as '), writeq(MF), nl.
read_metadata__c(meta_args) :-
    '$qread'('multifile:$meta_args'(Meta2)),
    write('  '), writeq(Meta2), nl.
read_metadata__c(context) :-
    '$qread'('multifile:$context'(MF, A, Context)),
    write('  '), writeq(MF), write('/'),
    writeq(A), write(' has '), writeq(Context), nl.

% ---------------------------------------------------------------------------
% Disassemble assembler code (architecture-dependent native code)

% Note: it depends on external tools ('otool' in macOS and
%   'objdump' in Linux).

:- use_module(engine(system_info), [get_os/1]).

archdump(Name) :-
    ( get_os('DARWIN') ->
        process_call(path(otool), ['-Vt', Name], [status(0)])
    ; process_call(path(objdump), ['-d', Name], [status(0)])
    ).

% ---------------------------------------------------------------------------
% Count used opcodes in a bytecode file

:- data used_opcode/1.
:- data size/1.
size(0).

:- use_module(compiler(open_and_protect)).
:- use_module(compiler(module_ipexp)).
:- use_module(compiler(dynload), [get_memo/1]).
:- use_module(compiler(frontend), [load_absmach/0]).

bytecode_dectok(BytecodeName) :-
    dynload:get_memo(Memo),
    call((
      memo :: memoize <- Memo,
      frontend:load_absmach
    )),
    reset_dectok,
    TmpName = '/tmp/dectok.txt',
    write_dectok_to_file(BytecodeName, TmpName),
    read_dectok_from_file(TmpName),
    dump_size,
    dump_opcodes,
    reset_dectok.

reset_dectok :-
    retractall_fact(used_opcode(_)),
    retractall_fact(size(_)),
    asserta_fact(size(0)).

write_dectok_to_file(BytecodeName, TmpName) :-
    open_and_protect(TmpName, TmpStream, Ref),
    write_dectok(BytecodeName, TmpStream),
    close(TmpStream),
    end_protect(Ref).

write_dectok(Name, TmpStream) :-
    current_output(OldOutput),
    set_output(TmpStream),
    Ok = ( write_dectok__2(Name) ? yes | no ),
    set_output(OldOutput),
    Ok = yes.

write_dectok__2(Name) :-
    '$open'(Name, r, Stream),
    '$qread_begin'(Stream),
    repeat,
      ( '$qread'(Goal) ->
          ( Goal = d(PredName, Bits) ->
              write('% '), writeq(PredName),
              write(' '),
              writeq(Bits), nl
          ; Goal = b(Bytecode, f(Bits, Key)) ->
              write('% '), 
              writeq(f(Bits, Key)), nl,
              '$dectok'(Bytecode)
          ; true
          ),
          fail
      ; true
      ), !,
    '$qread_end',
    close(Stream).

:- use_module(library(read)).
read_dectok_from_file(TmpName) :-
    '$open'(TmpName, r, Stream),
    repeat,
      read(Stream, X),
      ( X = end_of_file
      ; X = bytecode(S, Xs),
        sum_size(S),
        mark_opcodes(Xs),
        fail
      ), !,
    close(Stream).

mark_opcodes([]).
mark_opcodes([X|Xs]) :-
    ( current_fact(used_opcode(X)) ->
        true
    ; assertz_fact(used_opcode(X))
    ),
    mark_opcodes(Xs).

sum_size(S) :-
    current_fact(size(T0)),
    T is T0 + S,
    set_fact(size(T)).

dump_size :-
    current_fact(size(T)),
    % TODO: only instructions, not symbols or atoms
    display(bytecode_size(T)), display('.'), nl.

dump_opcodes :-
    '$absmach'(Absmach),
    Absmach.ins_op(Ins, Op), 
      ( current_fact(used_opcode(Op)) ->
          true
      ; display(unused_opcode(Op)), display('. % '), display(Ins), nl
      ),
      fail.
dump_opcodes.
