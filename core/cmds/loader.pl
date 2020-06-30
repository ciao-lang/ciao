:- module(_, _, [noprelude, compiler(complang)]).

%! \title Executable loader (optim_comp)
%  \author Jose F. Morales

:- use_module(engine(basiccontrol)).
:- use_module(engine(arithmetic)).
:- use_module(engine(term_basic)).
:- use_module(engine(basic_props)).

:- use_module(engine(rt_exp), [do_initialization/1]).
:- use_module(engine(dynlink), [link_pack/1]).
:- use_module(engine(internals), ['$unix_shift_arg'/0]).
:- use_module(engine(debugger_support), []). % TODO: can this be made optional? (otherwise toplevel breaks)

:- use_module(compiler(errlog)).
:- use_module(compiler(memoize)).
:- use_module(compiler(action__load)).

:- import(internal_init, ['$eager_load'/2, '$main_module'/1, '$main_entry'/1]).

main([File|Args]) :-
    '$unix_shift_arg', % TODO: shift arguments, better solution? 
    dynlink:link_pack(File),
    Errs = ~errlog.new,
    Verbose = off,
    % TODO: use runtime memoize?
    Errs.add(verbose(Verbose)),
    Memo = ~memoize.new(Errs),
    ( '$eager_load'(Spec, Bits),
      HasQL = ( Bits /\ 1 =:= 1 ? yes | no ),
      HasSO = ( Bits /\ 2 =:= 2 ? yes | no ),
      load_module__internal(Spec, HasQL, HasSO),
      fail
    ; true
    ),
    Errs.summary,
    Memo.delete,
    Errs.delete,
    ( '$main_module'(MainModule),
      rt_exp:do_initialization(MainModule),
      fail
    ; true
    ),
    '$main_entry'(Args).
