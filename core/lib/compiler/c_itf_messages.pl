% (included code)

% Message context for c_itf

% requires:
%   doing_verbose/1
%   compiling_src/1 (data)

% ---------------------------------------------------------------------------
% pass name

:- data doing_what/1, doing_written/1.

now_doing(M) :-
    doing_verbose(VF),
    now_doing_(VF, M).

now_doing_(on, M)  :- message(error0, ['{'| M]).
now_doing_(off, M) :- asserta_fact(doing_what(M)).

end_doing :-
    doing_verbose(VF),
    end_doing_(VF).

end_doing_(on)  :- message(error0, '}').
end_doing_(off) :-
    retract_fact(doing_what(M)), !,
    ( retract_fact(doing_written(M)) ->
        message(error0, '}')
    ; true
    ).

put_doing(Type) :- message_type_visible(Type), !, % only if message is visible
    doing_verbose(VF),
    put_doing_(VF).
put_doing(_).

put_doing_(on).
put_doing_(off) :-
    current_fact(doing_what(M)), !,
    ( doing_written(M) -> true
    ; asserta_fact(doing_written(M)),
      message(error0, ['{'| M])
    ).
put_doing_(off).

% ---------------------------------------------------------------------------
% source name

:- data last_error_in_src/1.

put_src_if_needed(Type, Src) :- message_type_visible(Type), !, % only if message is visible
    put_src_if_needed_(Src).
put_src_if_needed(_, _).

put_src_if_needed_(Src) :-
    current_fact(last_error_in_src(Src0), Ref), !,
    ( Src = Src0 -> true
    ; erase(Ref),
      message(error0, '}'),
      put_src_if_needed_(Src)
    ).
put_src_if_needed_(Src) :-
    current_fact(compiling_src(Src)), !.
put_src_if_needed_(Src) :-
    message(error0, ['{In ',Src]),
    asserta_fact(last_error_in_src(Src)).

end_brace_if_needed :-
    ( retract_fact(last_error_in_src(_)) ->
        message(error0, '}')
    ; true
    ).

