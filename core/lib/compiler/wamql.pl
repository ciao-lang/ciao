% Compiler Back-End

/*** I_GAUGE
asm_insns([ifshallow, neck(N), else, endif|Insns], InsnModel, Off0, Off) -->
	{Off1 is Off0+2, collapse(Insns, CInsns, [])},
	[4],
	asm_cinsns([none(65 )|CInsns],InsnModel, _, Off1, Off, _), !.
asm_insns(Insns, InsnModel, Off0, Off) -->
	{Off1 is Off0+2, collapse(Insns, CInsns, [])},
	[ElseOff],
	asm_cinsns(CInsns, InsnModel, ElseOff, Off1, Off, _), !.

asm_cinsns([], [], 2, Off, Off, 0) --> [].   % 2 if deep pc = shallow pc
asm_cinsns([ifshallow|Is], InsnModel, ElseOff, Off0, Off, InsCt) --> !,
	asm_cinsns(Is, InsnModel, ElseOff, Off0, Off, InsCt).
asm_cinsns([else|Is], InsnModel, Off1, Off0, Off, InsCt) --> !,
	{arg(1, i(68 ,_), Opcode)},
	{Off1 is Off0+4},
	c([Opcode, Distance|T], T),
	asm_cinsns(Is, InsnModel, EndifOff, Off1, Off, InsCt0),
	{InsCt is InsCt0 + 1},
	{Distance is EndifOff-Off0-2}.
asm_cinsns([endif|Is], InsnModel, Off0, Off0, Off, InsCt) --> !,
	asm_cinsns(Is, InsnModel, _, Off0, Off, InsCt).
asm_cinsns([profile_point(PP)|Is], [PP - insn_cnt(_,InsCt,_)|InsnModel], 
	   ElseOff, Off0, Off, 0) --> !,
	asm_cinsns(Is, InsnModel, ElseOff, Off0, Off, InsCt).
asm_cinsns([profile_point(_)|Is], InsnModel, ElseOff, Off0, Off, InsCt) --> !,
	asm_cinsns(Is, InsnModel, ElseOff, Off0, Off, InsCt).
asm_cinsns([counter(X,Y)|Is],InsnModel, XOff, Off0, Off, InsCt) --> !,
	asm_args(counter(X,Y), Off0, Off1),
	asm_cinsns(Is, InsnModel, XOff, Off1, Off, InsCt).
asm_cinsns([I|Is], InsnModel, XOff, Off0, Off, InsCt) --> 
	asm_args(I, Off0, Off1),
	asm_cinsns(Is, InsnModel, XOff, Off1, Off, InsCt0),
	{InsCt is InsCt0 + 1}.

S_GAUGE ***/

asm_insns([ifshallow, neck(_), else, endif|Insns], _, Off0, Off) -->
	{Off1 is Off0+2, collapse(Insns, CInsns, [])},
	[4],
	asm_cinsns([none(65 )|CInsns], _, Off1, Off), !.
asm_insns(Insns, _, Off0, Off) -->
	{Off1 is Off0+2, collapse(Insns, CInsns, [])},
	[ElseOff],
	asm_cinsns(CInsns, ElseOff, Off1, Off), !.

asm_cinsns([], 2, Off, Off) --> [].		% 2 if deep pc = shallow pc
asm_cinsns([ifshallow|Is], ElseOff, Off0, Off) --> !,
	asm_cinsns(Is, ElseOff, Off0, Off).
asm_cinsns([else|Is], Off1, Off0, Off) --> !,
	{arg(1, i(68 ,_), Opcode)},
	{Off1 is Off0+4},
	c([Opcode, Distance|T], T),
	asm_cinsns(Is, EndifOff, Off1, Off),
	{Distance is EndifOff-Off0-2}.
asm_cinsns([endif|Is], Off0, Off0, Off) --> !,
	asm_cinsns(Is, _, Off0, Off).
asm_cinsns([I|Is], XOff, Off0, Off) --> 
	asm_args(I, Off0, Off1),
	asm_cinsns(Is, XOff, Off1, Off).

/*** F_GAUGE ***/

collapse([]) --> [].
collapse([Insn|Insns]) --> 
	collapse(Insn, Insns).

collapse(ifshallow, Insns) --> !,
	[ifshallow], collapse(Insns).
collapse(else, Insns) --> !,
	[else], collapse(Insns).
collapse(endif, Insns) --> !,
	[endif], collapse(Insns).
collapse(call(X,Y), Insns) --> !,
	collapse_call(call(X,Y), Insns, L, L).
collapse(cutb, [neck(_),proceed|Insns]) --> !,
	[none(213 )], collapse(Insns).
collapse(cutb, [neck(_)|Insns]) --> !,
	[none(211 )], collapse(Insns).
collapse(cutb, Insns) --> !,			% MOVED FROM PLWAM
	collapse(Insns).
collapse(cute, [neck(_)|Insns]) --> !,
	[none(217 )], collapse(Insns).
collapse(cute, Insns) --> !,			% MOVED FROM PLWAM
	collapse(Insns).
collapse(cutb_x(A), [neck(_),proceed|Insns]) --> !,
	[x(212 ,A)], collapse(Insns).
collapse(cutb_x(A), [neck(_)|Insns]) --> !,
	[x(210 ,A)], collapse(Insns).
collapse(cute_x(A), [neck(_)|Insns]) --> !,
	[x(216 ,A)], collapse(Insns).
collapse(neck(_), [proceed|Insns]) --> !,
	[none(66 )], collapse(Insns).
collapse(allocate, [Insn|Insns]) --> !,
	collapse_allocate(Insn, Insns).
collapse(deallocate, [Insn|Insns]) --> !,
	collapse_deallocate(Insn, Insns).
collapse(put_x_void(X), Insns) --> !,
	collapse_call(put_x_void(X), Insns, L, L).
collapse(put_x_variable(Y,X), Insns) --> !,
	collapse_call(put_x_variable(Y,X), Insns, L, L).
collapse(put_y_value(Y,X), Insns) --> !,
	collapse_call(put_y_value(Y,X), Insns, L, L).
collapse(put_y_unsafe_value(Y,X), Insns) --> !,
	collapse_call(put_y_unsafe_value(Y,X), Insns, L, L).
collapse(get_x_variable(To,From), [get_x_variable(To1,From1)|Insns]) --> !,
	[rev_x_x_x_x(108 ,To,From,To1,From1)], collapse(Insns).
collapse(get_y_variable(To,From), [get_y_variable(To1,From1)|Insns]) --> !,
	[rev_y_x_y_x(110 ,To,From,To1,From1)], collapse(Insns).
collapse(get_constant(C,X), [neck(_),proceed|Insns]) --> !,
	[tagged_x(112 ,C,X)], collapse(Insns).
collapse(get_nil(X), [neck(_),proceed|Insns]) --> !,
	[x(113 ,X)], collapse(Insns).
collapse(put_x_value(From,To), [put_x_value(From1,To1)|Insns]) --> !,
	[rev_x_x_x_x(85 ,From,To,From1,To1)], collapse(Insns).
collapse(init(L), [call(M,S)|Insns]) --> !,
	[ylist_label_size(3 ,L,M,S)], collapse(Insns).
collapse(init(L), [true(S)|Insns]) --> !,
	[ylist_size(261 ,L,S)], collapse(Insns).
collapse(put_y_variable(Y,X), [put_y_variable(Y1,X1)|Insns]) --> !,
	[rev_yv_x_yv_x(84 ,Y,X,Y1,X1)], collapse(Insns).
collapse(unify_constant(C), [neck(_),proceed|Insns]) --> !,
	[tagged(134 ,C)], collapse(Insns).
collapse(unify_nil, [neck(_),proceed|Insns]) --> !,
	[none(135 )], collapse(Insns).
collapse(unify_void, Insns0) --> !,
	{collapse_voids(Insns0, [Insn|Insns], 1, X)},
	collapse_u2_void(Insn, Insns, X).
collapse(unify_x_variable(X), [Insn|Insns]) --> !,
	collapse_u2_xvar(Insn, Insns, X).
collapse(unify_y_variable(X), [Insn|Insns]) --> !,
	collapse_u2_yvar(Insn, Insns, X).
collapse(unify_x_value(X), [Insn|Insns]) --> !,
	collapse_u2_xval(Insn, Insns, X).
collapse(unify_x_local_value(X), [Insn|Insns]) --> !,
	collapse_u2_xlval(Insn, Insns, X).
collapse(unify_y_first_value(X), [Insn|Insns]) --> !,
	collapse_u2_yfval(Insn, Insns, X).
collapse(unify_y_value(X), [Insn|Insns]) --> !,
	collapse_u2_yval(Insn, Insns, X).
collapse(unify_y_local_value(X), [Insn|Insns]) --> !,
	collapse_u2_ylval(Insn, Insns, X).
collapse(execute(A), Insns) --> !,
	[label(63 ,A)], collapse(Insns).
collapse(proceed, Insns) --> !,
	[none(64 )], collapse(Insns).
collapse(neck(_), Insns) --> !,
	[none(65 )], collapse(Insns).
collapse(fail, Insns) --> !,
	[none(67 )], collapse(Insns).
collapse(put_x_value(A,B), Insns) --> !,
	[rev_x_x(71 ,A,B)], collapse(Insns).
collapse(put_x_unsafe_value(A,B), Insns) --> !,
	[rev_x_x(72 ,A,B)], collapse(Insns).
collapse(put_y_variable(A,B), Insns) --> !,
	[rev_yv_x(74 ,A,B)], collapse(Insns).
collapse(put_constant(A,B), Insns) --> !,
	[tagged_x(78 ,A,B)], collapse(Insns).
collapse(put_large(A,B), Insns) --> !,
	[large_x(253 ,A,B)], collapse(Insns).
collapse(put_structure(A,B), Insns) --> !,
	[functor_x(80 ,A,B)], collapse(Insns).
collapse(put_nil(A), Insns) --> !,
	[x(81 ,A)], collapse(Insns).
collapse(put_list(A), Insns) --> !,
	[x(82 ,A)], collapse(Insns).
collapse(get_x_variable(A,B), Insns) --> !,
	[rev_x_x(90 ,A,B)], collapse(Insns).
collapse(get_x_value(A,B), Insns) --> !,
	[rev_x_x(91 ,A,B)], collapse(Insns).
collapse(get_y_variable(A,B), Insns) --> !,
	[rev_y_x(93 ,A,B)], collapse(Insns).
collapse(get_y_first_value(A,B), Insns) --> !,
	[rev_y_x(94 ,A,B)], collapse(Insns).
collapse(get_y_value(A,B), Insns) --> !,
	[rev_y_x(95 ,A,B)], collapse(Insns).
collapse(get_constant(A,B), Insns) --> !,
	[tagged_x(97 ,A,B)], collapse(Insns).
collapse(get_large(A,B), Insns) --> !,
	[large_x(255 ,A,B)], collapse(Insns).
collapse(get_structure(A,B), Insns) --> !,
	[functor_x(99 ,A,B)], collapse(Insns).
collapse(get_nil(A), Insns) --> !,
	[x(100 ,A)], collapse(Insns).
collapse(get_list(A), Insns) --> !,
	[x(101 ,A)], collapse(Insns).
collapse(get_constant_x0(A), Insns) --> !,
	[tagged(103 ,A)], collapse(Insns).
collapse(get_large_x0(A), Insns) --> !,
	[large(257 ,A)], collapse(Insns).
collapse(get_structure_x0(A), Insns) --> !,
	[functor(105 ,A)], collapse(Insns).
collapse(get_nil_x0, Insns) --> !,
	[none(106 )], collapse(Insns).
collapse(get_list_x0, Insns) --> !,
	[none(107 )], collapse(Insns).
collapse(unify_constant(A), Insns) --> !,
	[tagged(128 ,A)], collapse(Insns).
collapse(unify_large(A), Insns) --> !,
	[large(259 ,A)], collapse(Insns).
collapse(unify_structure(A), Insns) --> !,
	[functor(130 ,A)], collapse(Insns).
collapse(unify_nil, Insns) --> !,
	[none(131 )], collapse(Insns).
collapse(unify_list, Insns) --> !,
	[none(132 )], collapse(Insns).
collapse(cutb_x(A), Insns) --> !,
	[x(208 ,A)], collapse(Insns).
collapse(cute_x(A), Insns) --> !,
	[x(214 ,A)], collapse(Insns).
collapse(cutf, Insns) --> !,
	[none(209 )], collapse(Insns).
collapse(cutf_x(A), Insns) --> !,
	[x(215 ,A)], collapse(Insns).
collapse(cut_y(A), Insns) --> !,
	[y(218 ,A)], collapse(Insns).
collapse(choice_x(A), Insns) --> !,
	[x(219 ,A)], collapse(Insns).
collapse(choice_y(A), Insns) --> !,
	[y(221 ,A)], collapse(Insns).
collapse(function_1(A,B,C,D,E), Insns) --> !,
	[function_x_x(223 ,A,B,C,D,E)], collapse(Insns).
collapse(function_2(A,B,C,D,E,F), Insns) --> !,
	[function_x_x_x(225 ,A,B,C,D,E,F)], collapse(Insns).
collapse(builtin_1(A,B), Insns) --> !,
	[builtin_x(227 ,A,B)], collapse(Insns).
collapse(builtin_2(A,B,C), Insns) --> !,
	[builtin_x_x(229 ,A,B,C)], collapse(Insns).
collapse(builtin_3(A,B,C,D), Insns) --> !,
	[builtin_x_x_x(231 ,A,B,C,D)], collapse(Insns).
collapse(ci_call(A,B), Insns) --> !,
	[i_i(241 ,A,B)], collapse(Insns).
collapse(ci_inarg(A,B), Insns) --> !,
	[i_i(242 ,A,B)], collapse(Insns).
collapse(ci_outarg(A,B), Insns) --> !,
	[i_i(243 ,A,B)], collapse(Insns).
collapse(ci_retval(A,B), Insns) --> !,
	[i_i(244 ,A,B)], collapse(Insns).
collapse(heapmargin_call(A,B), Insns) --> !,
	[l_i(246 ,A,B)], collapse(Insns).
/*** B_GAUGE
collapse(bump_counter(A), Insns) --> !,
	[counter(249 ,A)], collapse(Insns).
collapse(counted_neck(_,A,B), Insns) --> !,
	[counter_counter(251 ,A,B)], collapse(Insns).
collapse(profile_point(A), Insns) --> !,
	[profile_point(A)], collapse(Insns).
E_GAUGE ***/

collapse_voids([unify_void|Insns0], Insns, I, N) :- !,
	I1 is I+1, collapse_voids(Insns0, Insns, I1, N).
collapse_voids(Insns, Insns, N, N).

collapse_call_1([], [], _, L, L).
collapse_call_1([Insn|S0], S, I, L0, L) :-
	collapse_call_1(Insn, I, I1, U), !,
	collapse_call_1(S0, S, I1, L0, [U|L]).
collapse_call_1([Insn|S0], [Insn|S], I, L0, L) :-
	collapse_call_1(Insn, I),
	collapse_call_1(S0, S, I, L0, L).

collapse_call_1(put_y_value(Y,I), I, I1, v(Y)) :- I1 is I+1.
collapse_call_1(put_y_unsafe_value(Y,I), I, I1, u(Y)) :- I1 is I+1.

collapse_call_1(put_y_value(_,X), I) :- X>=I.
collapse_call_1(put_y_unsafe_value(_,X), I) :- X>=I.
collapse_call_1(put_x_void(X), I) :- X>=I.
collapse_call_1(put_x_variable(X,X1), I) :- X>=I, X1>=I.

collapse_call_2([put_y_value(Y,X)|Insns]) --> !,
	collapse_call_2_yval(Insns, Y,X).
collapse_call_2([put_y_unsafe_value(Y,X)|Insns]) --> !,
	collapse_call_2_yuval(Insns, Y,X).
collapse_call_2([put_x_void(X)|Insns]) --> !,
	[x(69 ,X)], collapse_call_2(Insns).
collapse_call_2([put_x_variable(X,A)|Insns]) -->
	[rev_x_x(70 ,X,A)], collapse_call_2(Insns).
collapse_call_2([]) --> [].

collapse_call_2_yval([put_y_value(Y1,X1)|Insns], Y,X) --> !,
	[rev_y_x_y_x(86 ,Y,X,Y1,X1)],
	collapse_call_2(Insns).
collapse_call_2_yval([put_y_unsafe_value(Y1,X1)|Insns], Y,X) --> !,
	[rev_y_x_y_x(87 ,Y,X,Y1,X1)],
	collapse_call_2(Insns).
collapse_call_2_yval(Insns, Y,X) -->
	[rev_y_x(75 ,Y,X)], collapse_call_2(Insns).

collapse_call_2_yuval([put_y_value(Y1,X1)|Insns], Y,X) --> !,
	[rev_y_x_y_x(88 ,Y,X,Y1,X1)],
	collapse_call_2(Insns).
collapse_call_2_yuval([put_y_unsafe_value(Y1,X1)|Insns], Y,X) --> !,
	[rev_y_x_y_x(89 ,Y,X,Y1,X1)],
	collapse_call_2(Insns).
collapse_call_2_yuval(Insns, Y,X) -->
	[rev_y_x(76 ,Y,X)], collapse_call_2(Insns).

collapse_call(put_x_void(X), [Insn|Insns], [put_x_void(X)|L0], L) --> !,
	collapse_call(Insn, Insns, L0, L).
collapse_call(put_x_variable(Y,X), [Insn|Insns], [put_x_variable(Y,X)|L0], L) --> !,
	collapse_call(Insn, Insns, L0, L).
collapse_call(put_y_value(Y,X), [Insn|Insns], [put_y_value(Y,X)|L0], L) --> !,
	collapse_call(Insn, Insns, L0, L).
collapse_call(put_y_unsafe_value(Y,X), [Insn|Insns], [put_y_unsafe_value(Y,X)|L0], L) --> !,
	collapse_call(Insn, Insns, L0, L).
collapse_call(call(X,Y), Insns, [], L) -->
	{collapse_call_1(L, L1, 0, Arg, [])},
	collapse_call_2(L1),
	[zlist_label_size(23 ,Arg,X,Y)],
	!, collapse(Insns).
collapse_call(deallocate, [execute(X)|Insns], [], L) -->
	{collapse_call_1(L, L1, 0, Arg, [])},
	collapse_call_2(L1),
	[zlist_label(43 ,Arg,X)],
	!, collapse(Insns).
collapse_call(Insn, Insns, [], L) -->
	collapse_call_2(L),
	collapse(Insn, Insns).

collapse_allocate(choice_y(Y), Insns) --> !,
	[y(220 ,Y)], collapse(Insns).
collapse_allocate(init(_), [call(L,S)|Insns]) --> !,
	[label_size(1 ,L,S)], collapse(Insns).
collapse_allocate(init(_), [true(S)|Insns]) --> !,
	[size(260 ,S)], collapse(Insns).
collapse_allocate(put_y_variable(Y,X), [put_y_variable(Y1,X1)|Insns]) --> !,
	[rev_yv_x_yv_x(83 ,Y,X,Y1,X1)], collapse(Insns).
collapse_allocate(put_y_variable(Y,X), Insns) --> !,
	[rev_yv_x(73 ,Y,X)], collapse(Insns).
collapse_allocate(get_y_variable(To,From), [get_y_variable(To1,From1)|Insns]) --> !,
	[rev_y_x_y_x(109 ,To,From,To1,From1)], collapse(Insns).
collapse_allocate(get_y_variable(X,Y), Insns) --> !,
	[rev_y_x(92 ,X,Y)], collapse(Insns).
collapse_allocate(unify_y_variable(X), [Insn|Insns]) -->
	collapse_u2_yfvar(Insn, Insns, X).

collapse_deallocate(execute(X), Insns) -->
	[zlist_label(43 ,[],X)], collapse(Insns).

collapse_u2_yfvar(unify_void, Insns0, X) --> !,
	{collapse_voids(Insns0, Insns, 1, Y)},
	[y_i(153 ,X,Y)], collapse(Insns).
collapse_u2_yfvar(unify_x_variable(Y), Insns, X) --> !,
	[y_x(155 ,X,Y)], collapse(Insns).
collapse_u2_yfvar(unify_y_variable(Y), Insns, X) --> !,
	[y_y(157 ,X,Y)], collapse(Insns).
collapse_u2_yfvar(unify_x_value(Y), Insns, X) --> !,
	[y_x(159 ,X,Y)], collapse(Insns).
collapse_u2_yfvar(unify_x_local_value(Y), Insns, X) --> !,
	[y_x(161 ,X,Y)], collapse(Insns).
collapse_u2_yfvar(unify_y_value(Y), Insns, X) --> !,
	[y_y(163 ,X,Y)], collapse(Insns).
collapse_u2_yfvar(unify_y_local_value(Y), Insns, X) --> !,
	[y_y(165 ,X,Y)], collapse(Insns).
collapse_u2_yfvar(Insn, Insns, X) -->
	[y(122 ,X)], collapse(Insn, Insns).

collapse_u2_void(unify_x_variable(Y), Insns, X) --> !,
	[i_x(136 ,X,Y)], collapse(Insns).
collapse_u2_void(allocate, [unify_y_variable(Y)|Insns], X) --> !,
	[i_y(139 ,X,Y)], collapse(Insns).
collapse_u2_void(unify_y_variable(Y), Insns, X) --> !,
	[i_y(140 ,X,Y)], collapse(Insns).
collapse_u2_void(unify_x_value(Y), Insns, X) --> !,
	[i_x(137 ,X,Y)], collapse(Insns).
collapse_u2_void(unify_x_local_value(Y), Insns, X) --> !,
	[i_x(138 ,X,Y)], collapse(Insns).
collapse_u2_void(unify_y_first_value(Y), Insns, X) --> !,
	[i_y(141 ,X,Y)], collapse(Insns).
collapse_u2_void(unify_y_value(Y), Insns, X) --> !,
	[i_y(142 ,X,Y)], collapse(Insns).
collapse_u2_void(unify_y_local_value(Y), Insns, X) --> !,
	[i_y(143 ,X,Y)], collapse(Insns).
collapse_u2_void(Insn, Insns, X) -->
	[voids(114 ,X)], collapse(Insn, Insns).

collapse_u2_xvar(unify_void, Insns0, X) --> !,
	{collapse_voids(Insns0, Insns, 1, Y)},
	[x_i(144 ,X,Y)], collapse(Insns).
collapse_u2_xvar(unify_x_variable(Y), Insns, X) --> !,
	[x_x(145 ,X,Y)], collapse(Insns).
collapse_u2_xvar(allocate, [unify_y_variable(Y)|Insns], X) --> !,
	[x_y(148 ,X,Y)], collapse(Insns).
collapse_u2_xvar(unify_y_variable(Y), Insns, X) --> !,
	[x_y(149 ,X,Y)], collapse(Insns).
collapse_u2_xvar(unify_x_value(Y), Insns, X) --> !,
	[x_x(146 ,X,Y)], collapse(Insns).
collapse_u2_xvar(unify_y_first_value(Y), Insns, X) --> !,
	[x_y(150 ,X,Y)], collapse(Insns).
collapse_u2_xvar(unify_y_value(Y), Insns, X) --> !,
	[x_y(151 ,X,Y)], collapse(Insns).
collapse_u2_xvar(unify_y_local_value(Y), Insns, X) --> !,
	[x_y(152 ,X,Y)], collapse(Insns).
collapse_u2_xvar(Insn, Insns, X) -->
	[x(119 ,X)], collapse(Insn, Insns).

collapse_u2_yvar(unify_void, Insns0, X) --> !,
	{collapse_voids(Insns0, Insns, 1, Y)},
	[y_i(154 ,X,Y)], collapse(Insns).
collapse_u2_yvar(unify_x_variable(Y), Insns, X) --> !,
	[y_x(156 ,X,Y)], collapse(Insns).
collapse_u2_yvar(unify_y_variable(Y), Insns, X) --> !,
	[y_y(158 ,X,Y)], collapse(Insns).
collapse_u2_yvar(unify_x_value(Y), Insns, X) --> !,
	[y_x(160 ,X,Y)], collapse(Insns).
collapse_u2_yvar(unify_x_local_value(Y), Insns, X) --> !,
	[y_x(162 ,X,Y)], collapse(Insns).
collapse_u2_yvar(unify_y_value(Y), Insns, X) --> !,
	[y_y(164 ,X,Y)], collapse(Insns).
collapse_u2_yvar(unify_y_local_value(Y), Insns, X) --> !,
	[y_y(166 ,X,Y)], collapse(Insns).
collapse_u2_yvar(Insn, Insns, X) -->
	[y(123 ,X)], collapse(Insn, Insns).

collapse_u2_xval(unify_void, Insns0, X) --> !,
	{collapse_voids(Insns0, Insns, 1, Y)},
	[x_i(167 ,X,Y)], collapse(Insns).
collapse_u2_xval(unify_x_variable(Y), Insns, X) --> !,
	[x_x(169 ,X,Y)], collapse(Insns).
collapse_u2_xval(allocate, [unify_y_variable(Y)|Insns], X) --> !,
	[x_y(171 ,X,Y)], collapse(Insns).
collapse_u2_xval(unify_y_variable(Y), Insns, X) --> !,
	[x_y(173 ,X,Y)], collapse(Insns).
collapse_u2_xval(unify_x_value(Y), Insns, X) --> !,
	[x_x(175 ,X,Y)], collapse(Insns).
collapse_u2_xval(unify_x_local_value(Y), Insns, X) --> !,
	[x_x(177 ,X,Y)], collapse(Insns).
collapse_u2_xval(unify_y_first_value(Y), Insns, X) --> !,
	[x_y(179 ,X,Y)], collapse(Insns).
collapse_u2_xval(unify_y_value(Y), Insns, X) --> !,
	[x_y(181 ,X,Y)], collapse(Insns).
collapse_u2_xval(unify_y_local_value(Y), Insns, X) --> !,
	[x_y(183 ,X,Y)], collapse(Insns).
collapse_u2_xval(Insn, Insns, X) -->
	[x(120 ,X)], collapse(Insn, Insns).

collapse_u2_xlval(unify_void, Insns0, X) --> !,
	{collapse_voids(Insns0, Insns, 1, Y)},
	[x_i(168 ,X,Y)], collapse(Insns).
collapse_u2_xlval(unify_x_variable(Y), Insns, X) --> !,
	[x_x(170 ,X,Y)], collapse(Insns).
collapse_u2_xlval(allocate, [unify_y_variable(Y)|Insns], X) --> !,
	[x_y(172 ,X,Y)], collapse(Insns).
collapse_u2_xlval(unify_y_variable(Y), Insns, X) --> !,
	[x_y(174 ,X,Y)], collapse(Insns).
collapse_u2_xlval(unify_x_value(Y), Insns, X) --> !,
	[x_x(176 ,X,Y)], collapse(Insns).
collapse_u2_xlval(unify_x_local_value(Y), Insns, X) --> !,
	[x_x(178 ,X,Y)], collapse(Insns).
collapse_u2_xlval(unify_y_first_value(Y), Insns, X) --> !,
	[x_y(180 ,X,Y)], collapse(Insns).
collapse_u2_xlval(unify_y_value(Y), Insns, X) --> !,
	[x_y(182 ,X,Y)], collapse(Insns).
collapse_u2_xlval(unify_y_local_value(Y), Insns, X) --> !,
	[x_y(184 ,X,Y)], collapse(Insns).
collapse_u2_xlval(Insn, Insns, X) -->
	[x(121 ,X)], collapse(Insn, Insns).

collapse_u2_yval(unify_void, Insns0, X) --> !,
	{collapse_voids(Insns0, Insns, 1, Y)},
	[y_i(186 ,X,Y)], collapse(Insns).
collapse_u2_yval(unify_x_variable(Y), Insns, X) --> !,
	[y_x(189 ,X,Y)], collapse(Insns).
collapse_u2_yval(unify_y_variable(Y), Insns, X) --> !,
	[y_y(191 ,X,Y)], collapse(Insns).
collapse_u2_yval(unify_x_value(Y), Insns, X) --> !,
	[y_x(194 ,X,Y)], collapse(Insns).
collapse_u2_yval(unify_x_local_value(Y), Insns, X) --> !,
	[y_x(197 ,X,Y)], collapse(Insns).
collapse_u2_yval(unify_y_first_value(Y), Insns, X) --> !,
	[y_y(200 ,X,Y)], collapse(Insns).
collapse_u2_yval(unify_y_value(Y), Insns, X) --> !,
	[y_y(203 ,X,Y)], collapse(Insns).
collapse_u2_yval(unify_y_local_value(Y), Insns, X) --> !,
	[y_y(206 ,X,Y)], collapse(Insns).
collapse_u2_yval(Insn, Insns, X) -->
	[y(125 ,X)], collapse(Insn, Insns).

collapse_u2_yfval(unify_void, Insns0, X) --> !,
	{collapse_voids(Insns0, Insns, 1, Y)},
	[y_i(185 ,X,Y)], collapse(Insns).
collapse_u2_yfval(unify_x_variable(Y), Insns, X) --> !,
	[y_x(188 ,X,Y)], collapse(Insns).
collapse_u2_yfval(unify_x_value(Y), Insns, X) --> !,
	[y_x(193 ,X,Y)], collapse(Insns).
collapse_u2_yfval(unify_x_local_value(Y), Insns, X) --> !,
	[y_x(196 ,X,Y)], collapse(Insns).
collapse_u2_yfval(unify_y_first_value(Y), Insns, X) --> !,
	[y_y(199 ,X,Y)], collapse(Insns).
collapse_u2_yfval(unify_y_value(Y), Insns, X) --> !,
	[y_y(202 ,X,Y)], collapse(Insns).
collapse_u2_yfval(unify_y_local_value(Y), Insns, X) --> !,
	[y_y(205 ,X,Y)], collapse(Insns).
collapse_u2_yfval(Insn, Insns, X) -->
	[y(124 ,X)], collapse(Insn, Insns).

collapse_u2_ylval(unify_void, Insns0, X) --> !,
	{collapse_voids(Insns0, Insns, 1, Y)},
	[y_i(187 ,X,Y)], collapse(Insns).
collapse_u2_ylval(unify_x_variable(Y), Insns, X) --> !,
	[y_x(190 ,X,Y)], collapse(Insns).
collapse_u2_ylval(unify_y_variable(Y), Insns, X) --> !,
	[y_y(192 ,X,Y)], collapse(Insns).
collapse_u2_ylval(unify_x_value(Y), Insns, X) --> !,
	[y_x(195 ,X,Y)], collapse(Insns).
collapse_u2_ylval(unify_x_local_value(Y), Insns, X) --> !,
	[y_x(198 ,X,Y)], collapse(Insns).
collapse_u2_ylval(unify_y_first_value(Y), Insns, X) --> !,
	[y_y(201 ,X,Y)], collapse(Insns).
collapse_u2_ylval(unify_y_value(Y), Insns, X) --> !,
	[y_y(204 ,X,Y)], collapse(Insns).
collapse_u2_ylval(unify_y_local_value(Y), Insns, X) --> !,
	[y_y(207 ,X,Y)], collapse(Insns).
collapse_u2_ylval(Insn, Insns, X) -->
	[y(126 ,X)], collapse(Insn, Insns).

asm_args(none(Opcode), Off, Off1) -->
	{Off1 is Off+2},
	[Opcode].
asm_args(builtin_x(Opcode,Op,A), Off, Off1) -->
	evenop(Opcode, Off, Off0),
        {Off1 is Off0+6},
	xop(A),
        [builtin(Op)].
asm_args(builtin_x_x(Opcode,Op,A,B), Off, Off1) -->
	oddop(Opcode, Off, Off0),
        {Off1 is Off0+8},
	xop(A), xop(B),
        [builtin(Op)].
asm_args(builtin_x_x_x(Opcode,Op,A,B,C), Off, Off1) -->
	evenop(Opcode, Off, Off0),
        {Off1 is Off0+10},
	xop(A), xop(B), xop(C),
        [builtin(Op)].
asm_args(function_x_x(Opcode,Op,A,B,Heap,Arg), Off, Off1) -->
	oddop(Opcode, Off, Off0),
        {Off1 is Off0+14},
	xop(A), xop(B),
        [builtin(Op),long(Heap),Arg].
asm_args(function_x_x_x(Opcode,Op,A,B,C,Heap,Arg), Off, Off1) -->
	evenop(Opcode, Off, Off0),
        {Off1 is Off0+16},
	xop(A), xop(B), xop(C),
        [builtin(Op),long(Heap),Arg].
asm_args(voids(Opcode,Int), Off, Off1) -->
	{Off1 is Off+2, Int<5, Opcode1 is Opcode+Int}, !,
	[Opcode1].
asm_args(voids(Opcode,Int), Off, Off1) -->
	{Off1 is Off+4},
	[Opcode, Int].
asm_args(i(Opcode,Int), Off, Off1) -->
	{Off1 is Off+4},
	[Opcode, Int].
asm_args(x(Opcode,X), Off, Off1) -->
	{Off1 is Off+4},
	[Opcode], xop(X).
asm_args(y(Opcode,Y), Off, Off1) -->
	{Off1 is Off+4},
	[Opcode], yop(Y).
asm_args(rev_x_x(Opcode,X1,X), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode], xop(X), xop(X1).
asm_args(rev_x_x_x_x(Opcode,X1,X2,X3,X4), Off, Off1) -->
	{Off1 is Off+10},
	[Opcode], xop(X2), xop(X1), xop(X4), xop(X3).
asm_args(rev_y_x(Opcode,Y,X), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode], xop(X), yop(Y).
asm_args(rev_y_x_y_x(Opcode,X1,X2,X3,X4), Off, Off1) -->
	{Off1 is Off+10},
	[Opcode], xop(X2), yop(X1), xop(X4), yop(X3).
asm_args(rev_yv_x(Opcode,Y,X), Off, Off1) -->
	asm_args(rev_y_x(Opcode,Y,X), Off, Off1).
asm_args(rev_yv_x_yv_x(Opcode,X1,X2,X3,X4), Off, Off1) -->
	asm_args(rev_y_x_y_x(Opcode,X1,X2,X3,X4), Off, Off1).
asm_args(i_i(Opcode,Int,Int2), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode, Int, Int2].
asm_args(l_i(Opcode,LongInt,Int), Off, Off1) -->
	oddop(Opcode, Off, Off0),
	{Off1 is Off0+6},
	[long(LongInt), Int].
asm_args(i_x(Opcode,Int,X), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode, Int], xop(X).
asm_args(i_y(Opcode,Int,Y), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode, Int], yop(Y).
asm_args(x_i(Opcode,X,I), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode], xop(X), [I].
asm_args(x_x(Opcode,X1,X2), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode], xop(X1), xop(X2).
asm_args(x_y(Opcode,X,Y), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode], xop(X), yop(Y).
asm_args(y_i(Opcode,Y,I), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode], yop(Y), [I].
asm_args(y_x(Opcode,Y,X), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode], yop(Y), xop(X).
asm_args(y_y(Opcode,Y1,Y2), Off, Off1) -->
	{Off1 is Off+6},
	[Opcode], yop(Y1), yop(Y2).
asm_args(ylist_label_size(Opcode,List,Label,Size), Off, Off2) -->
	{length(List, N)},
	ylist_opcode(N, Opcode, Off, Off1),
        subop_ylist(List),
	{Off2 is Off1+6},
	[emul_entry(Label)], sizeop(Size).
asm_args(zlist_label_size(Opcode,List,Label,Size), Off, Off2) -->
	{length(List, N)},
	zlist_opcode(N, Opcode, Off, Off1),
        subop_zlist(List),
	{Off2 is Off1+6},
	[emul_entry(Label)], sizeop(Size).
asm_args(label_size(Opcode,Label,Size), Off, Off1) -->
	oddop(Opcode, Off, Off0),
	{Off1 is Off0+6},
	[emul_entry(Label)], sizeop(Size).
asm_args(size(Opcode,Size), Off, Off1) -->
	{Off1 is Off+4},
	[Opcode], sizeop(Size).
asm_args(ylist_size(Opcode,List,Size), Off, Off1) -->
	{length(List, N), Off1 is Off+6+(N<<1)},
	[Opcode, N],
        subop_ylist(List),
	sizeop(Size).
asm_args(zlist_label(Opcode,List,Label), Off, Off2) -->
	{length(List, N)},
	zlist_opcode(N, Opcode, Off, Off1),
        subop_zlist(List),
	{Off2 is Off1+4},
	[emul_entry(Label)].
asm_args(label(Opcode,Label), Off, Off1) -->
	oddop(Opcode, Off, Off0),
	{Off1 is Off0+4},
	[emul_entry(Label)].
asm_args(tagged(Opcode,C), Off, Off1) -->
	oddop(Opcode, Off, Off0),
	{Off1 is Off0+4},
	[tagged(C)].
asm_args(large(Opcode,C), Off, Off1) -->
	oddop(Opcode, Off, Off0),
	{large_heap_usage(C, H), Off1 is Off0+(H<<2)-4},
	[large(C,H)].
asm_args(functor(Opcode,C), Off, Off1) -->
	oddop(Opcode, Off, Off0),
	{Off1 is Off0+4},
	[functor(C)].
asm_args(tagged_x(Opcode,C,X), Off, Off1) -->
	evenop(Opcode, Off, Off0),
	{Off1 is Off0+6},
	xop(X), [tagged(C)].
asm_args(large_x(Opcode,C,X), Off, Off1) -->
	evenop(Opcode, Off, Off0),
	{large_heap_usage(C, H), Off1 is Off0+(H<<2)-2},
	xop(X), [large(C,H)].
asm_args(functor_x(Opcode,C,X), Off, Off1) -->
	evenop(Opcode, Off, Off0),
	{Off1 is Off0+6},
	xop(X), [functor(C)].
/*** B_GAUGE
asm_args(counter(Opcode,_), Off, Off1) -->
	oddop(Opcode, Off, Off0),
	{Off1 is Off0 + 4},
	[counter].
asm_args(counter_counter(Opcode,_,_), Off, Off1) -->
	oddop(Opcode, Off, Off0),
	{Off1 is Off0 + 8},
	[counter,counter].
E_GAUGE ***/

% X encoding: as offset in "struct worker"
% Y encoding: as offset in "struct frame"

x_offset_from_worker(X) :- new_x_offset(X), compiler_mode(ql(_)), !.
x_offset_from_worker(X) :- x_and_y_offsets(X, _).

y_offset_from_frame(Y) :- new_y_offset(Y), compiler_mode(ql(_)), !.
y_offset_from_frame(Y) :- x_and_y_offsets(_, Y).

xop(X) --> 
    {x_offset_from_worker(Off), Op is (X+Off)<<2}, [Op].

yop(X) --> 
    {y_offset_from_frame(Off), Op is (X+Off)<<2}, [Op].

% TODO: use y_offset_from_frame/1 instead of +2
zop(v(X)) --> {Op is (X+2)<<2}, [Op].		% Y value
zop(u(X)) --> {Op is ((X+2)<<2)-1}, [Op].	% Y unsafe value

% TODO: use y_offset_from_frame/1 instead of +2
sizeop(X) --> {Op is (X+2)<<2}, [Op].

ylist_opcode(N, Opcode, Off, Off1) -->
	{N<9, !, Off0 is Off+N+N, Op is Opcode+N+N}, oddop(Op, Off0, Off1)
    ;   {Off0 is Off+2+N+N, Op is Opcode+18}, oddop(Op, Off0, Off1), [N].

subop_ylist([]) --> !.
subop_ylist([Y|Ops]) --> yop(Y), subop_ylist(Ops).

zlist_opcode(N, Opcode, Off, Off1) -->
	{N<9, !, Off0 is Off+N+N, Op is Opcode+N+N}, oddop(Op, Off0, Off1)
    ;   {Off0 is Off+2+N+N, Op is Opcode+18}, oddop(Op, Off0, Off1), [N].

oddop(Op, Off, Off1) -->
	{(Off mod 4) =:= 0, !, Off1 is Off+4, Op1 is Op-1}, [Op1, 0]
    ;   {Off1 is Off+2}, [Op].

evenop(Op, Off, Off1) -->
	{(Off mod 4) =\= 0, !, Off1 is Off+4, Op1 is Op-1}, [Op1, 0]
    ;   {Off1 is Off+2}, [Op].


subop_zlist([]) --> !.
subop_zlist([Op1|Ops]) --> zop(Op1), subop_zlist(Ops).

/*** B_GAUGE
ql_compile_file_emit(clause_model(Clause,Counters), Stream) :-
	ql_emit_directive(install_clause_model(Clause,Counters), _, _,Stream).
ql_compile_file_emit(instruction_model(Clause,InsnModel), Stream) :-
	ql_emit_directive(install_insn_model(Clause,InsnModel), _, _, Stream).
E_GAUGE ***/
ql_compile_file_emit(clause(Pred/_,Code,ProfileData,TypeKey,Data), Stream) :- !,
	Data = f(EffType,EffKey),
	profile_struct(_,ProfileData,_,InsnModel,Counters),
	incore_parse_key(TypeKey, EffType, EffKey),
	asm_insns(Code, InsnModel, 0, Size, Tokens, []),
	qdump_load_dbnode(0, Size, Counters, Stream),
	qdump(Tokens, 0, Dic, Stream),
	dic_lookup(Sdic, Goal, 0),
	define_predicate_mode(Mode),
	ql_emit_directive('internals:$compiled_clause'(Pred,Goal,Mode,Data),
			  Dic, Sdic, Stream).
ql_compile_file_emit(declare_predicate(Pred), Stream) :- !,
	ql_declare_predicate(Pred, Goal),
	ql_emit_directive(Goal, _, _, Stream).
ql_compile_file_emit(clause(Pred,Clause), Stream) :- !,
	ql_emit_directive('internals:$interpreted_clause'(Pred, Clause), _, _, Stream).
ql_compile_file_emit(declare_clause(Name/Ar,No), _) :- !,
	get_key(Name, Ar, F),
	ql_record_predicate(No, F, Name/Ar).
ql_compile_file_emit(set_property(Key,Prop), Stream) :- !,
	functor(Key, Name, Ar),
	ql_add_prop(Key, Name/Ar, Prop),
	ql_emit_directive('internals:$set_property'(Key, Prop), _, _, Stream).
ql_compile_file_emit(_, _).			% ignore others

ql_record_predicate(0, F, Pred) :-
	current_fact('$predicate'(F,Pred,_)), !.
ql_record_predicate(0, F, Pred) :- !,
	compile_file_emit(declare_predicate(Pred)),
	asserta_fact('$predicate'(F,Pred,[])).
ql_record_predicate(1, _, Pred) :- !,
	compile_file_emit(declare_predicate(Pred)).
ql_record_predicate(_, _, _).

ql_add_prop(F, Pred, Prop) :-
	current_fact('$predicate'(F,Pred,Props0), Ref), !,
	erase(Ref),
	sort([Prop|Props0], Props),
	asserta_fact('$predicate'(F,Pred,Props)).
ql_add_prop(F, Pred, Prop) :-
	compile_file_emit(declare_predicate(Pred)),
	asserta_fact('$predicate'(F,Pred,[Prop])).

 
ql_declare_predicate(Pred, 'internals:$define_predicate'(Pred, Mode)) :-
	define_predicate_mode(Mode).

ql_emit_mode_version(wam, Stream) :-
	poversion(V), % in engine(internals)
	display(Stream, poversion(V)),
	display(Stream, '.\n\n'),
	!.
ql_emit_mode_version(_,   Stream) :-
	ql_emit_version(Stream).

ql_emit_version(Stream) :-
	poversion(V), % in engine(internals)
	ql_emit_directive(V, _, _, Stream).

ql_emit_directive(D, Dic, Sdic, Stream) :-
	ql_postlude_size(Dic, 128, Size0),
	ql_term_size(D, Size0, Size),
	qdump_ensure_space(Size, Stream),
	qlval_begin(R0, Q0),
	qdump_postlude(Dic, Sdic, R0, R1, Q0, Q1, Stream),
	ql_lookup_term(D, S1, Sdic, R1, R, Q1, Q, Stream),
	qlval_end(R, Q),
	qdump_return(S1, Stream).		% run S1

/*** B_GAUGE
incore_compile_file_emit(clause_model(Clause,Counters)) :-
	install_clause_model(Clause,Counters).
incore_compile_file_emit(instruction_model(Clause,InsnModel)) :-
	install_insn_model(Clause,InsnModel).
E_GAUGE ***/
incore_compile_file_emit(clause(Pred/_,Code,ProfileData,TypeKey,Data)) :- !,
	Data = f(EffType,EffKey),
	profile_struct(_,ProfileData,_,InsnModel,Counters),
	incore_parse_key(TypeKey, EffType, EffKey),
	asm_insns(Code, InsnModel, 0, Size, Tokens, []),
	'$make_bytecode_object'(Size, Counters, Tokens, Obj),
	define_predicate_mode(Mode),
	'$compiled_clause'(Pred, Obj, Mode, Data).
incore_compile_file_emit(clause(Pred,Clause)) :- !,
	'$interpreted_clause'(Pred, Clause).
incore_compile_file_emit(declare_clause(Name/Ar,No)) :- !,
	incore_subdef(No, Name/Ar).
incore_compile_file_emit(set_property(Pred, Decl)) :- !,
        '$set_property'(Pred, Decl).
incore_compile_file_emit(_).			% ignore others

% TODO: see incore_decl_pred/6
incore_subdef(1, PredName) :- !,
	define_predicate_mode(Mode),
	'$define_predicate'(PredName, Mode),
	PredName = F/A, functor(Head0, F, A),
	asserta_fact(incore_mode_of(Head0, Mode)).
incore_subdef(_, _).

incore_parse_key(type_key(Type,hash(N/A)), Type, F) :- !, functor(F, N, A).
incore_parse_key(type_key(Type,hash(K)), Type, K) :- !.
incore_parse_key(type_key(Type,nohash), Type, _).

% DCG
incore_ql_compile_file_emit(clause(Pred/_,Code,ProfileData,TypeKey,Data), Stream) :- !,
	Data = f(EffType,EffKey),
	profile_struct(_,ProfileData,_,InsnModel,Counters),
	incore_parse_key(TypeKey, EffType, EffKey),
	asm_insns(Code, InsnModel, 0, Size, Tokens, []),
	'$make_bytecode_object'(Size, Counters, Tokens, Obj),
	qdump_load_dbnode(0, Size, Counters, Stream),
	qdump(Tokens, 0, Dic, Stream),
	dic_lookup(Sdic, Goal, 0),
	define_predicate_mode(Mode),
	'$compiled_clause'(Pred, Obj, Mode, Data),
	ql_emit_directive('internals:$compiled_clause'(Pred,Goal,Mode,Data),
			  Dic, Sdic, Stream).
incore_ql_compile_file_emit(declare_predicate(Pred), Stream) :- !,
	ql_declare_predicate(Pred, Goal),
	ql_emit_directive(Goal, _, _, Stream).
incore_ql_compile_file_emit(clause(Pred,Clause), Stream) :- !,
	'$interpreted_clause'(Pred, Clause),
	ql_emit_directive('internals:$interpreted_clause'(Pred, Clause), _, _, Stream).
incore_ql_compile_file_emit(declare_clause(Name/Ar,No), _) :- !,
	incore_subdef(No, Name/Ar),
	get_key(Name, Ar, F),
	ql_record_predicate(No, F, Name/Ar).
incore_ql_compile_file_emit(set_property(Key,Prop), Stream) :- !,
	functor(Key, Name, Ar),
	ql_add_prop(Key, Name/Ar, Prop),
	ql_emit_directive('internals:$set_property'(Key, Prop), _, _, Stream),
        '$set_property'(Key, Prop).
incore_ql_compile_file_emit(_, _).			% ignore others

qdump_postlude(D, _, R0, R, Q0, Q, _) :- var(D), !, R0=R, Q0=Q.
qdump_postlude(dic(K, V, Le, Ri), Sdic, R0, R, Q0, Q, Stream) :-
	qdump_prev_last(V, 0, Offset),
	qdump_postlude(Le, Sdic, R0, R1, Q0, Q1, Stream),
	qdump_post(K, Offset, Sdic, R1, R2, Q1, Q2, Stream),
	qdump_postlude(Ri, Sdic, R2, R, Q2, Q, Stream).

qdump_prev_last(V, Prev0, Prev) :- var(V), !, Prev0=Prev.
qdump_prev_last([I0|V], _, Prev) :- qdump_prev_last(V, I0, Prev).

qdump_post(functor(Fu), Offset, _, R, R, Q0, Q, Stream) :-
	get_qlval(Fu, Sreg, Q0, Q, Stream),
	qdump_reloc_pointer(Sreg, Offset, Stream).
qdump_post(tagged(F), Offset, Sdic, R0, R, Q0, Q, Stream) :-
	ql_lookup_term(F, Sreg, Sdic, R0, R, Q0, Q, Stream),
	qdump_reloc_pointer(Sreg, Offset, Stream).
qdump_post(emul_entry(Spec), Offset, Sdic, R0, R, Q0, Q, Stream) :-
	ql_lookup_term(Spec, Sreg, Sdic, R0, R, Q0, Q, Stream),
	qdump_reloc_emul_entry(Sreg, Offset, Stream).
%%qdump_post(native_entry(Spec), Offset, Sdic, R0, R, Q0, Q, Stream) :-
%%	ql_lookup_term(Spec, Sreg, Sdic, R0, R, Q0, Q, Stream),
%%	qdump_reloc_native_entry(Sreg, Offset, Stream).
%%qdump_post(local(Off), Offset, _, R, R, Q, Q, Stream) :-
%%	qdump_reloc_offset(Off, Offset, Stream).
%%qdump_post(absolute(X,Y), Offset, _, R, R, Q, Q, Stream) :-
%%	qdump_reloc_absolute(X, Y, Offset, Stream).
%%qdump_post(relative(X,Y), Offset, _, R, R, Q, Q, Stream) :-
%%	qdump_reloc_relative(X, Y, Offset, Stream).
/*** B_GAUGE
qdump_post(counter, Offset, _, R, R, Q, Q, Stream) :-
	qdump_reloc_counter(Offset, Stream).
E_GAUGE ***/

ql_lookup_term(Term, Sreg, Sdic, R0, R, Q, Q, Stream) :-
	var(Term), !,
	dic_lookup(Sdic, Term, Sreg),
	(   integer(Sreg) ->
	      R0=R
	;   R0=Sreg,
	    R is R0-1,
	    qdump_load_variable(R0, Stream)
	).
ql_lookup_term(Term, R0, _, R0, R, Q, Q, Stream) :-
	number(Term),
	large_heap_usage(Term, _), !,
	R is R0-1,
	qdump_load_number(R0, Term, Stream).
ql_lookup_term(Term, Sreg, _, R, R, Q0, Q, Stream) :-
	atomic(Term), !,
	get_qlval(Term, Sreg, Q0, Q, Stream).
ql_lookup_term([X|Y], R0, Sdic, R0, R, Q0, Q, Stream) :- !,
	R1 is R0-1,
	ql_lookup_term(X, Sreg1, Sdic, R1, R2, Q0, Q1, Stream),
	ql_lookup_term(Y, Sreg2, Sdic, R2, R, Q1, Q, Stream),
	qdump_load_list(R0, Stream),
	qdump_load_argument(Sreg1, Stream),
	qdump_load_argument(Sreg2, Stream).
ql_lookup_term(Term, R0, Sdic, R0, R, Q0, Q, Stream) :-
	functor(Term, F, A),
	R1 is R0-1,
	get_qlval(F/A, S0, Q0, Q1, Stream),
	ql_lookup_args(0, A, Term, Sdic, R1, R, Q1, Q, Stream, Ss, []),
	qdump_load_tuple(R0, Stream),
	qdump_args([S0|Ss], Stream).

ql_lookup_args(A, A, _, _, R, R, Q, Q, _) --> !.
ql_lookup_args(I, A, Term, Sdic, R0, R, Q0, Q, Stream) -->
	[Sreg],
	{   I<A,
	    I1 is I+1,
	    arg(I1, Term, Arg),
	    ql_lookup_term(Arg, Sreg, Sdic, R0, R1, Q0, Q1, Stream)
	},
	ql_lookup_args(I1, A, Term, Sdic, R1, R, Q1, Q, Stream).

ql_postlude_size(D, S0, S) :- var(D), !, S=S0.
ql_postlude_size(dic(K, _, Le, Ri), S0, S) :-
	ql_postlude_size(Le, S0, S1),
	ql_post_size(K, S1, S2),
	ql_postlude_size(Ri, S2, S).

ql_post_size(emul_entry(Spec), S0, S) :- !,
	ql_term_size(Spec, S0, S).
ql_post_size(native_entry(Spec), S0, S) :- !,
	ql_term_size(Spec, S0, S).
ql_post_size(_, S, S).

ql_term_size(Term, S0, S) :-
	var(Term), !,
	S=S0.
ql_term_size(Term, S0, S) :-
	number(Term),
	large_heap_usage(Term, U), !,
	S is S0+U.
ql_term_size(Term, S0, S) :-
	atomic(Term), !,
	S=S0.
ql_term_size([T1|T2], S0, S) :- !,
	S1 is S0+2,
	ql_term_size(T1, S1, S2),
	ql_term_size(T2, S2, S).
ql_term_size(Term, S0, S) :-
	functor(Term, _, A),
	S1 is S0+A+1,
	ql_term_size(A, Term, S1, S).

ql_term_size(0, _, S, S) :- !.
ql_term_size(A, Term, S0, S) :-
	arg(A, Term, Arg),
	A1 is A-1,
	ql_term_size(Arg, S0, S1),
	ql_term_size(A1, Term, S1, S).

qlval_begin(-1, Q) :-
	current_fact('$qlval'(_,Q0)), !,
	Q is Q0+1.
qlval_begin(-1, 1).

qlval_end(_, _).

get_qlval(N/A, Sreg, Q, Q, _) :-
	functor(Fu, N, A),
	current_fact('$qlval'(Fu,Sreg)), !.
get_qlval(N/A, Q0, Q0, Q, Stream) :-
	functor(Fu, N, A), !,
	asserta_fact('$qlval'(Fu,Q0)),
	Q1 is Q0+1,
	get_qlval(N, Sreg, Q1, Q, Stream),
	qdump_load_functor(Q0, Sreg, A, Stream).
get_qlval(S, Sreg, Q, Q, _) :-
	atomic(S),
	current_fact('$qlval'(S,Sreg)), !.
get_qlval([], Q0, Q0, Q, Stream) :- !,
	asserta_fact('$qlval'([], Q0)),
	Q is Q0+1,
	qdump_load_nil(Q0, Stream).
get_qlval(A, Q0, Q0, Q, Stream) :-
	atom(A), !,
	asserta_fact('$qlval'(A,Q0)),
	Q is Q0+1,
	qdump_load_atom(Q0, A, Stream).
get_qlval(N, Q0, Q0, Q, Stream) :-
	number(N),
	asserta_fact('$qlval'(N,Q0)),
	Q is Q0+1,
	qdump_load_number(Q0, N, Stream).


qdump([], _, _, Stream) :- 
	put_code(Stream, 0).				% end of program
qdump([Tok|Toks], Off, Dic, Stream) :-
	qdump_token(Tok, Off, Off1, Dic, Stream),
	qdump(Toks, Off1, Dic, Stream).

qdump_token(X, O, O1, _, Stream) :-
	integer(X), !, O1 is O+2,
	qdump_short(X, Stream).
qdump_token(builtin(Op), O, O4, _, Stream) :- !,
	O4 is O+4,
	put_code(Stream, 0'C),
	qdump_long(Op, Stream).
qdump_token(long(Op), O, O4, _, Stream) :- !,
	O4 is O+4,
	put_code(Stream, 0'+),
	qdump_long(Op, Stream).
qdump_token(large(Op,H), O, O1, _, Stream) :- !,
	O1 is O+((H-1)<<2),
	put_code(Stream, 0'G),
	qdump_long(Op, Stream).
qdump_token(Reloc, O, O4, Dic, Stream) :- 
	O4 is O+4,
	qdump_patch(Reloc, Old, O, Dic),
	put_code(Stream, 0'+),
	qdump_long(Old, Stream).

qdump_patch(Key, Oldval, Newval, Olddic) :-
	dic_lookup(Olddic, Key, List),
	qdump_prev_last(List, 0, Oldval, Newval).

qdump_prev_last(V, Prev, Prev, Last) :- var(V), !, V=[Last|_].
qdump_prev_last([I0|V], _, Prev, Last) :- qdump_prev_last(V, I0, Prev, Last).

qdump_load_atom(Li, Str, Stream) :-
	put_code(Stream, 65),
	qdump_short(Li, Stream),
	qdump_atom(Str, Stream).

qdump_load_functor(Li, L2, A, Stream) :-
	put_code(Stream, 66),
	qdump_short(Li, Stream),
	qdump_short(L2, Stream),
	qdump_short(A, Stream).

qdump_load_number(Li, Num, Stream) :-
	integer(Num), -32768 =< Num, Num =< 32767, !,
	put_code(Stream, 67),
	qdump_short(Li, Stream),
	qdump_short(Num, Stream).
qdump_load_number(Li, Num, Stream) :-
	integer(Num), !,
	put_code(Stream, 68),
	qdump_short(Li, Stream),
	qdump_long(Num, Stream).
qdump_load_number(Li, Num, Stream) :-
	float(Num),
	put_code(Stream, 69),
	qdump_short(Li, Stream),
	qdump_float(Num, Stream).

qdump_load_variable(Li, Stream) :-
	put_code(Stream, 70),
	qdump_short(Li, Stream).

qdump_load_nil(Li, Stream) :-
	put_code(Stream, 71),
	qdump_short(Li, Stream).

qdump_load_list(Li, Stream) :-
	put_code(Stream, 72),
	qdump_short(Li, Stream).

qdump_load_tuple(Li, Stream) :-
	put_code(Stream, 73),
	qdump_short(Li, Stream).

qdump_load_argument(Li, Stream) :-
	put_code(Stream, 74),
	qdump_short(Li, Stream).

qdump_load_dbnode(Li, Size, Counters, Stream) :- % Dumps partial insn only.
	put_code(Stream, 75),
	qdump_short(Li, Stream), 
	qdump_short(Size, Stream), 
	qdump_short(Counters, Stream).

qdump_return(Li, Stream) :-
	put_code(Stream, 76),
	qdump_short(Li, Stream).

qdump_ensure_space(Size, Stream) :-
	Size>1152, !,
	put_code(Stream, 64),
	qdump_long(Size, Stream).
qdump_ensure_space(_, _).


/*** B_GAUGE
qdump_reloc_counter(Label, Stream) :-
	put_code(Stream, 79),
	qdump_long(Label, Stream).
E_GAUGE ***/
qdump_reloc_pointer(Li, Label, Stream) :-
	put_code(Stream, 77),
	qdump_short(Li, Stream),
	qdump_long(Label, Stream).

qdump_reloc_emul_entry(Li, Label, Stream) :-
	put_code(Stream, 78),
	qdump_short(Li, Stream),
	qdump_long(Label, Stream).

%% qdump_reloc_native_entry(Li, Label, Stream) :-
%% 	put_code(Stream, RELOC_NATIVE_ENTRY),
%% 	qdump_short(Li, Stream),
%% 	qdump_long(Label, Stream).
%% 
%% qdump_reloc_offset(Target, Label, Stream) :-
%% 	put_code(Stream, RELOC_OFFSET),
%% 	qdump_long(Target, Stream),
%% 	qdump_long(Label, Stream).
%% 
%% qdump_reloc_absolute(X, Y, Label, Stream) :-
%% 	put_code(Stream, RELOC_ABSOLUTE),
%% 	qdump_short(X, Stream),
%% 	qdump_short(Y, Stream),
%% 	qdump_long(Label, Stream).
%% 
%% qdump_reloc_relative(X, Y, Label, Stream) :-
%% 	put_code(Stream, RELOC_RELATIVE),
%% 	qdump_short(X, Stream),
%% 	qdump_short(Y, Stream),
%% 	qdump_long(Label, Stream).
%% 
%% qdump_reloc_rehash(_Table, Size, Label, Stream) :-
%% 	put_code(Stream, RELOC_REHASH),
%% %	qdump_long(Table, Stream), same as Label
%% 	qdump_long(Size, Stream),
%% 	qdump_long(Label, Stream).

qdump_args([], _).
qdump_args([S|Ss], Stream) :-
	qdump_load_argument(S, Stream),
	qdump_args(Ss, Stream).

% changed '$display' to display (4 clauses) - PBC
qdump_atom(X, Stream) :-
	display(Stream, X), put_code(Stream, 0).	% string arg.

qdump_short(X, Stream) :-
	display(Stream, X), put_code(Stream, 0).	% short arg.

qdump_long(X, Stream) :-
	display(Stream, X), put_code(Stream, 0).	% long arg.

qdump_float(X, Stream) :-
	display(Stream, X), put_code(Stream, 0).	% float arg.


%------------------------------------------------------------------------
% Object file printer.  Instructions are printed one per line.

/*** B_GAUGE
wam_compile_file_emit(clause_model(Clause,Counters), Stream) :-
	write_counter_model(Counters,Clause,Stream).
wam_compile_file_emit(instruction_model(Clause,InsnModel), Stream) :-
	write_instruction_model(InsnModel,Clause,Stream).
E_GAUGE ***/

name_key(Name/A/N, Key) :-
	atom_number(NA,N),
	atom_concat('/',NA,Key0),
	atom_number(AA,A),
	atom_concat(AA,Key0,Key1),
	atom_concat('/',Key1,Key2),
	atom_concat(Name,Key2,Key).

wam_compile_file_emit(clause(Pred/No,Code,_,TypeKey,Data), Stream) :- !,
	Data = f(EffType,EffKey),
	incore_parse_key(TypeKey, EffType, EffKey),
	wam_clause_name(Pred/No, ClName),
        current_output(Cout),
        set_output(Stream),
	name_key(ClName, ClKey),
	display('clause('), displayq(ClKey), display(', '),
	write_code(Code, '   ['),
	display(']).'), nl, nl,
        set_output(Cout).
wam_compile_file_emit(clause(_,Clause), Stream) :- !,
	portray_clause(Stream, Clause).
wam_compile_file_emit(predicate(Pred,_,Code), Stream) :- !,
        current_output(Cout),
        set_output(Stream),
	display('predicate('), displayq(Pred), display(', '),
	write_code(Code, '   ['),
	display(']).'), nl, nl,
        set_output(Cout).
wam_compile_file_emit(declare_predicate(_), _) :- !.
wam_compile_file_emit(set_property(Key, Prop), Stream) :- !,
	functor(Key, N, A),
	Goal =.. [Prop,N/A],
	ql_add_prop(Key, N/A, Prop),
        current_output(Cout),
        set_output(Stream),
        display(':- '), displayq(Goal), display('.'), nl,
	set_output(Cout).
wam_compile_file_emit(declare_clause(Name/Ar,No), _) :- !,
	get_key(Name, Ar, F),
	ql_record_predicate(No, F, Name/Ar).
wam_compile_file_emit(_, _).			%ignore others

write_code([], _) :- !.
write_code([Insn|Insns], Del) :-
        nl, display(Del), displayq(Insn),
	write_code(Insns, '   ,').
