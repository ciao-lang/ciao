:- module(disj, 
	[
	    test/1, 
	    result/1,
	    spend_time/2
	]).

:- include(tabling_type).

:- dynamic results/1.
:- set_prolog_flag(multi_arity_warnings, off).

 %% :- bridge /(el,2).
 %% :- bridge /(rev,3).
 %% :- bridge /(disjunct,1).
 %% :- bridge /(ellist,3).
 %% :- bridge /(makedisj2,4).
 %% :- bridge /(makedisj1,3).
 %% :- bridge /(makedisj,4).
 %% :- bridge /(makeminnf,2).
 %% :- bridge /(makeminsf,2).
 %% :- bridge /(makeminaf,2).
 %% :- bridge /(makemaxef,2).
 %% :- bridge /(makemaxnf,2).
 %% :- bridge /(makeprec,2).
 %% :- bridge /(memberEl,3).
 %% :- bridge /(makevars,2).

:- table set_unify/2.
:- table prec__1/1.
:- table resources__1/1.
:- table minaf__1/1.
:- table minnf__1/1.
:- table maxef__1/1.
:- table minsf__1/1.
:- table maxnf__1/1.
:- table duration__1/2.
:- table jobs__1/1.
:- table p__1/2.
:- table el__1/2.
:- table rev__1/3.
:- table disj__1/4.
:- table disjunct__1/1.
:- table ellist__1/3.
:- table makedisj2__1/4.
:- table makedisj1__1/3.
:- table makedisj__1/4.
:- table makeminnf__1/2.
:- table makeminsf__1/2.
:- table makeminaf__1/2.
:- table smeqc__1/3.
:- table makemaxef__1/2.
:- table makemaxnf__1/2.
:- table gteqc__1/3.
:- table makeprec__1/2.
:- table setup__1/3.
:- table memberEl__1/3.
:- table makevars__1/2.
:- table top__1/1.
:- table zero200__1/1.

spend_time(NT,T) :-
	abolish_all_tables,
        statistics(runtime,[_,_]),
	tp, abolish_all_tables,
        statistics(runtime,[_,Tt]),
	N is NT / Tt,
        statistics(runtime,[_,_]),
        (
            between(1,N,_),
            tp, abolish_all_tables,
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin]),
        (
            between(1,N,_),
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin2]),
        T is (Tfin - Tfin2) / N.

test(_) :- abolish_all_tables, tp, fail.
test(L) :- show_facts, get_list(L).

get_list([R|L]) :-
	retract(results(R)), !,
	get_list(L).

get_list([]).

unify_sets([],[]).
unify_sets([A|As],[B|Bs]) :- 
       set_unify(A,B), unify_sets(As,Bs).


%---------------- Transformed program ----------------------

zero200__1(_102) :- 
        [_89] = _104, 
        normalize_result([_104],
                          [_102]).
top__1(_205) :- 
        [_178] = _207, 
        setup([_178],[_182],[_186]),
        normalize_result([_207],
                         [_205]).
makevars__1(_116,_118) :- 
        [list] = _120, [list] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
makevars__1(_380,_382) :- 
        [list,_350] = _384, [list,_354] = _386, 
        duration([_323],[_327]),
        memberEl([_334],[num],[num]),
        makevars([_350],[_354]),
        normalize_result([_384,_386],
                         [_380,_382]).
memberEl__1(_155,_157,_159) :- 
        [_135] = _161, [_135] = _163, [_139] = _165, 
        normalize_result([_161,_163,_165],
                          [_155,_157,_159]).
memberEl__1(_413,_415,_417) :- 
        [_376] = _419, [_361] = _421, [_384] = _423, 
        'my =<'([_361],[_384]),
        'my is'([_380],[[_361],[num]]),
        memberEl([_376],[_380],[_384]),
        normalize_result([_419,_421,_423],
                         [_413,_415,_417]).
setup__1(_1219,_1221,_1223) :- 
        [_1146] = _1225, [_965] = _1227, [_1130] = _1229, 
        jobs([_990]),
        makevars([_990],[_1146]),
        resources([_1109]),
        prec([_1013]),
        makeprec([_1013],[_1146]),
        maxnf([_1030]),
        makemaxnf([_1030],[_1146]),
        maxef([_1047]),
        makemaxef([_1047],[_1146]),
        minaf([_1064]),
        makeminaf([_1064],[_1146]),
        minsf([_1081]),
        makeminsf([_1081],[_1146]),
        minnf([_1098]),
        makeminnf([_1098],[_1146]),
        makedisj([_1109],[_1146],[list],[_1130]),
        disjunct([_1130]),
        el([list,list,list,list],[_1146]),
        normalize_result([_1225,_1227,_1229],
                         [_1219,_1221,_1223]).
makeprec__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
makeprec__1(_496,_498) :- 
        [list,_463] = _500, [_467] = _502, 
        el([list,list,list,list],[_467]),
        el([list,list,list,list],[_467]),
        gteqc([_447],[_451],[_455]),
        makeprec([_463],[_467]),
        normalize_result([_500,_502],
                         [_496,_498]).
gteqc__1(_263,_265,_267) :- 
        [_224] = _269, [_233] = _271, [_237] = _273, 
        'my >='([_224],[[_233],[_237]]),
        normalize_result([_269,_271,_273],
                         [_263,_265,_267]).
makemaxnf__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
makemaxnf__1(_595,_597) :- 
        [list,_559] = _599, [_563] = _601, 
        el([list,list,list,list],[_563]),
        el([list,list,list,list],[_563]),
        'my is'([_551],[[_528],[_532]]),
        smeqc([_543],[_547],[_551]),
        makemaxnf([_559],[_563]),
        normalize_result([_599,_601],
                         [_595,_597]).
makemaxef__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
makemaxef__1(_631,_633) :- 
        [list,_595] = _635, [_599] = _637, 
        el([list,list,list,list],[_599]),
        el([list,list,list,list],[_599]),
        'my is'([_587],[[[_558],[_562]],[_568]]),
        smeqc([_579],[_583],[_587]),
        makemaxef([_595],[_599]),
        normalize_result([_635,_637],
                         [_631,_633]).
smeqc__1(_263,_265,_267) :- 
        [_224] = _269, [_233] = _271, [_237] = _273, 
        'my =<'([_224],[[_233],[_237]]),
        normalize_result([_269,_271,_273],
                         [_263,_265,_267]).
makeminaf__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
makeminaf__1(_496,_498) :- 
        [list,_463] = _500, [_467] = _502, 
        el([list,list,list,list],[_467]),
        el([list,list,list,list],[_467]),
        gteqc([_447],[_451],[_455]),
        makeminaf([_463],[_467]),
        normalize_result([_500,_502],
                         [_496,_498]).
makeminsf__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
makeminsf__1(_595,_597) :- 
        [list,_559] = _599, [_563] = _601, 
        el([list,list,list,list],[_563]),
        el([list,list,list,list],[_563]),
        'my is'([_551],[[_528],[_532]]),
        smeqc([_543],[_547],[_551]),
        makeminsf([_559],[_563]),
        normalize_result([_599,_601],
                         [_595,_597]).
makeminnf__1(_122,_124) :- 
        [list] = _126, [_109] = _128, 
        normalize_result([_126,_128],
                          [_122,_124]).
makeminnf__1(_593,_595) :- 
        [list,_557] = _597, [_561] = _599, 
        el([list,list,list,list],[_561]),
        el([list,list,list,list],[_561]),
        'my is'([_549],[[_526],[_545]]),
        gteqc([_541],[_545],[_549]),
        makeminnf([_557],[_561]),
        normalize_result([_597,_599],
                         [_593,_595]).
makedisj__1(_178,_180,_182,_184) :- 
        [list] = _186, [_151] = _188, [_159] = _190, [_159] = _192, 
        normalize_result([_186,_188,_190,_192],
                          [_178,_180,_182,_184]).
makedisj__1(_511,_513,_515,_517) :- 
        [list,_467] = _519, [_471] = _521, [_455] = _523, [_479] = _525, 
        ellist([_435],[_471],[_451]),
        makedisj1([_451],[_455],[_475]),
        makedisj([_467],[_471],[_475],[_479]),
        normalize_result([_519,_521,_523,_525],
                         [_511,_513,_515,_517]).
makedisj1__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
makedisj1__1(_389,_391,_393) :- 
        [list,_355] = _395, [_342] = _397, [_363] = _399, 
        makedisj2([_334],[_355],[_342],[_359]),
        makedisj1([_355],[_359],[_363]),
        normalize_result([_395,_397,_399],
                         [_389,_391,_393]).
makedisj2__1(_178,_180,_182,_184) :- 
        [_147] = _186, [list] = _188, [_159] = _190, [_159] = _192, 
        normalize_result([_186,_188,_190,_192],
                          [_178,_180,_182,_184]).
makedisj2__1(_363,_365,_367,_369) :- 
        [list,list,list] = _371, [list,_327] = _373, [_299] = _375, [_337] = _377, 
        makedisj2([list,list,list],[_327],[list,_299],[_337]),
        normalize_result([_371,_373,_375,_377],
                         [_363,_365,_367,_369]).
ellist__1(_145,_147,_149) :- 
        [list] = _151, [_125] = _153, [list] = _155, 
        normalize_result([_151,_153,_155],
                          [_145,_147,_149]).
ellist__1(_371,_373,_375) :- 
        [list,_337] = _377, [_341] = _379, [list,_345] = _381, 
        el([list,list,list,list],[_341]),
        ellist([_337],[_341],[_345]),
        normalize_result([_377,_379,_381],
                         [_371,_373,_375]).
disjunct__1(_93) :- 
        [list] = _95, 
        normalize_result([_95],
                          [_93]).
disjunct__1(_285) :- 
        [list,_265] = _287, 
        disj([_244],[_248],[_252],[_256]),
        disjunct([_265]),
        normalize_result([_287],
                         [_285]).
disj__1(_288,_290,_292,_294) :- 
        [_259] = _296, [_263] = _298, [_255] = _300, [_239] = _302, 
        gteqc([_255],[_259],[_263]),
        normalize_result([_296,_298,_300,_302],
                         [_288,_290,_292,_294]).
disj__1(_288,_290,_292,_294) :- 
        [_255] = _296, [_231] = _298, [_259] = _300, [_263] = _302, 
        gteqc([_255],[_259],[_263]),
        normalize_result([_296,_298,_300,_302],
                         [_288,_290,_292,_294]).
rev__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
rev__1(_279,_281,_283) :- 
        [list,_246] = _285, [_227] = _287, [_256] = _289, 
        rev([_246],[list,_227],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
el__1(_138,_140) :- 
        [_119] = _142, [list,_106] = _144, 
        normalize_result([_142,_144],
                          [_138,_140]).
el__1(_215,_217) :- 
        [_191] = _219, [list,_195] = _221, 
        el([_191],[_195]),
        normalize_result([_219,_221],
                         [_215,_217]).
p__1(_200,_202) :- 
        [_178] = _204, [_155] = _206, 
        'my =:='([_178],[_178]),
        normalize_result([_204,_206],
                         [_200,_202]).
jobs__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
jobs__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
duration__1(_116,_118) :- 
        [atom] = _120, [atom] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
duration__1(_116,_118) :- 
        [atom] = _120, [atom] = _122, 
        normalize_result([_120,_122],
                          [_116,_118]).
maxnf__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
maxnf__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
minsf__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
minsf__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
maxef__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
maxef__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
minnf__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
minnf__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
minaf__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
minaf__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
resources__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
resources__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
prec__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).
prec__1(_93) :- 
        [atom] = _95, 
        normalize_result([_95],
                          [_93]).

%---------------- Definitions of tabled preds --------------
prec(_63) :- 
        prec__1(_65),
        unify_sets([_63], [_65]).
resources(_63) :- 
        resources__1(_65),
        unify_sets([_63], [_65]).
minaf(_63) :- 
        minaf__1(_65),
        unify_sets([_63], [_65]).
minnf(_63) :- 
        minnf__1(_65),
        unify_sets([_63], [_65]).
maxef(_63) :- 
        maxef__1(_65),
        unify_sets([_63], [_65]).
minsf(_63) :- 
        minsf__1(_65),
        unify_sets([_63], [_65]).
maxnf(_63) :- 
        maxnf__1(_65),
        unify_sets([_63], [_65]).
duration(_63,_65) :- 
        duration__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
jobs(_63) :- 
        jobs__1(_65),
        unify_sets([_63], [_65]).
p(_63,_65) :- 
        p__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
el(_63,_65) :- 
        el__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
rev(_63,_65,_67) :- 
        rev__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
disj(_63,_65,_67,_69) :- 
        disj__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
disjunct(_63) :- 
        disjunct__1(_65),
        unify_sets([_63], [_65]).
ellist(_63,_65,_67) :- 
        ellist__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
makedisj2(_63,_65,_67,_69) :- 
        makedisj2__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
makedisj1(_63,_65,_67) :- 
        makedisj1__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
makedisj(_63,_65,_67,_69) :- 
        makedisj__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
makeminnf(_63,_65) :- 
        makeminnf__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
makeminsf(_63,_65) :- 
        makeminsf__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
makeminaf(_63,_65) :- 
        makeminaf__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
smeqc(_63,_65,_67) :- 
        smeqc__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
makemaxef(_63,_65) :- 
        makemaxef__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
makemaxnf(_63,_65) :- 
        makemaxnf__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
gteqc(_63,_65,_67) :- 
        gteqc__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
makeprec(_63,_65) :- 
        makeprec__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
setup(_63,_65,_67) :- 
        setup__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
memberEl(_63,_65,_67) :- 
        memberEl__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
makevars(_63,_65) :- 
        makevars__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
top(_63) :- 
        top__1(_65),
        unify_sets([_63], [_65]).
zero200(_63) :- 
        zero200__1(_65),
        unify_sets([_63], [_65]).

%---------------- Tp ---------------------------------------

tp :- prec__1(_64), fail.
tp :- resources__1(_64), fail.
tp :- minaf__1(_64), fail.
tp :- minnf__1(_64), fail.
tp :- maxef__1(_64), fail.
tp :- minsf__1(_64), fail.
tp :- maxnf__1(_64), fail.
tp :- duration__1(_64,_66), fail.
tp :- jobs__1(_64), fail.
tp :- p__1(_64,_66), fail.
tp :- el__1(_64,_66), fail.
tp :- rev__1(_64,_66,_68), fail.
tp :- disj__1(_64,_66,_68,_70), fail.
tp :- disjunct__1(_64), fail.
tp :- ellist__1(_64,_66,_68), fail.
tp :- makedisj2__1(_64,_66,_68,_70), fail.
tp :- makedisj1__1(_64,_66,_68), fail.
tp :- makedisj__1(_64,_66,_68,_70), fail.
tp :- makeminnf__1(_64,_66), fail.
tp :- makeminsf__1(_64,_66), fail.
tp :- makeminaf__1(_64,_66), fail.
tp :- smeqc__1(_64,_66,_68), fail.
tp :- makemaxef__1(_64,_66), fail.
tp :- makemaxnf__1(_64,_66), fail.
tp :- gteqc__1(_64,_66,_68), fail.
tp :- makeprec__1(_64,_66), fail.
tp :- setup__1(_64,_66,_68), fail.
tp :- memberEl__1(_64,_66,_68), fail.
tp :- makevars__1(_64,_66), fail.
tp :- top__1(_64), fail.
tp :- zero200__1(_64), fail.
tp.


%---------------- Builtin Preds ----------------------------

'my ='(X1,X2) :- 'my =__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my ==='(X1,X2) :- 'my ===__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my =='(X1,X2) :- 'my ==__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my is'(X1,X2) :- 'my is__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my =:='(X1,X2) :- 'my =:=__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my <'(X1,X2) :- 'my <__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my >'(X1,X2) :- 'my >__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my >='(X1,X2) :- 'my >=__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my =<'(X1,X2) :- 'my =<__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).

'my =__1'(X,X).
'my ===__1'(_,_).
'my ==__1'(num,num).
'my is__1'(num,num).
'my <__1'(num,num).
'my >__1'(num,num).
'my >=__1'(num,num).
'my =<__1'(num,num).
'my =:=__1'(num,num).


%---------------- Show Result ------------------------------

show_facts :- prec__1(_63),
              numbervars([_63]),
              assert(results(prec(_63))), fail.
show_facts :- resources__1(_63),
              numbervars([_63]),
              assert(results(resources(_63))), fail.
show_facts :- minaf__1(_63),
              numbervars([_63]),
              assert(results(minaf(_63))), fail.
show_facts :- minnf__1(_63),
              numbervars([_63]),
              assert(results(minnf(_63))), fail.
show_facts :- maxef__1(_63),
              numbervars([_63]),
              assert(results(maxef(_63))), fail.
show_facts :- minsf__1(_63),
              numbervars([_63]),
              assert(results(minsf(_63))), fail.
show_facts :- maxnf__1(_63),
              numbervars([_63]),
              assert(results(maxnf(_63))), fail.
show_facts :- duration__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(duration(_63,_65))), fail.
show_facts :- jobs__1(_63),
              numbervars([_63]),
              assert(results(jobs(_63))), fail.
show_facts :- p__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(p(_63,_65))), fail.
show_facts :- el__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(el(_63,_65))), fail.
show_facts :- rev__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(rev(_63,_65,_67))), fail.
show_facts :- disj__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(disj(_63,_65,_67,_69))), fail.
show_facts :- disjunct__1(_63),
              numbervars([_63]),
              assert(results(disjunct(_63))), fail.
show_facts :- ellist__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(ellist(_63,_65,_67))), fail.
show_facts :- makedisj2__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(makedisj2(_63,_65,_67,_69))), fail.
show_facts :- makedisj1__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(makedisj1(_63,_65,_67))), fail.
show_facts :- makedisj__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(makedisj(_63,_65,_67,_69))), fail.
show_facts :- makeminnf__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(makeminnf(_63,_65))), fail.
show_facts :- makeminsf__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(makeminsf(_63,_65))), fail.
show_facts :- makeminaf__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(makeminaf(_63,_65))), fail.
show_facts :- smeqc__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(smeqc(_63,_65,_67))), fail.
show_facts :- makemaxef__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(makemaxef(_63,_65))), fail.
show_facts :- makemaxnf__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(makemaxnf(_63,_65))), fail.
show_facts :- gteqc__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(gteqc(_63,_65,_67))), fail.
show_facts :- makeprec__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(makeprec(_63,_65))), fail.
show_facts :- setup__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(setup(_63,_65,_67))), fail.
show_facts :- memberEl__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(memberEl(_63,_65,_67))), fail.
show_facts :- makevars__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(makevars(_63,_65))), fail.
show_facts :- top__1(_63),
              numbervars([_63]),
              assert(results(top(_63))), fail.
show_facts :- zero200__1(_63),
              numbervars([_63]),
              assert(results(zero200(_63))), fail.
show_facts.
%----------------------------------------------------------
%:- import set_unify/2 from set_unify.

set_unify(A,B) :-
	flatten(A,AF), flatten(B,BF),
        (my_ground(AF), my_ground(BF) -> AF==BF
          ;
         ord_union(AF,BF,ToCover),
         ord_setproduct(AF,BF,AxB),
         drop_nonuni(AxB,UTable),
         covering_u_table(UTable,ToCover,CovUTable),
         keysort(CovUTable,CovUTableS),
         table2graph(CovUTableS,CovUGraph),
         u_graph_proceed(CovUGraph)
        ).

drop_nonuni([],[]).
drop_nonuni([X-Y|AxB],Out) :-
        (\+ \+ X=Y -> Out = [X-Y|NewAxB] ; Out = NewAxB),
        drop_nonuni(AxB,NewAxB).

covering_u_table([],[],[]).
covering_u_table([X-Y|XsYs],ToCover,[X-L,Y-L|UTRest]) :-
        (X @< Y ->
         ord_subtract(ToCover,[X,Y],ToCoverRest)
          ;
         ord_subtract(ToCover,[Y,X],ToCoverRest)),
        covering_u_table(XsYs,ToCoverRest,UTRest).
covering_u_table([_|XsYs],ToCover,UTable) :-
        covering_u_table(XsYs,ToCover,UTable).

u_graph_proceed([]).
u_graph_proceed([V-E|G]) :- match_single(V,E), u_graph_proceed(G).

match_single(S,Y) :-
	var(S) -> S=Y ; match_list(Y,S).	% simple atom

match_list([],_).
match_list([S|Xs],S) :- match_list(Xs,S).

normalize_result(Args,SortedArgsC) :-!,
        flatten_sets(Args,ArgsF),
        occur_graph(ArgsF,OG),
        swap_keys_values(OG,GO),
        keysort(GO,GOS),
        drop_equiv(GOS,NewGO),
        graph2table(NewGO,OGTable),
	keysort(OGTable,OGTableS),	% was merge_keysort
        table2graph(OGTableS,KeysArgs),
        keys_and_values(KeysArgs,_,ArgsC),
	sortall(ArgsC,SortedArgsC).

sortall([], []).
sortall([H|T], [NH|NT]) :- sort(H,NH), sortall(T,NT).

drop_equiv([], []).
drop_equiv([One|Rest], Out) :- drop_equiv(One, Rest, Out).

%:- index drop_equiv/3-2.

drop_equiv(X, [], [X]).
drop_equiv(E1-V1, [E2-V2|Vs],Out) :-
        (E1==E2, my_variant(V1,V2) ->
         V1=V2, Out=OutRest
          ;
         Out=[E1-V1|OutRest]),
        drop_equiv(E2-V2, Vs, OutRest).

og_table([],_,[]).
og_table([One|Args],I,OG_Table) :- og_table(One,Args,I,OG_Table).

og_table([],Args,I,OG_Table) :- !, J is I+1, og_table(Args,J,OG_Table).
og_table([V|Vs],Args,I,[V-I|OG_Table]) :- og_table(Vs,Args,I,OG_Table).

table2graph([],[]).
table2graph([One|Rest],Out) :- table2graph(One,Rest,Out).

%:- index table2graph/3-2.

table2graph(V-I,[],[V-[I]]).
table2graph(V-I,[W-J|Vs],Out) :-
        ( V==W ->
          Out = [V-[I|Is]|VsClps], Cont = [V-Is|VsClps]
          ;
          Out = [V-[I]|VsClps], Cont = VsClps),
        table2graph(W-J,Vs,Cont).

graph2table([],[]).
graph2table([Elem-V|Vs],Edges) :- graph2table(Elem,V,Vs,Edges).

graph2table([],_,Vs,Edges) :- graph2table(Vs,Edges).
graph2table([E|Es],V,Vs,[E-V|Edges]) :- graph2table(Es,V,Vs,Edges).

swap_keys_values([],[]).
swap_keys_values([Key-Val|KVs],[Val-Key|VKs]) :- swap_keys_values(KVs,VKs).

/*------------------ Bypasses a bug of SWI -----------------------------

merge_keysort([],[]) :- !.
merge_keysort([X],[X]) :- !.
merge_keysort(VL,VS) :-
        split(VL,VL1,VL2),
        merge_keysort(VL1,VL1S), merge_keysort(VL2,VL2S),
        merge(VL1S,VL2S,VS).

split([],[],[]) :- !.
split([X],[X],[]) :- !.
split([X1,X2|Xs],[X1|X1s],[X2|X2s]) :- split(Xs,X1s,X2s).

merge([],[],[]) :- !.
merge(L,[],L) :- !.
merge([],L,L) :- !.
merge([K1-V1|X1s],[K2-V2|X2s],[K1-V1|Xs]) :- K1@<K2, !,
        merge(X1s,[K2-V2|X2s],Xs).
merge([X1|X1s],[X2|X2s],[X2|Xs]) :- merge([X1|X1s],X2s,Xs).

  ----------------------------------------------------------------------*/

%%-----------------------------------------------------------------------------

keys_and_values([], [], []).
keys_and_values([Key-Value|Pairs], [Key|Keys], [Value|Values]) :-
   keys_and_values(Pairs, Keys, Values).

%%-----------------------------------------------------------------------------
flatten(Set,SetF) :- colterms(Set,[],SetMemb), sort(SetMemb,SetF).

colterms(V,I,O) :- var(V), !, O = [V|I].
colterms([],I,O) :- !, O = I.
colterms(A,I,O) :- atom(A), !, O=[A|I].
colterms([T|Ts],I,O) :- colterms(T,I,IT), colterms(Ts,IT,O).

flatten_sets([],[]).
flatten_sets([T|Ts],[FT|FTs]) :- flatten(T,FT), flatten_sets(Ts,FTs).

%%-----------------------------------------------------------------------------
%% Operations on graphs

insert_vertice([],N,[N]).
insert_vertice([V-E|G],V1-E1,[V-EE1|G]) :- V==V1, ord_union(E,E1,EE1), !.
insert_vertice([N|G],N1,[N|NewG]) :- insert_vertice(G,N1,NewG).

%%-----------------------------------------------------------------------------
occur_graph(ArgsVs,OG) :-
        og_table(ArgsVs,0,OG_Table),
        sort(OG_Table,OG_TableSorted),
        table2graph(OG_TableSorted,OG).

occur_graph(ArgsV,OG) :- occur_graph(ArgsV,0,OG).
occur_graph([],_,[]).
occur_graph([L|A],I,OG) :- J is I+1, occur_graph(A,J,O1), ogl(L,I,O1,OG) .

ogl([],_,O,O).
ogl([V|R],I,In,Out) :- ogl(R,I,In,O1), insert_vertice(O1,V-[I],Out).

%%-----------------------------------------------------------------------------

numbervars(X):-my_number_vars(X,0,_).

my_number_vars(T, N0, N) :- 
    var(T),!, 
    T='$VAR'(N0),
    N is N0+1.
my_number_vars(T, N0, N) :- 
    atomic(T),!,
     N0=N.
my_number_vars([X|Xs], N0, N) :- !,
    my_number_vars(X, N0, N1),
    my_number_vars(Xs, N1, N).
my_number_vars(X, N0, N) :-
    functor(X, _, A),
    my_number_vars(0, A, X, N0, N).

my_number_vars(A, A, _, N0, N) :- !,
    N0=N.
my_number_vars(A0, A, X, N0, N) :-
    A1 is A0+1,
    arg(A1, X, X1),
    my_number_vars(X1, N0, N1),
    my_number_vars(A1, A, X, N1, N).


%   list_to_ord_set(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  The only reason for giving it
%   a name at all is that you may not have realised that sort/2 could be
%   used this way.

list_to_ord_set(List, Set) :-
	sort(List, Set).


%   merge(+List1, +List2, -Merged)
%   is true when Merged is the stable merge of the two given lists.
%   If the two lists are not ordered, the merge doesn't mean a great
%   deal.  Merging is perfectly well defined when the inputs contain
%   duplicates, and all copies of an element are preserved in the
%   output, e.g. merge("122357", "34568", "12233455678").  Study this
%   routine carefully, as it is the basis for all the rest.

merge([Head1|Tail1], [Head2|Tail2], [Head2|Merged]) :-
	Head1 @> Head2, !,
	merge([Head1|Tail1], Tail2, Merged).
merge([Head1|Tail1], List2, [Head1|Merged]) :-
	List2 \== [], !,
	merge(Tail1, List2, Merged).
merge([], List2, List2) :- !.
merge(List1, [], List1).



%   ord_disjoint(+Set1, +Set2)
%   is true when the two ordered sets have no element in common.  If the
%   arguments are not ordered, I have no idea what happens.

ord_disjoint([], _) :- !.
ord_disjoint(_, []) :- !.
ord_disjoint([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_disjoint(Order, Head1, Tail1, Head2, Tail2).

ord_disjoint(<, _, Tail1, Head2, Tail2) :-
	ord_disjoint(Tail1, [Head2|Tail2]).
ord_disjoint(>, Head1, Tail1, _, Tail2) :-
	ord_disjoint([Head1|Tail1], Tail2).



%   ord_insert(+Set1, +Element, ?Set2)
%   is the equivalent of add_element for ordered sets.  It should give
%   exactly the same result as merge(Set1, [Element], Set2), but a bit
%   faster, and certainly more clearly.

ord_insert([], Element, [Element]).
ord_insert([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_insert(Order, Head, Tail, Element, Set).


ord_insert(<, Head, Tail, Element, [Head|Set]) :-
	ord_insert(Tail, Element, Set).
ord_insert(=, Head, Tail, _, [Head|Tail]).
ord_insert(>, Head, Tail, Element, [Element,Head|Tail]).



%   ord_intersect(+Set1, +Set2)
%   is true when the two ordered sets have at least one element in common.
%   Note that the test is == rather than = .

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).

ord_intersect(=, _, _, _, _).
ord_intersect(<, _, Tail1, Head2, Tail2) :-
	ord_intersect(Tail1, [Head2|Tail2]).
ord_intersect(>, Head1, Tail1, _, Tail2) :-
	ord_intersect([Head1|Tail1], Tail2).



%   ord_intersect(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersect(_, [], []) :- !.
ord_intersect([], _, []) :- !.
ord_intersect([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersect(=, Head,  Tail1, _,     Tail2, [Head|Intersection]) :-
	ord_intersect(Tail1, Tail2, Intersection).
ord_intersect(<, _,     Tail1, Head2, Tail2, Intersection) :-
	ord_intersect(Tail1, [Head2|Tail2], Intersection).
ord_intersect(>, Head1, Tail1, _,     Tail2, Intersection) :-
	ord_intersect([Head1|Tail1], Tail2, Intersection).



%   ord_seteq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be ordered representations, they must be identical.


ord_seteq(Set1, Set2) :-
	Set1 == Set2.


%   ord_setproduct(+Set1, +Set2, ?Product)
%   is in fact identical to setproduct(Set1, Set2, Product).
%   If Set1 and Set2 are ordered sets, Product will be an ordered
%   set of x1-x2 pairs.  Note that we cannot solve for Set1 and
%   Set2, because there are infinitely many solutions when
%   Product is empty, and may be a large number in other cases.

ord_setproduct([], _, []).
ord_setproduct([H|T], L, Product) :-
	ord_setproduct(L, H, Product, Rest),
	ord_setproduct(T, L, Rest).

ord_setproduct([], _, L, L).
ord_setproduct([H|T], X, [X-H|TX], TL) :-
	ord_setproduct(T, X, TX, TL).


%   ord_subset(+Set1, +Set2)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.

ord_subset([], _) :- !.
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Head2, Tail2).

ord_subset(=, _, Tail1, _, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset(>, Head1, Tail1, _, Tail2) :-
	ord_subset([Head1|Tail1], Tail2).



%   ord_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2.


ord_subtract([], _, []).
ord_subtract([Head1|Tail1], Set2, Difference) :-
	ord_subtract(Set2, Head1, Tail1, Difference).

ord_subtract([], Head1, Tail1, [Head1|Tail1]).
ord_subtract([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
        'ord subtract'(Tail1, Head2, Tail2, Difference).
ord_subtract(>, Head1, Tail1, _,     Tail2, Difference) :-
        ord_subtract(Tail2, Head1, Tail1, Difference).
ord_subtract(=, _,     Tail1, _,     Tail2, Difference) :-
        ord_subtract(Tail1, Tail2, Difference).

'ord subtract'([], _, _, []).
'ord subtract'([Head1|Tail1], Head2, Tail2, Difference) :-
        compare(Order, Head1, Head2),
        ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).


%   ord_symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2.

ord_symdiff(Set1, [], Set1) :- !.
ord_symdiff([], Set2, Set2) :- !.
ord_symdiff([Head1|Tail1], [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_symdiff(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_symdiff(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_symdiff(Tail1, Tail2, Difference).
ord_symdiff(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_symdiff(Tail1, [Head2|Tail2], Difference).
ord_symdiff(>, Head1, Tail1, Head2, Tail2, [Head2|Difference]) :-
	ord_symdiff([Head1|Tail1], Tail2, Difference).



%   ord_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

ord_union([], Set2, Set2).
ord_union([Head1|Tail1], Set2, Union) :-
	ord_union(Set2, Head1, Tail1, Union).

ord_union([], Head1, Tail1, [Head1|Tail1]).
ord_union([Head2|Tail2], Head1, Tail1, Union) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union(=, Head,  Tail1, _,     Tail2, [Head|Union]) :-
	ord_union(Tail1, Tail2, Union).
ord_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_union(Tail1, [Head2|Tail2], Union).
ord_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_union([Head1|Tail1], Tail2, Union).


%   ord_union(+ListOfSets, ?Union)
%   is true when ListOfSets is given as a proper list of ordered sets
%   and Union is their union.  Letting K be the length of ListOfSets,
%   and N the sum of the sizes of its elements, the cost is of order
%   N.lg(K).  The auxiliary routine
%   ord_union_3(N, L, U, R)
%   is true when the union of the first N sets in L is U and
%   R is the remaining elements of L.

ord_union(ListOfSets, Union) :-
	length(ListOfSets, NumberOfSets),
	ord_union_3(NumberOfSets, ListOfSets, Union, []).

ord_union_3(0, R, [], R) :- !.
ord_union_3(1, [U|R], U, R) :- !.
ord_union_3(2, [A,B|R], U, R) :- !,
	ord_union(A, B, U).
ord_union_3(N, R0, U, R) :-
	P is N>>1,      % |first  half of list|
	Q is N- P,      % |second half of list|
	ord_union_3(P, R0, A, R1),
	ord_union_3(Q, R1, B, R),
	ord_union(A, B, U).


%   ord_del_element(+Set1, +Element, ?Set2)
%   is the equivalent of del_element for ordered sets.  Because it uses
%   ordering, it typically builds less structure, but is slower than
%   del_element.  I am beginning to wonder whether a predicate
%       set_plus(SmallSet, Element, LargeSet)
%   would be a better way of doing this, the idea being that
%   LargeSet = SmallSet U {Element} and Element is not in SmallSet.
%   There is already a predicate with this effect called select/3.

ord_del_element([], _, []).
ord_del_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_del_element(Order, Element, Head, Tail, Set).

ord_del_element(<, Element, Head, Tail, [Head|Set]) :-
	ord_del_element(Tail, Element, Set).
ord_del_element(=, _, _, Set, Set).
ord_del_element(>, _, Head, Tail, [Head|Tail]).


my_ground(X):-var(X),!,fail.
my_ground(X):-atomic(X),!,true.
my_ground([X|Xs]):-!,my_ground(X),my_ground(Xs).
my_ground(X):-functor(X,_,N),my_ground(X,N).

my_ground(_,N):-N=:=0,!,true.
my_ground(X,N):-N>0,arg(N,X,A),my_ground(A),N1 is N-1,my_ground(X,N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   subsumes_chk(General, Specific)
%   is true when Specific is an instance of General. However, this
%   predicate performs the test without binding any variables neither
%   in General nor in Specific.

subsumes_chk(General, Specific) :-
    \+((numbervars(Specific),
       \+ (General = Specific)
       )).

%   subsumes(General, Specific)
%   is true when Specific is an instance of General. Unlike subsumes_chk/2
%   this predicate will bind variables in General (but not those in Specific)
%   so that General becomes identical to Specific.

subsumes(General, Specific) :-
    subsumes_chk(General, Specific),
    General = Specific.

/*** SB-Prolog one
subsumes(X,Y) :- \+ (X=Y),!,fail.
subsumes(X,Y) :- numbervars(Y,0,_),\+ (X=Y),!,fail.
subsumes(_,_).
***/

%   my_variant(Term1, Term2)
%   is true when Term1 and Term2 are alphabetic variants. The definition 
%   here is not quite right;  if Term1 and Term2 share any variables it 
%   may give the wrong answer. It is only meant to be used when the two 
%   terms have no variables in common. 

/*** A naive first version */
my_variant(Term1, Term2) :-
    subsumes_chk(Term1, Term2),
    subsumes_chk(Term2, Term1).

result([prec([atom]), resources([atom]), minaf([atom]), minnf([atom]),
	maxef([atom]), minsf([atom]), maxnf([atom]),
	duration([atom],[atom]), jobs([atom]), p([num],[A]),
	el([A],[B,list]), rev([list],[A],[A]),
	rev([list],[A],[A,list]), disj([num],[num],[num],[A]),
	disj([num],[A],[num],[num]), disjunct([list]),
	ellist([list],[A],[list]), ellist([list],[A,list],[list]),
	ellist([list],[list],[list]), makedisj2([A],[list],[B],[B]),
	makedisj2([list],[list],[A],[A,list]),
	makedisj1([list],[A],[A]), makedisj1([list],[A],[A,list]),
	makedisj([list],[A],[B],[B]),
	makedisj([list],[A],[B],[B,list]),
	makedisj([list],[A,list],[B],[B]),
	makedisj([list],[A,list],[B],[B,list]),
	makedisj([list],[list],[A],[A]),
	makedisj([list],[list],[A],[A,list]), makeminnf([list],[A]),
	makeminnf([list],[A,list]), makeminnf([list],[list]),
	makeminsf([list],[A]), makeminsf([list],[A,list]),
	makeminsf([list],[list]), makeminaf([list],[A]),
	makeminaf([list],[A,list]), makeminaf([list],[list]),
	smeqc([num],[num],[num]), makemaxef([list],[A]),
	makemaxef([list],[A,list]), makemaxef([list],[list]),
	makemaxnf([list],[A]), makemaxnf([list],[A,list]),
	makemaxnf([list],[list]), gteqc([num],[num],[num]),
	makeprec([list],[A]), makeprec([list],[A,list]),
	makeprec([list],[list]), memberEl([A],[A],[B]),
	memberEl([num],[num],[num]), makevars([list],[list]),
	zero200([A])]).
