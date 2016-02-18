:- module( ta3 , main/0 , [] ).

:- push_prolog_flag(unused_pred_warnings, no).

:- use_module(library(time_analyzer)).

:- use_module(library(lists)).


qsort([],[]).
qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1), 
        append(R1,[X|R2],R).

partition([],_B,[],[]).
partition([E|R],C,[E|Left1],Right):- 
	E < C,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E >= C,
	partition(R,C,Left,Right1).


qsort2([X|L],R,R2) :-
	partition2(L,X,L1,L2),
	qsort2(L2,R,[X|R1]),
        qsort2(L1,R1,R2).
qsort2([],R,R).

partition2([],_,[],[]).
partition2([E|R],C,[E|Left1],Right):- E < C, !,
	partition2(R,C,Left1,Right).
partition2([E|R],C,Left,[E|Right1]):-
	partition2(R,C,Left,Right1).

 
test_qsort(  A ) :- qsort(  A , _ ).
test_qsort2( A ) :- qsort2( A , _ , [] ).

generate_benchmark( [] , [] ).
generate_benchmark( [A|R] , [(A,L)|RR] ) :-
	length( A , L ),
	generate_benchmark( R , RR ).


%       log
test3 :-
	generate_benchmark( [ "Just a Sentence" , 
	                      "A bigger sentence than previous one" ,
			      "A really much more larger sentences than the precious 2 one",
			      "asdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmn"
%			      ,"asdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmasdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmn"
%			      ,"asdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmasdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmnasdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmasdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmnasdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmasdf sadf sd fas dasd f asdf asd asd  asdf asd f asd fas df asdf asd fasdfasdfqerqwlekjd.mnb.smhdfglkjqhrquewlasjkfha.sdmn.amndfljkncv.xbnlakjsdhflfhqoeityreuahlgfbnj.cmzvnb.zm,nvc.mnalsghfalkjsdhfoqiytljfdhg.cxn.zm,xcnbjakshdflkjrheolaiuhdlfjhvnasmdnflksajdhglakjlfkjhadlghdlfghalskjhdfsda.nfa.sdmn"
			    ] , BL ),
    get_general_options( A ),
    compare_benchmark( [ function( 'x/100 title "x=y" with lines' ) , 
                         (test_qsort  , [title("qsort")          ,with(lines)]) , 
			 (test_qsort2 , [title("optimized qsort"),with(lines)]) 
		       ] , 
		       BL , 
		       average , 
		       5 ,
		       'output' ,
		       runtime ,
                       [ 
			title("Comparing qsort and qsort2"),
			xlabel("Argument Length"),
			ylabel("Time in ms")|A 
		       ] ).
% 	benchmark( test_qsort  , BL , 5 , Times1 ),
% 	benchmark( test_qsort2 , BL , 5 , Times2 ),
% 	generate_plot( 'qsort_test',[ (Times1,[]) , (Times2,[]) ] ).


main :-
	test3.

:- pop_prolog_flag(unused_pred_warnings).
