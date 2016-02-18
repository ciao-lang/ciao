 %% USER MANUAL
 %% - path_dc(N) executes an standard query (N is the maximun path length)
 %% - times(N,path_dc(N)) repeats the execution of path_dc(N)  
 %%   to get its execution time on average.
 %%   N is the minimum time we want to execute times/2.

:- module(path_dc,
	[
	    times/2,
	    p/1,
	    path/4
	],[]).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints/difference_constraints_tab)).
:- use_package(library(difference_constraints)).

p(N) :- 
	X #>= 0, 
	X #=< N, 
	path(1,_,X,N).

:- include(times).

:- table path/4.

path(A,A,_X,_N).
path(A,C,X,N) :-
	edge(A,B),
	NX #= X + 1,
	NX #=< N,
	path(B,C,NX,N).

edge(30,16). 	 
edge(30,14). 	 
edge(30,27). 	 
edge(30,22). 	 
edge(30,29). 	 
edge(30,6). 	 
edge(30,5). 	 
edge(30,23). 	 
edge(30,19). 	 
edge(30,7). 	 
edge(30,18). 	 
edge(30,9). 	 
edge(30,16). 	 
edge(30,8). 	 
edge(30,26). 	 
edge(30,19). 	 
edge(30,3). 	 
edge(30,9). 	 
edge(30,29). 	 
edge(30,15). 	 
edge(30,6). 	 
edge(30,24). 	 
edge(30,13). 	 
edge(30,25). 	 
edge(30,2). 	 
edge(30,4). 	 
edge(29,7). 	 
edge(29,16). 	 
edge(29,2). 	 
edge(29,6). 	 
edge(29,13). 	 
edge(29,18). 	 
edge(29,28). 	 
edge(29,3). 	 
edge(29,4). 	 
edge(29,22). 	 
edge(29,26). 	 
edge(29,25). 	 
edge(29,26). 	 
edge(29,23). 	 
edge(29,27). 	 
edge(29,15). 	 
edge(29,19). 	 
edge(29,14). 	 
edge(29,6). 	 
edge(29,8). 	 
edge(29,12). 	 
edge(29,9). 	 
edge(29,5). 	 
edge(29,9). 	 
edge(28,23). 	 
edge(28,19). 	 
edge(28,9). 	 
edge(28,12). 	 
edge(28,27). 	 
edge(28,7). 	 
edge(28,16). 	 
edge(28,2). 	 
edge(28,24). 	 
edge(28,6). 	 
edge(28,3). 	 
edge(28,26). 	 
edge(28,4). 	 
edge(28,15). 	 
edge(28,19). 	 
edge(28,29). 	 
edge(28,17). 	 
edge(28,29). 	 
edge(28,26). 	 
edge(28,6). 	 
edge(28,25). 	 
edge(28,13). 	 
edge(28,18). 	 
edge(28,5). 	 
edge(28,16). 	 
edge(27,26). 	 
edge(27,2). 	 
edge(27,16). 	 
edge(27,15). 	 
edge(27,25). 	 
edge(27,29). 	 
edge(27,5). 	 
edge(27,23). 	 
edge(27,28). 	 
edge(27,4). 	 
edge(27,16). 	 
edge(27,12). 	 
edge(27,18). 	 
edge(27,26). 	 
edge(27,22). 	 
edge(27,6). 	 
edge(27,14). 	 
edge(27,7). 	 
edge(27,29). 	 
edge(27,17). 	 
edge(27,19). 	 
edge(27,9). 	 
edge(27,13). 	 
edge(27,6). 	 
edge(27,9). 	 
edge(27,3). 	 
edge(27,24). 	 
edge(26,19). 	 
edge(26,7). 	 
edge(26,17). 	 
edge(26,6). 	 
edge(26,5). 	 
edge(26,4). 	 
edge(26,25). 	 
edge(26,6). 	 
edge(26,15). 	 
edge(26,2). 	 
edge(26,19). 	 
edge(26,27). 	 
edge(26,24). 	 
edge(26,12). 	 
edge(26,29). 	 
edge(26,28). 	 
edge(26,29). 	 
edge(26,9). 	 
edge(26,26). 	 
edge(26,23). 	 
edge(26,14). 	 
edge(26,13). 	 
edge(26,9). 	 
edge(26,18). 	 
edge(26,16). 	 
edge(25,29). 	 
edge(25,5). 	 
edge(25,3). 	 
edge(25,17). 	 
edge(25,18). 	 
edge(25,28). 	 
edge(25,27). 	 
edge(25,26). 	 
edge(25,16). 	 
edge(25,8). 	 
edge(25,14). 	 
edge(25,4). 	 
edge(25,9). 	 
edge(25,12). 	 
edge(25,13). 	 
edge(25,19). 	 
edge(25,19). 	 
edge(25,23). 	 
edge(25,26). 	 
edge(25,22). 	 
edge(25,16). 	 
edge(25,9). 	 
edge(25,7). 	 
edge(25,15). 	 
edge(25,6). 	 
edge(25,6). 	 
edge(25,2). 	 
edge(24,19). 	 
edge(24,19). 	 
edge(24,5). 	 
edge(24,28). 	 
edge(24,29). 	 
edge(24,16). 	 
edge(24,17). 	 
edge(24,16). 	 
edge(24,14). 	 
edge(24,25). 	 
edge(24,12). 	 
edge(24,3). 	 
edge(24,7). 	 
edge(24,18). 	 
edge(24,6). 	 
edge(24,26). 	 
edge(24,15). 	 
edge(24,23). 	 
edge(24,29). 	 
edge(24,27). 	 
edge(24,9). 	 
edge(23,15). 	 
edge(23,17). 	 
edge(23,2). 	 
edge(23,9). 	 
edge(23,12). 	 
edge(23,4). 	 
edge(23,16). 	 
edge(23,29). 	 
edge(23,29). 	 
edge(23,19). 	 
edge(23,9). 	 
edge(23,25). 	 
edge(23,28). 	 
edge(23,19). 	 
edge(23,24). 	 
edge(23,26). 	 
edge(23,16). 	 
edge(23,7). 	 
edge(23,22). 	 
edge(23,8). 	 
edge(23,5). 	 
edge(23,14). 	 
edge(23,6). 	 
edge(23,27). 	 
edge(22,19). 	 
edge(22,5). 	 
edge(22,3). 	 
edge(22,23). 	 
edge(22,28). 	 
edge(22,9). 	 
edge(22,19). 	 
edge(22,7). 	 
edge(22,13). 	 
edge(22,26). 	 
edge(22,29). 	 
edge(22,17). 	 
edge(22,24). 	 
edge(22,15). 	 
edge(22,25). 	 
edge(22,6). 	 
edge(22,16). 	 
edge(22,2). 	 
edge(22,9). 	 
edge(22,27). 	 
edge(22,26). 	 
edge(22,6). 	 
edge(22,12). 	 
edge(21,16). 	 
edge(21,7). 	 
edge(21,25). 	 
edge(21,15). 	 
edge(21,26). 	 
edge(21,24). 	 
edge(21,14). 	 
edge(21,12). 	 
edge(21,22). 	 
edge(21,13). 	 
edge(21,9). 	 
edge(21,23). 	 
edge(21,18). 	 
edge(21,28). 	 
edge(21,16). 	 
edge(21,5). 	 
edge(21,29). 	 
edge(21,6). 	 
edge(21,17). 	 
edge(21,2). 	 
edge(21,9). 	 
edge(21,4). 	 
edge(21,19). 	 
edge(21,3). 	 
edge(21,27). 	 
edge(21,19). 	 
edge(20,7). 	 
edge(20,12). 	 
edge(20,16). 	 
edge(20,5). 	 
edge(20,23). 	 
edge(20,22). 	 
edge(20,3). 	 
edge(20,9). 	 
edge(20,26). 	 
edge(20,29). 	 
edge(20,24). 	 
edge(20,18). 	 
edge(20,6). 	 
edge(20,26). 	 
edge(20,27). 	 
edge(20,25). 	 
edge(20,4). 	 
edge(20,28). 	 
edge(20,19). 	 
edge(20,2). 	 
edge(20,9). 	 
edge(20,6). 	 
edge(20,16). 	 
edge(20,15). 	 
edge(20,14). 	 
edge(19,17). 	 
edge(19,28). 	 
edge(19,26). 	 
edge(19,18). 	 
edge(19,2). 	 
edge(19,6). 	 
edge(19,7). 	 
edge(19,23). 	 
edge(19,12). 	 
edge(19,16). 	 
edge(19,27). 	 
edge(19,25). 	 
edge(19,6). 	 
edge(19,29). 	 
edge(19,3). 	 
edge(19,22). 	 
edge(19,24). 	 
edge(19,9). 	 
edge(19,5). 	 
edge(19,16). 	 
edge(19,14). 	 
edge(19,15). 	 
edge(19,9). 	 
edge(19,13). 	 
edge(19,8). 	 
edge(18,16). 	 
edge(18,14). 	 
edge(18,15). 	 
edge(18,9). 	 
edge(18,9). 	 
edge(18,27). 	 
edge(18,19). 	 
edge(18,3). 	 
edge(18,4). 	 
edge(18,12). 	 
edge(18,29). 	 
edge(18,23). 	 
edge(18,7). 	 
edge(18,26). 	 
edge(18,5). 	 
edge(18,8). 	 
edge(18,19). 	 
edge(18,16). 	 
edge(18,6). 	 
edge(18,22). 	 
edge(18,13). 	 
edge(18,6). 	 
edge(18,17). 	 
edge(17,26). 	 
edge(17,9). 	 
edge(17,22). 	 
edge(17,26). 	 
edge(17,28). 	 
edge(17,3). 	 
edge(17,29). 	 
edge(17,15). 	 
edge(17,14). 	 
edge(17,6). 	 
edge(17,13). 	 
edge(17,27). 	 
edge(17,2). 	 
edge(17,6). 	 
edge(17,12). 	 
edge(17,9). 	 
edge(17,16). 	 
edge(17,19). 	 
edge(17,24). 	 
edge(17,16). 	 
edge(17,4). 	 
edge(17,8). 	 
edge(17,23). 	 
edge(17,5). 	 
edge(17,19). 	 
edge(17,18). 	 
edge(17,7). 	 
edge(17,29). 	 
edge(16,14). 	 
edge(16,6). 	 
edge(16,9). 	 
edge(16,2). 	 
edge(16,18). 	 
edge(16,28). 	 
edge(16,6). 	 
edge(16,19). 	 
edge(16,26). 	 
edge(16,5). 	 
edge(16,27). 	 
edge(16,25). 	 
edge(16,16). 	 
edge(16,13). 	 
edge(16,26). 	 
edge(16,9). 	 
edge(16,7). 	 
edge(16,15). 	 
edge(16,8). 	 
edge(16,22). 	 
edge(16,24). 	 
edge(16,17). 	 
edge(16,19). 	 
edge(16,4). 	 
edge(15,24). 	 
edge(15,8). 	 
edge(15,14). 	 
edge(15,19). 	 
edge(15,29). 	 
edge(15,6). 	 
edge(15,19). 	 
edge(15,16). 	 
edge(15,5). 	 
edge(15,7). 	 
edge(15,28). 	 
edge(15,29). 	 
edge(15,12). 	 
edge(15,25). 	 
edge(15,3). 	 
edge(15,17). 	 
edge(15,2). 	 
edge(15,16). 	 
edge(15,9). 	 
edge(15,9). 	 
edge(15,27). 	 
edge(15,22). 	 
edge(15,4). 	 
edge(15,23). 	 
edge(15,13). 	 
edge(14,13). 	 
edge(14,15). 	 
edge(14,19). 	 
edge(14,6). 	 
edge(14,6). 	 
edge(14,28). 	 
edge(14,26). 	 
edge(14,22). 	 
edge(14,4). 	 
edge(14,16). 	 
edge(14,17). 	 
edge(14,3). 	 
edge(14,19). 	 
edge(14,12). 	 
edge(14,24). 	 
edge(14,5). 	 
edge(14,29). 	 
edge(14,18). 	 
edge(14,16). 	 
edge(14,29). 	 
edge(14,2). 	 
edge(14,9). 	 
edge(14,26). 	 
edge(14,9). 	 
edge(14,23). 	 
edge(13,6). 	 
edge(13,24). 	 
edge(13,18). 	 
edge(13,19). 	 
edge(13,12). 	 
edge(13,16). 	 
edge(13,9). 	 
edge(13,5). 	 
edge(13,23). 	 
edge(13,9). 	 
edge(13,6). 	 
edge(13,26). 	 
edge(13,25). 	 
edge(13,29). 	 
edge(13,7). 	 
edge(13,17). 	 
edge(13,8). 	 
edge(13,2). 	 
edge(13,28). 	 
edge(13,15). 	 
edge(13,4). 	 
edge(13,22). 	 
edge(13,14). 	 
edge(13,29). 	 
edge(12,24). 	 
edge(12,16). 	 
edge(12,19). 	 
edge(12,23). 	 
edge(12,9). 	 
edge(12,6). 	 
edge(12,8). 	 
edge(12,26). 	 
edge(12,26). 	 
edge(12,29). 	 
edge(12,6). 	 
edge(12,4). 	 
edge(12,19). 	 
edge(12,18). 	 
edge(12,15). 	 
edge(12,5). 	 
edge(12,27). 	 
edge(12,28). 	 
edge(12,29). 	 
edge(12,2). 	 
edge(12,14). 	 
edge(12,17). 	 
edge(12,22). 	 
edge(12,3). 	 
edge(11,13). 	 
edge(11,9). 	 
edge(11,26). 	 
edge(11,5). 	 
edge(11,28). 	 
edge(11,25). 	 
edge(11,12). 	 
edge(11,27). 	 
edge(11,19). 	 
edge(11,3). 	 
edge(11,29). 	 
edge(11,24). 	 
edge(11,7). 	 
edge(11,2). 	 
edge(11,17). 	 
edge(11,4). 	 
edge(11,15). 	 
edge(11,18). 	 
edge(11,6). 	 
edge(11,26). 	 
edge(11,22). 	 
edge(11,14). 	 
edge(11,6). 	 
edge(11,8). 	 
edge(11,19). 	 
edge(11,16). 	 
edge(11,23). 	 
edge(10,14). 	 
edge(10,9). 	 
edge(10,15). 	 
edge(10,29). 	 
edge(10,27). 	 
edge(10,18). 	 
edge(10,3). 	 
edge(10,26). 	 
edge(10,19). 	 
edge(10,23). 	 
edge(10,9). 	 
edge(10,12). 	 
edge(10,2). 	 
edge(10,7). 	 
edge(10,17). 	 
edge(10,8). 	 
edge(10,28). 	 
edge(10,29). 	 
edge(10,6). 	 
edge(10,26). 	 
edge(10,4). 	 
edge(10,13). 	 
edge(10,24). 	 
edge(10,16). 	 
edge(10,6). 	 
edge(9,22). 	 
edge(9,19). 	 
edge(9,18). 	 
edge(9,29). 	 
edge(9,29). 	 
edge(9,4). 	 
edge(9,17). 	 
edge(9,5). 	 
edge(9,15). 	 
edge(9,8). 	 
edge(9,19). 	 
edge(9,26). 	 
edge(9,14). 	 
edge(9,12). 	 
edge(9,3). 	 
edge(9,6). 	 
edge(9,6). 	 
edge(9,25). 	 
edge(9,16). 	 
edge(9,7). 	 
edge(9,16). 	 
edge(9,27). 	 
edge(9,23). 	 
edge(9,26). 	 
edge(8,5). 	 
edge(8,9). 	 
edge(8,29). 	 
edge(8,15). 	 
edge(8,19). 	 
edge(8,22). 	 
edge(8,19). 	 
edge(8,18). 	 
edge(8,6). 	 
edge(8,7). 	 
edge(8,17). 	 
edge(8,2). 	 
edge(8,3). 	 
edge(8,13). 	 
edge(8,16). 	 
edge(8,28). 	 
edge(8,6). 	 
edge(8,12). 	 
edge(8,23). 	 
edge(8,4). 	 
edge(8,26). 	 
edge(8,9). 	 
edge(7,9). 	 
edge(7,15). 	 
edge(7,26). 	 
edge(7,22). 	 
edge(7,29). 	 
edge(7,26). 	 
edge(7,29). 	 
edge(7,5). 	 
edge(7,3). 	 
edge(7,18). 	 
edge(7,27). 	 
edge(7,2). 	 
edge(7,16). 	 
edge(7,6). 	 
edge(7,9). 	 
edge(7,23). 	 
edge(7,6). 	 
edge(7,8). 	 
edge(7,4). 	 
edge(7,13). 	 
edge(6,18). 	 
edge(6,16). 	 
edge(6,8). 	 
edge(6,4). 	 
edge(6,27). 	 
edge(6,25). 	 
edge(6,3). 	 
edge(6,29). 	 
edge(6,26). 	 
edge(6,6). 	 
edge(6,22). 	 
edge(6,12). 	 
edge(6,7). 	 
edge(6,13). 	 
edge(6,5). 	 
edge(6,16). 	 
edge(6,15). 	 
edge(6,9). 	 
edge(6,19). 	 
edge(6,23). 	 
edge(6,19). 	 
edge(6,28). 	 
edge(6,14). 	 
edge(6,2). 	 
edge(6,17). 	 
edge(5,24). 	 
edge(5,14). 	 
edge(5,26). 	 
edge(5,7). 	 
edge(5,6). 	 
edge(5,27). 	 
edge(5,19). 	 
edge(5,28). 	 
edge(5,15). 	 
edge(5,4). 	 
edge(5,16). 	 
edge(5,13). 	 
edge(5,22). 	 
edge(5,23). 	 
edge(5,16). 	 
edge(5,26). 	 
edge(5,8). 	 
edge(5,6). 	 
edge(5,9). 	 
edge(5,9). 	 
edge(5,29). 	 
edge(5,18). 	 
edge(5,19). 	 
edge(5,3). 	 
edge(5,29). 	 
edge(5,17). 	 
edge(5,12). 	 
edge(4,6). 	 
edge(4,7). 	 
edge(4,17). 	 
edge(4,12). 	 
edge(4,18). 	 
edge(4,19). 	 
edge(4,27). 	 
edge(4,16). 	 
edge(4,26). 	 
edge(4,29). 	 
edge(4,19). 	 
edge(4,3). 	 
edge(4,6). 	 
edge(4,25). 	 
edge(4,26). 	 
edge(4,5). 	 
edge(4,24). 	 
edge(4,13). 	 
edge(4,9). 	 
edge(4,16). 	 
edge(4,23). 	 
edge(4,9). 	 
edge(4,22). 	 
edge(4,14). 	 
edge(4,8). 	 
edge(3,16). 	 
edge(3,9). 	 
edge(3,8). 	 
edge(3,6). 	 
edge(3,7). 	 
edge(3,14). 	 
edge(3,28). 	 
edge(3,29). 	 
edge(3,5). 	 
edge(3,17). 	 
edge(3,25). 	 
edge(3,26). 	 
edge(3,22). 	 
edge(3,29). 	 
edge(3,26). 	 
edge(3,12). 	 
edge(3,19). 	 
edge(3,9). 	 
edge(3,4). 	 
edge(3,27). 	 
edge(3,16). 	 
edge(3,6). 	 
edge(3,13). 	 
edge(3,19). 	 
edge(3,23). 	 
edge(3,24). 	 
edge(3,18). 	 
edge(3,2). 	 
edge(2,16). 	 
edge(2,6). 	 
edge(2,15). 	 
edge(2,24). 	 
edge(2,5). 	 
edge(2,13). 	 
edge(2,19). 	 
edge(2,4). 	 
edge(2,8). 	 
edge(2,6). 	 
edge(2,14). 	 
edge(2,19). 	 
edge(2,28). 	 
edge(2,16). 	 
edge(2,22). 	 
edge(2,23). 	 
edge(2,27). 	 
edge(2,9). 	 
edge(2,29). 	 
edge(2,12). 	 
edge(2,3). 	 
edge(2,25). 	 
edge(2,18). 	 
edge(2,7). 	 
edge(2,26). 	 
edge(1,2). 	 
edge(1,25). 	 
edge(1,22). 	 
edge(1,14). 	 
edge(1,13). 	 
edge(1,28). 	 
edge(1,24). 	 
edge(1,19). 	 
edge(1,3). 	 
edge(1,8). 	 
edge(1,18). 	 
edge(1,4). 	 
edge(1,7). 	 
edge(1,16). 	 
edge(1,26). 	 
edge(1,15). 	 
edge(1,9). 	 
edge(1,19). 	 
edge(1,6). 	 
edge(1,29). 	 
edge(1,5). 	 
edge(1,27). 	 
edge(1,29). 	 

 %% edge(30,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa .
 %% edge(30,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 1.
 %% edge(30,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa .
 %% edge(30,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa .
 %% edge(30,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(30,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa .
 %% edge(30,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(30,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa .
 %% edge(30,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(30,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa .
 %% edge(30,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(30,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(30,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(30,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(30,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(30,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(30,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(30,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(30,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(30,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(30,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(30,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(30,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(30,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(30,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(30,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(29,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(29,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(29,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(29,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(29,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(29,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(29,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(29,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(29,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(29,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(29,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(29,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(29,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(29,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(29,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(29,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(29,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(29,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(29,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(29,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(29,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(29,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(29,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(29,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(28,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(28,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(28,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(28,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(28,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(28,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(28,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(28,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(28,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(28,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(28,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(28,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(28,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(28,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(28,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(28,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(28,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(28,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(28,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(28,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(28,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(28,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(28,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(28,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(28,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(27,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(27,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(27,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(27,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(27,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(27,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(27,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(27,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(27,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(27,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(27,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(27,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(27,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(27,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(27,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(27,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(27,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(27,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(27,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(27,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(27,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(27,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(27,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(27,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(27,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(27,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(27,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(26,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(26,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(26,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(26,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(26,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(26,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(26,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(26,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(26,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(26,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(26,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(26,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(26,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(26,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(26,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(26,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(26,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(26,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(26,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(26,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(26,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(26,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(26,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(26,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(26,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(25,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(25,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(25,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(25,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(25,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(25,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(25,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(25,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(25,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(25,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(25,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(25,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(25,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(25,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(25,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(25,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(25,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(25,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(25,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(25,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(25,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(25,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(25,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(25,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(25,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(25,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(25,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(24,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(24,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(24,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(24,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(24,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(24,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(24,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(24,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(24,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(24,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(24,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(24,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(24,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(24,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(24,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(24,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(24,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(24,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(24,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(24,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(24,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(23,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(23,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(23,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(23,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(23,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(23,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(23,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(23,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(23,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(23,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(23,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(23,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(23,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(23,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(23,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(23,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(23,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(23,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(23,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(23,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(23,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(23,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(23,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(23,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(22,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(22,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(22,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(22,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(22,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(22,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(22,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(22,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(22,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(22,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(22,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(22,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(22,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(22,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(22,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(22,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(22,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(22,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(22,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(22,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(22,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(22,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(22,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(21,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(21,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(21,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(21,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(21,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(21,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(21,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(21,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(21,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(21,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(21,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(21,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(21,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(21,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(21,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(21,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(21,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(21,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(21,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(21,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(21,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(21,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(21,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(21,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(21,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(21,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(20,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(20,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(20,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(20,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(20,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(20,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(20,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(20,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(20,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(20,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(20,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(20,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(20,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(20,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(20,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(20,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(20,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(20,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(20,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(20,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(20,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(20,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(20,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(20,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(20,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(19,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(19,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(19,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(19,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(19,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(19,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(19,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(19,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(19,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(19,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(19,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(19,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(19,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(19,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(19,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(19,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(19,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(19,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(19,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(19,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(19,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(19,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(19,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(19,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(19,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(18,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(18,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(18,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(18,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(18,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(18,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(18,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(18,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(18,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(18,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(18,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(18,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(18,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(18,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(18,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(18,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(18,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(18,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(18,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(18,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(18,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(18,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(18,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(17,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(17,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(17,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(17,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(17,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(17,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(17,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(17,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(17,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(17,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(17,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(17,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(17,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(17,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(17,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(17,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(17,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(17,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(17,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(17,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(17,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(17,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(17,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(17,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(17,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(17,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(17,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(17,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(16,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(16,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(16,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(16,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(16,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(16,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(16,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(16,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(16,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(16,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(16,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(16,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(16,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(16,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(16,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(16,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(16,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(16,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(16,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(16,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(16,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(16,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(16,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(16,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(15,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(15,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(15,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(15,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(15,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(15,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(15,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(15,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(15,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(15,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(15,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(15,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(15,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(15,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(15,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(15,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(15,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(15,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(15,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(15,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(15,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(15,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(15,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(15,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(15,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(14,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(14,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(14,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(14,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(14,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(14,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(14,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(14,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(14,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(14,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(14,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(14,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(14,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(14,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(14,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(14,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(14,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(14,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(14,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(14,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(14,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(14,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(14,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(14,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(14,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(13,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(13,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(13,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(13,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(13,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(13,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(13,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(13,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(13,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(13,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(13,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(13,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(13,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(13,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(13,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(13,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(13,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(13,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(13,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(13,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(13,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(13,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(13,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(13,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(12,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(12,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(12,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(12,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(12,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(12,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(12,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(12,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(12,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(12,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(12,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(12,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(12,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(12,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(12,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(12,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(12,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(12,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(12,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(12,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(12,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(12,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(12,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(12,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(11,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(11,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(11,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(11,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(11,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(11,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(11,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(11,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(11,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(11,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(11,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(11,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(11,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(11,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(11,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(11,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(11,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(11,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(11,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(11,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(11,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(11,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(11,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(11,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(11,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(11,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(11,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(10,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(10,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(10,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(10,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(10,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(10,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(10,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(10,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(10,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(10,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(10,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(10,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(10,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(10,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(10,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(10,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(10,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(10,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(10,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(10,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(10,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(10,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(10,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(10,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(10,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(9,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(9,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(9,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(9,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(9,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(9,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(9,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(9,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(9,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(9,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(9,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(9,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(9,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(9,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(9,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(9,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(9,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(9,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(9,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(9,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(9,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(9,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(9,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(9,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(8,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(8,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(8,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(8,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(8,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(8,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(8,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(8,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(8,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(8,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(8,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(8,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(8,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(8,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(8,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(8,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(8,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(8,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(8,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(8,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(8,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(8,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(7,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(7,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(7,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(7,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(7,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(7,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(7,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(7,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(7,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(7,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(7,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(7,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(7,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(7,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(7,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(7,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(7,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(7,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(7,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(7,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(6,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(6,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(6,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(6,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(6,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(6,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(6,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(6,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(6,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(6,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(6,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(6,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(6,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(6,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(6,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(6,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(6,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(6,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(6,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(6,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(6,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(6,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(6,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(6,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(6,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(5,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(5,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(5,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(5,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(5,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(5,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(5,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(5,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(5,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(5,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(5,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(5,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(5,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(5,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(5,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(5,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(5,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(5,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(5,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(5,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(5,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(5,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(5,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(5,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(5,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(5,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(5,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(4,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(4,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(4,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(4,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(4,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(4,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(4,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(4,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(4,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(4,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(4,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(4,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(4,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(4,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(4,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(4,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(4,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(4,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(4,9,Xa,Xb) :- 	 
 %%  	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(4,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(4,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(4,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(4,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(4,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(4,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(3,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(3,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(3,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(3,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(3,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(3,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(3,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(3,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(3,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(3,17,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(3,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(3,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(3,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(3,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(3,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(3,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(3,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(3,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(3,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(3,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(3,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(3,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(3,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(3,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(3,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(3,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(3,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(3,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(2,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(2,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(2,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(2,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(2,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(2,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(2,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(2,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(2,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(2,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(2,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(2,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(2,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(2,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(2,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(2,23,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(2,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(2,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(2,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(2,12,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(2,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(2,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(2,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(2,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(2,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(1,2,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(1,25,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(1,22,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(1,14,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(1,13,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(1,28,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(1,24,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(1,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(1,3,Xa,Xb) :- 	 
 %% 	Xa .<. 10, 
 %% 	Xb = Xa.
 %% edge(1,8,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(1,18,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(1,4,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. 1 + Xa.
 %% edge(1,7,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(1,16,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb .=. Xa - 1.
 %% edge(1,26,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(1,15,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(1,9,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(1,19,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(1,6,Xa,Xb) :- 	 
 %% 	Xa .>. 3, 
 %% 	Xb = Xa.
 %% edge(1,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa - 1.
 %% edge(1,5,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
 %% edge(1,27,Xa,Xb) :- 	 
 %% 	Xa .<. 7, 
 %% 	Xb = Xa.
 %% edge(1,29,Xa,Xb) :- 	 
 %% 	Xa .>. 0, 
 %% 	Xb .=. Xa + 3.
