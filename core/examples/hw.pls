#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-

main(_) :- 
	write('Hello world!'), nl, nl,
	write('Hit return to proceed... '), 
	flush_output,
	get_code(_).


