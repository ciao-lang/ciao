:- module(streams, [
        open_null_stream/1,
        open_input/2, close_input/1, open_output/2, close_output/1
        ],[assertions]).

:- use_module(engine(internals)).
:- use_module(engine(streams_basic), [stream/1]).

:- doc(title,"Structured stream handling").

:- pred open_null_stream(S) => stream(S).

open_null_stream(S) :-
	'$open'('/dev/null', w, S).

:- pred open_input(FileName,InputStreams)
         : sourcename(FileName)
        => input_handler(InputStreams).

open_input(FileName, i(OldInput, NewInput)) :-
        current_input(OldInput),
        open(FileName, read, NewInput),
        set_input(NewInput).

:- pred close_input(InputStreams)
         : input_handler(InputStreams)
        => input_handler(InputStreams).

close_input(i(OldInput, NewInput)) :- !,
        set_input(OldInput),
        close(NewInput).
close_input(X) :-
        throw(error(domain_error(open_input_handler, X), close_input/1-1)).

:- pred open_output(FileName,OutputStreams)
         : sourcename(FileName)
        => output_handler(OutputStreams).

open_output(FileName, o(OldOutput, NewOutput)) :-
        current_output(OldOutput),
        open(FileName, write, NewOutput),
        set_output(NewOutput).

:- pred close_output(OutputStreams)
         : output_handler(OutputStreams)
        => output_handler(OutputStreams).

close_output(o(OldOutput, NewOutput)) :- !,
        set_output(OldOutput),
        close(NewOutput).
close_output(X) :-
        throw(error(domain_error(open_output_handler, X), close_output/1-1)).

:- prop input_handler/1 + regtype.

input_handler(i(Old,New)):-
	stream(Old),
	stream(New).

:- prop output_handler/1 + regtype.

output_handler(o(Old,New)):-
	stream(Old),
	stream(New).


