:- module(register_in_script, [], [assertions, regtypes, dcg, fsyntax, basicmodes]).

:- use_module(library(system), [file_exists/1, time/1]).
:- use_module(library(system_extra), [backup_file/1]).
:- use_module(library(file_utils), [file_to_string/2, string_to_file/2]).
:- use_module(library(lists), [length/2, append/3, reverse/2]).

:- doc(title, "Register/unregister Code in Scripts (for bash, csh, emacs, etc.)").
:- doc(author, "Jose F. Morales").
:- doc(author, "CLIP Group").

% TODO: We could want a special Key in the register command, so that
%       different tools can register themselves? Or we can just add a
%       reference to some Ciao code and register code there.

:- export(register_in_script/3).
:- pred register_in_script(File, CommentMark, Code)

   # "Inserts @var{Code} in the script file @var{File}. Code is
     surrounded by special comments (starting with @var{CommentMark}),
     that allow identifying the inserted code later. If the @var{File}
     contained some inserted code, it is replaced.".

register_in_script(File, CommentMark, Code) :-
	( file_exists(File) ->
	    file_to_string(File, String)
	; String = []
	),
	( script_with_marked_code(CommentMark, Key0, Before0, _, After0, String) ->
	    Key = Key0, Before = Before0, After = After0
	; Key = ~unique_key_string, Before = String, After = []
	),
	script_with_marked_code(CommentMark, Key, Before, Code, After, NewString),
	backup_and_write(NewString, File).

% A 'unique' key string
% TODO: We do nothing to ensure that it is unique
unique_key_string := Value :-
	Min = 10000000,
	Max = 99999999,
	time(Time),
	Value0 is Min + (Time mod (Max - Min + 1)),
	Value = ~number_codes(Value0).

:- export(unregister_from_script/2).
unregister_from_script(File, CommentMark) :-
	( file_exists(File) ->
	    file_to_string(File, String),
	    ( script_with_marked_code(CommentMark, _Key, Before, _, After, String) ->
	        NewString = ~append(Before, After),
	        backup_and_write(NewString, File)
	    ; true
	    )
	; true
	).

% (decompose S in Before+SurroundedCode+After)
script_with_marked_code(CommentMark, Key, Before, Code, After, S) :-
	emit_string(Before, S, S0),
	( Before = [] -> FirstLine = yes ; FirstLine = no ),
	surrounded_code(FirstLine, CommentMark, Key, Code, S0, After),
	!.

% Usage:
%   (unbound string: emit)
%   (bound string: extract Key and Code)
surrounded_code(FirstLine, CommentMark, Key, Code) -->
	begin_mark(FirstLine, CommentMark, Key),
	emit_string(Code),
	end_mark(CommentMark, Key).

begin_mark(FirstLine, CommentMark, Key) -->
	% @tt{\\n} is not necessary in the first line
	( { FirstLine = yes } -> [] ; "\n" ),
	emit_string(CommentMark), " @begin(", emit_key(Key),
	")@ - Do not edit these lines - added automatically!\n".

end_mark(CommentMark, Key) -->
	emit_string(CommentMark), " @end(", emit_key(Key),
	")@ - End of automatically added lines.\n".

% (this can either recognize or emit a key)
emit_key("") --> "".
emit_key([D|H]) --> digit(D), emit_key(H).

digit(A, [A|S], S) :- A >= 0'0, A =< 0'9.

emit_string(Word, S, T) :-
	append(Word, T, S).

% ----------------------------------------------------------------------------

backup_and_write(String, FileName) :-
	backup_file(FileName),
	string_to_file(String, FileName).

