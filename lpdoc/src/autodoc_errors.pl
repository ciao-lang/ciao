:- module(autodoc_errors,
	[error_text/3],
	[assertions, regtypes, basicmodes]).

:- doc(title, "Error Messages").
:- doc(author, "Manuel Hermenegildo").

error_text(error, error, "could not rewrite command ~w into ~w format").
error_text(unrecognizedcmd, warning,
	    "Unrecognized command in string (passed on): @~w{~s}").
error_text(docstring, error, "While parsing docstring:~n * HERE *~n~s").
error_text(handle,    error, "Could not process @~w command").
error_text(cannot_read, error, "Could not read ~w").
error_text(rewrite, error,
	    "~w backend failed while parsing/processing the string: ``~s''").
error_text(tryinclude, error, "~w not found in program text").
error_text(aritynot1, warning,
	    "Arity different from 1 -- will take first argument").
error_text(nobegin, error, "@end{~w} with no begin command").
error_text(noend, error, "@begin{~w} not closed").
error_text(wrongend, error, "@begin{~w} closed with @end{~w}").

