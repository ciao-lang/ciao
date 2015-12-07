:- use_package(assertions).

%% Prevent documenter from doing anything with previous package
:- doc(nodoc, assertions).

%% File type not strictly needed (package is default for non-module
%% files), but just in case things change 
:- doc(filetype, package).

:- doc(title,"Pattern (regular expression) matching").

:- doc(author,"The CLIP Group").

:- op(200, fy, (=~)).   % Like unary '+'

:- use_module(library(regexp/regexp_code)).

:- doc(doinclude, [
        match_shell/3,
        match_shell/2,
        match_posix/2,
        match_posix/4,
        match_posix_rest/3,
        match_posix_matches/3,
        match_struct/4,
        match_pred/2,
        replace_first/4,
        replace_all/4
                      ]).

:- doc(module,"This library provides facilities for matching
   strings and terms against @index{patterns}. There are some @concept{prolog flags}
  @begin{itemize}

   @item There is a @concept{prolog flag} to case insensitive
   match. Its name is case_insensitive. If its value is on, matching is
   case insenseitive, but if its value is off matching isn't case
   insensitive. By default, its value is off.

   @item There is a syntax facility to use matching more or less like
   a unification. You can type, \" =~ \"regexp\" \" as an argument of
   a predicate. Thus, that argument must match with regexp. For example:
 
 @begin{verbatim}
          pred ( =~ \"ab*c\", B) :- ...    
 @end{verbatim}

is equivalent to

 @begin{verbatim}
          pred (X,B) :- match_posix(\"ab*c\",X,R), ...
 @end{verbatim}

    So, there are two @concept{prolog flags} about this. One of this
    @concept{prolog flags} is \"format\". Its values are shell, posix,
    list and pred, and sustitute in the example match_posix by
    match_shell, match_posix, match_struct and macth_pred respectivly. By
    default its value is posix.  The other @concept{prolog flag} is
    exact. Its values are on and off. If its value is off sustitute in the
    example R by []. If its value is on, R is a variable. By default, its
    value is on.  

@end{itemize}").

:- doc(regexp_shell/1,
        "Special characters for @var{regexp_shell} are:
  @begin{description}
  @item{*} Matches any string that not begin by the character '.'.
  @item{?} Matches any single character whenever it is not a starting point.
  @item{[...]} Matches any one of the enclosed characters.  A pair of
   characters separated by a minus sign denotes a range; any character
   lexically between those two characters, inclusive, is matched.  If the
   first character following the [ is a ^ then any character not enclosed
   is matched.  No other character is special inside this construct.  To
   include a ] in a character set, you must make it the first character.
   To include a `-', you must use it in a context where it cannot possibly
   indicate a range: that is, as the first character, or immediately after
   a range.
  @item{|} Specifies an alternative.  Two @var{regexp_shell} A and B with
   | in between form an expression that matches anything that either A or B
   will match.
  @item{@{...@}} Groups alternatives inside larger @var{regexp_shell}.
  @item{\\} Quotes a special character (including itself).
 @end{description}").


