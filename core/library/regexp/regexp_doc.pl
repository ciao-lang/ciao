:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).
:- doc(filetype, package).

:- doc(title,"Pattern (regular expression) matching").

:- doc(author,"The Ciao Development Team").

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

:- doc(module,"This library provides facilities for matching strings
   and terms against @index{patterns}. Some @concept{prolog
   flags} are available to control its behavior.

   @begin{itemize}

   @item The flag @tt{@concept{case_insensitive}} controls whether to
   perform case-insensitive (@tt{on}) or case-sensitive (@tt{off})
   match. The default value is @tt{off}.

   @item There is a syntax facility that allows using this regular
   expression matching in a similar way to unification. You can use 
   @tt{=~ \"regexp\"} as an argument of a predicate, and then that
   argument must match with the regexp. For example: 
@begin{verbatim}
   pred ( =~ \"ab*c\", B) :- ...  
@end{verbatim}

is equivalent to

 @begin{verbatim}
      pred (X,B) :- match_posix(\"ab*c\",X,R), ...
 @end{verbatim}

    Two additional flags control this matching. The first one is
    @tt{@concept{format}}. Its values are @tt{shell}, @tt{posix},
    @tt{list} and @tt{pred}.  Their effect is as if changing in the
    example above the call to @pred{match_posix/3} by a call to,
    respectively, @tt{match_shell/2}, @tt{match_posix/3},
    @tt{match_struct/3}, and @tt{match_pred/3}.  The default value is
    @tt{posix}.  The other prolog flag is @tt{@concept{exact}}. Its
    values are @tt{on} and @tt{off}. The @tt{off} value means
    replacing in the example @tt{R} with @tt{[]}. If the value is
    @tt{on}, then @tt{R} is a variable. The default value is @tt{on}.

@end{itemize}").

:- doc(regexp_shell/1,
    "Special characters for @var{regexp_shell} are:
  @begin{description}

  @item{@tt{*}} Matches any string that not begin with the character '@tt{.}'.

  @item{@tt{?}} Matches any single character whenever it is not a starting point.

  @item{@tt{[}...@tt{]}} Matches any one of the enclosed characters.
     A pair of characters separated by a minus sign denotes a range;
     any character lexically between those two characters, inclusive,
     is matched.  If the first character following the @tt{[} is a
     @tt{^} then any character not enclosed is matched.  No other
     character is special inside this construct.  To include a @tt{]}
     in a character set, you must make it the first character.  To
     include a @tt{-}, you must use it in a context where it cannot
     possibly indicate a range: that is, as the first character, or
     immediately after a range.

  @item{@tt{|}} Specifies an alternative.  Two regexps @tt{A} and
     @tt{B} with @tt{|} in between form an expression that matches
     anything that either @tt{A} or @tt{B} matches.

  @item{@{...@}} Groups alternatives inside larger @em{regexp_shell}s.

  @item{@tt{\\\\}} Quotes a special character (including itself).

  @end{description}").


