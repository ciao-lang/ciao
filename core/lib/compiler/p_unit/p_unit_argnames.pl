:- package(p_unit_argnames).

% Argnames for some p_unit structures

:- use_package(argnames).

:- argnames as(module, status, type, head, compat, 
           call, succ, comp,
           dic, locator, comment, fromwhere).
% fromwhere = read, asserted

% :- regtype as(Module, Status, Type, Head, Compat, Call, Succ, Comp, Dic, 
%               Locator, Comment, Fromwhere)
% #
% "
% @var{Module} module where the assertion is defined.
% @var{Status} is the assertion status (check, checkd, true, false...).
% @var{Type} is the assertion type (call,success,entry,exi).
% @var{Call} is the call field.
% @var{Succ} is the success field.
% @var{Comp} is the computation field.
% @var{Dic}  is the assertion dictionary.
% @var{locator} is the assertion locator.
% @var{comment} is the assertion comment.
% @var{fromwhere}. Ignore this field (can take the values: read,
% asserted, commented), but maybe will dissapear in future
% implementations.  ".
