:- package(attr).

% Deactivated until we find better solution.
% % The package assertions is loaded to hide within the documentation of
% % the module using attr the following multifile predicates:
% %   - 'attr_rt:unify_hook'/3. 
% %   - 'attr_rt:portray_hook'/3.
% %   - 'attr_rt:attribute_goals'/4.
% :- use_package(assertions).

:- use_module(library(attr/attr_rt), [get_attr_local/2, put_attr_local/2, del_attr_local/1, attvar/1]).

:- load_compilation_module(library(attr/attr_tr)).
:- add_sentence_trans(attr_tr:sentence/3, 910).


