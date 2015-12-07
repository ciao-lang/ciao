:- package(xml_path).

:- use_module(library(xml_path/xml_path_rt)).
:- include(library(xml_path/xml_path_syntax)).

:- load_compilation_module(library(xml_path/xml_path_tr)).
:- add_goal_trans(xml_path_tr:xml_path_tr/2, 750).
