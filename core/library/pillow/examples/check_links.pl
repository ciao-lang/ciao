:- module(check_links, [main/1,check_links/2],[]).
:- use_package(pillow).

:- use_module(library(format)).

main([URL]) :- !,
        check_links(URL,BadLinks),
        report_bad_links(BadLinks).
main(_) :-
        error(['Usage: check_links <URL>']).

check_links(URL,BadLinks) :-
        url_info(URL,URLInfo),
        fetch_url(URLInfo,[],Response),
        member(content_type(text,html,_),Response),
        member(content(Content),Response),
        html2terms(Content,Terms),
        check_source_links(Terms,URLInfo,[],BadLinks).

check_source_links([],_,BL,BL).
check_source_links([E|Es],BaseURL,BL0,BL) :-
        check_source_links1(E,BaseURL,BL0,BL1),
        check_source_links(Es,BaseURL,BL1,BL).

check_source_links1(env(a,AnchorAtts,_),BaseURL,BL0,BL) :-
        member((href=URL),AnchorAtts), !,
        check_link(URL,BaseURL,BL0,BL).
check_source_links1(env(_Name,_Atts,Env_html),BaseURL,BL0,BL) :- !,
        check_source_links(Env_html,BaseURL,BL0,BL).
check_source_links1(_,_,BL,BL).

check_link(URL,BaseURL,BL0,BL) :-
	format("~s", [URL]),
        url_info_relative(URL,BaseURL,URLInfo), !,
        fetch_url_status(URLInfo,Status,Phrase),
        ( Status \== success ->
	    format(" [Error]\n", []),
	    atom_codes(P,Phrase),
	    atom_codes(U,URL),
	    BL = [badlink(U,P)|BL0]
        ; format(" [OK]\n", []), BL = BL0
        ).
check_link(_,_,BL,BL).

fetch_url_status(URL,Status,Phrase) :-
        fetch_url(URL,[head,timeout(20)],Response), !,
        member(status(Status,_,Phrase),Response).
fetch_url_status(_,timeout,timeout).

report_bad_links([]).
report_bad_links([badlink(U,P)|BLs]) :-
        message(['URL ',U,' : ',P]),
        report_bad_links(BLs).
