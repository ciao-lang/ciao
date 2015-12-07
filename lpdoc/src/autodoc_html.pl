:- module(autodoc_html, [], [assertions, regtypes, fsyntax]).
% (Nothing is exported, because everything works using hooks)

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_structure)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_index)).
:- use_module(lpdoc(autodoc_refsdb)).
:- use_module(lpdoc(autodoc_images)).
:- use_module(lpdoc(autodoc_settings)).
:- use_module(library(lists), [append/3, list_concat/2]).
:- use_module(library(dict)).
:- use_module(lpdoc(comments), [stringcommand/1]).
:- use_module(library(format_to_string), [format_to_string/3]).

% (Web-site extensions)
:- use_module(lpdoc(autodoc_html_template)).
:- use_module(lpdoc(pbundle_download)).

:- doc(title, "HTML Backend").
:- doc(author, "Jose F. Morales").

% ======================================================================

:- multifile autodoc_escape_string_hook/5.

autodoc_escape_string_hook(html, _InputType, NS, _DocSt, VS) :- !,
	html_escape(NS, VS).

% ======================================================================

:- multifile autodoc_rw_command_hook/4.

:- pred autodoc_rw_command_hook(Backend, DocSt, Command, NewCommand)
	: backend_id * docstate * doctree * doctree.

autodoc_rw_command_hook(html, DocSt, Command, NewCommand) :- !,
	rw_command(Command, DocSt, NewCommand).

% ......................................................................

rw_command(ref_link(Link, Text), DocSt, R) :- !,
	fmt_link(default, no_label, Link, DocSt, string_esc(Text), R).
rw_command(missing_link(Text), DocSt, R) :- !,
	fmt_link(missing, no_label, no_link, DocSt, string_esc(Text), R).
rw_command(cite_link(Link, Text), DocSt, R) :- !,
	fmt_link(default, no_label, Link, DocSt, string_esc(Text), R).
% TODO: 'sp' replaced by just 'p', which yield better documents
rw_command(sp(_), _, R) :- !, R = raw("<p>").
%rw_command(sp(NS), _, R) :- !,
%	number_codes(N, NS),
%	N1 is N+1,
%	html_blank_lines(N1, NewCommand),
%	R = raw(NewCommand).
rw_command(p(""),                _, raw("<p>")) :- !.
rw_command(mathenv(S),           _, R) :- !,
	% environment using MathJax (in-line formula)
	R = htmlenv(script, [type="math/tex"], raw(S)).
rw_command(mathenv(display,S),   _, R) :- !,
	% environment using MathJax (long formula)
	R = htmlenv(script, [type="math/tex; mode=display"], raw(S)).
rw_command(defmathcmd_(Cmd,"0",Def), _, R) :- !,
	% new math command (MathJax)
	R = htmlenv(script, [type="math/tex"], [
              raw("\\newcommand{"), raw(Cmd), raw("}{"), raw(Def), raw("}")
            ]).
rw_command(defmathcmd_(Cmd,N,Def), _, R) :- !,
	% new math command (MathJax)
	R = htmlenv(script, [type="math/tex"], [
              raw("\\newcommand{"), raw(Cmd), raw("}["), raw(N), raw("]{"), raw(Def), raw("}")
            ]).
rw_command(noindent(""),           _, nop) :- !.
rw_command(newblock(""),           _, [raw("<br/>")]) :- !. % TODO: remove? just for bibrefs
rw_command(env_('itemize', X),     _, htmlenv(ul, X)) :- !.
rw_command(env_('enumerate', X),   _, htmlenv(ol, X)) :- !.
rw_command(env_('description', X), _, htmlenv(dl, X)) :- !.
rw_command(env_('cartouche', X),   _, cartouche(X)) :- !.
rw_command(env_('alert', X), _, alert(X)) :- !.
rw_command(env_('verbatim', X),     _, htmlenv(pre, X)) :- !.
rw_command(item(S), _DocSt, NBody) :- !, % (items for lists and descriptions)
	( doctree_is_empty(S) ->
	    NBody = raw("<LI>")
	; NBody = [raw("<DT>"), S, raw("<dd>")]
	).
rw_command(item_num(S), _,   NBody) :- !, % (items for enumerations)
	( S = "" -> Props = [] ; Props = [value=S] ),
	NBody = htmlenv0(li, Props).
rw_command(footnote(Text), _DocSt, NBody) :- !,
	NBody = [raw("<P>"), htmlenv(b, raw("Note:")), raw(" "), Text, raw("<P>")].
rw_command('}',                   _, raw("}")) :- !.
rw_command('{',                   _, raw("{")) :- !.
rw_command('@',                   _, raw("@")) :- !.
rw_command(today(""),             _, R) :- !, fmt_date(R).
rw_command(hfill(""),             _, raw("")) :- !.
rw_command('`'([X]),              _, raw("&"||([X|"grave;"]))) :- !.
rw_command(''''([X]),             _, raw("&"||([X|"acute;"]))) :- !.
% NOTE: Escaped ^ due to fsyntax!
rw_command(^'^'([X]),              _, raw("&"||([X|"circ;"]))) :- !.
rw_command('..'([X]),             _, raw("&"||([X|"uml;"]))) :-	!.
rw_command('"'([X]),              _, raw("&"||([X|"uml;"]))) :-	!.
% NOTE: Escaped ~ due to fsyntax!
rw_command(^'~'([X]),              _, raw("&"||([X|"tilde;"]))) :- !.
% TODO: support for accented characters is incomplete in HTML (fix missing)
rw_command('='([X]),              _, raw([X])) :- !.
rw_command('.'([X]),              _, raw([X])) :- !.
rw_command('u'([X]),              _, raw([X])) :- !.
rw_command('v'([X]),              _, raw([X])) :- !.
rw_command('H'([X]),              _, raw([X])) :- !.
rw_command('t'([X, Y]),           _, raw([X, Y])) :- !.
rw_command('c'([X]),              _, raw([X])) :- !.
rw_command('d'([X]),              _, raw([X])) :- !.
rw_command('b'([X]),              _, raw([X])) :- !.
rw_command('oe'(""),              _, raw("&oelig;")) :- !.
rw_command('OE'(""),              _, raw("&OElig;")) :- !.
rw_command('ae'(""),              _, raw("&aelig;")) :- !.
rw_command('AE'(""),              _, raw("&AElig;")) :- !.
rw_command('aa'(""),              _, raw("&aring;")) :- !.
rw_command('AA'(""),              _, raw("&Aring;")) :- !.
rw_command('o'(""),               _, raw("&oslash;")) :- !.
rw_command('O'(""),               _, raw("&Oslash;")) :- !.
rw_command('l'(""),               _, raw("l")) :- !.
rw_command('L'(""),               _, raw("L")) :- !.
rw_command('ss'(""),              _, raw("&szlig;")) :- !.
rw_command('?'(""),               _, raw("&iquest;")) :- !.
rw_command('!'(""),               _, raw("&iexcl;")) :- !.
rw_command('i'(""),               _, raw("i")) :- !.
rw_command('j'(""),               _, raw("j")) :- !.
rw_command(copyright(""),         _, raw("&#169;")) :- !.
rw_command(iso(""), _, htmlenv(span, [class="iso"], [raw("ISO")])) :- !.
rw_command(bullet(""), _,       raw("&#186;")) :- !.
rw_command(result(""), _,       raw("&rArr;")) :- !. % =>
rw_command(href(URL), _DocSt, NBody) :- !,
	NBody = htmlenv(a, [href=URL], [raw(URL)]).
rw_command(href(URL, Text), _DocSt, NBody) :- !,
	NBody = htmlenv(a, [href=URL], Text).
rw_command(email(Address), _DocSt, NBody) :- !,
	NBody = [raw("<A HREF=""mailto:"), Address, raw(""">&lt;"), Address, raw("&gt;</A>")].
rw_command(email(Text, Address), _DocSt, NBody) :- !,
	NBody = [raw("<A HREF=""mailto:"), Address, raw(""">"), Text, raw("</A>")].
rw_command(image_auto(IFile0, Opts), DocSt, NBody) :- !,
	locate_and_convert_image(IFile0, ['png', 'jpg'], DocSt, IFile),
	( Opts = [] ->
	    NBody = [raw("<IMG SRC="""), raw(IFile), raw(""">")]
	; Opts = [Width, Height] ->
	    NBody = [raw("<IMG SRC="""), raw(IFile), raw(""" WIDTH="), raw(Width),
		     raw(" HEIGHT="), raw(Height), raw(">")]
        ).
rw_command(bf(Body),  _DocSt, R) :- !, R = htmlenv(strong, Body).
rw_command(em(Body),  _DocSt, R) :- !, R = htmlenv(em, Body).
rw_command(tt(Body),  _DocSt, R) :- !, R = htmlenv(tt, Body).
rw_command(key(Body), _DocSt, R) :- !, R = htmlenv(span, [class="emacskey"], Body).
rw_command(var(Body), _DocSt, R) :- !, R = htmlenv(span, [class="var"], Body).
% .......... (plug-in commands) ..........
% TODO: Move to external module/
rw_command(html_template(FileC), _DocSt, R) :- !,
	atom_codes(File, FileC),
	fmt_html_template(File, [], R).
rw_command(html_template_internal(File, Params), _DocSt, R) :- !,
	fmt_html_template(File, Params, R).
rw_command(pbundle_download(BranchC, ViewC), _DocSt, R) :- !,
	% TODO: Define the language of Branch (it is a the branch name,
	%       or relative subdirectory in the repository,
	%       e.g. trunk, branches/1.14, etc.; but it could be
	%       richer and specify the revision, etc.)
	atom_codes(Branch, BranchC),
	atom_codes(View, ViewC),
	fmt_pbundle_download(Branch, View, R).
rw_command(pbundle_href(BranchC, Manual, RelC, Text), _DocSt, R) :- !,
	% TODO: Define the language of Branch (it is a the branch name,
	%       or relative subdirectory in the repository,
	%       e.g. trunk, branches/1.14, etc.; but it could be
	%       richer and specify the revision, etc.)
	atom_codes(Branch, BranchC),
	atom_codes(Rel, RelC),
	fmt_pbundle_href(Branch, Manual, Rel, Text, R).
% .......... (icmd) ..........
% TODO: Share common definitions with autodoc_texinfo
rw_command(htmlenv(Cmd, Body), _, NewCommand) :- !, % <cmd>BODY</cmd>
	atom_codes(Cmd, CmdS),
	NewCommand = [raw("<"), raw(CmdS), raw(">"), Body, raw("</"), raw(CmdS), raw(">")].
rw_command(htmlenv(Cmd, Props, Body), DocSt, NewCommand) :- !, % <cmd PROPS>BODY</cmd>
	atom_codes(Cmd, CmdS),
	fmt_html_props(Props, DocSt, PropsR),
	NewCommand = [raw("<"), raw(CmdS), raw(" "), PropsR, raw(">"), Body, raw("</"), raw(CmdS), raw(">")].
rw_command(htmlenv1(Cmd), _, NewCommand) :- !, % <cmd/>
	atom_codes(Cmd, CmdS),
	NewCommand = [raw("<"), raw(CmdS), raw("/>")].
rw_command(htmlenv1(Cmd, Props), DocSt, NewCommand) :- !, % <cmd PROPS/>
	atom_codes(Cmd, CmdS),
	fmt_html_props(Props, DocSt, PropsR),
	NewCommand = [raw("<"), raw(CmdS), raw(" "), PropsR, raw("/>")].
rw_command(htmlenv0(Cmd, Props), DocSt, NewCommand) :- !, % <cmd PROPS> % TODO: Valid syntax anymore?
	atom_codes(Cmd, CmdS),
	fmt_html_props(Props, DocSt, PropsR),
	NewCommand = [raw("<"), raw(CmdS), raw(" "), PropsR, raw(">")].
%
rw_command(htmldecl(C), _, NewCommand) :- !,
        NewCommand = [raw("<!"), raw(C), raw(">")].
rw_command(htmlcomment(C), _, NewCommand) :- !,
        NewCommand = [raw("<!-- "), raw(C), raw(" -->")].
%
rw_command(section_env(SecProps, SectLabel, TitleR, Body), DocSt, R) :- !,
	fmt_section_env(SecProps, SectLabel, TitleR, Body, DocSt, R).
rw_command(backend_include_component(_), _DocSt, nop) :- !.
rw_command(hfill, _DocSt, R) :- !, % vertical space
	% TODO: finish
	R = raw(" ").
rw_command(linebreak, _DocSt, R) :- !,
	R = raw("<br/>").
rw_command(subsection_title(X), _DocSt, R) :- !,
	R = htmlenv(h2, X).
rw_command(twocolumns(X), _DocSt, R) :- !,
	R = htmlenv(div, [class="twocolumns"], X).
rw_command(itemize_none(Xs), _DocSt, R) :- !,
	R = htmlenv(ul, [class="itemize_none"], Xs).
rw_command(itemize_plain(Xs), _DocSt, R) :- !,
	R = htmlenv(ul, [class="itemize_plain"], Xs).
rw_command(itemize_minus(Xs), _DocSt, R) :- !,
	R = htmlenv(ul, [class="itemize_minus"], Xs).
rw_command(itemize_bullet(Xs), _DocSt, R) :- !,
	R = htmlenv(ul, Xs).
rw_command(description_env(Xs), _DocSt, R) :- !,
	R = htmlenv(dl, Xs).
rw_command(cartouche(X), _DocSt, R) :- !,
	R = htmlenv(div, [class="cartouche"], X).
rw_command(optional_cartouche(X), _DocSt, R) :- !,
	R = cartouche(X).
rw_command(alert(X), _DocSt, R) :- !,
	R = htmlenv(div, [class="alert"], X).
rw_command(bibitem(Label,Ref), _DocSt, R) :- !,
	R0 = [string_esc("["), string_esc(Label), string_esc("]")],
	R = [item(htmlenv(strong, [id=Ref], R0))].
rw_command(idx_anchor(_Indices, IdxLabel, _Key, OutLink, Text), DocSt, R) :- !,
	fmt_link(idx_anchor, IdxLabel, OutLink, DocSt, Text, R).
rw_command(cover_title(TitleR, SubtitleRs), _DocSt, R) :- !,
	R = htmlenv(div, [class="cover_title"], [
              htmlenv(h1, [class="cover_h1"], TitleR)
              |Rs]),
	sep_nl(SubtitleRs, Rs).
rw_command(cover_subtitle_extra(Rs), _DocSt, R) :- !,
	sep_nl(Rs, R).
rw_command(authors(AuthorRs), _DocSt, R) :- !,
	sep_nl(AuthorRs, R).
rw_command(backend_comment(_String), _DocSt, R) :- !,
	R = nop.
rw_command(quotation(X), _DocSt, R) :- !,
	R = htmlenv(div, X).
rw_command(left_and_right(Left, Right), _DocSt, R) :- !,
	R = [htmlenv(span, [class="on_right"], Right),
             htmlenv(span, Left)].
rw_command(navigation_env(Left, Right), _DocSt, R) :- !,
	R = [htmlenv(div, [class="nav"], [
               htmlenv(span, [class="on_right"], Right),
               htmlenv(span, Left)])].
rw_command(defpred(IdxLabel, Type, Text, PN, Body), DocSt, R) :- !,
	PN = F/A, format_to_string("~w/~w", [F, A], S),
	( get_idxbase(Type, DocSt, IdxBase) ->
	    OutLink = link_to(IdxBase, local_label(S))
	; % TODO: warning?
	  OutLink = no_link
	),
	idx_get_indices(def, Type, Indices),
	R = [htmlenv(div, [
               htmlenv(span, [class="predtag_on_right"], [raw(Text)]),
               htmlenv(div, [class="defname"], [
		 idx_anchor(Indices, IdxLabel, string_esc(S), OutLink, string_esc(S)),
%	         string_esc(S),
		 raw(":")
               ]),
%	       linebreak,
               htmlenv(div, [class="deftext"], [Body])
             ])
            ].
rw_command(defassrt(Status, AType, HeaderStr, HeadR, DescR, UsageProps), _DocSt, R) :- !,
	( AType = test -> HeaderStyle = "test_header" % TODO: Status ignored
	; Status = true -> HeaderStyle = "true_header"
	; Status = false -> HeaderStyle = "false_header"
	; Status = check -> HeaderStyle = "check_header"
	; Status = checked -> HeaderStyle = "checked_header"
	; Status = trust -> HeaderStyle = "trust_header"
	; throw(error(unknown_assrt_status(Status), rw_command/3))
	),
	( HeaderStr = "" -> HeaderR = []
	; HeaderR =
            [p(""),
	     htmlenv(span, [class=HeaderStyle], [bf(string_esc(HeaderStr))])]
        ),
	R = [HeaderR,
	     htmlenv(span, [class="usagedecl"], HeadR),
	     htmlenv(p, DescR),
	     UsageProps].
rw_command(assrtprops(DPR, CPR, APR, NGPR), _DocSt, R) :- !,
	R = itemize_minus([
	       DPR,
	       CPR,
	       APR,
	       NGPR
            ]).
%
rw_command(simple_link(Style, Label, Link, Title), DocSt, R) :- !,
	fmt_link(Style, Label, Link, DocSt, Title, R).
rw_command(X, _DocSt, _R) :- !,
	throw(error(not_in_domain_rw_command(X), rw_command/3)).

:- pred fmt_link(Style, IdLabel, Link, DocSt, Text, R) ::
	atm * doclabel * doclink * docstate * doctree * doctree
   # "@var{R} is a hyperlink showing text @var{Text}, pointing to
     @var{Link} and identified with @var{IdLabel}.".

fmt_link(Style, IdLabel, Link, DocSt, Text, R) :-
	% The style for this element
	( Style = default ->
	    Props = Props0
	; atom_codes(Style, StyleC),
	  Props = [class=StyleC|Props0]
	),
	% The identifier of this element (e.g. for incoming links)
	doclabel_to_html_id(IdLabel, Id),
	( Id = "" ->
	    Props0 = Props1
	; Props0 = [id=Id|Props1]
	),
	%
	( Link = no_link ->
	    % no link, use a span env
	    R = htmlenv(span, Props, Text)
	; % The outcoming link from this element (i.e. when clicking)
	  doctree_to_href(Link, DocSt, HRef),
	  Props1 = [href=HRef],
	  R = htmlenv(a, Props, Text)
	).

sep_nl([],     []).
sep_nl([R|Rs], [R2|Rs2]) :-
	R2 = [R, raw("<br/>")],
	sep_nl(Rs, Rs2).

fmt_html_props([], _, []) :- !.
fmt_html_props([P0], DocSt, [P]) :- !,
	fmt_html_prop(P0, DocSt, P).
fmt_html_props([P0|Ps0], DocSt, [P, raw(" ")|Ps]) :- !,
	fmt_html_prop(P0, DocSt, P),
	fmt_html_props(Ps0, DocSt, Ps).
fmt_html_props(Ps, _, _) :-
	throw(error(bad_html_props(Ps), fmt_html_props/3)).

fmt_html_prop(Attr=Val, _DocSt, R) :- !,
	atom_codes(Attr, AttrS),
	% TODO: Missing escape " in Val
	R = [raw(AttrS), raw("=\""), raw(Val), raw("\"")].
fmt_html_prop(attr(Attr,Val), _DocSt, R) :- !, % TODO: like =/2 but process the value
	atom_codes(Attr, AttrS),
	% TODO: Missing escape " in Val
	R = [raw(AttrS), raw("=\""), Val, raw("\"")].
fmt_html_prop(P, _, _) :-
	throw(error(bad_html_prop(P), fmt_html_props/3)).

% TODO: refine this code
fmt_section_env(SecProps, SectLabel, TitleR, BodyR, DocSt, ModR) :-
	section_prop(file_top_section, SecProps),
	!,
	fmt_nav(DocSt, SectPathR, UpPrevNextR),
	( section_prop(coversec(_,_,_,_,_,_,_), SecProps) ->
	    IsCover = yes
	; IsCover = no
	),
	( docst_gdata_query(DocSt, main_title(MainTitleR)) ->
	    true
	; throw(error(no_main_title, fmt_section_env/6))
	),
	( IsCover = yes ->
	    TitleR2 = MainTitleR
	; TitleR2 = [TitleR, raw(" &mdash; "), MainTitleR]
	),
	%
	% TODO: remove pragmas, define new comment types instead
	( section_prop(pragmas(Pragmas), SecProps) -> true ; Pragmas = [] ),
	%
	( member(section_image(SectImg), Pragmas) ->
	    img_url(SectImg, ImgSrc),
	    PreSect = htmlenv(div, [style="text-align: center;"], 
                              htmlenv1(img, [src=ImgSrc, class="section_image"]))
	; PreSect = []
	),
	%
	( setting_value(html_layout, Layout0),
	  Layout0 = 'website_layout' ->
	    SidebarR2 = [PreSect, show_toc(vertical_menu)],
	    Layout = nav_searchbox_menu_main,
	    % TODO: Hardwired, fix
	    MaybeIcon = yes('ciao-icon16.ico'),
	    CssList = ['lpdoc.css', 'css/website.css'],
	    %
            SectR = [htmlenv(h1, TitleR), raw_nl, BodyR]
	; Layout = nav_sidebar_main,
	  % Optional logo
	  ( IsCover = no, docst_gdata_query(DocSt, main_logo(Logo)) ->
	      atom_codes(Logo, LogoS),
	      LogoR = image(LogoS)
	  ; LogoR = []
	  ),
	  SidebarR1 = show_toc(toc_view(yes)),
	  doctree_simplify([LogoR, SidebarR1], SidebarR2),
	  MaybeIcon = no,
	  CssList = ['lpdoc.css'],
	  %
	  ( IsCover = yes ->
	      fmt_cover(SecProps, TitleR, BodyR, DocSt, SectR)
	  ; fmt_section(SecProps, SectLabel, TitleR, BodyR, DocSt, SectR)
	  )
	),
	%
	fmt_layout(Layout, SectPathR, UpPrevNextR, SidebarR2, SectR, DocSt, R),
	fmt_headers(MaybeIcon, CssList, TitleR2, R, ModR).
fmt_section_env(SecProps, SectLabel, TitleR, BodyR, DocSt, R) :-
	fmt_section(SecProps, SectLabel, TitleR, BodyR, DocSt, R).

% Format a module as a cover
fmt_cover(SecProps, TitleR, BodyR, DocSt, SectR) :-
	section_prop(coversec(SubtitleRs,
	                      SubtitleExtraRs,
	                      AuthorRs,
	                      AddressRs,
			      GVersShortR,
			      _GVersR,
			      _CopyrightR),
	             SecProps),
	% Add version (GVers) to subtitle
	( doctree_is_empty(GVersShortR) ->
	    GVersShortRs = []
	; GVersShortRs = [GVersShortR]
	),
	% Address box (optional)
	( AddressRs = [] ->
	    AddressRs2 = []
	; sep_nl(AddressRs, AddressRs1),
	  AddressRs2 = htmlenv(div, [class="cover_address"], AddressRs1)
	),
	% Document skeleton
	( docst_gdata_query(DocSt, main_logo(Logo)) ->
	    atom_codes(Logo, LogoS),
	    MainLogoR = image(LogoS)
	; MainLogoR = []
	),
	SectR = [
	  htmlenv(div, [
            linebreak, % add some margin here
	    MainLogoR,
	    cover_title(TitleR, SubtitleRs),
	    AddressRs2,
	    htmlenv(div, [class="cover_authors"], [
	      authors(AuthorRs)
            ]),
	    htmlenv(div, [class="cover_subtitle_extra"], [
	      cover_subtitle_extra(SubtitleExtraRs),
	      cover_subtitle_extra(GVersShortRs)
            ]),
	    htmlenv(div, [class="clearer"], [])
	  ]),
	  raw_nl,
	  BodyR
        ].

% Navigation, sidebar, and main contents
fmt_layout(nav_sidebar_main, SectPathR, UpPrevNextR, SidebarR, MainR, DocSt, R) :-
	colophon(DocSt, Colophon),
	R = [%htmlenv(div, [class="header"], [
             %  htmlenv(h1, [raw("HEADER")])
             %]),
	     navigation_env(SectPathR, UpPrevNextR),
	     htmlenv(div, [class="documentwrapper"], [
	       htmlenv(div, [class="document"], [
	         htmlenv(div, [class="mainwrapper"], 
	           htmlenv(div, [class="main"], MainR)
                 )
               ]),
	       htmlenv(div, [class="sidebarwrapper"], 
	         htmlenv(div, [class="sidebar"], SidebarR)
               ),
	       htmlenv(div, [class="clearer"], [])
             ]),
	     % repeat navigation here too
	     navigation_env(raw("&nbsp;"), UpPrevNextR),
	     % the footer
	     Colophon
            ].
fmt_layout(nav_searchbox_menu_main, _SectPathR, _UpPrevNextR, SidebarR, FrameR, DocSt, R) :-
	% TODO: Generalize, this contains many definitions that are only valid
	%   for the Ciao website. They should be defined externally.
%	LogoImg = 'ciao2-96-shadow-reduced.png',
	LogoImg = 'ciao2-small-shadow-reduced.png',
	img_url(LogoImg, LogoSrc),
	fmt_html_template('google_search.html', [], SearchBoxR),
	colophon(DocSt, Colophon),
	R = [%
	     htmlenv(div, [class="documentwrapper"], [
	       htmlenv(div, [class="title"], [
	         SearchBoxR, % must precede the image (due to float:right)
                 htmlenv(a, [href="index.html"], [
                   htmlenv1(img, [src=LogoSrc,
                                  'ALT'="Ciao",
                                  class="logo"])
                 ])
               ]),
	       htmlenv(div, [class="document"], [
	         htmlenv(div, [class="mainwrapper"], [
                   htmlenv(div, [class="main"], FrameR)
                 ])
               ]),
	       htmlenv(div, [class="sidebarwrapper"], [
	         htmlenv(div, [class="sidebar"], [SidebarR])
               ]),
	       htmlenv(div, [class="clearer"], [])
             ]),
             Colophon
            ].

% colophon: "a the brief description of the publication or production
% notes relevant to the edition"
% (this is standard also for Web pages)
colophon(DocSt, R) :-
	(  docst_opt(no_lpdocack, DocSt)
	-> Ack = ""
        ;  Ack = "Generated with LPdoc using Ciao" ),
	R = htmlenv(div, [class="footer"], [string_esc(Ack)]).

fmt_headers(MaybeIcon, CssList, Title, Body, R) :-
	MetaR = htmlenv1(meta, ['http-equiv'="Content-Type", content="text/html; charset=iso-8859-1"]),
	fmt_icon(MaybeIcon, IconR),
	fmt_css(CssList, CssR),
	fmt_mathjax(MathJaxR),
	R = [
	  raw("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"),
          htmlenv(html, [
            htmlenv(head, [
	      MetaR,
	      CssR,
	      IconR,
	      MathJaxR,
              htmlenv(title, Title)
            ]),
	    htmlenv(body, Body)
	  ])
	].

fmt_icon(MaybeIcon, R) :-
	( MaybeIcon = [IconImg] ->
	    img_url(IconImg, IconHRef),
	    R = htmlenv1(link, [rel="shortcut icon", href=IconHRef])
	; R = nop
	).

fmt_css([], []).
fmt_css([X|Xs], [R|Rs]) :-
	atom_codes(X, HRef),
	R = htmlenv1(link, [rel="stylesheet", href=HRef, type="text/css"]),
	fmt_css(Xs, Rs).

fmt_section(SecProps, SectLabel, TitleR, Body, _DocSt, R) :-
	doclabel_to_html_id(SectLabel, Id),
	fmt_structuring(SecProps, TitleR, SectR),
	R = htmlenv(div, [id=Id], [SectR, Body]).

fmt_structuring(SecProps, TitleR, R) :-
	( section_prop(level(Level), SecProps) ->
	    ( Level = 1 -> Cmd = h1
	    ; Level = 2 -> Cmd = h2
	    ; Level = 3 -> Cmd = h3
	    ; Level = 4 -> Cmd = h4
	    )
	; throw(error(missing_level_prop(SecProps), fmt_structuring/3))
	),
	R = htmlenv(Cmd, TitleR).

% From a doclabel, obtain a HTML element identifier
% in HTML, only local_label is significant; global_label is ignored
doclabel_to_html_id(local_label(Label), Id) :- !,
	Id = Label.
doclabel_to_html_id(localnum_label(Label), Id) :- !,
	Id = Label.
doclabel_to_html_id(_, Id) :- !, Id = "".

% From a doclink, obtain a HTML href 
doctree_to_href(Link, DocSt, HRef) :-
	Link = link_to(Base, SectLabel), !,
	docst_currmod(DocSt, Name),
	( Base = Name ->
	    HRef = HRef0
	; Backend = html,
          absfile_for_subtarget(Base, Backend, cr, F0),
	  absfile_to_relfile(F0, Backend, F),
	  atom_codes(F, FC),
	  append(FC, HRef0, HRef)
	),
	doclabel_to_html_id(SectLabel, SectId),
	( SectId = "" ->
	    HRef0 = ""
	; HRef0 = "#"||SectId
	).
doctree_to_href(no_link, _DocSt, "#").

html_blank_lines(0, "") :- !.
html_blank_lines(N, "<BR>"||R) :-
	N1 is N-1,
	html_blank_lines(N1, R).

html_escape("``"||S0, "&ldquo;"||S) :- !, html_escape(S0, S).
html_escape("''"||S0, "&rdquo;"||S) :- !, html_escape(S0, S).
html_escape([0'"|S0], "&quot;"||S) :- !, html_escape(S0, S).
html_escape([0''|S0], "&apos;"||S) :- !, html_escape(S0, S).
html_escape("&#"||S0, "&#"||S) :- !, html_escape(S0, S).
html_escape([0'&|S0], "&amp;"||S) :- !, html_escape(S0, S).
html_escape([0'<|S0], "&lt;"||S) :- !, html_escape(S0, S).
html_escape([0'>|S0], "&gt;"||S) :- !, html_escape(S0, S).
html_escape([X|S0], [X|S]) :- !,
	html_escape(S0, S).
html_escape([], []).

% ---------------------------------------------------------------------------
% Format the navigation links
% (path to the root node, links to the previous and next nodes)

fmt_nav(DocSt, PathR, UpPrevNextR):-
	( docst_mvar_get(DocSt, nav, Nav) ->
	    true
	; throw(error(no_navigation, fmt_nav/3))
	),
	Nav = nav(Path, _Top, Up, Prev, Next),
	navpath_links(Path, Path2),
	sep_list(Path2, raw(" &raquo; "), PathR0),
	( PathR0 = [] -> PathR = raw("&nbsp;") ; PathR = [PathR0, raw(" &raquo; ")] ),
	UpUnicode = raw("&#x25B2;"),
	LeftUnicode = raw("&#x25C4;"),
	RightUnicode = raw("&#x25BA;"),
	navlink(Up, UpUnicode, UpR),
	navlink(Prev, LeftUnicode, PrevR),
	navlink(Next, RightUnicode, NextR),
	UpPrevNextR = [UpR, PrevR, NextR].

% :- regtype step := step(..., ...).
% :- regtype path := ~list(step).
% :- pred pathlink/2 :: path * list(doctree).
navpath_links([], []).
navpath_links([step(Link, Title)|Path], [R|Rs]) :-
	R = [simple_link(default, no_label, Link, Title)],
	navpath_links(Path, Rs).

navlink(Link, Text, R) :-
	navlink_style(Link, Style),
	R = simple_link(Style, no_label, Link, Text).

navlink_style(no_link, Style) :- !, Style = 'navbutton_disabled'. % deactivated link
navlink_style(_, 'navbutton').

% sep_list(As, Sep, Bs): Bs contains the elements of As separarted with Sep
sep_list([], _, []) :- !.
sep_list([A], _, [A]) :- !.
sep_list([A|As], Sep, [A,Sep|Bs]) :-
	sep_list(As, Sep, Bs).

% ===========================================================================
% Obtain the current date (for '@today' command)

:- use_module(library(system), [time/1, datime/9]).

% TODO: Share with other backends?
fmt_date(R) :-
	time(Time),
        datime(Time, Year, Month, Day, _Hour, _Min, _Sec, _, _),
	format_to_string("~w/~w/~w", [Year,Month,Day], S),
	R = string_esc(S).

% ===========================================================================
% Mathematical notation
%
% Note: currently, only MathJax is supported

:- use_module(lpdoc(autodoc_html_assets), [using_mathjax/1]).

% Include mathjax, if available (for TeX output in HTML)
fmt_mathjax(R) :-
	using_mathjax(MathJaxJS0),
	!,
	atom_codes(MathJaxJS0, MathJaxJS),
	R = [htmlenv(script, [type="text/x-mathjax-config"], [
               raw("MathJax.Hub.Config({"),
               raw(" jax: [\"input/TeX\",\"output/HTML-CSS\"],"),
               raw(" TeX: {extensions: [\"AMSmath.js\",\"AMSsymbols.js\"]}"),
               raw("});")
             ]),
	     htmlenv(script, [type="text/javascript", src=MathJaxJS], [])].
fmt_mathjax([]).

% ===========================================================================

:- multifile autodoc_finish_hook/1.
autodoc_finish_hook(html) :- finish_html.

% TODO: Move to autodoc_html
finish_html :-
	% Note: I added 'htmlmeta' so that there exist a node in
	% the dependency graph (a directed graph) that is
	% connected to all the files in the document. The 'cr'
	% files are directly generated in '.html' format. That
	% could be generalized to more backends. (JFMC)
	main_absfile_in_format('htmlmeta', Out),
	Mod = ~get_mainmod,
	atom_codes(Mod, ModS),
	string_to_file(ModS, Out).

:- use_module(library(file_utils), [string_to_file/2]).

:- multifile autodoc_gen_alternative_hook/2.
% note: no alternative formats here
autodoc_gen_alternative_hook(html, _) :- fail.

% ===========================================================================

:- doc(bug, "hfill is not treated correctly.").

:- doc(bug, "Needed a better way to define new LaTex commands than
this: @@begin@{displaymath@} @@newcommand@{@@neck@}@{@@textbf@{:-@}@}
@@end@{displaymath@} (JFMC)").
