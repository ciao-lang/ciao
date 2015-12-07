:- module(_, [], [assertions, fsyntax]).

:- doc(title, "LPdoc Plugin to Show pbundle Download Lists").
:- doc(author, "Jos@'{e} F. Morales").

:- doc(module, "This module generates a download list of packaged
   bundles (as a LPdoc @tt{docstring}).").

:- use_module(library(aggregates)).

:- use_module(library(pathnames)).
:- use_module(ciaobld(pbundle_meta)).

:- use_module(library(terms), [atom_concat/2]).

:- use_module(lpdoc(autodoc_settings)).
:- use_module(lpdoc(autodoc_html_template)).

% ---------------------------------------------------------------------------

% Load the latest metadata for the given branch
load_latest_meta(Branch, PMeta) :-
	PDir = ~pbundle_root(meta),
	( AllPMetas = ~load_pbundle_metas(Branch, PDir),
	  PMeta = ~newest_pbundle_meta(AllPMetas) ->
	    true
	; % TODO: This should be a normal user error, not a bug.
	  % TODO: This is a bug if pbundle_meta is corrupted.
	  throw(error(pkgmeta_not_found(PDir, Branch), load_latest_meta/2))
	).

% ---------------------------------------------------------------------------

:- export(fmt_pbundle_download/3).
:- pred fmt_pbundle_download(Branch, View, R)
   # "Format in @var{R} the view @var{View} (@tt{code} or @tt{docs})
      of branch @var{Branch} of the distribution packages specified in
      the documentation settings.".

fmt_pbundle_download(Branch, View, R) :-
	load_latest_meta(Branch, PMeta),
	fmt_pbundle_download_(PMeta, View, R).

fmt_pbundle_download_(PMeta, View, R) :-
	SrcR = ~gen_source_list(PMeta),
	DocR = ~gen_manual_list(PMeta),
	%
	PName = ~pbundle_meta_attr(PMeta, packname),
	CommitDate = ~pbundle_meta_attr(PMeta, commit_date),
	CommitDesc = ~pbundle_meta_attr(PMeta, commit_desc),
	Params = [src_formats = SrcR,
	       	  manuals = DocR,
	       	  packname = string_esc(~atom_codes(PName)),
	       	  commit_date = string_esc(~atom_codes(CommitDate)),
	       	  commit_desc = string_esc(~atom_codes(CommitDesc))],
	% TODO: generate Params lazily
	( View = docs -> Tmpl = 'download_docs.html'
	; View = code -> Tmpl = 'download_code.html'
	; % TODO: this should be a normal user error, not a bug
	  throw(error(bad_view(View), fmt_pbundle_download_/3))
	),
        R = html_template_internal(Tmpl, Params).

% ---------------------------------------------------------------------------
% TODO: This may be slow, cache some results?

:- export(fmt_pbundle_href/5).
:- pred fmt_pbundle_href(Branch, Manual, Rel, Text, R)
   # "Format in @var{R} a link to the @var{Rel} page in HTML manual
      @var{Manual} for branch @var{Branch}. The link text will be
      @var{Text}".

fmt_pbundle_href(Branch, Manual, Rel, Text, R) :-
	% Load the latest metadata for the given branch
	load_latest_meta(Branch, PMeta),
	fmt_pbundle_href_(PMeta, Manual, Rel, Text, R).

fmt_pbundle_href_(PMeta, Manual, Rel, Text, R) :-
	Ds = ~pbundle_meta_attr(PMeta, docs),
	( member(D, Ds),
	  D = pbundle_item(ItemKind, Desc, File),
	  ItemKind = manual_html,
	  Desc = Manual ->
	    Path = ~pbundle_file_path(ItemKind, PMeta, File),
	    Url = ~pbundle_file_url(ItemKind, PMeta, File, Path, Rel)
	; % TODO: this should be a normal user error, not a bug
	  throw(error(manual_not_found(Manual), fmt_pbundle_href_/5))
	),
	R = htmlenv(a, [href = ~atom_codes(Url)], Text).

% ---------------------------------------------------------------------------

gen_source_list(PMeta) := R :-
	Ds0 = ~pbundle_meta_attr(PMeta, code),
	Ds = ~resolve_dfiles(Ds0, PMeta),
	gen_download_list(Ds, yes, R).

% ---------------------------------------------------------------------------
% Generate a download list of files.
%
% UseCGI=yes iff the file needs to be downloaded through a custom CGI
% application (e.g., to keep track of download statistics).

gen_download_list(Ds, UseCGI, R) :-
	gen_download_list_(Ds, UseCGI, BodyR),
	R = htmlenv(table, [class="download_table"], [
	      htmlenv(tbody, BodyR)
            ]).

gen_download_list_([], _, []).
gen_download_list_([DFile|R], UseCGI, [Row|Next]) :- !,
	DFile = pbundle_item_r(PFileKind, PFileTitle, PFilePath, PFileUrl),
	%
	format_sizeof(PFilePath, Size),
	% Include download_cgi in the href (if necessary)
	( UseCGI = yes ->
	    Url = ~wrap_download_cgi(PFileUrl)
	; Url = PFileUrl
	),
	%
	Row = htmlenv(tr, ~file_html_row(PFileKind, Url, PFileTitle, Size)),
	%
	gen_download_list_(R, UseCGI, Next).

% ---------------------------------------------------------------------------
% Custom link using download_cgi 

% TODO: generalize as server-side computation

% Obtain a URL that passes through the download CGI
wrap_download_cgi(Url) := NewUrl :-
	Maillist = ~maillist,
	DownloadCGI = ~download_cgi,
	% TODO: Relative URL (w.r.t. the current location) are not supported by download_cgi
        %       (when fixed this will disappear)
	( absolute_url(Url) ->
	    Url2 = Url
	; Url2 = ~atom_concat(~download_full_url, Url)
	),
	%
	NewUrl = ~atom_concat([DownloadCGI, '?url=', Url2, '&list=', Maillist]).

% This is the location of the download script
download_cgi := '/cgi-bin/download.cgi'.
download_full_url := '/'. % TODO: a hack (see wrap_download_cgi)
% Mailing list recommended (leave empty for no list)
maillist := 'ciao-users'.

% Is a given URL an absolute path in the server?
absolute_url(Url) :- atom_concat('/', _, Url).
absolute_url(Url) :- atom_concat('file:', _, Url).
absolute_url(Url) :- atom_concat('http:', _, Url).

% ---------------------------------------------------------------------------

gen_manual_list(PMeta) := R :-
	Ds0 = ~pbundle_meta_attr(PMeta, docs),
	Ds = ~resolve_dfiles(Ds0, PMeta),
	extract_manuals(Ds, Manuals),
	%
	gen_manual_list_(Manuals, Ds, BodyR),
	R = htmlenv(table, [class="download_table"], [
	      htmlenv(tbody, BodyR)
            ]).

% Obtain the manual list (HTML and PDF)
gen_manual_list_([], _Ds, []).
gen_manual_list_([Desc|Descs], Ds, [Row|Next]) :- !,
	gen_manual1(manual_html, Desc, Ds, Cols1),
	gen_manual1(manual_pdf, Desc, Ds, Cols2),
	Row = htmlenv(tr, [Cols1, Cols2]),
	gen_manual_list_(Descs, Ds, Next).

gen_manual1(ItemKind, Desc, Ds, Cols) :-
	( member(DFile, Ds),
	  DFile = pbundle_item_r(ItemKind, Desc, PFilePath, PFileUrl) ->
	    format_sizeof(PFilePath, Size),
	    Url = PFileUrl,
	    Cols = ~file_html_row(ItemKind, Url, Desc, Size)
	; Cols = []
	).

% ---------------------------------------------------------------------------
% TODO: This could not be necessary if we change the desc.tmpl file

:- use_module(library(lists), [reverse/2]).

extract_manuals(Ds, Manuals) :-
	extract_manuals_(Ds, [], Manuals0),
	reverse(Manuals0, Manuals).

% TODO: factorize this code (it appears hundreds of times)
extract_manuals_([], Manuals, Manuals).
extract_manuals_([D|Ds], Manuals0, Manuals) :-
	D = pbundle_item_r(_, Desc, _, _),
	( member(Desc, Manuals0) ->
	    Manuals1 = Manuals0
	; Manuals1 = [Desc|Manuals0]
	),
	extract_manuals_(Ds, Manuals1, Manuals).

% ---------------------------------------------------------------------------

% A table row for file download entry
% The url is in @var{Url}, its size is @var{Size}
file_html_row(Kind, Url, Desc, Size) := R :-
	pbundle_file_kind_info(Kind, KindText, KindImage),
	% Use the description as link name, otherwise use the kind text
	SizeR = raw(Size),
	( Desc = "" ->
	    LinkText = raw(KindText),
	    SubR = SizeR
	; Kind = manual_html -> % do not show size for online files
	    % TODO: I would need to rewrite this...
	    LinkText = raw(Desc),
	    SubR = raw(KindText)
	; LinkText = raw(Desc),
	  SubR = [raw(KindText), raw(" - "), SizeR]
	),
	%
	DescR = htmlenv1(img, [src = ~img_url(KindImage), border="0"]),
	LinkR = htmlenv(a, [href = ~atom_codes(Url)], LinkText),
	R = [
              htmlenv(td, [align="right"], DescR), 
              htmlenv(td, [align="left"], [
                LinkR, htmlenv1(br, []),
		htmlenv(strong, SubR)
              ])
            ].

% Information of each packaged bundle file kind
% TODO: some images like download_debian.png, download_fedora.png, etc. are unused
% TODO: The images are in the website skel (CIAOROOT/website/skel/images/)
pbundle_file_kind_info(tar_gz, "Source (All Platforms)", 'download_sources.png').
pbundle_file_kind_info(i386_rpm, "RPM-based Linux (Fedora, Redhat, Centos, Mandriva)", 'download_rpms.png').
pbundle_file_kind_info(i386_deb, "Debian-based Linux (Ubuntu, XUbuntu, Knoppix)", 'download_debs.png').
pbundle_file_kind_info(windows, "Windows 2000/XP/Vista/7", 'download_windows.png').
pbundle_file_kind_info(macosx, "Mac OS X (Leopard, Snow Leopard, Lion)", 'download_mac.png').
%
pbundle_file_kind_info(manual_pdf, "PDF", 'download_pdf.png').
pbundle_file_kind_info(manual_html, "online HTML", 'download_manual.png').

% ---------------------------------------------------------------------------
% Find and format the size of a file

:- use_module(library(system), [file_exists/1, file_property/2, working_directory/2]).
:- use_module(library(format)).

format_sizeof(File, Size) :-
	sizeof_file(File, Size0),
	formatted_size(Size0, Size).

sizeof_file(FileName, Size) :-
	( file_exists(FileName) ->
	    file_property(FileName, size(Size))
	; working_directory(W, W),
	  warning(['In dir ', W, ', file ', FileName, ' not found']),
	  Size = 0
	).

formatted_size(Size, FmtSize) :-
	(
	    Size > 2** 20 * 0.1 ->
	    Size2 is Size / 2** 20,
	    sformat(FmtSize, "~2f MB", [Size2])
	;
	    Size > 2** 10 * 0.7 ->
	    Size2 is Size / 2** 10,
	    sformat(FmtSize, "~1f KB", [Size2])
	;
	    sformat(FmtSize, "~0f bytes", [Size])
	).

% ---------------------------------------------------------------------------

% TODO: Define a pbundle_repository object for those values
%       We can define 'local' and 'remote' repositories. Fetching is
%       just some move operation between repositories.

% TODO: Document: the valid values for pbundle_root are @tt{meta},
%   other @tt{code} and other @tt{docs}. Each one specifies the kind
%   of items that we want to query; since different items may be
%   stored in diferent directories.

% The absolute pbundle root directory
pbundle_root(Kind) := Path :-
	( pbundle_kind_is_doc(Kind) ->
	    Path = ~setting_value(pbundle_localdocdir)
	; Path = ~setting_value(pbundle_localpkgdir)
	).

% TODO: Provide operations to obtain the full and relative URL
% The URL to the pbundle root (for downloading)
pbundle_url(Kind) := Url :-
	% TODO: Use relative URLs if htmlurl is '' (search uses of htmlurl)
	Url0 = ~setting_value(htmlurl),
	( pbundle_kind_is_doc(Kind) ->
	    RelUrl = ~setting_value(pbundle_localdocurl)
	; RelUrl = ~setting_value(pbundle_localpkgurl)
	),
	Url = ~path_concat(Url0, RelUrl).

% TODO: Move to some pbundle module
pbundle_kind_is_doc(manual_html).
pbundle_kind_is_doc(manual_pdf).

% ---------------------------------------------------------------------------
% Obtain the resolved files (usable for a download list) of a PMeta

% Each file is represented by a tuple:
%   pbundle_item_r(Kind, Desc, Path, Url): Kind is the file kind,
%     Desc its description, Path the filesystem path to the file and
%     Url the URL where it is accessible.

resolve_dfiles([], _, []).
resolve_dfiles([D|Ds], PMeta, [R|Rs]) :-
	D = pbundle_item(ItemKind, Desc, File),
	Path = ~pbundle_file_path(ItemKind, PMeta, File),
	% TODO: Rel='' because we resolve to the document top (for manual_html)
	Url = ~pbundle_file_url(ItemKind, PMeta, File, Path, ''),
	R = pbundle_item_r(ItemKind, Desc, Path, Url),
	resolve_dfiles(Ds, PMeta, Rs).

% Obtain the File path for a given pbundle item
pbundle_file_path(ItemKind, PMeta, PFile) := Path :-
	Rest = ~pbundle_rootrel(PMeta),
	atom_concat(~pbundle_root(ItemKind), Rest, BaseDir),
	Path = ~path_concat(BaseDir, PFile).

% Obtain the URL for a given pbundle item
% Note: URL for manual_html point to Rel, which is its main file if
%       Rel=''.  Rel is ignored for any other kind of manuals.
% Note: URL to documents in trunk/ point to the symbolic links
pbundle_file_url(ItemKind, PMeta, PFile, PFilePath, Rel) := Url :-
	% TODO: See item links in ciao_builder (share part of this code)
	ItemKind = manual_html,
	!,
	html_manual_mainfile(PFilePath, Main),
	PUrl = ~pbundle_url(ItemKind),
	( Branch = ~pbundle_meta_attr(PMeta, branch),
	  Branch = trunk ->
	    % e.g., docs/ciao/ciao.html
	    Url0 = ~atom_concat([PUrl, '/', Main])
	; % e.g., docs/branches/1.14/Ciao-.../ciao.html
	  Rest = ~pbundle_rootrel(PMeta),
	  PUrl = ~pbundle_url(ItemKind),
	  Url0 = ~atom_concat([PUrl, Rest, '/', PFile])
	),
	( Rel = '' ->
	    Url = ~atom_concat([Url0, '/', Main, '.html'])
	; Url = ~atom_concat([Url0, '/', Rel])
	).
pbundle_file_url(ItemKind, PMeta, PFile, _PFilePath, _Rel) := Url :-
	Rest = ~pbundle_rootrel(PMeta),
	PUrl = ~pbundle_url(ItemKind),
	Url = ~atom_concat([PUrl, Rest, '/', PFile]).

% Relative path to root (usually, the branch name)
pbundle_rootrel(PMeta) := RootRel :-
	BaseDir = ~pbundle_meta_attr(PMeta, basedir),
	atom_concat(~pbundle_root(meta), RootRel, BaseDir).

% ---------------------------------------------------------------------------

:- use_module(library(glob), [glob/3]).
:- use_module(library(file_utils), [file_to_string/2]).

% Obtain the main file for the given HTML documentation
html_manual_mainfile(PFilePath, Main) :-
	Fs = ~glob(PFilePath, '*.htmlmeta'),
	( Fs = [F],
	  F2 = ~path_concat(PFilePath, F),
	  file_to_string(F2, Str),
	  atom_codes(Main, Str) ->
	    true
	; % TODO: this should be a normal user error, not a bug
	  throw(error(not_found(PFilePath), html_manual_mainfile/3))
	).

% ---------------------------------------------------------------------------

% TODO: todo items from previous version
:- doc(bug, "Nicer formatting of dates").
:- doc(bug, "Redesign @apl{download_cgi}").
:- doc(bug, "Are the texts of LGPL and GPL available?").

