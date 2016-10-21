:- module(autodoc_images, [], [dcg, assertions, regtypes, fsyntax]). 

:- doc(title,"Image Handling").
:- doc(author,"Jose F. Morales").

:- doc(module, "
	This module defines the handling of image commands.
        It defines predicates to locate and convert images in the
	different formats required for documentation.

@begin{alert}   
@bf{Note: This part needs better documentation. -- JFMC}
@end{alert}
   ").

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_settings)).

:- use_module(library(system), [copy_file/3]).
:- use_module(library(system_extra), [warn_on_nosuccess/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(pathnames), [path_basename/2]).
:- use_module(library(errhandle), [error_protect/1]).
:- use_module(library(messages)).

% ---------------------------------------------------------------------------
:- use_module(library(format)).

:- export(locate_and_convert_image/4).
% TODO: Allow file specs in ImageSpecS (see spec_add_suffix/3)
% TODO: directory output for target is missing
% TODO: [URGENT] Remember converted images!
:- pred locate_and_convert_image(SrcSpecS, AcceptedExts, DocSt, TargetFileS) ::
	string * list(atm) * docstate * string # 
        "The image at @var{SrcSpecS} is located (as one of the known
         image extensions @pred{known_ext/1}) and converted to one of the
         @var{AcceptedExts}. The target file is called
         @var{TargetFileS}".

locate_and_convert_image(SrcSpecS, AcceptedExts, DocSt, TargetFileS) :-
	atom_codes(SrcSpec, SrcSpecS),
	% TODO: Use the same rules than for modules to locate the images
	( known_ext(SrcExt), % (may backtrack)
	  atom_concat(SrcSpec, SrcExt, SrcSpecExt),
	  catch(find_file(SrcSpecExt, SrcFile), _, fail) ->
	    % Image found!
	    atom_concat(SrcBase, SrcExt, SrcFile),
	    % Determine the target format
	    ( member(SrcExt, AcceptedExts) ->
	        % The source format is accepted, keep it
	        TargetExt = SrcExt
	    ; % Otherwise, use the first accepted format
	      % TODO: This should be done in the image_convert predicate
              %       to find the more optimal conversion
	      AcceptedExts = [TargetExt|_]
	    ),
	    % Determine the target file name
	    path_basename(SrcBase, SrcName),
	    atom_concat(SrcName, '_autofig', TargetBase),
	    atom_concat(TargetBase, TargetExt, TargetFile),
	    cached_image_convert(SrcBase, SrcExt, TargetBase, TargetExt, DocSt),
	    %
	    atom_codes(TargetFile, TargetFileS)
	; error_message("-> Image ~w not found in any known format", [SrcSpec]),
	  fail
	).

% Known image extensions
% TODO: extend?
known_ext('.eps').
known_ext('.png').
known_ext('.jpg').

% ---------------------------------------------------------------------------
:- doc(section, "Cached Image Copy/Conversions").
% TODO: This part is not incremental (and it should be).

% TODO: good indexing?
:- data cached_image/4.

:- export(clean_image_cache/0).
:- pred clean_image_cache/0 # "Clean the cache for image copy/conversions.".
clean_image_cache :-
        retractall_fact(cached_image(_,_,_,_)).

cached_image_convert(SrcBase, SrcExt, TargetBase, TargetExt, _DocSt) :-
        current_fact(cached_image(SrcBase, SrcExt, TargetBase, TargetExt)), !.
cached_image_convert(SrcBase, SrcExt, TargetBase, TargetExt, DocSt) :-
	% Convert the image
	docst_message("-> Including image ~w in documentation as ~w", [SrcBase, TargetBase], DocSt),
	%format(user_error, "-> Including image ~w in documentation as ~w~n", [SrcFile, TargetFile]),
	% ( verbose_message("Converting/Copying file from ~w to ~w", [SrcFile, TargetFile]),
	image_convert(SrcBase, SrcExt, TargetBase, TargetExt, DocSt),
        assertz_fact(cached_image(SrcBase, SrcExt, TargetBase, TargetExt)).

% ---------------------------------------------------------------------------
:- doc(section, "Image Copy/Conversion").

%% Names and paths of external commands used by lpdoc and other paths
%% which get stored in the executable on installation:
:- use_module(library(system_extra),
	[del_file_nofail/1,
	 set_file_perms/2]).
:- use_module(library(process), [process_call/3]).

image_convert(SrcBase, SrcExt, TargetBase, TargetExt, DocSt) :-
	atom_concat(SrcBase, SrcExt, Source),
	atom_concat(TargetBase, TargetExt, Target),
	%
	%% Deprecate use of 'pstogif' ('convert' is better)
        %%( TargetExt = 'gif' ->
	%%  process_call(path(~pstogif), [Source], []),
	%%  del_file_nofail(~atom_concat([SrcBase, '.ppm'])),
	%%  del_file_nofail(~atom_concat([SrcBase, '.ppm.tmp']))
	%%; TargetExt = 'ppm' ->
	%%    process_call0(path(~pstogif), [Source], []),
	%%    del_file_nofail(~atom_concat(SrcBase, '.gif')),
	%%    del_file_nofail(~atom_concat(SrcBase, '.ppm.tmp'))
	%%;
	docst_backend(DocSt, Backend),
	absfile_for_aux(Target, Backend, AbsFile),
	( SrcExt = TargetExt ->
	    % same format, just copy
	    warn_on_nosuccess(copy_file(Source, AbsFile, [overwrite]))
	; TargetExt = 'txt' ->
	    % TODO: This is a dummy output (necessary?)
	    open(Target, write, O),
	    format(O, "~n[Image file: ~w.eps]~n", [SrcBase]),
	    close(O)
        ; % TODO: use other commands?
          process_call(path(~convertc), [Source, AbsFile], [])
%	; throw(error(unknown_target_ext(TargetExt), image_convert/5))
	),
	DataMode = ~setting_value_or_default(perms),
	warn_on_nosuccess(set_file_perms(AbsFile, DataMode)).

%% This is a command that converts .eps files into .gif and .ppm files
%% (the -debug option of pstogif does this!)
%% 
% pstogif := 'pstogif -debug'.

% image_convert(ppm, jpg, SrcBase) :- !,
% 	atom_concat([SrcBase,'.jpg'],Target),
% 	atom_concat([SrcBase,'.ppm'],Src),
% 	process_call(path(~ppmtojpeg), [Src], [stdout(file(Target))]),
% 	-set_file_perms(Target,~get_datamode).
%
%% This is a command that converts .ppm files into .jpg files on stdout
%% 
%% ppmtojpeg := 'cjpeg -progressive'.

% ---------------------------------------------------------------------------

:- doc(bug, "Image conversion can be improved to skip .eps and
accept more sources. E.g., tikz input, etc.").
