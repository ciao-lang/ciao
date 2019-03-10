:- use_package(assertions).
:- doc(filetype, documentation).

:- doc(title, "Bundle management").

:- doc(author,"Jose F. Morales").
:- doc(author,"The Ciao Development Team").

:- doc(module, "@include{BundleManagement.lpdoc}").

% TODO: Symbolic links to bundles may not work as expected (due to
%   mismatches between the real and symbolic path of files).

% TODO: Detect at runtime the following: @tt{CIAOPATH} cannot be the
%   same as your Ciao sources. Ciao sources cannot be in a
%   subdirectory from any entry in @tt{CIAOPATH}.

% TODO: CIAOALIASPATH is needed at least for bootstrap (see builder
%   scripts), but it is use is not recommended for users.

% TODO: Add rationale for design? There are many approaches and
%   implementations for software package systems (Rust, Go, Python,
%   Haskell, etc.). Our proposal tries to get the best ideas while
%   preserving certain Prolog compatibility. In workspaces we "favor
%   convention over configuration".
