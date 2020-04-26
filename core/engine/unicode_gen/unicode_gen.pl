:- module(_, [], [fsyntax, foreign_interface]).

%! \title Generator of Unicode tables for Ciao
%  \author Jose F. Morales
%
%  \module This module generates the Unicode tables for dealing with
%  Ciao source code in Unicode (see
%  [[https://www.unicode.org/reports/tr44/]] for details). It has two
%  parts:
%   - Parsing Unicode databases and generate our own classification
%     for Ciao.
%   - Efficient encoding of tables (experimentally seems to be an optimal
%     trade-off between size and speed) as a dictionary of 32 bit words
%     (each encoding info for 8 code points) and an ordered range list
%     (all around 8KB).
%
%  A dichotomic search algorithm (see `engine/rune.c`) is used on this
%  table to lookup rune Ciao classes with around 11 to 12 memory
%  accesses.
%
%  ## Character classification for Ciao source code
%
%  The Unicode Standard Annex 31
%  ([[https://unicode.org/reports/tr31/#Case_and_Stability]]) deals
%  with the use of Unicode in identifiers for programming languages.
%  In Ciao we use extend the identifier syntax as follows:
%  
%   - Identifiers can begin with `XID_Start` characters and must be
%     followed with zero or more `XID_Continue` (see the Unicode
%     Derived Core Properties), extended with categorty `No`.
%     Variables are those identifiers that start with characters
%     in the `Lu` category.
%
%   - Use `Z*` as layout characters, as well as other control
%     characters (`Cc` categoty) with bidirectional category `WS`
%     (whitespace), `S` (segment separator), or `B` (paragraph
%     separator).
%
%   - Use `S*` (`Sm`, `Sc`, `Sk`, `So`) and `P*` (`Pc`, `Pd`, `Ps`, `Pe`,
%     `Pi`, `Pf`, `Po`) as symbols.

% TODO: read DerivedCoreProperties, implement changes in tokenizer.pl, commit

:- use_module(library(format)).
:- use_module(library(system)).
:- use_module(library(http_get)).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

% ---------------------------------------------------------------------------
%! # Unicode 13.0.0 databases

% Unicode Character Database (UnicodeData.txt)
db_url('http://www.unicode.org/Public/13.0.0/ucd/UnicodeData.txt',
       'UnicodeData.txt').
% Derived Properties (for XID_Start, XID_Continue)
db_url('http://www.unicode.org/Public/13.0.0/ucd/DerivedCoreProperties.txt',
       'DerivedCoreProperties.txt').

% Ensure that databases are available
ensure_db :-
    ( db_url(URL, File),
      ( file_exists(File) ->
          true
      ; format("Fetching ~w...~n", [URL]),
        http_get(URL, file(File))
      ),
      fail
    ; true
    ).
  
% ---------------------------------------------------------------------------
%! # Generate `unicode_tbl.h`

:- use_foreign_source(unicode_gen).

:- trust pred read_unicode_db(go(R)) :: c_int + (returns(R), foreign).
:- trust pred classify_unicode(go(R)) :: c_int + (returns(R), foreign).
:- trust pred gen_tables(go(R)) :: c_int + (returns(R), foreign).
:- trust pred print_tables(in(F),go(R)) :: atm * c_int + (returns(R), foreign).

:- export(prepare_tables/0).
prepare_tables :-
    bundle_path(core, 'engine/unicode_gen', D),
    working_directory(ThisDir, D),
    ensure_db,
    read_unicode_db(1),
    classify_unicode(1),
    gen_tables(1),
    working_directory(_, ThisDir).

:- export(gen/0).
:- pred gen # "Prepare tables and write @tt{unicode_tbl.h}".
gen :-
    prepare_tables,
    bundle_path(core, 'engine/unicode_tbl.h', TBL),
    print_tables(TBL, 1).

% ---------------------------------------------------------------------------
%! # Support for testing

% Version using input table (not compressed)
:- export(get_rune_class/2).
:- trust pred get_rune_class(in(R), go(T)) :: c_int * c_int + (returns(T), foreign).

