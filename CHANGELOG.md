# Changelog
 
## [1.22.0] - 2022-9-28 
 TBD
 
## [1.21.0] - 2022-3-2 
 - Build system:
   - IMPROVED: Partial rewrite of network-based installation
     (better selection of releases, allow prebuilt docs and
     binaries).
   - IMPROVED: Tighter integration of `ciao publish` into the
     builder.
   - IMPROVED: Third-party commands moved to "advanced" help. 
 - Core (compiler/engine, toplevel, libraries):
   - ADDED: Support PowerPC 64-bit in little-endian mode.
   - ADDED: Replaced `using_tty/0` with `system_extra:istty/1`
     (specify fd).
   - ADDED: cgoal/1 property (to distinguish from callable/1 ISO
     pred).
   - ADDED: `binexec` option for automatic spawning of active
     modules. This produces multi-purpose binaries that can start
     as either normal processes or active modules.
   - ADDED: Added `ivar/1` meta-property in assertions
     (expands to `var/1` plus independence from all other vars).
   - ADDED: New rtchecks (run-time assertion checking) code for
     `det/1`, `semidet/1`, `multidet/1`, `nondet/1` 
     properties.
   - ADDED: Option to run unit tests in the same process.
   - ADDED: Custom headers in foreign interface gluecode (useful
     for custom type translations).
   - IMPROVED: Faster dependency checks in the compiler (see
     `itf_sections`). Improves x2 loading time of large
     executables.
   - IMPROVED: `iso` package renamed `iso_strict`, code
     adapted.
   - IMPROVED: merged `bf` and `af` search rule
     translation modules.
   - IMPROVED: Document need for `devenv` for running tests.
   - IMPROVED: Preserve timestamps when engine metadata does not
     change.
   - IMPROVED: More resilient unit test runner.
   - FIXED: Fixes to termux compilation (Android).
   - FIXED: Cleanup of itf when `opt_suff/1` used.
   - FIXED: Default hostname to `localhost` in active modules.
   - FIXED: Location of relative paths in `reexport`.
   - FIXED: Documentation of timeout library.
   - FIXED: Allow multiple doccomments before condcomp directives.
   - FIXED: Markdown parser issues.
   - FIXED: Fix `|` operator priority to 1105 (ISO DCG draft).
   - FIXED: Bug in `sub_atom/5`.
   - FIXED: Removed bashism in `config-sysdep.sh`.
   - FIXED: Use stderr consistently in unit test output. 
 - Ciao emacs mode:
   - ADDED: Distribute flycheck and company support for
     ciao-mode.
   - ADDED: New `ciao-emacs` command (start emacs with
     "batteries included"). It preinstalls markdown mode and
     contrib packages.
   - IMPROVED: library for navigating options menus (used in,
     e.g., CiaoPP): allow cursor navigation, eliminated need for
     OK/cancel buttons, allow integer values in responses.
   - CHANGED: Change check assertions binding to `C-c V`.
   - FIXED: `=>` is no longer a prompt recognised my ciao-emacs.
   - FIXED: Issue in passing of system-args to ciao process.  
 
## [1.20.0] - 2021-3-18 
 - Build system:
   - ADDED: Experimental `analyze` build grade for analyzing whole
     bundles (depends on CiaoPP).
   - FIXED: Fix activation links for `pkgconfig` files in
     3rd-party installation.
   - DEPRECATED: Removed support for 3rd-party `bower` package
     system (all existing uses ported to `npm`). 
 - Language, compiler, and toplevel:
   - FIXED: Fix a bug that redirected `main/1` calls from the
     toplevel to the `ciaosh` `main/1` predicate (accidentally via
     the `user` module).
   - FIXED: Detect wrong arity in `add_goal_trans` directives.
   - IMPROVED: Towards a more modular `native_props.pl`,
     documentation improvements.
   - IMPROVED: Progress towards merging optim-comp branch for native
     compilation.
   - IMPROVED: Refactored default language packages (they can be used
     from modules and toplevels).
   - CHANGED: `note` messages go to user error (like other
     compiler messages). 
 - Engine:
   - ADDED: Support for new Apple M1. This port is based on existing
     support for the aarch64 (ARM64) architecture. NOTE: some
     executable formats depend on redoing 'codesign' after the
     executable is linked (this may cause issues when distributing
     binaries). 
 - Runtime checks, testing, and debugging:
   - IMPROVED: `mshare/1` internally represented as
     `mshare/2`, with explicit relevant arguments (for
     `rtchecks`).
   - IMPROVED: Refactor pieces of embedded debugger.
   - IMPROVED: Note message for modules compiled with the trace
     or debug packages.
   - FIXED: Do not ignore assertions with empty compats, calls,
     success, and comps fields (also for `texec`).
   - FIXED: Improve debugging of modules with rtcheck instrumentation.
   - FIXED: Expansion for runtime checks preserve `?- ...` 
     directives. 
 - Libraries:
   - ADDED: Support for Unicode (UTF8) in source code:
     - Pre-generated character code classes for 0..127
     - Documented code class types and identifier syntax for Unicode
       source code (see `tokenize.pl`).
     - Very efficient and compact (8KB) code class table (see
       `unicode_gen.pl` for details). 
   - ADDED: Unicode escape \\uDDDD and \\UDDDDDDDD in strings and atoms.
   - ADDED: Added byte-oriented predicates (see `stream_utils.pl`) and
     types (`basic_props:bytelist/1`), using them when needed.
   - ADDED: Added string_bytes/2 predicate. This predicate bidirectionally
     transforms between lists of character codes and lists of bytes
     (using UTF8 encoding/decoding). It is equivalent to =/2 when at
     least one of the lists is already a list of ASCII codes (0..127).
   - ADDED: Stronger redirection predicates (`open_std_redirect/3` and
     `close_std_redirect/1`), which allow the redirection of the standard
     output/error file descriptors together with the default output
     stream, `user_output`, and `user_error`. Added `system:fd_dup/2` and
     `system:fd_close/1` predicates to manipulate POSIX file
     descriptors.
   - ADDED: Added `stream_utils:copy_stream/3` predicate (copies bytes from one
     stream to the other).
   - ADDED: Extended `io_once_port_reify/@{3,4@}` with better redirections
     (subset of process channels available for `process_call/3`).
   - ADDED: Parsing of version strings (`version_strings:version_parse/4`).
   - IMPROVED: Heuristics in `write_assertion/@{6,7@}`, more
     readable output.
   - IMPROVED: Allow JSON values (not only lists) as top argument for
     JSON write and parse.
   - IMPROVED: Faster and more reliable sockets predicates. New
     `socket_sendall/2`, `socket_send_stream/2`, changed
     `socket_send/3`. `socket_recv_code/3` is replaced by
     `socket_recv/3` (treating the returned length is encouraged, do
     not use `socket_recv/2`).
   - IMPROVED: Faster and more robust HTTP libraries (sockets improvements,
     faster IO). Several bug fixes.
   - FIXED: Fixed bug in markdown parser which confused some
     predicate heads with items.
   - FIXED: Allow numbers (as constants) in assertion head arguments.
   - FIXED: Fixed bug in `attrdump.pl` introduced when `assoc` replaced
     `dict`.
   - FIXED: Write blanks before -0.Nan if needed. Fixed some corner cases in
     parser for 0.Inf, 0.Nan .
   - FIXED: Cleanups in messages_basic:messages/1.
   - FIXED: Allow `use_package(tabling)` in a toplevel. 
 - ISO and Portability:
   - ADDED: Implemented `at_end_of_stream/0`,
     `at_end_of_stream/1`. Peek byte in `at_end_of_stream/@{0,1@}` for
     improved ISO compatibility.
   - ADDED: Implemented `peek_byte/1`, `peek_byte/2`.
   - ADDED: Added call_det/2 predicate (compatible with `gprolog`).
   - ADDED: Added forall/2 predicate (compatible with `gprolog`).
   - FIXED: callable/1 is an instantiation check.
   - IMPROVED: Added version_data flag as
     `ciao(Major,Minor,Patch,Extra)`.
   - IMPROVED: Added stream property `type(_)` in `open/4` 
     predicate (for compatibility).
   - ADDED: `--iso-strict` flag in `ciaoc` and `ciaosh` to enable
     stricter compatibility ISO mode by default. Use with care, switching the flag
     will not enforce the recompilation of already compiled user files
     and modules (i.e., .po files).
   - IMPROVED: Additions to the `iso_strict.pl` package (only for code
     using this package):
     - More compatible version of `absolute_file_name/2` (do not
       repeat last path component when resolving library paths).
     - Allow stream aliases in most IO operations.
     - Import `keysort/2` and `format/?` predicates by default.
     - Enable `call/N` by default.
     - Enable `runtime_ops` package by default.  
 - Ciao emacs mode:
   - ADDED: Initial support for `flycheck`, integrating ciaoc,
     ciaopp, lpdoc, and testing.
   - ADDED: Support for company mode (text completion). Manuals
     are located dynamically. Completion list obtained using
     @lib{librowser}.
   - ADDED: Extended Ciao mode (`ciao-emacs-plus.el`) using
     `flycheck` and `company` extensions.
   - ADDED: `M-x ciao-server-start`, `M-x ciao-server-stop` 
     to start/stop the `ciao-serve` process (serving local HTML
     documentation and hub for HTTP based interface to active
     modules).
   - ADDED: Mark and color new `passed`, `failed`,
     `aborted` message types (for unit tests).
   - IMPROVED: Replace outdated `word-help` by
     `info-look`.
   - IMPROVED: Better binding for next error, better code
     highlight, narrow error location.
   - IMPROVED: Allow short location paths (resolved from the
     elisp side). See `bundle_paths:bundle_extend_path/2` for
     details on path extension.
   - IMPROVED: Faster font-lock in `ciao-inferior-mode`, only
     treat keywords.
   - FIXED: Fixed indentation and coloring of `=:=`.
   - FIXED: Fix build errors when compiling using emacs 27.1. 
 - Unit Tests:
   - ADDED: Allow `opt_suff/1` in unit tests (for alternative
     source files, e.g., for flycheck).
   - ADDED: New options for handling tests for stdout and stderr
     in unittest.
   - ADDED: Added support for test filters (see options in
     `run_tests/4`).
   - ADDED: Using 'passed', 'failed', and 'aborted' message types.
   - ADDED: Test timeout.
   - ADDED: Initial support for integrated regression testing
     (see `save`, `compare`, etc. actions in `run_tests/3`).
   - IMPROVED: More flexible quering of results and statistics
     (`get_statistical_summary/2`, `print_statistical_summary/1`,
     `status(S)` option).
   - IMPROVED: Simpler one-line output per test.
   - IMPROVED: Warnings when predicate fails/throws and there were no
     failure/exception properties in test assertion.
   - FIXED: Unittest regression does not depend on Ciao root path.  
 
## [1.19.0] - 2020-3-20 
 Highlights of this release:
 - Build system: optional (weak) dependencies in bundle
   Manifest, out-of-tree builds by default in bundles (sources are
   no longer polluted with .po/.itf files).
 - Language and libraries: a more natural argument order in
   partial applications, clarified semantics of shared variables in
   predicate abstractions (and faster implementation), improvements
   in tabling with constraints (TCLP).
 - System: several fixes in small and large integer operations,
   new algorithm for float to string conversion (based on Ryu,
   fixing the round-trip property), and thread-safe exception
   handling mechanism.
 - Runtime checks and unit tests: major cleanups, fixes, and
   improvements in functionality and efficiency.
 - Top level: cyclic terms detected by default (`check_cycles` 
   flag no longer needed), faster pretty printing of solutions.
 - Ciao emacs mode: 4-space indentation by default. Improvements
   to syntax coloring. Coloring of info manuals generated by
   LPdoc. Connection with ciao-serve. Dropped support for xemacs.
 - Installation: new instructions for Windows based on WSL and
   for Android based on Termux. 
 
 Detailed list of new features and changes:
 - Builder and installation:
   - ADDED: Optional (weak) dependencies in bundle
     Manifests. Weak dependencies allow bundles with conditional
     code which depends on the availability of other bundles.
   - ADDED: Allow gitlab aliases in `ciao get`, i.e.,
     `ciao get gitlab.x.y.z/some/path` will recognize (as a
     heuristic) that we are trying to install a bundle from a Gitlab
     repository. It should work for any instance of gitlab (as long
     as there is public access to the repository).
   - ADDED: Exposed `third-party-install` command (e.g.,
     `ciao third-party-install ciao_ppl.ppl` to install `ppl` 
     3rd-party from `ciao_ppl` bundle).
   - ADDED: Execute `autoreconf -i` when GNU build system is
     selected and the `configure` file is not available
     (3rd-party installer).
   - ADDED: Support for `zsh` shell, unified config in
     `--core:update_shell=[yes|no]` 
   - ENHANCED: Improved interactive `ciao-boot.sh` (reordered
     questions, check deps).
   - EXPERIMENTAL: Support for `--parallel=yes` build option.
   - FIXED: Throw exception if foreign config tool is not found.
   - FIXED: Absolute path in rpath for 3rd-party libs.
   - FIXED: Make sure that `cache/` dir exists before loading
     bundle manifest hooks.
   - FIXED: `ciao clean-tree` works with non-absolute paths.
   - FIXED: Do not assume `/bin/rm` is there, use `TMPDIR` 
     if defined.
   - FIXED: Make `./ciao-boot.sh clean` work even if
     `core` is not built.
   - FIXED: Make sure that building `core` prepares the bin grade
     (`core.ciaobase`). 
 - Language, compiler, and toplevel:
   - ADDED: Out-of-tree builds, enabled by default. Use
     `CIAOCCACHE=0` to disable it. The compilation of modules
     under a bundle produces `.itf` and `.po` files located in
     the build/cache directory of their corresponding workspace.
   - ENHANCED: Using atomic file writes everywhere in the
     compiler. Now several processes may compile simultaneously the
     same code base without corrupting the `.po`/`.itf` 
     compiler output files (there are a few documented bugs in the
     build scripts that should be fixed in the next commits to allow
     parallel systems builds).
   - ENHANCED: New internal representation for predicate
     abstraction, which fixes a potential performance problem due to
     unnecessary renaming of shared variables.
   - ENHANCED: Pretty printing of solutions from the toplevel is
     now orders of magnitude faster for some corner cases.
   - CHANGED: Using standard hiord argument order. This changes
     the argument ordering for predicate abstractions for `hiord` 
     to make it compatible with other systems and languages. The old
     order was implemented to favor 1st argument indexing, but it
     can be confusing because of the difference with other languages
     with higher-order (specially for partial applications).
     Note that was overdue because it was a complicated backwards
     incompatible change that required many changes in the compiler
     and libraries (including the assertion language, parametric
     properties, runtime checks, and parts of the CiaoPP analysis
     framework).
   - CHANGED: Made the semantics of shared variables in
     predicate abstractions more strict. Now only the variables
     specified in `ShVs` for `call((ShVs -> ''(...) :- ...), ...)` 
     will share with the caller's body variables. No other
     variables will be implicitly shared.
   - CHANGED: Conditional compilation is built-in in the
     compiler now. This improves the portability of some
     experimental libraries.
   - EXPERIMENTAL: `string_type` package for native strings.
   - EXPERIMENTAL: a default behavior was defined for
     `ciao-serve` so that it provides the available manuals in
     `index.html` (via the HTTP protocol).
   - FIXED: Make partial application work as expected (e.g.,
     `X=append, Y = X([1]), Y([2], Z)`)
   - FIXED: Syntactic errors in assertion normalization are now
     treated correctly by `c_itf.pl`.
   - FIXED: Program point assertions
     (`check/1`,`true/1`,etc.) are not removed by
     `mexpand.pl` 
   - FIXED: `load_compilation_module/1` was incorrectly
     ignoring modules that were already processed (e.g., compiled)
     but not loaded in the context of `c_itf.pl`.
   - ENHANCED: Pretty printing of solutions detects cyclic terms
     automatically. The `check_cycles` flag is no longer needed
     and has been removed.
   - REMOVED: `check_cycles` flag is no longer needed. 
 - Engine:
   - ADDED: Support for `aarch64` on Android (using Termux).
   - ADDED: Support for 64-bits in tabling libraries. Trie
     adapted to allow the use of big numbers.
   - FIXED: using C `-fno-stack-check` option as a workaround
     for Darwin19/Xcode-11 bug (macOS Catalina)
   - FIXED: Using C `MoveFileEx()` intead of `rename()` in
     Win32.
   - FIXED: Normalize `c_headers_directory()` in Win32.
   - FIXED: Fix C warnings due to casts of integers of different
     sizes.
   - FIXED: Using `__builtin_mul_overflow()` for better
     32-bit/64-bit portability.
   - FIXED: Fix evaluation of right shift with large numbers.
   - FIXED: Fixed round-trip property in float to string and
     string to float conversions. The new code is based on the
     extremely fast Ryu algorithm (see 2018 paper).
   - FIXED: Added `lib(engine)` to `core/Manifest` (this
     ensures that `engine/` files are copied in global
     installations).
   - FIXED: Thread-safe and more efficient reimplementation of
     exceptions.
   - FIXED: Handler for signals that are not intercepted.
   - FIXED: Fix right shift of negative `smallint`. For
     `Shift` in 0..70 and `V=(-1<<57)` or `V=(-1)`, `X
     is V>>Shift` produced `X=0` instead of a negative number. 
 - Runtime checks, testing, and debugging:
   - ADDED: New `timeout/2` property for test assertions (10
     min default).
   - ADDED: New `generate_from_calls_n/2` property for test
     assertions (generate multiple test states from the calls field,
     1 by default).
   - ENHANCED: Major cleanups in `rtchecks` package.
   - CHANGED: Default value of `try_sols/2` is 2 (rather than
     infinite).
   - CHANGED: `unittest` aborts on compilation errors.
   - CHANGED: Replaced `num_solutions('>'(N))` by
     `num_solutions('<'(N))` (due to new hiord).
   - FIXED: rtchecks for exception properties rethrow rtcheck
     error exceptions.
   - FIXED: Added `@@` option also in embedded debugger (see
     `debug` or `trace` package). 
 - Libraries:
   - ADDED: Improvements to TCLP: support for 64 bits, interface
     with the Mod TCLP modular framework, new solver interfaces
     (difference constraints, CLP(Q), CLP(R)), new constraint solver
     over lattices (`abs_new_constraint`), a new framework for
     incremental evaluation of lattice-based aggregates
     (`tclp_aggregates`).
   - ADDED: New `system:get_numcores/1`, obtains the number
     of logical CPU cores.
   - ENHANCED: Updated pretty printing-style formatting of
     clauses and assertions.
   - CHANGED: Inline foreign code feature moved to
     `foreign_inliner` package.
   - CHANGED: Update indentation rules in
     `write:portray_clause/@{1,2@}`.
   - CHANGED: All code ported to the new hiord argument order
     and predicate abstraction sharing rules.
   - FIXED: Bug in fastrw due to wrong integer casting (64-bit).
   - FIXED: `C-c` restarts the toplevel only if it was
     started (for embedded toplevels).
   - FIXED: `bfall` and `afall` search rules compatible
     with more language extensions.
   - FIXED: Add missing parenthesis for `(,)/2` in
     `assrt_write` predicates.
   - FIXED: Fix foreign interface C embedding example.
   - FIXED: Missing cuts, `==/2`, and meta_predicate
     declarations in `assoc.pl`.
   - FIXED: Avoid invalid cross-device link errors in
     `file_buffer.pl` predicates.
   - FIXED: Missing GC roots in `system:extract_paths/2`. 
 - Reference manual:
   - ADDED: Installation instructions for Windows based on WSL
     and for Android based on Termux.
   - ENHANCED: Improved documentation of several libraries
     (`regexp`, `runtime_control`, read).
   - ENHANCED: Separate language conventions from introduction.
   - FIXED: Document that `call_with_time_limit/@{2,3@}` behaves
     as `once/1`.
   - FIXED: Added `classic_predicates.pl` (for classic
     compatibility package) 
 - Ciao emacs mode:
   - ENHANCED: Indentation code rewritten (supporting block
     syntax, better indentation of if-then-else, argument-based
     indentation for columns).
   - ENHANCED: New syntax coloring code (better multiline
     coloring of strings and comments, quoted atoms, doccomments
     blocks, documentation commands, assertion syntax)
   - ENHANCED: Coloring of info manuals generated by LPdoc.
   - ENHANCED: Do not set tty colors for ciao faces (modern
     terminals look great with the default colors).
   - CHANGED: Using 4-space indentation by default.
   - CHANGED: `C-g` clears compilation error marks.
   - CHANGED: Blanks instead of tabs in ciao-mode.
   - CHANGED: Set default emacs init file to
     `~/.emacs.d/init.d` (`~/.emacs.el`, `~/.emacs` are
     still detected if present)
   - EXPERIMENTAL: `M-x ciao-serve` (starts a Ciao server),
     `M-x ciao-dist` (prepares data for a Ciao service).
   - FIXED: `word-help-extract-index` ignore missing indices.
   - FIXED: use the lpdoc toplevel to generate/view buffer
     documentation (instead of a shell).
   - FIXED: Modified error location colors for clarity when
     background is dark.
   - FIXED: Fixing many elisp compilation warnings.
   - REMOVED: Dropped support for xemacs.  
 
## [1.18.0] - 2018-12-06 
 - Backward-incompatible changes in this version:
   - Changed the defaults for modules declared with
     `module/3`. The following predicates and features are no
     longer included by default in module/3. They should be
     enabled explicitly with the following packages or modules:
     - `call/N`: `hiord` package.
     - `data`, `concurrent` declarations,
       `assertz_fact/1`, etc.: `datafacts` package.
     - `dynamic` declarations, `assertz/1`, etc.:
       `dynamic` package.
     - `set_prolog_flag/2`, etc.:
       `engine(runtime_control)` (which merges deprecated
       `engine(prolog_flags)` and `engine(prolog_sys)`).
     - nl/0, nl/1, display/0, open/3, etc.: library(streams)
       (which reexports stream handling and operations, namely
       `engine(stream_basic)` and `engine(io_basic)`). 
   - Added `noprelude` that prevents loading the prelude
     (default definitions).
   - The `pure` package now includes a minimum set of
     control constructs `(,)/2`, `true/0`, `fail/0`. 
 - Language, compiler, toplevel:
   - Major update of the Ciao manual (basic language, language
     extensions, Ciao standard library, additional libraries,
     abstract data types, ISO and compatibility, etc.).
   - Built-in build system and software packaging system
     (*bundles*) (see documentation for details).
   - Added (optional) `CIAOROOT` and `CIAOPATH` 
     environment variables (replace `CIAOLIB`). `CIAOROOT` 
     points to the root of the Ciao sources rather than the lib
     directory.
   - New `ciao-env` command to set up the environment for
     some specific Ciao installations.
   - Fix `MANPATH`,`INFOPATH` in `ciao-env` (trailing
     `:` was incorrectly removed, it is meaningful and
     represents default paths).
   - Fixed `ciaosh -e Goal` (accepts any goal), removed
     `-g` option.
   - DCG `phrase/3` available by default in classic mode
     (toplevel, user modules, and modules declared with
     `module/2`).
   - Fixes in runtime check versions of `mshare/1`,
     `indep/1`, `indep/2`, and `covered/2`.
   - Fixed issues with cyclic terms in debugger (when
     `check_cycles` flag is activated).
   - (experimental) `ciao-serve` command to start a Ciao
     server to serve both HTTP and active module requests. 
 - Ciao emacs mode:
   - Added `M-x ciao-set-ciao-root`, `M-x
     ciao-set-ciao-path` (see `CIAOROOT` and `CIAOPATH` 
     changes).
   - Improved syntax highlighting. 
 - Engine:
   - Fixed a bug while freeing sources in `eng_call/@{3,4@}`.
   - Fixed bug in dynamic/data predicates (uninitialized
     registers may lead to memory corruption during garbage
     collection).
   - Added `ciao_root/1`, replaces `ciao_lib_dir/1`.
   - Improved documentation and examples for interfacing with
     C/C++ (including embedding engines in C/C++ applications). 
 - Libraries:
   - Fixed bug in tokenizer (dealing with `\\^` escape
     sequences in strings).
   - Refurbished HTTP libraries (separated from pillow, see
     documentation).
   - Added `library(io_port_reify)` (like `port_reify` 
     but allows IO redirection).
   - Added `filter/3`, `partition/4`, `maplist/N` to
     `library(hiordlib)`.
   - Added `library(opendoc)` (opens a document with the
     default OS viewer).
   - Revamped active modules model and implementation (see
     documentation for details).
   - Renamed `library(file_utils)` to
     `library(stream_utils)`.
   - Predicates `stream_to_string/@{2,3@}` replaced by
     `read_to_end/@{2,3@}` (which do not close the stream).
   - Added `library(terms_io)` (`terms_to_file/2`,
     `file_to_terms/2`).
   - (experimental) `library(timeout)` 
     (`call_with_time_limit/@{2,3@}`).
   - (experimental) package for traits (interfaces).  
 
@comment{We skip development version 1.17 this time} 
 
## [1.16.0] - 2016-12-31 
 - Engine:
   - Generating the emulator loop with our own code expansion
     and emulator generator (emugen).
   - Refactor, clean up, rewrite some engine parts.
   - Reworking custom engine compilation (under `build/` 
     directory). Ciao headers must be included now using
     `#include <ciao/...>` rather than double quotes.
   - Fix bug in arithmetic shifting operators by 0.
   - Fix `X is (1<<20)*(1<<10)` returned `X=0` (detect
     multiplication overflows using `__builtin_smul_overflow` 
     to avoid C undefined behaviours, i.e., in clang).
   - Fixes in bignums and float to integer conversion in
     64-bits mode.
   - `lib/engine/` merged into `engine/` (no need to
     separate Prolog and C files).
   - 64-bit port, enabled by default (this was a very large
     change which required rewriting some parts of the engine).
   - Adding `--trace-instr` engine option (traces
     instructions, for debugging).
   - Faster implementation of `unify_with_occurs_check/2`.
   - Fix potential overflow in string to number conversion.
   - Better support for `UTF8`.
   - Properly escaping all control characters in quoted atom
     print (ISO compliance).
   - Fix ending of quoted atom and strings (ISO compliance).
   - Fix `get_char/1` (ISO conformance, past end of file).
   - Fix in treatment of `EOF` in IO predicates (do not
     assume `EOF == -1`).
   - `CIAOARCH` replaced by `CIAOOS` (e.g., Linux) and
     `CIAOARCH` (e.g., `i686`). New `ciao_sysconf` 
     command (replaces `ciao_get_arch` script), which accepts
     the arguments `--os`, `--arch`, and `--osarch`. 
 - Portability and OS support:
   - Using `clang` as default C compiler in MacOS.
   - Identify `MINGW64_NT` as Win32 (which is commonly
     accepted as a generic OS name which does not necessarily mean
     32-bits).
   - Drop support for IRIX and SunOS4.
   - Improved support for NetBSD (NetBSD 7), FreeBSD.
   - Support for Raspberry Pi.
   - (experimental) support for MINGW32 and MINGW64 (and MSYS2)
     builds (for Windows).
   - (experimental) support for EMSCRIPTEN as compilation
     target. 
 - Language, compiler, toplevel:
   - Conditional compilation library `library(condcomp)` 
     enabled by default.
   - Deprecated `alias(a(b(...)))` as a module specifier
     name (using the more compatible `alias(a/b/...)` instead).
   - Fix exit status (returns 1) for toplevel and executables
     on abort, e.g., due to uncaught exceptions or unexpected
     failure.
   - New `ciaoc_sdyn` tool to help in the distribution of
     standalone executables with foreign code (collects all
     required dynamic libraries).
   - Starting work on new build system.
   - (experimental) syntax extension for infix dot `(A.B)` 
     (see `set_prolog_flag(read_infix_dot, on)`).
   - (experimental) syntax extension for string data type (see
     `set_prolog_flag(read_string_data_type, on)`). 
 - Libraries:
   - Fix `system:touch/1`, implemented through C
     `utime()`.
   - Fix buffer overflow in `absolute_file_name/?` with
     `nul/NUL` in Win32 (it is a reserved name).
   - Fix bug in check for cyclic terms, implemented faster C
     (low-level) version.
   - Fix `get_tmp_dir/1` so that it always produces a
     normalized path, with no trailing `/`, and considering
     `TMPDIR` on POSIX systems.
   - Better use of `current_executable/1` implementation
     (macOS: `_NSGetExecutablePath()`, Linux: `readlink` on
     `/proc/self/exe`, Windows: `GetModuleFileName()` with
     `hModule = NULL`).
   - Added support for `phrase/2` and `phrase/3` in DCGs
     (in `dcg_phrase` package).
   - Added `library(global_vars)`, backtrackable global
     variables.
   - Added `library(datetime)`, manipulate date and time in
     different formats.
   - Added `library(clpfd)`, new CLP(FD) implementation.
   - Added `library(glob)`, support *glob* patterns,
     filenames with wildcard characters.
   - Added `library(pathnames)`, predicates for file path
     name manipulation, compatible with common semantics in other
     languages.
   - Added `library(port_reify)`, metacalls which reify the
     `exit` port so that it can be delayed.
   - Added `library(process)`, portable high-level
     interface for child process creation, supporting stream
     redirection, background processes, signals, etc.
   - Added `library(text_template)`, text-based templates.
   - Added `library(http_get)`, retrieve files via
     HTTP/HTTPs/FTP protocol.
   - Added `system:get_home/1`,
     `system:find_executable/2`.
   - Added `system:extract_paths/2`, split atom containing a
     colon-separated path list as individual paths.
   - Deprecated `exec/?` from `library(system)`.
   - Deprecated `system:get_exec_dir/1`, can be replaced
     by `current_executable/1` and `path_dirname/2`.
   - (experimental) `library(indexer)`, a package that
     extends first-argument indexing.
   - (experimental) heap limits exceptions
     (`set_heap_limit/1`).
   - (experimental) `library(stream_wait)`, wait for input
     to be available, with timeouts. 
 - Ciao emacs mode:
   - Cleanups, refactoring into smaller individual components
     (highlighting, interaction with Ciao, etc.).
   - `M-x ciao-grep*` emacs command (search over all code). 
 - Foreign interface:
   - Fix exception throw from C builtins during shallow
     backtracking.
   - Allow exception throwing using arbitrary terms.
   - Foreign interface types corresponding to different
     fixed-width C types
     (`c_int`,`c_size`,`c_uint8`, etc.).
 
## [1.14.2] - 2011-08-12 

Merging r13606 (trunk) into 1.14.
This backports an optimization for DARWIN platforms (Jose Morales)
 
## [1.14.1] - 2011-08-10 

Merging r13583 through r13586 (trunk) into 1.14. This fixes problems
in the Windows version of Ciao (Edison Mera, Jose Morales)
 
## [1.15.0] - 2011-07-08 

New development version (Jose Morales)
 
## [1.14.0] - 2011-07-08 

It has been a long while since declaring the last major version
(basically since moving to subversion after 1.10/1.12), so quite a bit
is included in this release. Here is the (longish) summary:
 
 - Extensions to functional notation:
   - Introduced `fsyntax` package (just functional
     syntax). (Daniel Cabeza)
   - Added support to define on the fly a return argument
     different from the default one
     (e.g. `~functor(~,f,2)`). (Daniel Cabeza)
   - Use of '`:- function(defined(true)).`' so that the
     defined function does not need to be preceded by `~` in the
     return expression of a functional clause. (Daniel Cabeza)
   - Functional notation: added to documentation to reflect more
     of the FLOPS paper text and explanations. Added new
     functional syntax examples: arrays, combination with
     constraints, using func notation for properties, lazy
     evaluation, etc. (Manuel Hermenegildo)
   - Added functional abstractions to `fsyntax` and correct
     handling of predicate abstractions (the functions in the body
     where expanded outside the abstraction). (Jose Morales)
   - Improved translation of functions. In particular, old
     translation could lose last call optimization for functions
     with body or with conditional expressions. Furthermore, the
     translation avoids now some superfluous intermediate
     unifications. To be studied more involved
     optimizations. (Daniel Cabeza, Jose Morales).
   - More superfluous unifications taken out from translated code,
     in cases where a goal `~f(X) = /Term/` appears in the
     body. (Daniel Cabeza)
   - Added `library/argnames_fsyntax.pl`: Package to be able to
     use `$~/2` as an operator. (Daniel Cabeza)
   - Added a new example for lazy evaluation, saving memory using
     lazy instead of eager evaluation. (Amadeo Casas) 
 
 - Improvements to signals and exceptions:
   - Distinguished between exceptions and signals. Exceptions are
     thrown and caught (using @pred{throw/1} and @pred{catch/3}).
     Signals are sent and intercepted (using @pred{send_signal/1} 
     and @pred{intercept/3}). (Jose Morales, Remy Haemmerle)
   - Back-port of the (improved) low-level exception handling from
     `optim_comp` branch. (Jose Morales)
   - Fixed @pred{intercept/3} bug, with caused the toplevel to not
     properly handle exceptions after one was handled and
     displayed (bug reported by Samir Genaim on 04 Dec 05, in ciao
     mailing list, subject ```ciao top-level : exception
     handling`''). Updated documentation. (Daniel Cabeza)
   - @pred{intercept/3} does not leave pending choice points if
     the called goal is deterministic (the same optimization that
     was done for @pred{catch/3}). (Jose Morales) 
 
 - New/improved libraries:
   - New `assoc` library to represent association tables.
     (Manuel Carro, Pablo Chico)
   - New `regexp` library to handle regular expressions.
     (Manuel Carro, Pablo Chico)
   - Fixed bug in string_to_number that affected ASCII to
     floating point number conversions (@pred{number_codes/2} 
     and bytecode read). (Jose Morales)
   - `system.pl`: Added predicates @pred{copy_file/2} and
     @pred{copy_file/3}. Added predicates @pred{get_uid/1},
     @pred{get_gid/1}, @pred{get_pwnam/1}, @pred{get_grnam/1} 
     implemented natively to get default user and groups of the
     current process. (Edison Mera)
   - Added library for mutable variables. (Remy Haemmerle)
   - Added package for block declarations (experimental). (Remy
     Haemmerle)
   - Ported CHR as a Ciao package (experimental). (Tom
     Schrijvers)
   - Debugged and improved performance of the CHR library port.
     (Remy Haemmerle)
   - `contrib/math`: A library with several math functions
     that depends on the GNU Scientific Library (GSL). (Edison
     Mera)
   - `io_aux.pl`: Added @pred{messages/1} 
     predicate. Required to facilitate printing of compact
     messages (compatible with emacs). (Edison Mera)
   - Added library `hrtimer.pl` that allow us to measure the
     time using the higest resolution timer available in the
     current system. (Edison Mera)
   - Global logical (backtrackable) variables (experimental).
     (Jose Morales)
   - New dynamic handling (`dynamic_clauses` package). Not
     yet documented. (Daniel Cabeza)
   - Moved `\\=` from `iso_misc` to
     `term_basic`. (Daniel Cabeza)
   - `lib/lists.pl`: Added predicate
     @pred{sequence_to_list/2}. (Daniel Cabeza)
   - `lib/lists.pl`: Codification of @pred{subordlist/2} 
     improved. Solutions are given in other order. (Daniel
     Cabeza)
   - `lib/filenames.pl`: Added
     @pred{file_directory_base_name/3}. (Daniel Cabeza)
   - `library/symlink_locks.pl`: preliminary library to make
     locks a la emacs. (Daniel Cabeza)
   - `lib/between.pl`: Bug in `between/3` fixed: when the
     low bound was a float, an smaller integer was
     generated. (Daniel Cabeza)
   - Fixed bug related to implication operator `->` in Fuzzy
     Prolog (Claudio Vaucheret)
   - `contrib/gendot`: Generator of dot files, for drawing graphs
     using the dot tool. (Claudio Ochoa)
   - Addded `zeromq` library (bindings for the Zero Message
     Queue (ZeroMQ, 0MQ) cross-platform messaging middleware)
     (Dragan Ivanovic)
   - Minor documentation changes in `javall` library (Jesus
     Correas)
   - Fix a bug in calculator `pl2java` example (Jesus
     Correas)
   - `lib/aggregates.pl`: Deleted duplicated clauses of
     @pred{findnsols/4}, detected by Pawel. (Daniel Cabeza)
   - Added library to transform between color spaces (HSL and
     HVS) (experimental). (Jose Morales)
   - Added module qualification in DCGs. (Remy Haemmerle, Jose
     Morales)
   - @pred{prolog_sys:predicate_property/2} behaves similar to
     other Prolog systems (thanks to Paulo Moura for reporting
     this bug). (Jose Morales)
   - Added DHT library (implementation of distributed hash
     table) (Arsen Kostenko)
   - Adding property `intervals/2` in `native_props.pl` 
     (for intervals information) (Luthfi Darmawan)
   - Added code to call polynomial root finding of GSL (Luthfi
     Darmawan)
   - Some improvements (not total, but easy to complete) to
     error messages given by errhandle.pl . Also, some of the
     errors in `sockets_c.c` are now proper exceptions
     instead of faults. (Manuel Carro)
   - `sockets` library: added a library (`nsl`) needed
     for Solaris (Manuel Carro)
   - Driver, utilities, and benchmarking programs from the ECRC
     suite. These are aimed at testing some well-defined
     characteristics of a Prolog system. (Manuel Carro)
   - `library/getopts.pl`: A module to get command-line
     options and values. Intended to be used by Ciao
     executables. (Manuel Carro) 
 
 - Improved ISO compliance:
   - Ported the Prolog ISO conformance testing.
   - Fixed read of files containing single ```%`'' char
     (reported by Ulrich Neumerkel). (Jose Morales)
   - Added exceptions in @pred{=../2}. (Remy Haemmerle)
   - Added exceptions in arithmetic predicates. (Remy
     Haemmerle)
   - Arithmetics integer functions throw exceptions when used
     with floats. (Remy Haemmerle)
   - Added exceptions for resource errors. (Remy Haemmerle) 
 
 - Improvements to constraint solvers:
   - Improved CLPQ documentation. (Manuel Hermenegildo)
   - Added `clp_meta/1` and `clp_entailed/1` to the clpq and clpr
     packages (Samir Genaim):
     - `clp_meta/1`: meta-programming with clp constraints,
       e.g, `clp_meta([A.>.B,B.>.1])`.
     - `clp_entailed/1`: checks if the store entails
       specific cnstraints, e.g, `clp_entailed([A.>.B])` 
       succeeds if the current store entailes `A.>.B`,
       otherwise fails. 
   - Exported the simplex predicates from CLP(Q,R). (Samir Genaim) 
 
 - Other language extensions:
   - Added new `bf/bfall` package. It allows running all
     predicates in a given module in breadth-first mode without
     changing the syntax of the clauses (i.e., no `<-` 
     needed). Meant basically for experimentation and,
     specially, teaching pure logic programming. (Manuel
     Hermenegildo)
   - Added `afall` package in the same line as `bf/bfall` 
     (very useful!). (Manuel Hermenegildo)
   - Improved documentation of `bf` and `af` 
     packages. (Manuel Hermenegildo)
   - Added partial commons-style dialect support, including
     dialect flag. (Manuel Hermenegildo)
   - `yap_compat` and `commons_compat` compatibility
     packages (for Yap and Prolog Commons dialects). (Jose
     Morales)
   - `argnames` package: enhanced to allow argument name
     resolution at runtime. (Jose Morales)
   - A package for conditional compilation of code (`:-
     use_package(condcomp)`). (Jose Morales) 
 
 - Extensions for parallelism (And-Prolog):
   - Low-level support for andprolog library has been taken out
     of the engine and moved to `library/apll` in a similar
     way as the sockets library. We are planning to reduce the
     size of the actual engine further, by taking some
     components out of engine, such as locks, in future
     releases. (Amadeo Casas)
   - Improved support for deterministic parallel goals,
     including some bug fixes. (Amadeo Casas)
   - Goal stack definition added to the engine. (Amadeo Casas)
   - And-parallel code and the definition of goal stacks in the
     engine are now wrapped with conditionals (via
     `AND_PARALLEL_EXECUTION` variable), to avoid the
     machinery necessary to run programs in parallel affects in
     any case the sequential execution. (Amadeo Casas)
   - Stack expansion supported when more than one agent is
     present in the execution of parallel deterministic
     programs. This feature is still in experimental. Support
     for stack expansion in nondeterministic benchmarks will be
     added in a future release. (Amadeo Casas)
   - Support for stack unwinding in deterministic parallel
     programs, via `metachoice`/`metacut`. However,
     garbage collection in parallel programs is still
     unsupported. We are planning to include support for it in
     a future release. (Amadeo Casas)
   - Backward execution of nondeterministic parallel goals made
     via events, without speculation and continuation
     join. (Amadeo Casas)
   - Improved agents support. New primitives included that aim
     at increasing the flexibility of creation and management
     of agents. (Amadeo Casas)
   - Agents synchronization is done now by using locks, instead
     of using `assertz`/`retract`, to improve efficiency
     in the execution of parallel programs. (Amadeo Casas)
   - Optimized version of `call/1` to invoke deterministic
     goals in parallel has been added
     (`call_handler_det/1`). (Amadeo Casas)
   - Optimization: locks/`new_atom` only created when the
     goal is stolen by other process, and not when this is
     pushed on to the `goal_stack`. (Amadeo Casas)
   - Integration with the new annotation algorithms supported
     by CiaoPP, both with and without preservation of the order
     of the solutions. (Amadeo Casas)
   - New set of examples added to the `andprolog` 
     library. (Amadeo Casas)
   - Several bug fixes to remove some cases in execution of
     parallel code in which races could appear. (Amadeo Casas)
   - `andprolog_rt:&` by `par_rt:&` have been moved to
     `native_builtin` (Amadeo Casas)
   - `indep/1` and `indep/2` have been moved to
     `native_props`, as `ground/1`, `var/1`,
     etc. (Amadeo Casas)
   - Added assertions to the `library/apll` and
     `library/andprolog` libraries. (Amadeo Casas)
   - Removed clauses in `pretty_print` for the `&>/2` and
     `<&/1` operators. (Amadeo Casas)
   - Shorter code for `<& / 1` and `<&! / 1` (Manuel
     Carro)
   - Trying to solve some problems when resetting WAM pointers
     (Manuel Carro)
   - Better code to clean the stacks (Manuel Carro) 
 
 - Improvements to foreign (C language) interface:
   - Better support for cygwin and handling of dll libraries in
     Windows. Now usage of external dll libraries are supported
     in Windows under cygwin. (Edison Mera)
   - Improvements to documentation of foreign interface (examples).
     (Manuel Hermenegildo)
   - Allow reentrant calls from Prolog to C and then from C to
     Prolog. (Jose Morales)
   - Fix bug that prevented `ciaoc -c MODULE` from generating
     dynamic `.so` libraries files. (Jose Morales)
   - Fix bug that prevented `ciaoc MODULE && rm MODULE && ciaoc
     MODULE` from emitting correct executables (previously,
     dynamic `.so` libraries files where ignored in executable
     recompilations when only the main file was missing). (Jose
     Morales) 
 
 - Run-Time Checking and Unit Tests:
   - Added support to perfom run-time checking of assertions
     and predicates outside @apl{ciaopp} (see the documentation
     for more details). In addition to those already
     available, the new properties that can be run-time checked
     are: `exception/1`, `exception/2`,
     `no_exception/1`, `no_exception/2`,
     `user_output/2`, `solutions/2`,
     `num_solutions/2`, `no_signal/1`, `no_signal/2`,
     `signal/1`, `signal/2`, `signals/2`,
     `throws/2`. See library
     `assertions/native_props.pl` (Edison Mera)
   - Added support for testing via the @lib{unittest} library.
     Documentation available at
     `library(unittest/unittest)`. (Edison Mera) 
 
 - Profiling:
   - Improved profiler, now it is cost center-based and works
     together with the run-time checking machinery in order to
     also validate execution time-related properties. (Edison
     Mera)
   - A tool for automatic bottleneck detection has been
     developed, which is able to point at the predicates
     responsible of lack of performance in a program. (Edison
     Mera)
   - Improved profiler documentation. (Manuel Hermenegildo) 
 
 - Debugger enhancements:
   - Added the flag `check_cycles` to control whether the
     debugger takes care of cyclic terms while displaying
     goals. The rationale is that to check for cyclic terms
     may lead to very high response times when having big
     terms. By default the flag is in off, which implies that
     a cyclic term in the execution could cause infinite loops
     (but otherwise the debugger is much more speedy). (Daniel
     Cabeza)
   - Show the variable names instead of underscores with
     numbers. Added option `v` to show the variables
     list. Added `v <N>` option, where `N` is the
     `Name` of the variable you like to watch
     (experimental). (Edison Mera)
   - Distinguish between program variables and
     compiler-introduced variables. Show variables modified in
     the current goal. (Edison Mera)
   - `debug_mode` does not leave useless choicepoints (Jose
     Morales) 
 
 - Emacs mode:
   - Made ciao mode NOT ask by default if one wants to set up
     version control when first saving a file. This makes more
     sense if using other version control systems and probably
     in any case (several users had asked for this). There is a
     global customizable variable (which appears in the LPdoc
     area) which can be set to revert to the old behaviour.
     Updated the manual accordingly. (Manuel Hermenegildo)
   - Added possibility of chosing which emacs Ciao should use
     during compilation, by LPdoc, etc. Previously only a
     default emacs was used which is not always the right
     thing, specially, e.g., in Mac OS X, where the
     latest/right emacs may not even be in the paths. Other
     minor typos etc. (Manuel Hermenegildo)
   - Moved the version control menu entries to the LPdoc
     menu. (Manuel Hermenegildo)
   - Updated highlighting for new functional syntax, unit
     tests, and all other new features. (Manuel Hermenegildo)
   - Completed CiaoPP-java environment (menus, buttons, etc.)
     and automated loading when visiting Java files (still
     through hand modification of .emacs). CiaoPP help (e.g.,
     for properties) now also available in Java mode. (Manuel
     Hermenegildo)
   - Changes to graphical interface to adapt better to current
     functionality of CiaoPP option browser. Also some minor
     aesthetic changes. (Manuel Hermenegildo)
   - Various changes and fixes to adapt to emacs-22/23 lisp. In
     particular, fixed cursor error in emacs 23 in Ciao shell
     (from Emilio Gallego). Also fixed prompt in ciaopp and
     LPdoc buffers for emacs 23. (Manuel Hermenegildo)
   - Unified several versions of the Ciao emacs mode (including
     the one with the experimental toolbar in xemacs) that had
     diverged. Sorely needed to be able to make progress
     without duplication. (Manuel Hermenegildo)
   - New version of ciao.el supporting tool bar in xemacs and
     also, and perhaps more importantly, in newer emacsen (>=
     22), where it previously did not work either. New icons
     with opaque background for xemacs tool bar. (Manuel
     Hermenegildo)
   - Using `key-description` instead of a combination of
     `text-char-description` and `string-to-char`. This
     fixes a bug in the Ciao Emacs Mode when running in emacs
     23, that shows wrong descriptions for `M-...` key
     bindings. The new code runs correctly in emacs 21 and 22.
     (Jose Morales)
   - Coloring strings before functional calls and `0'` 
     characters (strings like `"~w"` were colored
     incorrectly) (Jose Morales)
   - `@@begin@{verbatim@}` and `@@include` colored as
     LPdoc commands only inside LPdoc comments. (Jose Morales)
   - Fixed colors for dark backgrounds (workaround to avoid a
     bug in emacs) (Jose Morales)
   - Added an automatic indenter (contrib/plindent) and
     formatting tool, under emacs you can invoque it using the
     keyword `C-c I` in the current buffer containing your
     prolog source. (Edison Mera) 
 
 - Packaging and distribution:
   - User-friendly, binary installers for several systems are
     now generated regularly and automatically: Ubuntu/Debian,
     Fedora/RedHat, Windows (XP, Vista, 7) and MacOSX. (Edison
     Mera, Remy Haemmerle) 
 
 - Improvements in Ciao toplevel:
   - Introduced `check_cycles` `prolog_flag` which
     controls whether the toplevel handles or not cyclic terms.
     Flag is set to false by default (cycles not detected and
     handled) in order to speed up responses. (Daniel Cabeza)
   - Modified @pred{valid_solution/2} so that it asks no
     question when there are no pending choice points and the
     `prompt_alternatives_no_bindings` prolog flag is
     on. (Jose Morales)
   - Now 'Y' can be used as well as 'y' to accept a solution of a
     query. (Daniel Cabeza)
   - Added newline before `true` when displaying empty
     solutions. (Jose Morales)
   - Multifile declarations of packages used by the toplevel were
     not properly handled. Fixed. (Daniel Cabeza)
   - Fixed bug in output of bindings when current output
     changed.
   - Changes so that including files in the toplevel (or loading
     packages) does not invoke an expansion of the ending
     `end_of_file`. This makes sense because the toplevel code is
     never completed, and thus no cleanup code of translations is
     invoked. (Daniel Cabeza) 
 
 - Compiler enhancements and bug fixes:
   - Added a command line option to `ciaoc` for generating code
     with runtime checks. (Daniel Cabeza)
   - Now the compiler reads assertions by default (when using the
     assertion package), and verifies their syntax. (Edison Mera)
   - Added option `-w` to `ciaoc` compiler to generate the
     WAM code of the specified prolog files. (Edison Mera)
   - Fixed bug in exemaker: now when
     @pred{main/0} and @pred{main/1} exists, @pred{main/0} is
     always the program entry (before in modules either could
     be). (Daniel Cabeza)
   - Fixed bug: when compiling a file, if an imported file had no
     itf and it used the redefining declaration, the declaration was
     forgotten between the reading of the imported file (to get
     its interface) and its later compilation. By now those
     declarations are never forgotten, but perhaps it could be
     done better. (Daniel Cabeza)
   - The unloading of files kept some data related to them, which
     caused in some cases errors or warnings regarding module
     redefinitions. Now this is fixed. (Daniel Cabeza)
   - Undefined predicate warnings also for predicate calls
     qualified with current module (bug detected by Pawel
     Pietrzak). (Daniel Cabeza)
   - Fixed bug `debugger_include` (that is, now a change in a
     file included from a module which is debugged is detected
     when the module is reloaded). (Daniel Cabeza)
   - Fixed `a(B) :- _=B, b, c(B)` bug in compilation of
     unification. (Jose Morales) 
 
 - Improving general support for language extensions:
   - Every package starts with '`:- package(...)`' declaration
     now. This allows a clear distinction between packages,
     modules, and files that are just included; all of them using
     the same `.pl` extension. (Jose Morales)
   - Added priority in syntax translations. Users are not required
     to know the details of translations in order to use them
     (experimental: the the correct order for all the Ciao
     packages is still not fixed) (Jose Morales)
   - Now the initialization of sentence translations is done in
     the translation package, when they are added. In this way,
     previous active translations cannot affect the initialization
     of new translations, and initializations are not started each
     time a new sentence translation is added. Additionally, now
     the initialization of sentence translations in the toplevel
     is done (there was a bug). (Daniel Cabeza)
   - Added `addterm(Meta)` meta-data specification for the
     implementation of the changes to provide a correct
     @pred{clause/2} predicate. (Daniel Cabeza)
   - Generalized `addmodule` meta-data specification to
     `addmodule(Meta)`, `addmodule` is now an alias for
     `addmodule(?)`. Needed for the implementation of the
     changes to provide a correct @pred{clause/2} 
     predicate. (Daniel Cabeza) 
 
 - Improvements to system assertions:
   - Added regtype @pred{basic_props:num_code/1} and more
     assertions to `basic_props.pl` (German Puebla)
   - Added trust assertion for
     @pred{atomic_basic:number_codes/2} in order to have more
     accurate analysis info (first argument a number and second
     argument is a list of num_codes) (German Puebla)
   - Added some more binding insensitivity assertions in
     `basic_props.pl` (German Puebla)
   - Added the @pred{basic_props:filter/2} property which is
     used at the global control level in order to guarantee
     termination. (German Puebla)
   - Added `equiv` assertion for @pred{basiccontrol:fail/0} 
     (German Puebla)
   - Modified eval assertion so that partial evaluation does
     not loop with ill-typed, semi-instantiated calls to
     @pred{is/2} (this problem was reported some time ago)
     (German Puebla)
   - Replaced `true` assertions for arithmetic predicates
     with `trust` assertions (`arithmetic.pl`). (German
     Puebla)
   - Added assertions for @pred{term_basic:'\\='/2} (the *not
     unification*) (German Puebla)
   - Added assertions for @pred{lists:nth/3} predicate and
     @pred{lists:reverse/3}. (German Puebla)
   - Changed calls to @pred{atom/1} to @pred{atm/1} in
     @pred{c_itf_props:moddesc/1} (it is a regular type) (Jesus
     Correas)
   - @pred{formulae:assert_body_type/1} switched to `prop`,
     it is not a `regtype`. (Jesus Correas)
   - Added assertions to @pred{atom_concat/2}. (Jesus Correas)
   - Added some assertions to `dec10_io`, `lists`,
     `strings` libraries. (Jesus Correas)
   - Removed `check` from pred and success froom many
     library assertions. (Jesus Correas)
   - Fixed a problem when reading multiple disjunction in
     assertions (`library/formulae.pl` and
     `lib/assertions/assrt_write.pl`). (Pawel Pietrzak)
   - Added/improved assertions in several modules under
     `lib/` (Pawel Pietrzak) 
 
 - Engine enhancements:
   - Added support for Ciao compilation in `ppc64` 
     architecture. (Manuel Carro)
   - `sun4v` added in `ciao_get_arch`. (Amadeo Casas)
   - Solved compilation issue in Sparc. (Manuel Carro, Amadeo
     Casas)
   - Support for 64 bits Intel processor (in 32-bit compatibility
     mode). (Manuel Carro)
   - Switched the default memory manager from linear to the binary
     tree version (which improves management of small memory
     blocks). (Remy Haemmerle)
   - Using `mmap` in Linux/i86, Linux/Sparc and Mac OS X
     (Manuel Carro)
   - A rename of the macro `REGISTER` to `CIAO_REGISTER`.
     There have been reports of the macro name clashing with an
     equally-named one in third-party packages (namely, the PPL
     library). (Manuel Carro)
   - A set of macros `CIAO_REG_n` (`n` currently goes from
     `1` to `4`, but it can be enlarged) to force the GCC
     compiler to store a variable in a register. This includes
     assignments of hardware registers for `n = 1` to `3`,
     in seemingly ascending order of effectiveness. See coments
     in registers.h (Manuel Carro)
   - An assignement of (local) variables to be definitely stored
     in registers for some (not all) functions in the engine --
     notably `wam.c`. These were decided making profiling of C
     code to find out bottlenecks and many test runs with
     different assignments of C variables to registers. (Manuel
     Carro)
   - Changed symbol name to avoid clashes with other third-party
     packages (such as minisat). (Manuel Carro)
   - Fixed a memory alignment problem (for RISC architectures
     where words must be word-aligned, like Sparc). (Jose Morales)
   - Unifying some internal names (towards merge with optim_comp
     experimental branch). (Jose Morales) 
 
 - Attributed variables:
   - Attributes of variables are correctly displayed in the
     toplevel even if they contain cyclic terms. Equations added
     in order to define cyclic terms in attributes are output
     after the attributes, and do use always new variable names
     (doing otherwise was very involved). (Daniel Cabeza)
   - `lib/attrdump.pl`: The library now works for infinite
     (cyclic) terms. (Daniel Cabeza)
   - Changed multifile predicate @pred{dump/3} to
     @pred{dump_constraints/3}. (Daniel Cabeza)
   - Added @pred{copy_extract_attr_nc/3} which is a faster version
     of @pred{copy_extract_attr/3} but does not handle cyclic
     terms properly. (Daniel Cabeza)
   - Added @pred{term_basic:copy_term_nat/2} to copy a term
     taking out attributes. (Daniel Cabeza) 
 
 - Documentation:
   - Added `deprecated/1`. (Manuel Hermenegildo)
   - Improvements to documentation of `rtchecks` and
     tests. (Manuel Hermenegildo)
   - Many updates to manuals: dates, copyrights, etc. Some text
     updates also. (Manuel Hermenegildo)
   - Fixed all manual generation errors reported by LPdoc
     (still a number of warnings and notes left). (Manuel
     Hermenegildo)
   - Adding some structure (minor) to all manuals (Ciao, LPdoc,
     CiaoPP) using new LPdoc `doc_structure/1`. (Jose
     Morales) 
 
 - Ciao Website:
   - Redesigned the Ciao website. It is generated again through
     LPdoc, but with new approach. (Jose Morales)  
 
@comment{note: approximate release date (r7508)} 
## [1.10.8] - 2007-01-28 
 Backports and bug fixes to stable 1.10:
 - Changes to make Ciao 1.10 compile with the latest GCC
   releases.
 - Imported from
   `CiaoDE/branches/CiaoDE-memory_management-20051016`,
   changes from revisions 4909 to 4910: Changes to make Ciao
   issue a better message at startup if the allocated memory
   does not fall within the limits precomputed at compile time
   (plus some code tidying).
 - Port of revisions 5415, 5426, 5431, 5438, 5546, 5547 applied
   to Ciao 1.13 to Ciao 1.10 in order to make it use `mmap()` 
   when possible and to make it compile on newer Linux kernels.
   Tested in Ubuntu, Fedora (with older kernel) and MacOSX.
 - Configuration files for DARWIN (ppc) and 64-bit platforms
   (Intel and Sparc, both in 32-bit compatibility mode).
 - Force the creation of the module containing the foreign
   interface compilation options before they are needed. 
 
## [1.13.0] - 2005-07-03 
 New development version after 1.12. (Jose Morales)
 
## [1.12.0] - 2005-07-03 
 Temporary version before transition to SVN. (Jose Morales)
 
@comment{version: 1.11.247,2004-07-02
    Improved front cover (old authors are now listed as editors, mention UNM,
    new TR number including system version, pointer to
    @tt{www.ciaohome.org}, mention multi-paradigm, etc.). Also changed
    mention of GPL in summary to LGPL.  (Manuel Hermenegildo)
 } 
 
## [1.11.1] - 2003-04-04 
 New development version to begin the builtin modularization (Jose
 Morales)
 
@comment{version: 1.10.1,2003-04-04
    Version skipped (Jose Morales)
 } 
 
@comment{TODO: (pre SVN) missing notes from 1.10.0 to 1.10.7} 
 
## [1.10.0] - 2004-07-29 
 - Classical prolog mode as default behavior.
 - Emacs-based environment improved.
   - Improved emacs inferior (interaction) mode for Ciao and CiaoPP.
   - Xemacs compatibility improved (thanks to A. Rigo).
   - New icons and modifications in the environment for the
     preprocessor.
   - Icons now installed in a separate dir.
   - Compatibility with newer versions of @apl{Cygwin}.
   - Changes to programming environment:
     - Double-click startup of programming environment.
     - Reorganized menus: help and customization grouped in
       separate menus.
     - Error location extended.
     - Automatic/Manual location of errors produced when
       running Ciao tools now customizable.
     - Presentation of CiaoPP preprocessor output improved. 
   - Faces and coloring improved:
     - Faces for syntax-based highlighting more customizable.
     - Syntax-based coloring greatly
       improved. Literal-level assertions also correctly
       colored now.
     - Syntax-based coloring now also working on ASCII
       terminals (for newer versions of emacs).
     - Listing user-defined directives allowed to be colored in
       special face.
     - Syntax errors now colored also in inferior buffers.
     - Customizable faces now appear in the documentation.
     - Added new tool bar button (and binding) to refontify
       block/buffer.
     - Error marks now cleared automatically also when
       generating docs.
     - Added some fixes to hooks in lpdoc buffer.  
 - Bug fixes in compiler.
   - Replication of clauses in some cases (thanks to S. Craig). 
 
 - Improvements related to supported platforms
   - Compilation and installation in different palatforms have been
     improved.
   - New Mac OS X kernels supported. 
 
 - Improvement and bugs fixes in the engine:
   - Got rid of several segmentation violation problems.
   - Number of significant decimal digits to be printed now computed
     accurately.
   - Added support to test conversion of a Ciao integer into a machine
     int.
   - Unbound length atoms now always working.
   - C interface .h files reachable through a more standard location
     (thanks to R. Bagnara).
   - Compatibility with newer versions of gcc. 
 
 - New libraries and utilities added to the system:
   - Factsdb: facts defined in external files can now be automatically
     cached on-demand.
   - Symfnames: File aliasing to internal streams added. 
 
 - New libraries added (in beta state):
   - fd: clp(FD)
   - xml_path: XML querying and transformation to Prolog.
   - xdr_handle: XDR schema to HTML forms utility.
   - ddlist: Two-way traversal list library.
   - gnuplot: Interface to GnuPlot.
   - time_analyzer: Execution time profiling. 
 
 - Some libraries greatly improved:
   - Interface to Tcl/Tk very improved.
     - Corrected many bugs in both interaction Prolog to
       Tcl/Tk and viceversa.
     - Execution of Prolog goals from TclTk revamped.
     - Treatment of Tcl events corrected.
     - Predicate @pred{tcl_eval/3} now allows the execution of Tcl
       procedures running multiple Prolog goals.
     - Documentation heavily reworked.
     - Fixed unification of prolog goals run from the Tcl side. 
   - Pillow library improved in many senses.
     - HTTP media type parameter values returned are always strings
       now, not atoms.
     - Changed verbatim() pillow term so that newlines are translated
       to `<br>`.
     - Changed management of cookies so that special characters in
       values are correctly handled.
     - Added predicate @pred{url_query_values/2}, reversible.
       Predicate @pred{url_query/2} now obsolete.
     - Now attribute values in tags are escaped to handle values
       which have double quotes.
     - Improved @pred{get_form_input/1} and @pred{url_query/2} so
       that names of parameters having unusual characters are always
       correctly handled. 
   - Fixed bug in tokenizer regarding non-terminated single or
     multiple-line comments. When the last line of a file has a
     single-line comment and does not end in a newline, it is accepted
     as correct. When an open-comment /\* sequence is not terminated in
     a file, a syntax error exception is thrown. 
 
 - Other libraries improved:
   - Added native_props to assertions package and included
     @pred{nonground/1}.
   - In atom2terms, changed interpretation of double quoted strings so
     that they are not parsed to terms.
   - Control on exceptions improved.
   - Added @pred{native/1,2} to basic_props.
   - Davinci error processing improved.
   - Foreign predicates are now automatically declared as
     implementation-defined.
   - In lists, added @pred{cross_product/2} to compute the cartesian
     product of a list of lists. Also added
     @pred{delete_non_ground/3}, enabling deletion of nonground terms
     from a list.
   - In llists added @pred{transpose/2} and changed @pred{append/2} 
     implementation with a much more efficient code.
   - The make library has been improved.
   - In persdb, added @pred{pretractall_fact/1} and
     @pred{retractall_fact/1} as persdb native capabilities.
   - Improved behavior with user environment from persdb.
   - In persdb, added support for @pred{persistent_dir/4},
     which includes arguments to specify permission modes for
     persistent directory and files.
   - Some minor updates in persdb_sql.
   - Added treatment of operators and module:pred calls to
     pretty-printer.
   - Updated report of read of syntax errors.
   - File locking capabilities included in @pred{open/3}.
   - Several improvements in library system.
   - New input/output facilities added to sockets.
   - Added @pred{most_specific_generalization/3} and
     @pred{most_general_instance/3} to terms_check.
   - Added @pred{sort_dict/2} to library vndict.
   - The xref library now treats also empty references. 
 
 - Miscellaneous updates:
   - Extended documentation in libraries actmods, arrays,
     foreign_interface, javall, persdb_mysql, prolog_sys, old_database,
     and terms_vars.  
 
@comment{version: 1.9.355,2004-07-02
    Improved front cover (old authors are now listed as editors, mention UNM,
    new TR number including system version, pointer to
    @tt{www.ciaohome.org}, mention multi-paradigm, etc.). Also changed
    mention of GPL in summary to LGPL.  (Manuel Hermenegildo)
 } 
 
@comment{version: 1.9.38,2002-12-12
    Manual now posted in pdf format (since lpdoc now generates much better pdf).
    (Manuel Hermenegildo)
 } 
 
@comment{version: 1.9.34,2002-11-30
    Installation can now be done in Test distribution directory (for testing
    purposes).  (Manuel Hermenegildo)
 } 
 
@comment{version: 1.9.33,2002-11-30
    Modified installation site text to make more explicit the fact that we
    support Mac OS X and XP.  (Manuel Hermenegildo)
 } 
 
## [1.9.0] - 2002-05-16 
 New development version after stable 1.8p0 (MCL, DCG)
 
@comment{TODO: (pre SVN) missing notes from 1.8.0 to 1.8.3} 
 
## [1.8.0] - 2002-05-16 
 - Improvements related to supported platforms:
   - Support for Mac OS X 10.1, based on the Darwin kernel.
   - Initial support for compilation on Linux for Power PC
     (contributed by @author{Paulo Moura}).
   - Workaround for incorrect C compilation while using newer
     (> 2.95) gcc compilers.
   - .bat files generated in Windows. 
 
 - Changes in compiler behavior and user interface:
   - Corrected a bug which caused wrong code generation in some cases.
   - Changed execution of initialization directives. Now the
     initialization of a module/file never runs before the
     initializations of the modules from which the module/file
     imports (excluding circular dependences).
   - The engine is more intelligent when looking for an engine
     to execute bytecode; this caters for a variety of
     situations when setting explicitly the CIAOLIB
     environment variable.
   - Fixed bugs in the toplevel: behaviour of `module:main` 
     calls and initialization of a module (now happens after
     related modules are loaded).
   - Layout char not needed any more to end Prolog files.
   - Syntax errors now disable .itf creation, so that they
     show next time the code is used without change.
   - Redefinition warnings now issued only when an unqualified call
     is seen.
   - Context menu in Windows can now load a file into the toplevel.
   - Updated Windows installation in order to run CGI
     executables under Windows: a new information item is
     added to the registry.
   - Added new directories found in recent Linux distributions to
     INFOPATH.
   - Emacs-based environment and debugger improved:
     - Errors located immediataly after code loading.
     - Improved ciao-check-types-modes (preprocessor progress
       now visible).
     - Fixed loading regions repeatedly (no more predicate
       redefinition warnings).
     - Added entries in @apl{ciaopp} menu to set verbosity of output.
     - Fixed some additional xemacs compatibility issues
       (related to searches).
     - Errors reported by inferior processes are now
       explored in forward order (i.e., the first error
       rewported is the first one highlighted). Improved
       tracking of errors.
     - Specific tool bar now available, with icons for main
       fuctions (works from emacs 21.1 on). Also, other
       minor adaptations for working with emacs 21.1 and
       later.
     - Debugger faces are now locally defined (and better
       customization). This also improves comtability with xemacs
       (which has different faces).
     - Direct access to a common use of the preprocessor
       (checking modes/types and locating errors) from toolbar.
     - Inferior modes for Ciao and CiaoPP improved: contextual
       help turned on by default.
     - Fixes to set-query. Also, previous query now appears
       in prompt.
     - Improved behaviour of stored query.
     - Improved behaviour of recentering, finding errors, etc.
     - Wait for prompt has better termination characteristics.
     - Added new interactive entry points (M-x): ciao,
       prolog, ciaopp.
     - Better tracking of last inferior buffer used.
     - Miscellanous bugs removed; some colors changed to
       adapt to different Emacs versions.
     - Fixed some remaining incompatibilities with xemacs.
     - `:- doc` now also supported and highlighted.
     - Eliminated need for calendar.el
     - Added some missing library directives to fontlock
       list, organized this better.  
 
 - New libraries added to the system:
   - hiord: new library which needs to be loaded in order to use
     higher-order call/N and P(X) syntax. Improved model for predicate
     abstractions.
   - fuzzy: allows representing fuzzy information in the form or
     Prolog rules.
   - use_url: allows loading a module remotely by using a WWW
     address of the module source code
   - andorra: alternative search method where goals which become
     deterministic at run time are executed before others.
   - iterative deepening (id): alternative search method which makes a
     depth-first search until a predetermined depth is reached.
     Complete but in general cheaper than breadth first.
   - det_hook: allows making actions when a deterministic
     situation is reached.
   - ProVRML: read VRML code and translate it into Prolog terms,
     and the other way around.
   - `io_alias_redirection`: change where stdin/stdout/stderr point to
     from within Ciao programs.
   - `tcl_tk`: an interface to Tcl/Tk programs.
   - `tcl_tk_obj`: object-based interface to Tcl/Tk graphical
     objects.
   - CiaoPP: options to interface with the CiaoPP Prolog preprocessor. 
 
 - Some libraries greatly improved:
   - WebDB: utilities to create WWW-based database interfaces.
   - Improved java interface implementation (this forced
     renaming some interface primitives).
   - User-transparent persistent predicate database revamped:
     - Implemented `passerta_fact/1` (`asserta_fact/1`).
     - Now it is never necessary to explicitly call
       `init_persdb`, a call to `initialize_db` is only needed
       after dynamically defining facts of `persistent_dir/2`.
       Thus, `pcurrent_fact/1` predicate eliminated.
     - Facts of persistent predicates included in the
       program code are now included in the persistent
       database when it is created. They are ignored in
       successive executions.
     - Files where persistent predicates reside are now
       created inside a directory named as the module where
       the persistent predicates are defined, and are named
       as F_A\* for predicate F/A.
     - Now there are two packages: `persdb` and `persdb/ll`
       (for low level). In the first, the standard builtins
       `asserta_fact/1`, `assertz_fact/1`, and retract_fact/1
       are replaced by new versions which handle persistent
       data predicates, behaving as usual for normal data
       predicates. In the second package, predicates with
       names starting with 'p' are defined, so that there is
       not overhead in calling the standard builtins.
     - Needed declarations for `persistent_dir/2` are now
       included in the packages. 
   
   - SQL now works with mysql.
   - system: expanded to contain more predicates which act as
     interface to the underlying system / operating system. 
 
 - Other libraries improved:
   - xref: creates cross-references among Prolog files.
   - concurrency: new predicates to create new concurrent
     predicates on-the-fly.
   - sockets: bugs corrected.
   - objects: concurrent facts now properly recognized.
   - fast read/write: bugs corrected.
   - Added 'webbased' protocol for active modules: publication of
     active module address can now be made through WWW.
   - Predicates in library(dynmods) moved to library(compiler).
   - Expansion and meta predicates improved.
   - Pretty printing.
   - Assertion processing.
   - Module-qualified function calls expansion improved.
   - Module expansion calls goal expansion even at runtime. 
 
 - Updates to builtins (there are a few more; these are the most
   relevant):
   
   - Added a prolog_flag to retrieve the version and patch.
   - `current_predicate/1` in `library(dynamic)` now enumerates
     non-engine modules, `prolog_sys:current_predicate/2` no longer
     exists.
   - `exec/*` bug fixed.
   - `srandom/1` bug fixed. 
 
 - Updates for C interface:
   - Fixed bugs in already existing code.
   - Added support for creation and traversing of Prolog data
     structures from C predicates.
   - Added support for raising Prolog exceptions from C
     predicates.
   - Preliminary support for calling Prolog from C. 
 
 - Miscellaneous updates:
   - Installation made more robust.
   - Some pending documentation added.
   - 'ciao' script now adds (locally) to path the place where
     it has been installed, so that other programs can be located
     without being explicitly in the \$PATH.
   - Loading programs is somewhat faster now.
   - Some improvement in printing path names in Windows.  
 
@comment{version: 1.7.203,2002-04-20
    Minor changes to Ciao description.  (Manuel Hermenegildo)
 } 
 
@comment{version: 1.7.155,2001-11-24
    Minor changes to installation scripts to make sure permissions are left correctly
    if installation is aborted.  (Manuel Hermenegildo)
 } 
 
@comment{version: 1.7.154,2001-11-23
    'ciao' script now locally adds CIAOBIN path to PATH if not already present
    (MCL)
 } 
 
@comment{version: 1.7.108,2001-06-02
    Minor bug in main Makefile during uninstallation fixed: added rm -f of engine
    Makefile before linking.  (Manuel Hermenegildo)
 } 
 
@comment{version: 1.7.101,2001-05-15
    Minor error in manual fixed: the section explaining the Ciao name did not
    appear.  (Manuel Hermenegildo)
 } 
 
@comment{version: 1.7.100,2001-05-13
    Added @tt{/usr/share/info} to default @tt{INFOPATH} paths.  (Manuel
    Hermenegildo)
 } 
 
@comment{version: 1.7.87,2001-04-08
    Added @tt{doc} and @tt{install_doc} targets to top level installation @tt{Makefile}
    (can be used to regenerate and reinstall documentation if
    @apl{lpdoc} is available.  (Manuel Hermenegildo)
 } 
 
@comment{version: 1.7.14,2000-08-29
    Updated COMMON to include makefile-sysindep; changed SETLOCAL{CIAOC,CIAOSHELL} to
    SETLOCALCIAO (MCL)
 } 
 
@comment{version: 1.7.12,2000-08-22
    Changed a bug in the installation: the .sta engine was not being copied!
    (MCL)
 } 
 
## [1.7.0] - 2000-07-12 
 Development version following even 1.6 distribution.
 
@comment{TODO: (pre SVN) missing notes from 1.6.0 to 1.6.3} 
 
## [1.6.0] - 2000-07-12 
 - Source-level debugger in emacs, breakpts.
 - Emacs environment improved, added menus for Ciaopp and LPDoc.
 - Debugger embeddable in executables.
 - Standalone executables available for Unix-like operating
   systems.
 - Many improvements to emacs interface.
 - Menu-based interface to autodocumenter.
 - Threads now available in Win32.
 - Many improvements to threads.
 - Modular clp(R) / clp(Q).
 - Libraries implementing And-fair breadth-first and iterative
   deepening included.
 - Improved syntax for predicate abstractions.
 - Library of higher-order list predicates.
 - Better code expansion facilities (macros).
 - New delay predicates (when/2).
 - Compressed object code/executables on demand.
 - The size of atoms is now unbound.
 - Fast creation of new unique atoms.
 - Number of clauses/predicates essentially unbound.
 - Delayed goals with freeze restored.
 - Faster compilation and startup.
 - Much faster fast write/read.
 - Improved documentation.
 - Other new libraries.
 - Improved installation/deinstallation on all platforms.
 - Many improvements to autodocumenter.
 - Many bug fixes in libraries and engine. 
 
@comment{version: 1.5.134,2000-05-09
    Changed location of suite to examples, updated documentation.  (MCL)
 } 
 
@comment{version: 1.5.94,2000-03-28
    The manual intro now provides an overview of the different parts of the
    manual.  (Manuel Hermenegildo)
 } 
 
## [1.5.0] - 1999-11-29

Development version following even 1.4 distribution.
 
## [1.4.0] - 1999-11-27 
 - Documentation greatly improved.
 - Automatic (re)compilation of foreign files.
 - Concurrency primitives revamped; restored &Prolog-like
   multiengine capability.
 - Windows installation and overall operation greatly improved.
 - New version of O'Ciao class/object library, with improved performance.
 - Added support for *predicate abstractions* in `call/N`.
 - Implemented reexportation through reexport declarations.
 - Changed precedence of importations, last one is now higher.
 - Modules can now implicitly export all predicates.
 - Many minor bugs fixed. 
 
## [1.3.0] - 1999-06-16 

Development version following even 1.2 distribution.
 
@comment{TODO: this version does not seem to have been distributed} 
 
## [1.2.0] - 1999-06-14 

Temporary version distributed locally for extensive testing of
reexportation and other 1.3 features.
 
## [1.1.0] - 1999-06-04 

Development version following even 1.0 distribution.
 
@comment{TODO: (pre SVN) missing notes from 1.0.0 to 1.0.7} 
 
## [1.0.0] - 1999-06-04 

 - Added Tcl/Tk interface library to distribution.
 - Added push_prolog_flag/2 and pop_prolog_flag/1 declarations/builtins.
 - Filename processing in Windows improved.
 - Added redefining/1 declaration to avoid redefining warnings.
 - Changed syntax/1 declaration to use_package/1.
 - Added add_clause_trans/1 declaration.
 - Changed format of .itf files such that a '+' stands for all
   the standard imports from engine, which are included in c_itf
   source internally (from engine(builtin_exports)). Further
   changes in itf data handling, so that once an .itf file is
   read in a session, the file is cached and next time it is
   needed no access to the file system is required.
 - Many bugs fixed. 
 
@comment{version: 0.9.32,1999-04-05
 Improved uninstallation makefiles so that (almost) nothing is left
 behind. (Manuel Hermenegildo)
 } 
 
## [0.9.0] - 1999-03-10 

 - Test version before 1.0 release. Many bugs fixed. 
 
@comment{Previously to 0.8, all versions where released as stable.} 
@comment{TODO: (pre SVN) missing notes from 0.8.0 to 0.8.44} 
 
## [0.8.0] - 1998-10-27 

 - Changed compiler so that only one pass is done, eliminated `.dep` 
   files.
 - New concurrency primitives.
 - Changed assertion comment operator to #.
 - Implemented higher-order with call/N.
 - Integrated SQL-interface to external databases with
   persistent predicate concept.
 - First implementation of object oriented programming package.
 - Some bugs fixed. 
 
@comment{TODO: (pre SVN) missing notes from 0.7.0 to 0.7.28} 
 
## [0.7.0] - 1998-09-15 

 - Improved debugger capabilities and made easier to use.
 - Simplified assertion format.
 - New arithmetic functions added, which complete all ISO functions.
 - Some bugs fixed. 
 
@comment{TODO: (pre SVN) missing notes from 0.6.0 to 0.6.18} 
 
## [0.6.0] - 1998-07-16 
 - Defining other path aliases (in addition to 'library') which can
   be loaded dynamically in executables is now possible.
 - Added the posibility to define multifile predicates in the shell.
 - Added the posibility to define dynamic predicates dynamically.
 - Added addmodule meta-argument type.
 - Implemented persistent data predicates.
 - New version of PiLLoW WWW library (XML, templates, etc.).
 - Ported active modules from *distributed Ciao* (independent
   development version of Ciao).
 - Implemented lazy loading in executables.
 - Modularized engine(builtin).
 - Some bugs fixed. 
 
@comment{TODO: (pre SVN) missing notes from 0.5.0 to 0.5.50} 
 
## [0.5.0] - 1998-3-23 
 - First Windows version.
 - Integrated debugger in toplevel.
 - Implemented DCG's as (Ciao-style) expansions.
 - Builtins renamed to match ISO-Prolog.
 - Made ISO the default syntax/package. 
 
@comment{TODO: (pre SVN) missing notes from 0.4.0 to 0.4.12} 
 
## [0.4.0] - 1998-2-24 
 - First version with the new Ciao emacs mode.
 - Full integration of concurrent engine and compiler/library.
 - Added new_declaration/1 directive.
 - Added modular syntax enhancements.
 - Shell script interpreter separated from toplevel shell.
 - Added new compilation warnings. 
 
## [0.3.0] - 1997-8-20 
 - Ciao builtins modularized.
 - New prolog flags can be defined by libraries.
 - Standalone comand-line compiler available, with automatic `make`.
 - Added assertions and regular types.
 - First version using the automatic documentation generator. 
 
## [0.2.0] - 1997-4-16 
 - First module system implemented.
 - Implemented exceptions using catch/3 and throw/1.
 - Added functional & record syntax.
 - Added modular sentence, term, and goal translations.
 - Implemented attributed variables.
 - First CLPQ/CLPR implementation.
 - Added the posibility of linking external .so files.
 - Changes in syntax to allow `P(X)` and `"string"||L`.
 - Changed to be closer to ISO-Prolog.
 - Implemented Prolog shell scripts.
 - Implemented data predicates. 
 
## [0.1.0] - 1997-2-13 

First fully integrated, standalone Ciao distribution. Based on
integrating into an evolution of the &-Prolog
engine/libraries/preprocessor @cite{Hampaper,ngc-and-prolog} many
functionalities from several previous independent development versions
of Ciao
@cite{ciao-prolog-compulog,ciao-ppcp,att-var-iclp,ciao-manual-tr,
ciao-comp-dist-tr-deliv,ciao-ilps95,ciao-jicslp96-ws-update,pillow-ws-dist,
ciao-novascience}.
 
