#!/usr/bin/env bash

# Exit immediately if a simple command exits with a non-zero status
set -e

# ---------------------------------------------------------------------------

# Follow symbolic links
self_exec=`test -L "$0" && readlink "$0" || echo "$0"`
# Obtain the directory where this script is located
old_dir=`pwd`; cd `dirname ${self_exec}`; self=`pwd`; cd ${old_dir}; old_dir=

# ---------------------------------------------------------------------------
# Build a Ciao engine for Emscripten

# Directory for Emscripten output
distdir=$self/dist

# Guess CIAOROOT
ciaoc=`which ciaoc`
if [ ! -x "$ciaoc" ]; then
    cat <<EOF
ERROR: Cannot locate ciaoc binary. Please make sure that a Ciao (from
source) installation is available.  
EOF
    exit 1
fi
old_dir=`pwd`; cd `dirname "$ciaoc"`/../..; CIAOROOT=`pwd`; cd ${old_dir}; old_dir=

ENGNAME=ciaoenginejs
BLD_ENGDIR=${self}/../build/eng/$ENGNAME
ENG_CFG=LINUXx86_JS

# (top bundle -- do not include anything, just the bundlereg)
regdir_ciao=${CIAOROOT}/build/bundlereg
srcdir_ciao=${CIAOROOT}
srcs_ciao=""

# TODO: get automatically
regdir_core=${CIAOROOT}/build/bundlereg
srcdir_core=${CIAOROOT}/core
srcs_core="\
engine/internals \
engine/exceptions_db \
engine/attributes \
engine/attributed_variables \
engine/term_basic \
engine/arithmetic \
engine/atomic_basic \
engine/basic_props \
engine/basiccontrol \
engine/exceptions \
engine/io_aux \
engine/io_basic \
engine/prolog_flags \
engine/streams_basic \
engine/system_info \
engine/term_compare \
engine/term_typing \
engine/hiord_rt \
engine/debugger_support \
engine/data_facts \
engine/meta_inc \
lib/pure/pure \
lib/nonpure \
lib/prelude \
lib/default \
lib/default_for_ciaosh \
lib/default_predicates \
lib/bundle/bundlereg_db \
lib/bundle/bundlereg_load \
lib/bundle/paths_extra \
lib/bundle/doc_flags \
lib/bundle/bundle_info \
lib/bundle/bundle_params \
lib/bundle/bundle_flags \
lib/define_flag \
lib/errhandle \
lib/runtime_ops/runtime_ops_tr \
lib/aggregates \
lib/dynamic \
lib/tokenize \
lib/read \
lib/write \
lib/operators \
lib/iso_char \
lib/iso_misc \
lib/metatypes/metatypes \
lib/assertions/assertions \
lib/assertions/doc_props \
lib/assertions/native_props \
lib/assertions/assrt_lib \
lib/assertions/assrt_lib_extra \
lib/assertions/assertions_props \
lib/assertions/assrt_write \
lib/assertions/c_itf_props \
lib/isomodes/isomodes \
lib/basicmodes/basicmodes \
lib/regtypes/regtypes \
lib/format \
lib/lists \
lib/messages \
lib/sort \
lib/sets \
lib/between \
lib/system \
lib/prolog_sys \
lib/pathnames \
lib/dec10_io \
lib/old_database \
lib/ttyout \
lib/streams \
lib/dict \
lib/terms \
lib/dynmod_holder \
lib/hiord \
lib/apply \
lib/nortchecks \
lib/unittestdecls \
lib/debugger/debugger \
lib/debugger/debugger_lib \
lib/metatypes/metatypes_tr \
lib/regtypes/regtypes_tr \
lib/rtchecks/rtchecks_tr \
lib/rtchecks/rtchecks_utils \
lib/inliner/inliner_tr \
lib/dcg/dcg \
lib/dcg/dcg_ops \
lib/dcg/dcg_tr \
lib/dcg/dcg_phrase_rt \
lib/condcomp/condcomp \
lib/condcomp/condcomp_tr \
lib/runtime_ops/runtime_ops \
lib/file_utils \
lib/fsmemo/fsmemo \
lib/fsmemo/fsmemo_defs \
lib/fsmemo/fsmemo_rt \
lib/ctrlcclean \
lib/strings \
lib/port_reify \
lib/fastrw \
lib/read_from_string \
lib/varnames/complete_dict \
lib/varnames/dict_types \
lib/varnames/apply_dict \
lib/compiler/compiler \
lib/compiler/c_itf \
lib/compiler/c_itf_internal \
lib/compiler/exemaker \
lib/compiler/mexpand \
lib/compiler/translation \
lib/compiler/pl2wam \
lib/compiler/srcdbg \
lib/compiler/unused_pred_warnings \
lib/compiler/global_module_options \
lib/compiler/compressed_bytecode \
lib/compiler/basic_compilation_modules \
lib/process/process \
lib/process/process_channel \
lib/format_to_string \
lib/foreign_interface/build_foreign_interface \
library/logged_process \
library/source_tree/source_tree \
library/emacs/emacs_batch \
library/syntax_highlight/syntax_highlight \
library/syntax_highlight/css/ciao-htmlfontify.css \
library/doccomments/doccomments \
library/doccomments/doccomments_tr \
library/pillow/html \
library/pillow/ops \
library/pillow/pillow_aux \
library/pillow/pillow_types \
library/hiordlib \
library/terms_vars \
library/glob \
library/regexp/regexp \
library/regexp/regexp_code \
library/regexp/regexp_trans \
library/vndict \
library/pretty_print \
library/system_extra \
library/nativeprops \
library/idlists \
library/datetime \
library/layout_dcg/layout_dcg \
library/layout_dcg/layout_dcg_rt \
library/fsyntax/fsyntax \
library/fsyntax/ops \
library/fsyntax/functional_defs \
library/fsyntax/fsyntax_doc \
library/fsyntax/functionstr \
library/fsyntax/functional_basics \
library/markdown/markdown_translate \
library/markdown/markdown_syntax \
library/markdown/markdown_parser \
library/terms_check"
regdir_testsuite= # no bundlereg
srcdir_testsuite=${CIAOROOT}/core
srcs_testsuite="\
tests/suite/bignums \
tests/suite/fibonacci \
tests/suite/guardians \
tests/suite/jugs \
tests/suite/knights \
tests/suite/memsize \
tests/suite/queens \
tests/suite/suite \
tests/suite/robot"
regdir_lpdoc=${CIAOROOT}/build/bundlereg
srcdir_lpdoc=${CIAOROOT}/lpdoc
srcs_lpdoc="\
lib/doccfg \
lib/doccfg_defs \
lib/doccfg_props \
lib/doccfg_tr \
lib/SETTINGS_DEFAULT \
lib/lpdoc.css \
src/autodoc \
src/autodoc_aux \
src/autodoc_bibrefs \
src/autodoc_doctree \
src/autodoc_errors \
src/autodoc_filesystem \
src/autodoc_html \
src/autodoc_html_assets \
src/autodoc_html_template \
src/autodoc_images \
src/autodoc_index \
src/autodoc_man \
src/autodoc_parse \
src/autodoc_refsdb \
src/autodoc_settings \
src/autodoc_state \
src/autodoc_structure \
src/autodoc_texinfo \
src/comments \
src/doccfg_holder \
src/docmaker \
src/lpdoc_install \
src/pbundle_download \
src/version_auto"
regdir_builder=${CIAOROOT}/build/bundlereg
srcdir_builder=${CIAOROOT}/builder
srcs_builder="\
src/config_common \
src/pbundle_meta"

function build_engine() {
    EMCC=`which emcc`
    if [ ! -x "${EMCC}" ]; then
	cat <<EOF
Missing 'emcc'. 

Please update your environment (e.g., source emsdk_portable/emsdk_env.sh).
Remember to install Emscripten SDK.
EOF
	exit 1
    fi

    ciao build ciaojs/enginejs
}


# ===========================================================================

# ---------------------------------------------------------------------------
# Create the main Ciao executable

CIAOEXEC=ciaojs
function build_exec() {
    if [ x"${CIAOEXEC}" = x"" ]; then
	echo "Please specify some program to compile"
	return
    fi
    
    # echo "Compiling (statically) ${CIAOEXEC}"
    cd ${self}/src
    ciaoc -x -s ${CIAOEXEC}
}

# Do from prolog:
#   use_module(library(source_tree/bundle_source_tree), [bundle_contents/3]).
#   bundle_contents(core, [distributable_precomp(bin)], ModPath), display(ModPath), nl, fail; true.

# Pack and install a bundle
function install_bundle() { # BUNDLE
    local bundle=$1
    local srcdir
    eval srcdir=\$srcdir_$bundle
    local regdir
    eval regdir=\$regdir_$bundle

    echo "Packing $bundle"
    mkdir -p "$distdir"
    mkdir -p "$distdir"/"$bundle".bundle
    cat > "$distdir"/"$bundle".bundle.js <<EOF
// Preload modules and sources
var Ciao;
if (typeof Ciao === 'undefined') Ciao = eval('(function() { try { return Ciao || {} } catch(e) { return {} } })()');
if (!Ciao.requires) Ciao.requires = [];
Ciao.requires.push('$bundle');
(function () {
  var bundle = {};
  Ciao.bundle['$bundle'] = bundle;
  bundle.regdir = '$regdir';
  bundle.srcdir = '$srcdir';
  bundle.srcurl = '${bundle}.bundle';
  bundle.preload = function () {
EOF
    if [ x"$regdir" != x"" ]; then
	cat >> "$distdir"/"$bundle".bundle.js <<EOF
    Ciao.preload_file(bundle, 'bundlereg', '$bundle.bundlereg');
EOF
	cp "$regdir/$bundle".bundlereg "$distdir"/"$bundle".bundlereg
	if [ -r "$regdir/$bundle".bundlecfg ]; then
	    cat >> "$distdir"/"$bundle".bundle.js <<EOF
    Ciao.preload_file(bundle, 'bundlereg', '$bundle.bundlecfg');
EOF
	    cp "$regdir/$bundle".bundlecfg "$distdir"/"$bundle".bundlecfg
	fi
    fi
    local contents
    eval contents=\$srcs_$bundle
    for i in $contents; do
	d=`dirname "$i"`
	mkdir -p "$distdir"/"$bundle".bundle/"$d"
	if [ -r "$srcdir/$i" ]; then
	    cat >> "$distdir"/"$bundle".bundle.js <<EOF
    Ciao.preload_file(bundle, 'raw', '$i');
EOF
	    cp "$srcdir/$i" "$distdir"/"$bundle".bundle/"$i"
	elif [ -r "$srcdir/$i".itf ] && [ -r "$srcdir/$i".po ]; then
	    cat >> "$distdir"/"$bundle".bundle.js <<EOF
    Ciao.preload_file(bundle, 'mod', '$i');
EOF
	    cp "$srcdir/$i".pl "$distdir"/"$bundle".bundle/"$i".pl
	    cp "$srcdir/$i".po "$distdir"/"$bundle".bundle/"$i".po
	    cp "$srcdir/$i".itf "$distdir"/"$bundle".bundle/"$i".itf
	else
	    cat >> "$distdir"/"$bundle".bundle.js <<EOF
    Ciao.preload_file(bundle, 'src', '$i');
EOF
	    cp "$srcdir/$i".pl "$distdir"/"$bundle".bundle/"$i".pl
	fi
    done
    cat >> "$distdir"/"$bundle".bundle.js <<EOF
  };
})();
EOF
}
##     FILE_PACKAGER=`$EMCC/../tools/file_packager.py`
##     if [ ! -r "${FILE_PACKAGER}" ]; then
## 	cat <<EOF
## Missing 'file_packager.py'. 
## 
## Please update your environment (e.g., source emsdk_portable/emsdk_env.sh).
## Remember to install Emscripten SDK.
## EOF
## 	exit 1
##     fi
##     
##     python "${FILE_PACKAGER}" core.bundle.data --lz4 --preload /Users/jfran/Documents/git/ciao-devel/core --js-output=core.bundle.js

function do_install() {
    mkdir -p "$distdir"
    local outjs="$distdir"/ciao.js

    # Put together ciao.js and the ASMJS compiled engine
    cp ${self}/js/ciao.js "$outjs"
    cat "${self}/js/pre-js.js" >> "$outjs"
    cat "$BLD_ENGDIR"/objs/${ENG_CFG}/$ENGNAME.js >> "$outjs"
    cat "${self}/js/post-js.js" >> "$outjs"
    # Copy engine .js.mem and ciaojs bytecode
    cp "$BLD_ENGDIR"/objs/${ENG_CFG}/$ENGNAME.js.mem "$distdir"
    cp "${self}"/src/"${CIAOEXEC}" "$distdir"

    # Pack and install bundles
    install_bundle ciao
    install_bundle core
    install_bundle testsuite
    install_bundle builder
    install_bundle lpdoc
}

# ---------------------------------------------------------------------------

case $1 in
    "build") build_engine && build_exec ;;
    "build_engine") build_engine ;;
    "build_exec") build_exec ;;
    "install") do_install ;;
    *) echo "Usage: build.sh build|build_engine|build_exec|install"
esac    

