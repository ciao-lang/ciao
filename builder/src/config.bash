# ---------------------------------------------------------------------------
# Configuration variables

# cache_dir=${HOME}/.ciao-cache
cache_dir=$ciaoroot/build/oc-cache
# Set the directory where executables go
bin_dir=${cache_dir}/bin

tmpcomp_dir=${cache_dir}/tmpcomp

# ---------------------------------------------------------------------------
# Custom procedures and post-initialization of variables computed from
# the configuration variables

ensure_bin_dir() {
    mkdir -p "${bin_dir}"
}

# ---------------------------------------------------------------------------
# Regression data

regression_name="regr-db"
regression_base="$ciaoroot"
regression_dir="${regression_base}/${regression_name}"

ensure_regression_dir() {
    if [ -r "${regression_dir}/.git" ]; then
	true
    else
	cat <<EOF
The regression database directory \`${regression_dir}' is not found or
does not seem to be cloned. Please, initialize submodules.
EOF
	exit -1
    fi
}

