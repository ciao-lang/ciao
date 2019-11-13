# ---------------------------------------------------------------------------
# Configuration variables

# Ciao bin dir
bin_dir=$ciaoroot/build/bin
# cache dir
cache_dir=$ciaoroot/build/oc-cache
# Set the directory where executables go at cache dir
cache_bin_dir=$cache_dir/bin
# temporary compiler steps
tmpcomp_dir=$cache_dir/tmpcomp

# ---------------------------------------------------------------------------
# Custom procedures and post-initialization of variables computed from
# the configuration variables

ensure_cache_bin_dir() {
    mkdir -p "${cache_bin_dir}"
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

