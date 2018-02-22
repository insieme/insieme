get_pkg_path() {
	local path=$1
        name=$(basename $path)
        dir=$(dirname $path)
        printf '%s\n' "$INSTALLER_DIR/$dir/package_$name.sh"
}


# Loads a given package file into the current shell.
load_pkg() {
	source "$(get_pkg_path "$1")"
}

# Get a list of all available packages.
get_pkg_list() {
	find "$INSTALLER_DIR" -maxdepth 2 -name "package_*.sh" \
		-exec realpath --relative-to "$INSTALLER_DIR" {} \; \
		| sed -e 's/package_\([^/]*\)\.sh$/\1/'
}

# Returns a property defined in a given package file.
get_property() {
	local name=$1
	local property=$2
	(
                set +x
		load_pkg $name
		eval "echo -n \$$property"
	)
}

# Indicates whether the given package is installed.
is_pkg_installed() {
	local name=$1
	(
		load_pkg $name
		[[ -f "$PREFIX/$PACKAGE/.installed" ]]
	)
}

# Aborts if no package with the given name is found.
assert_pkg_exists() {
	local name=$1
	if [[ ! -f "$(get_pkg_path "$1")" ]]; then
		>&2 echo "Error: No file package_$name.sh found."
		exit 1
	fi
}

# Gets all dependencies of the given packages.
resolve_dependencies() {
	local names=$@
	for pkg in $names; do
		resolve_dependencies_for_pkg "$pkg"
	done | awk '!unique[$_]++'
}

# Recursively get dependencies of the given package.
resolve_dependencies_for_pkg() {
	local name=$1
	for pkg in $(get_property $name DEPENDS); do
		resolve_dependencies_for_pkg "$pkg"
	done
	echo "$name"
}
