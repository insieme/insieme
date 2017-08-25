# Loads a given package file into the current shell.
#  $1 = package name
load_pkg() {
	source "$INSTALLER_DIR/package_$1.sh"
}

# Get a list of all available packages.
get_pkg_list() {
	find "$INSTALLER_DIR" -maxdepth 1 -name "package_*.sh" \
		-exec basename {} \; \
		| sed -e "s/^package_//" -e "s/\.sh$//"
}

# Returns a property defined in a given package file.
#  $1 = package name
#  $2 = property
get_property() {
	(
		load_pkg $1
		eval "echo -n \$$2"
	)
}

# Indicates whether the given package is installed.
#  $1 = package name
is_pkg_installed() {
	(
		load_pkg $1
		[[ -f "$PREFIX/$PACKAGE/.installed" ]]
	)
}

# Aborts if no package with the given name is found.
#  $1 = package name
assert_pkg_exists() {
	if [[ ! -f "$INSTALLER_DIR/package_$1.sh" ]]; then
		>&2 echo "Error: No file package_$1.sh found."
		exit 1
	fi
}

# Gets all dependencies of the given packages.
#  $1 = list of package names
resolve_dependencies() {
	for pkg in $1; do
		resolve_dependencies_for_pkg "$pkg"
	done | awk '!unique[$_]++'
}

# Recursively get dependencies of the given package.
#  $1 = package name
resolve_dependencies_for_pkg() {
	for pkg in $(get_property $1 DEPENDS); do
		resolve_dependencies_for_pkg "$pkg"
	done
	echo "$1"
}
