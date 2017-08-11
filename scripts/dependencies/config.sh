# Name of the third party directory.
export THIRD_PARTY_DIR="third_party"

get_property() {
	echo $(echo "source \"$INSTALLER_DIR/package_$1.sh\" && echo -n \$$2" | bash -)
}
export -f get_property

get_pkg_prefix() {
	(
		source $INSTALLER_DIR/package_$1.sh
		if pkg_is_globally_installed ; then
			echo -n /usr
		else
			echo -n "$PREFIX/$PACKAGE"
		fi
	)
}
export -f get_pkg_prefix
