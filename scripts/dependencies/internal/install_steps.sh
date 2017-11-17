# This file contains the install steps for all packages.

# Installs a given package.
install_pkg() {
	local name=$1
	(
		set -e

		source "$INSTALLER_DIR/internal/default_implementation.sh"
		load_pkg $name

		mkdir -p $PREFIX

		PKG_TEMP=$(mktemp -d)
		pushd $PKG_TEMP

		echo "####   Downloading $PACKAGE   ####"
		pkg_download

		echo "####   Extracting  $PACKAGE   ####"
		pkg_extract

		pushd $PACKAGE

		echo "####   Preparing   $PACKAGE   ####"
		pkg_prepare

		echo "####   Configuring $PACKAGE   ####"
		pkg_configure

		echo "####   Building    $PACKAGE   ####"
		pkg_build

		echo "####   Checking    $PACKAGE   ####"
		pkg_check

		echo "####   Installing  $PACKAGE   ####"
		pkg_install

		popd

		pkg_install_done

		echo "####   Cleaning    $PACKAGE   ####"
		pkg_cleanup

		popd

		rmdir $PKG_TEMP
	)
}
