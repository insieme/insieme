NAME="ghc"
VERSION="8.0.2"
PACKAGE="$NAME-$VERSION"

FILE="$PACKAGE.tar.xz"

declare -A URL
URL=(
	["CentOS release 6.6 (Final)"]="https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-centos67-linux.tar.xz"
	["default"]="https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-deb8-linux.tar.xz")

declare -A SHA256SUM
SHA256SUM=(
	["CentOS release 6.6 (Final)"]="145f12323c869f65c79f7c2d2b6dadfd3d9bad8739ef56580b38f715a6dbbb76"
	["default"]="5ee68290db00ca0b79d57bc3a5bdce470de9ce9da0b098a7ce6c504605856c8f")

KEY="$(cat /etc/centos-release 2>/dev/null || echo default)"


DEPENDS="gmp"

GMP_PKG=$(get_property gmp PACKAGE)

pkg_download() {
	if [ -z "${URL["$KEY"]}" ]; then
		KEY="default"
	fi

	wget -nc "${URL["$KEY"]}" -O "$FILE"
	if [[ "${SHA256SUM["$KEY"]}" ]]; then
		echo "${SHA256SUM["$KEY"]}  $FILE" | sha256sum -c
	fi
}

pkg_configure() {
	./configure --prefix="$PREFIX/$PACKAGE" \
		--with-gmp-includes="$PREFIX/$GMP_PKG/include" \
		--with-gmp-libraries="$PREFIX/$GMP_PKG/lib"
}

pkg_build() {
	true
}
