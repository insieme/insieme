#
# This script can be used to update all license headers in case some
# files are missing those or the license header text has changed.
#

ROOT_DIR=../..

echo "Updating license headers ..."
find $ROOT_DIR/ \( \
	-ipath "$ROOT_DIR/code*.h" -or \
	-ipath "$ROOT_DIR/code*.hpp" -or \
	-ipath "$ROOT_DIR/code*.c" -or \
	-ipath "$ROOT_DIR/code*.cpp" -or \
	-ipath "$ROOT_DIR/code*.cxx" -or \
	-ipath "$ROOT_DIR/code*.cc" -or \
	-ipath "$ROOT_DIR/code*.cl" -or \
	-ipath "$ROOT_DIR/code*.inc" -or \
	-ipath "$ROOT_DIR/code*.def" -or \
	-ipath "$ROOT_DIR/code*.in" \
	\) \
	| xargs -r -n 1 -P 8 ruby ./licensor.rb ./license_header.txt

find $ROOT_DIR/ \( \
	-ipath "$ROOT_DIR/code*.hs" -or \
	-ipath "$ROOT_DIR/code*.chs" \
	\) \
	| xargs -r -n 1 -P 8 ruby ./licensor.rb ./license_header_haskell.txt
