# CMake Build Type (Debug / Release).
export BUILD_TYPE="${BUILD_TYPE:-Release}"

# Number of cores used for compilation and testing.
export NPROC="${NPROC:-$(nproc)}"

# Location of Third Party Libraries.
export THIRD_PARTY_LIBS="${THIRD_PARTY_LIBS:-$HOME/third_party_libs}"

# Assume Workspace if not set.
CI_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export WORKSPACE="${WORKSPACE:-"$(realpath "$CI_DIR/..")"}"

# Assume Build Directory if not set.
export BUILD_DIR="${BUILD_DIR:-$WORKSPACE/build}"

# Are we running on our CI server?
if [[ "$(hostname)" == "hudson.dps.uibk.ac.at" ]]; then
	export RUNNING_ON_CI_SERVER="1"
else
	export RUNNING_ON_CI_SERVER=""
fi
