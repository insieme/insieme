# CMake Build Type (Debug / Release).
export BUILD_TYPE="${BUILD_TYPE:-Release}"

# Number of cores used for compilation and testing.
export NPROC="${NPROC:-$(nproc)}"

# Set default nice level.
export NICE_LEVEL="${NICE_LEVEL:-10}"

# Location of Third Party Libraries.
export THIRD_PARTY_LIBS="${THIRD_PARTY_LIBS:-$HOME/third_party_libs}"

# Assume Workspace if not set.
CI_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export WORKSPACE="${WORKSPACE:-"$(readlink -f "$CI_DIR/../..")"}"

# Assume Build Directory if not set.
export BUILD_DIR="${BUILD_DIR:-$WORKSPACE/build}"

# Number of Insieme Runtime Workers.
export IRT_NUM_WORKERS="3"

# Are we running on our CI server?
if [[ "$(hostname)" == "hudson.dps.uibk.ac.at" ]]; then
	export RUNNING_ON_CI_SERVER="1"
else
	export RUNNING_ON_CI_SERVER=""
fi
