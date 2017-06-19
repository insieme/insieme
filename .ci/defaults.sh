# CMake Build Type (Debug / Release).
export BUILD_TYPE="${BUILD_TYPE:-Release}"

# Number of cores used for compilation and testing.
export NPROC="${NPROC:-$(nproc)}"

# Number of Insieme Runtime Workers.
export IRT_NUM_WORKERS="3"

# Location of Third Party Libraries.
export THIRD_PARTY_LIBS="${THIRD_PARTY_LIBS:-$HOME/third_party_libs}"

# Assume Workspace if not set.
export WORKSPACE="${WORKSPACE:-..}"

# Assume Build Directory if not set.
export BUILD_DIR="${BUILD_DIR:-$WORKSPACE/build}"
