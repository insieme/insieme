# Default install location.
export PREFIX="${PREFIX:-${THIRD_PARTY_LIBS:-$HOME/third_party_libs}}"

# Name of the third party directory.
export THIRD_PARTY_DIR="${THIRD_PARTY_DIR:-third_party}"

# Default compile flags.
export CFLAGS="${CFLAGS:-"-mtune=native -O3"}"
export CXXFLAGS="${CXXFLAGS:-"-mtune=native -O3"}"
export LDLAGS="${LDLAGS:-"-mtune=native -O3"}"

# Number of parallel jobs.
export SLOTS="${SLOTS:-$(nproc)}"

# Override Compiler.
#GCC_PKG
#export CC="$GCC_PKG/bin/gcc"
#export CXX="$GCC_PKG/bin/g++"
#export PATH="$GCC_PKG/bin:$PATH"
#export LD_LIBRARY_PATH="$GCC_PKG/lib64"
