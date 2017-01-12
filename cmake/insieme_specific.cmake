# Set C backend compiler
if(NOT INSIEME_C_BACKEND_COMPILER)
	message(WARNING "INSIEME_C_BACKEND_COMPILER not set, defaulting to gcc in PATH")
	set(INSIEME_C_BACKEND_COMPILER "gcc")
endif()
add_definitions(-DINSIEME_C_BACKEND_COMPILER_CMAKE="${INSIEME_C_BACKEND_COMPILER}")

# Set C++ backend compiler
if(NOT INSIEME_CXX_BACKEND_COMPILER)
	message(WARNING "INSIEME_CXX_BACKEND_COMPILER not set, defaulting to g++ in PATH")
	set(INSIEME_CXX_BACKEND_COMPILER "g++")
endif()
add_definitions(-DINSIEME_CXX_BACKEND_COMPILER_CMAKE="${INSIEME_CXX_BACKEND_COMPILER}")

# TODO this needs to be dropped
add_definitions(-DINSIEME_LIBS_HOME="$ENV{INSIEME_LIBS_HOME}/")

# TODO this should be dropped too
add_definitions(-DINSIEME_BUILD_ROOT="${PROJECT_BINARY_DIR}/")

add_definitions(-DINSIEME_VERSION="${GIT_REF}")
add_definitions(-DINSIEME_BRANCH="${GIT_BRANCH}")
