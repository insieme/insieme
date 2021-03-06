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

# TODO these should be dropped
add_definitions(-DINSIEME_SOURCE_ROOT="${PROJECT_SOURCE_DIR}/")
add_definitions(-DINSIEME_TEST_ROOT="${PROJECT_SOURCE_DIR}/../test/")
add_definitions(-DINSIEME_BUILD_ROOT="${PROJECT_BINARY_DIR}/")

find_program(TIME_EXECUTABLE time)
if(TIME_EXECUTABLE STREQUAL "TIME_EXECUTABLE-NOTFOUND" AND NOT MSVC)
	message(FATAL_ERROR "Unable to locate time utility!")
endif()

configure_file(integration_test_config.in ${PROJECT_BINARY_DIR}/integration_test_config @ONLY IMMEDIATE)
