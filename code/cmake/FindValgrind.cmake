# Try to find VALGRIND headers and libraries.
#
# Usage of this module as follows:
#
# find_package(VALGRIND)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# VALGRIND_ROOT Set this variable to the root installation of
# valgrind if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#  VALGRIND_FOUND System has valgrind
#  VALGRIND_INCLUDE_DIR where to find valgrind/memcheck.h, etc.
#  VALGRIND_EXECUTABLE the valgrind executable.

# Get hint from environment variable (if any)
if(NOT VALGRIND_ROOT AND DEFINED ENV{VALGRIND_ROOT})
  set(VALGRIND_ROOT "$ENV{VALGRIND_ROOT}" CACHE PATH "Valgrind base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(VALGRIND_ROOT)
endif()

# Search path for nonstandard locations
if(VALGRIND_ROOT)
  set(VALGRIND_INCLUDE_PATH PATHS "${VALGRIND_ROOT}/include" NO_DEFAULT_PATH)
  set(VALGRIND_BINARY_PATH PATHS "${VALGRIND_ROOT}/bin" NO_DEFAULT_PATH)
endif()

find_path(VALGRIND_INCLUDE_DIR valgrind HINTS ${VALGRIND_INCLUDE_PATH})

find_program(VALGRIND_EXECUTABLE NAMES valgrind PATH ${VALGRIND_BINARY_PATH}) 

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(VALGRIND DEFAULT_MSG
    VALGRIND_INCLUDE_DIR
    VALGRIND_EXECUTABLE)

mark_as_advanced(VALGRIND_INCLUDE_DIR VALGRIND_EXECUTABLE)
