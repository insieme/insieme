# Try to find SQLITE headers and libraries.
#
# Usage of this module as follows:
#
# find_package(SQLITE)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# SQLITE_ROOT Set this variable to the root installation of
# sqlite if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# SQLITE_FOUND System has SQLITE libraries and headers
# SQLITE_LIBRARIES The SQLITE library
# SQLITE_INCLUDE_DIRS The location of SQLITE headers

# Get hint from environment variable (if any)
if(NOT SQLITE_ROOT AND DEFINED ENV{SQLITE_ROOT})
  set(SQLITE_ROOT "$ENV{SQLITE_ROOT}" CACHE PATH "SQLITE base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(SQLITE_ROOT)
endif()

# Search path for nonstandard locations
if(SQLITE_ROOT)
  set(SQLITE_INCLUDE_PATH PATHS "${SQLITE_ROOT}/include" NO_DEFAULT_PATH)
  set(SQLITE_LIBRARY_PATH PATHS "${SQLITE_ROOT}/lib" NO_DEFAULT_PATH)
endif()


find_path(SQLITE_INCLUDE_DIRS NAMES sqlite3.h HINTS ${SQLITE_INCLUDE_PATH})
find_library(SQLITE_LIBRARIES NAMES sqlite3 HINTS ${SQLITE_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(SQLITE DEFAULT_MSG SQLITE_LIBRARIES SQLITE_INCLUDE_DIRS)

mark_as_advanced(SQLITE_LIBRARIES SQLITE_INCLUDE_DIRS)
