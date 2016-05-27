# Try to find ZLIB headers and libraries.
#
# Usage of this module as follows:
#
# find_package(ZLIB)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# ZLIB_ROOT Set this variable to the root installation of libzlib if the module
# has problems finding the proper installation path.
#
# Variables defined by this module:
#
# ZLIB_FOUND System has ZLIB libraries and headers
# ZLIB_LIBRARIES The ZLIB library
# ZLIB_INCLUDE_DIRS The location of ZLIB headers

# Get hint from environment variable (if any)
if(NOT ZLIB_ROOT AND DEFINED ENV{ZLIB_ROOT})
  set(ZLIB_ROOT "$ENV{ZLIB_ROOT}" CACHE PATH "ZLIB base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(ZLIB_ROOT)
endif()

# Search path for nonstandard locations
if(ZLIB_ROOT)
  set(ZLIB_INCLUDE_PATH PATHS "${ZLIB_ROOT}/include" NO_DEFAULT_PATH)
  set(ZLIB_LIBRARY_PATH PATHS "${ZLIB_ROOT}/lib" NO_DEFAULT_PATH)
endif()

find_path(ZLIB_INCLUDE_DIRS NAMES zlib.h HINTS ${ZLIB_INCLUDE_PATH})
find_library(ZLIB_LIBRARIES NAMES z HINTS ${ZLIB_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(ZLIB DEFAULT_MSG ZLIB_LIBRARIES ZLIB_INCLUDE_DIRS)

mark_as_advanced(ZLIB_ROOT ZLIB_LIBRARIES ZLIB_INCLUDE_DIRS)
