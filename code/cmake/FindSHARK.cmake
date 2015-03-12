# Try to find SHARK headers and libraries.
#
# Usage of this module as follows:
#
# find_package(SHARK)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# SHARK_ROOT Set this variable to the root installation of
# libpapi if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# SHARK_FOUND System has SHARK libraries and headers
# SHARK_LIBRARIES The SHARK library
# SHARK_INCLUDE_DIRS The location of SHARK headers

# Get hint from environment variable (if any)
if(NOT SHARK_ROOT AND DEFINED ENV{SHARK_ROOT})
  set(SHARK_ROOT "$ENV{SHARK_ROOT}" CACHE PATH "SHARK base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(SHARK_ROOT)
endif()

# Search path for nonstandard locations
if(SHARK_ROOT)
  set(SHARK_INCLUDE_PATH PATHS "${SHARK_ROOT}/include" NO_DEFAULT_PATH)
  set(SHARK_LIBRARY_PATH PATHS "${SHARK_ROOT}/lib" NO_DEFAULT_PATH)
endif()

find_path(SHARK_INCLUDE_DIRS NAMES SharkDefs.h HINTS ${SHARK_INCLUDE_PATH})
find_library(SHARK_LIBRARIES NAMES shark HINTS ${SHARK_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(SHARK DEFAULT_MSG SHARK_LIBRARIES SHARK_INCLUDE_DIRS)

mark_as_advanced( SHARK_ROOTS SHARK_LIBRARIES SHARK_INCLUDE_DIRS)
