# Try to find BARVINOK headers and libraries.
#
# Usage of this module as follows:
#
# find_package(BARVINOK)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# BARVINOK_ROOT Set this variable to the root installation of
# libpapi if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# BARVINOK_FOUND System has BARVINOK libraries and headers
# BARVINOK_LIBRARIES The BARVINOK library
# BARVINOK_INCLUDE_DIRS The location of BARVINOK headers

# Get hint from environment variable (if any)
if(NOT BARVINOK_ROOT AND DEFINED ENV{BARVINOK_ROOT})
  set(BARVINOK_ROOT "$ENV{BARVINOK_ROOT}" CACHE PATH "BARVINOK base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(BARVINOK_ROOT)
endif()

# Search path for nonstandard locations
if(BARVINOK_ROOT)
  set(BARVINOK_INCLUDE_PATH PATHS "${BARVINOK_ROOT}/include" NO_DEFAULT_PATH)
  set(BARVINOK_LIBRARY_PATH PATHS "${BARVINOK_ROOT}/lib" NO_DEFAULT_PATH)
endif()

find_path(BARVINOK_INCLUDE_DIRS NAMES barvinok/barvinok.h HINTS ${BARVINOK_INCLUDE_PATH})
find_library(BARVINOK_LIBRARIES NAMES barvinok HINTS ${BARVINOK_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(BARVINOK DEFAULT_MSG BARVINOK_LIBRARIES BARVINOK_INCLUDE_DIRS)

mark_as_advanced( BARVINOK_ROOTS BARVINOK_LIBRARIES BARVINOK_INCLUDE_DIRS)
