# Try to find CLOOG headers and libraries.
#
# Usage of this module as follows:
#
# find_package(CLOOG)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# CLOOG_ROOT Set this variable to the root installation of
# libpapi if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# CLOOG_FOUND System has CLOOG libraries and headers
# CLOOG_LIBRARIES The CLOOG library
# CLOOG_INCLUDE_DIRS The location of CLOOG headers

# Get hint from environment variable (if any)
if(NOT CLOOG_ROOT AND DEFINED ENV{CLOOG_ROOT})
  set(CLOOG_ROOT "$ENV{CLOOG_ROOT}" CACHE PATH "CLOOG base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(CLOOG_ROOT)
endif()

# Search path for nonstandard locations
if(CLOOG_ROOT)
  set(CLOOG_INCLUDE_PATH PATHS "${CLOOG_ROOT}/include" NO_DEFAULT_PATH)
  set(CLOOG_LIBRARY_PATH PATHS "${CLOOG_ROOT}/lib" NO_DEFAULT_PATH)
endif()

find_path(CLOOG_INCLUDE_DIRS NAMES cloog/cloog.h HINTS ${CLOOG_INCLUDE_PATH})
find_library(CLOOG_LIBRARIES NAMES cloog-isl HINTS ${CLOOG_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(CLOOG DEFAULT_MSG CLOOG_LIBRARIES CLOOG_INCLUDE_DIRS)

mark_as_advanced( CLOOG_ROOTS CLOOG_LIBRARIES CLOOG_INCLUDE_DIRS)
