# Try to find MPFR headers and libraries.
#
# Usage of this module as follows:
#
# find_package(MPFR)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# MPFR_ROOT Set this variable to the root installation of
# libpapi if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# MPFR_FOUND System has MPFR libraries and headers
# MPFR_LIBRARIES The MPFR library
# MPFR_INCLUDE_DIRS The location of MPFR headers
# MPFR_LIBRARY_DIRS The location of MPFR libraries for linking 

# Get hint from environment variable (if any)
if(NOT MPFR_ROOT AND DEFINED ENV{MPFR_ROOT})
  set(MPFR_ROOT "$ENV{MPFR_ROOT}" CACHE PATH "MPFR base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(MPFR_ROOT)
endif()

# Search path for nonstandard locations
if(MPFR_ROOT)
  set(MPFR_INCLUDE_PATH PATHS "${MPFR_ROOT}/include" NO_DEFAULT_PATH)
  set(MPFR_LIBRARY_PATH PATHS "${MPFR_ROOT}/lib" NO_DEFAULT_PATH)
  set(MPFR_LIBRARY_DIRS ${MPFR_LIBRARY_PATH})
endif()

find_path(MPFR_INCLUDE_DIRS NAMES mpfr.h HINTS ${MPFR_INCLUDE_PATH})
find_library(MPFR_LIBRARIES NAMES mpfr libmpfr HINTS ${MPFR_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(MPFR DEFAULT_MSG MPFR_INCLUDE_DIRS MPFR_LIBRARIES)

mark_as_advanced(MPFR_INCLUDE_DIS  MPFR_LIBRARIES MPFR_LIBRARY_DIRS)
