# Try to find the MPFR librairies
#  MPFR_FOUND - system has MPFR lib
#  MPFR_INCLUDE_DIR - the MPFR include directory
#  MPFR_LIBRARIES - Libraries needed to use MPFR

# Get hint from environment variable (if any)
if(NOT MPFR_ROOT AND DEFINED ENV{MPFR_ROOT})
  set(MPFR_ROOT "$ENV{MPFR_ROOT}" CACHE PATH "MPFR base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(MPFR_ROOT)
endif()

# Search path for nonstandard locations
if(MPFR_ROOT)
  set(MPFR_INCLUDE_PATH PATHS "${MPFR_ROOT}/include" NO_DEFAULT_PATH)
  set(MPFR_LIBRARY_PATH PATHS "${MPFR_ROOT}/lib" NO_DEFAULT_PATH)
endif()

find_path(MPFR_INCLUDE_DIR NAMES mpfr.h HINTS ${MPFR_INCLUDE_PATH})
find_library(MPFR_LIBRARIES NAMES mpfr libmpfr HINTS ${MPFR_LIBRARY_PATH})
MESSAGE(STATUS "MPFR libs: " ${MPFR_LIBRARIES})

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(MPFR DEFAULT_MSG MPFR_INCLUDE_DIR MPFR_LIBRARIES)

mark_as_advanced(MPFR_INCLUDE_DIR MPFR_LIBRARIES)
