# Try to find the GMP librairies
#  GMP_FOUND - system has GMP lib
#  GMP_INCLUDE_DIR - the GMP include directory
#  GMP_LIBRARIES - Libraries needed to use GMP

# Get hint from environment variable (if any)
if(NOT GMP_ROOT AND DEFINED ENV{GMP_ROOT})
  set(GMP_ROOT "$ENV{GMP_ROOT}" CACHE PATH "GMP base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(GMP_ROOT)
endif()

# Search path for nonstandard locations
if(GMP_ROOT)
  set(GMP_INCLUDE_PATH PATHS "${GMP_ROOT}/include" NO_DEFAULT_PATH)
  set(GMP_LIBRARY_PATH PATHS "${GMP_ROOT}/lib" NO_DEFAULT_PATH)
endif()

find_path(GMP_INCLUDE_DIR NAMES gmp.h HINTS ${GMP_INCLUDE_PATH})
find_library(GMP_LIBRARIES NAMES gmp libgmp HINTS ${GMP_LIBRARY_PATH})
find_library(GMPXX_LIBRARIES NAMES gmpxx libgmpxx HINTS ${GMP_LIBRARY_PATH})
MESSAGE(STATUS "GMP libs: " ${GMP_LIBRARIES} " " ${GMPXX_LIBRARIES} )

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(GMP DEFAULT_MSG GMP_INCLUDE_DIR GMP_LIBRARIES)

mark_as_advanced(GMP_INCLUDE_DIR GMP_LIBRARIES)
