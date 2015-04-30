# Try to find KOMPEX headers and libraries.
#
# Usage of this module as follows:
#
# find_package(KOMPEX)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# KOMPEX_ROOT Set this variable to the root installation of
# libpapi if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# KOMPEX_FOUND System has KOMPEX libraries and headers
# KOMPEX_LIBRARIES The KOMPEX library
# KOMPEX_INCLUDE_DIRS The location of KOMPEX headers

# Get hint from environment variable (if any)
if(NOT KOMPEX_ROOT AND DEFINED ENV{KOMPEX_ROOT})
  set(KOMPEX_ROOT "$ENV{KOMPEX_ROOT}" CACHE PATH "KOMPEX base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(KOMPEX_ROOT)
endif()

# Search path for nonstandard locations
if(KOMPEX_ROOT)
  set(KOMPEX_INCLUDE_PATH PATHS "${KOMPEX_ROOT}/inc" NO_DEFAULT_PATH)
  set(KOMPEX_LIBRARY_PATH PATHS "${KOMPEX_ROOT}/lib" NO_DEFAULT_PATH)
endif()

find_path(KOMPEX_INCLUDE_DIRS NAMES KompexSQLiteDatabase.h HINTS ${KOMPEX_INCLUDE_PATH})
find_library(KOMPEX_LIBRARIES NAMES KompexSQLiteWrapper_Static_d HINTS ${KOMPEX_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(KOMPEX DEFAULT_MSG KOMPEX_LIBRARIES KOMPEX_INCLUDE_DIRS)

mark_as_advanced( KOMPEX_ROOTS KOMPEX_LIBRARIES KOMPEX_INCLUDE_DIRS)
