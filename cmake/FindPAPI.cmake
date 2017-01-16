# Try to find PAPI headers and libraries.
#
# Usage of this module as follows:
#
# find_package(PAPI)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# PAPI_ROOT Set this variable to the root installation of
# libpapi if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# PAPI_FOUND System has PAPI libraries and headers
# PAPI_LIBRARIES The PAPI library
# PAPI_INCLUDE_DIRS The location of PAPI headers

# Get hint from environment variable (if any)
if(NOT PAPI_ROOT AND DEFINED ENV{PAPI_ROOT})
	set(PAPI_ROOT "$ENV{PAPI_ROOT}" CACHE PATH "PAPI base directory location (optional, used for nonstandard installation paths)")
	mark_as_advanced(PAPI_ROOT)
endif()

# Search path for nonstandard locations
if(PAPI_ROOT)
	set(PAPI_INCLUDE_PATH PATHS "${PAPI_ROOT}/include" NO_DEFAULT_PATH)
	set(PAPI_LIBRARY_PATH PATHS "${PAPI_ROOT}/lib" NO_DEFAULT_PATH)
endif()


find_path(PAPI_INCLUDE_DIRS NAMES papi.h HINTS ${PAPI_INCLUDE_PATH})
find_library(PAPI_LIBRARIES NAMES papi HINTS ${PAPI_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(PAPI DEFAULT_MSG PAPI_LIBRARIES PAPI_INCLUDE_DIRS)

mark_as_advanced(PAPI_LIBRARIES PAPI_INCLUDE_DIRS)
