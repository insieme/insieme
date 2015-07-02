# Try to find ISL headers and libraries.
#
# Usage of this module as follows:
#
# find_package(ISL)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# ISL_ROOT Set this variable to the root installation of
# libpapi if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# ISL_FOUND System has ISL libraries and headers
# ISL_LIBRARIES The ISL library
# ISL_INCLUDE_DIRS The location of ISL headers

# Get hint from environment variable (if any)
if(NOT ISL_ROOT AND DEFINED ENV{ISL_ROOT})
  set(ISL_ROOT "$ENV{ISL_ROOT}" CACHE PATH "ISL base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(ISL_ROOT)
endif()

# Search path for nonstandard locations
if(ISL_ROOT)
  set(ISL_INCLUDE_PATH PATHS "${ISL_ROOT}/include" NO_DEFAULT_PATH)
  set(ISL_LIBRARY_PATH PATHS "${ISL_ROOT}/lib" NO_DEFAULT_PATH)
endif()

find_path(ISL_INCLUDE_DIRS NAMES isl/ HINTS ${ISL_INCLUDE_PATH})
find_library(ISL_LIBRARIES NAMES isl HINTS ${ISL_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(ISL DEFAULT_MSG ISL_LIBRARIES ISL_INCLUDE_DIRS)

mark_as_advanced( ISL_ROOTS ISL_LIBRARIES ISL_INCLUDE_DIRS)
