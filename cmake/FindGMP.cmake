# Try to find GMP headers and libraries.
#
# Usage of this module as follows:
#
# find_package(GMP)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# GMP_ROOT Set this variable to the root installation of libgmp if the module
# has problems finding the proper installation path.
#
# Variables defined by this module:
#
# GMP_FOUND System has GMP libraries and headers
# GMP_LIBRARIES The GMP library
# GMP_INCLUDE_DIRS The location of GMP headers

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

find_path(GMP_INCLUDE_DIRS NAMES gmp.h HINTS ${GMP_INCLUDE_PATH})
find_library(GMP_LIBRARIES NAMES gmp HINTS ${GMP_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(GMP DEFAULT_MSG GMP_LIBRARIES GMP_INCLUDE_DIRS)

mark_as_advanced(GMP_ROOT GMP_LIBRARIES GMP_INCLUDE_DIRS)
