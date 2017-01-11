# Try to find Valgrind headers and libraries.
#
# Usage of this module as follows:
# 	find_package(Valgrind)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# Valgrind_ROOT Set this variable to the root installation of valgrind if the
# module has problems finding the proper installation path.
#
# Variables defined by this module:
# 	Valgrind_FOUND System has valgrind
# 	Valgrind_INCLUDE_DIR where to find valgrind/memcheck.h, etc.
# 	Valgrind_EXECUTABLE the valgrind executable.

# Get hint from environment variable (if any)
if(NOT Valgrind_ROOT AND DEFINED ENV{Valgrind_ROOT})
	set(Valgrind_ROOT "$ENV{Valgrind_ROOT}" CACHE PATH "Valgrind base directory location (optional, used for nonstandard installation paths)")
	mark_as_advanced(Valgrind_ROOT)
endif()

# Search path for nonstandard locations
if(Valgrind_ROOT)
	set(Valgrind_INCLUDE_PATH PATHS "${Valgrind_ROOT}/include" NO_DEFAULT_PATH)
	set(Valgrind_BINARY_PATH PATHS "${Valgrind_ROOT}/bin" NO_DEFAULT_PATH)
endif()

find_path(Valgrind_INCLUDE_DIR valgrind HINTS ${Valgrind_INCLUDE_PATH})
find_program(Valgrind_EXECUTABLE NAMES valgrind PATH ${Valgrind_BINARY_PATH}) 

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Valgrind DEFAULT_MSG Valgrind_INCLUDE_DIR Valgrind_EXECUTABLE)

mark_as_advanced(Valgrind_INCLUDE_DIR Valgrind_EXECUTABLE)
