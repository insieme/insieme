# Try to find LuaJIT headers and libraries.
#
# Usage of this module as follows:
#
# find_package(LuaJIT)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# LUAJIT_ROOT Set this variable to the root installation of
# libluajit if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# LuaJIT_FOUND System has LuaJIT libraries and headers
# LuaJIT_LIBRARIES The LuaJIT library
# LuaJIT_INCLUDE_DIRS The location of LuaJIT headers

# Get hint from environment variable (if any)
if(NOT LUAJIT_ROOT AND DEFINED ENV{LUAJIT_ROOT})
	set(LUAJIT_ROOT "$ENV{LUAJIT_ROOT}" CACHE PATH "LUAJIT base directory location (optional, used for nonstandard installation paths)")
	mark_as_advanced(LUAJIT_ROOT)
endif()

# Search path for nonstandard locations
if(LUAJIT_ROOT)
	set(LuaJIT_INCLUDE_PATH PATHS "${LUAJIT_ROOT}/include" NO_DEFAULT_PATH)
	# for MSVC, the luajit library is located in the root directory, not root/lib
	set(LuaJIT_LIBRARY_PATH PATHS "${LUAJIT_ROOT}/lib" "${LUAJIT_ROOT}" NO_DEFAULT_PATH)
endif()

find_path(LuaJIT_INCLUDE_DIRS NAMES luajit-2.0/ HINTS ${LuaJIT_INCLUDE_PATH})
find_library(LuaJIT_LIBRARIES NAMES luajit-5.1 lua51 HINTS ${LuaJIT_LIBRARY_PATH})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(LuaJIT DEFAULT_MSG LuaJIT_LIBRARIES LuaJIT_INCLUDE_DIRS)

mark_as_advanced(LUAJIT_ROOTS LuaJIT_LIBRARIES LuaJIT_INCLUDE_DIRS)
