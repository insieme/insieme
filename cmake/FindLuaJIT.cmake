# Try to find luajit
#
# LUAJIT_ROOT Set this variable to the root installation of
# libluajit if the module has problems finding the
# proper installation path.
#
# Once done this will define
#  LUAJIT_FOUND - System has luajit
#  LUAJIT_INCLUDE_DIRS - The luajit include directories
#  LUAJIT_LIBRARIES - The libraries needed to use luajit

if(NOT LUAJIT_ROOT AND DEFINED ENV{LUAJIT_ROOT})
  set(LUAJIT_ROOT "$ENV{LUAJIT_ROOT}" CACHE PATH "LUAJIT base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(LUAJIT_ROOT)
endif()

if(LUAJIT_ROOT)
  set(LUAJIT_INCLUDE_PATH PATHS "${LUAJIT_ROOT}/include" NO_DEFAULT_PATH)
  # for MSVC, the luajit library is located in the root directory, not root/lib
  set(LUAJIT_LIBRARY_PATH PATHS "${LUAJIT_ROOT}/lib" "${LUAJIT_ROOT}" NO_DEFAULT_PATH)
endif()

if(NOT USE_BUNDLED_LUAJIT)
  find_package(PkgConfig)
  if (PKG_CONFIG_FOUND)
    pkg_check_modules(PC_LUAJIT QUIET luajit)
  endif()
else()
  set(PC_LUAJIT_INCLUDEDIR)
  set(PC_LUAJIT_INCLUDE_DIRS)
  set(PC_LUAJIT_LIBDIR)
  set(PC_LUAJIT_LIBRARY_DIRS)
  set(LIMIT_SEARCH NO_DEFAULT_PATH)
endif()

set(LUAJIT_DEFINITIONS ${PC_LUAJIT_CFLAGS_OTHER})

find_path(LUAJIT_INCLUDE_DIR luajit.h
          HINTS ${LUAJIT_INCLUDE_PATH}
          PATHS ${PC_LUAJIT_INCLUDEDIR} ${PC_LUAJIT_INCLUDE_DIRS}
          PATH_SUFFIXES luajit-2.0
          ${LIMIT_SEARCH})

# If we're asked to use static linkage, add libluajit-5.1.a as a preferred
# library name.
if(LUAJIT_USE_STATIC)
  list(APPEND LUAJIT_NAMES
    "${CMAKE_STATIC_LIBRARY_PREFIX}luajit-5.1${CMAKE_STATIC_LIBRARY_SUFFIX}")
endif()

if(MSVC)
  list(APPEND LUAJIT_NAMES lua51)
elseif(MINGW)
  list(APPEND LUAJIT_NAMES libluajit libluajit-5.1)
else()
  list(APPEND LUAJIT_NAMES luajit-5.1)
endif()

find_library(LUAJIT_LIBRARY NAMES ${LUAJIT_NAMES}
             HINTS ${LUAJIT_LIBRARY_PATH}
             PATHS ${PC_LUAJIT_LIBDIR} ${PC_LUAJIT_LIBRARY_DIRS}
             ${LIMIT_SEARCH})

set(LUAJIT_LIBRARIES ${LUAJIT_LIBRARY})
set(LUAJIT_INCLUDE_DIRS ${LUAJIT_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LUAJIT_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(LuaJit DEFAULT_MSG
                                  LUAJIT_LIBRARY LUAJIT_INCLUDE_DIR)

if(NOT LUAJIT_ROOT)
  get_filename_component(LUAJIT_ROOT ${LUAJIT_INCLUDE_DIR} DIRECTORY)
endif()

mark_as_advanced(LUAJIT_ROOT LUAJIT_INCLUDE_DIR LUAJIT_LIBRARY)
