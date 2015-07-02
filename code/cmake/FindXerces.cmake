################################################################################
#
# CMake script for finding XERCES.
# If the optional XERCES_ROOT environment variable exists, header files and
# libraries will be searched in the XERCES_ROOT/include and XERCES_ROOT/libs
# directories, respectively. Otherwise the default CMake search process will be
# used.
#
# This script creates the following variables:
#  XERCES_FOUND: Boolean that indicates if the package was found
#  XERCES_INCLUDE_DIRS: Paths to the necessary header files
#  XERCES_LIBRARIES: Package libraries
#
################################################################################

include(FindPackageHandleStandardArgs)

# Get hint from environment variable (if any)
if(NOT XERCES_ROOT AND DEFINED ENV{XERCES_ROOT})
  set(XERCES_ROOT "$ENV{XERCES_ROOT}" CACHE PATH "XERCES base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(XERCES_ROOT)
endif()


# Search path for nonstandard locations
if(XERCES_ROOT)
  set(Xerces_INCLUDE_PATH PATHS "${XERCES_ROOT}/include" NO_DEFAULT_PATH)
  set(Xerces_LIBRARY_PATH PATHS "${XERCES_ROOT}/lib" NO_DEFAULT_PATH)
endif()

# Find headers and libraries
find_path(Xerces_INCLUDE_DIR       NAMES xercesc/dom/DOM.hpp ${Xerces_INCLUDE_PATH})
find_library(Xerces_C_LIBRARY      NAMES xerces-c            ${Xerces_LIBRARY_PATH})

# Set Xerces_FOUND honoring the QUIET and REQUIRED arguments
find_package_handle_standard_args(Xerces DEFAULT_MSG Xerces_C_LIBRARY Xerces_INCLUDE_DIR)

# Output variables
# Include dirs
set(Xerces_INCLUDE_DIRS ${Xerces_INCLUDE_DIR})

# Libraries
set(Xerces_LIBRARIES ${Xerces_C_LIBRARY})

# Advanced options for not cluttering the cmake UIs
#mark_as_advanced(Xerces_INCLUDE_DIR Xerces_C_LIBRARY Xerces_DEPDOM_LIBRARY)
mark_as_advanced(Xerces_INCLUDE_DIRS Xerces_INCLUDE_DIR Xerces_C_LIBRARY Xerces_LIBRARIES)

