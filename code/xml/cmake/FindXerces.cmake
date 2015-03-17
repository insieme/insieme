################################################################################
#
# CMake script for finding Xerces.
# If the optional Xerces_ROOT environment variable exists, header files and
# libraries will be searched in the Xerces_ROOT/include and Xerces_ROOT/libs
# directories, respectively. Otherwise the default CMake search process will be
# used.
#
# This script creates the following variables:
#  Xerces_FOUND: Boolean that indicates if the package was found
#  Xerces_INCLUDE_DIRS: Paths to the necessary header files
#  Xerces_LIBRARIES: Package libraries
#
################################################################################

include(FindPackageHandleStandardArgs)

# Get hint from environment variable (if any)
if(NOT Xerces_ROOT AND DEFINED ENV{Xerces_ROOT})
  set(Xerces_ROOT "$ENV{Xerces_ROOT}" CACHE PATH "Xerces base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(Xerces_ROOT)
endif()

# Search path for nonstandard locations
if(Xerces_ROOT)
  set(Xerces_INCLUDE_PATH PATHS "${Xerces_ROOT}/include" NO_DEFAULT_PATH)
  set(Xerces_LIBRARY_PATH PATHS "${Xerces_ROOT}/lib" NO_DEFAULT_PATH)
endif()

# Find headers and libraries
find_path(Xerces_INCLUDE_DIR       NAMES xercesc/dom/DOM.hpp ${Xerces_INCLUDE_PATH})
find_library(Xerces_C_LIBRARY      NAMES xerces-c            ${Xerces_LIBRARY_PATH})
#find_library(Xerces_DEPDOM_LIBRARY NAMES xerces-depdom       ${Xerces_LIBRARY_PATH})

# Set Xerces_FOUND honoring the QUIET and REQUIRED arguments
#find_package_handle_standard_args(Xerces DEFAULT_MSG Xerces_C_LIBRARY Xerces_DEPDOM_LIBRARY Xerces_INCLUDE_DIR)
find_package_handle_standard_args(Xerces DEFAULT_MSG Xerces_C_LIBRARY Xerces_INCLUDE_DIR)

# Output variables
if(Xerces_FOUND)
  # Include dirs
  set(Xerces_INCLUDE_DIRS ${Xerces_INCLUDE_DIR})

  # Libraries
  #set(Xerces_LIBRARIES ${Xerces_C_LIBRARY}
  #                     ${Xerces_DEPDOM_LIBRARY})

  set(Xerces_LIBRARIES ${Xerces_C_LIBRARY})
endif()

# Advanced options for not cluttering the cmake UIs
#mark_as_advanced(Xerces_INCLUDE_DIR Xerces_C_LIBRARY Xerces_DEPDOM_LIBRARY)
mark_as_advanced(Xerces_INCLUDE_DIR Xerces_C_LIBRARY)

