#
# This file sets up some general variables and include paths for the build environment common to both runtime and compiler
#

SET( CMAKE_CXX_FLAGS_RELWITHASSERTS "-O3 -DINS_REL_WITH_DBG" CACHE STRING
    "Flags used by the C++ compiler during Release with Asserts builds."
    FORCE )
SET( CMAKE_C_FLAGS_RELWITHASSERTS "-O3 -DINS_REL_WITH_DBG" CACHE STRING
    "Flags used by the C compiler during Release with Asserts builds."
    FORCE )
MARK_AS_ADVANCED(
    CMAKE_CXX_FLAGS_RELWITHASSERTS
    CMAKE_C_FLAGS_RELWITHASSERTS )
# Update the documentation string of CMAKE_BUILD_TYPE for GUIs
SET( CMAKE_BUILD_TYPE "${CMAKE_BUILD_TYPE}" CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel RelWithAsserts."
    FORCE )

# disable energy stuff if not explicitely requested
option(USE_ENERGY "Enable energy capabilities" OFF)
if (NOT MSVC)
	if(NOT USE_ENERGY)
		message(STATUS "Disabling energy capabilities in ${PROJECT_NAME}" )
		add_definitions(-DDISABLE_ENERGY)
	endif ( NOT USE_ENERGY)
endif (NOT MSVC)

