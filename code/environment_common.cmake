#
# This file sets up some general variables and include paths for the build environment common to both runtime and compiler
#

# disable energy stuff if not explicitely requested
option(USE_ENERGY "Enable energy capabilities" OFF)
if (NOT MSVC)
	if(NOT USE_ENERGY)
		message(STATUS "Disabling energy capabilities in ${PROJECT_NAME}" )
		add_definitions(-DDISABLE_ENERGY)
	endif ( NOT USE_ENERGY)
endif (NOT MSVC)

