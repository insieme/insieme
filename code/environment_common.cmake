#
# This file sets up some general variables and include paths for the build environment common to both runtime and compiler
#

SET( CMAKE_CXX_FLAGS_DEBUG "-DINS_DEBUG" CACHE STRING
    "Flags used by the C++ compiler during Debug builds."
    FORCE )
SET( CMAKE_C_FLAGS_DEBUG "-DINS_DEBUG" CACHE STRING
    "Flags used by the C++ compiler during Debug builds."
    FORCE )
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
		message(STATUS "Disabling energy capabilities" )
		add_definitions(-DDISABLE_ENERGY)
	endif ( NOT USE_ENERGY)
endif (NOT MSVC)

# toggle shared vs. static MSVC runtime library linking
option(MSVC_SHARED_RUNTIME "Use shared MSVC runtime linking" ON)

# Visual Studio customization
if(MSVC)
	# enable minimal rebuild
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Gm" )
	# enable debug information (required for /Gm)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Zi" )
	# disable optimizations (compilation speed)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Od" )
	# disable some warnings
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /D_CRT_SECURE_NO_WARNINGS" )
	# Boost: No auto-lib
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /DBOOST_ALL_NO_LIB" )
	# disable warning "assignment operator could not be generated"
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4512\"" )
	# disable warning "nonstandard extension: enum '[EnumName::ENUM]' used in qualified name"	
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4482\"" )
	# disable warning "unkown pragma"
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4068\"" )
	# disable warning "declaration hides class member"
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4458\"" )
	# disable warning "forcing value to bool 'true' or 'false' (performance warning)"
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4800\"" )
	
	# properly configure how to link the MSVC runtime library, static <-> shared and debug <-> release
	if(MSVC_SHARED_RUNTIME)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MD")
		set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MDd")
		set (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /MDd")
		set (CMAKE_CXX_FLAGS_RELASE "${CMAKE_CXX_FLAGS_RELASE} /MD")
		set (CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} /MD")
	else()
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MT")
		set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MTd")
		set (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /MTd")
		set (CMAKE_CXX_FLAGS_RELASE "${CMAKE_CXX_FLAGS_RELASE} /MT")
		set (CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} /MT")
	endif()

	# windows library naming policies
  	set(CMAKE_FIND_LIBRARY_PREFIXES "" )
	set(CMAKE_FIND_LIBRARY_SUFFIXES ".lib" ) # if you're thinking about adding ".dll" here, read up on "import libraries" in Windows
	
	# Boost linking options
	set(Boost_USE_STATIC_LIBS OFF) # default: OFF
	set(Boost_USE_DEBUG_RUNTIME ON) # default: ON
	set(Boost_USE_MULTITHREADED ON) # default: ON
	if(MSVC_SHARED_RUNTIME)
		set(Boost_USE_STATIC_RUNTIME OFF) # default: platform-dependent
	else()
		set(Boost_USE_STATIC_RUNTIME ON)
	endif()
	
endif()