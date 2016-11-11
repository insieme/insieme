#
# This file sets up some general variables and include paths for the build environment common to both runtime and compiler
#

# --------------------------------------------------------------------- including libraries
# set up insieme lib home either from THIRD_PARTY_LIBS_HOME or INSIEME_LIBS_HOME env var
if ( DEFINED ENV{THIRD_PARTY_LIBS_HOME} )
	set(THIRD_PARTY_LIBS_HOME $ENV{THIRD_PARTY_LIBS_HOME} CACHE PATH "Third party library home" )
elseif ( DEFINED ENV{INSIEME_LIBS_HOME} )
	set(THIRD_PARTY_LIBS_HOME $ENV{INSIEME_LIBS_HOME} CACHE PATH "Third party library home" )
endif()


# get code root directory (based on current file name path)
get_filename_component( insieme_code_dir ${CMAKE_CURRENT_LIST_FILE} PATH )
get_filename_component( insieme_root_dir ${insieme_code_dir} PATH )

#custom findxxx modules
list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/cmake/")

#find them in CMAKE_MODULE_PATH
include(default_library_configuration)
include(insieme_find_package)

# -------------------------------------------------------------- define some code locations

set ( insieme_common_include_dir			${insieme_code_dir}/common/include )

# set compiler flags
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

option(USE_ENERGY "Enable energy capabilities" OFF)
option(USE_PAPI "Enable PAPI support" OFF)

if (NOT MSVC)
	if(USE_ENERGY)
		message(STATUS "Enabling energy capabilities" )
		# energy measurements require PAPI for hardware specs support
		set(USE_PAPI ON)
	else()
		message(STATUS "Disabling energy capabilities" )
		# disable energy stuff if not explicitely requested
		add_definitions(-DDISABLE_ENERGY)
	endif()
	if(USE_PAPI)
		message(STATUS "Enabling PAPI")
		insieme_find_package(NAME PAPI)
		add_definitions("-DPAPI_ROOT_DIR=\"${PAPI_ROOT}/\"")
	else()
		message(STATUS "Disabling PAPI")
	endif()
endif (NOT MSVC)

# toggle shared vs. static MSVC runtime library linking
option(MSVC_SHARED_RUNTIME "Use shared MSVC runtime linking" ON)

add_definitions("-DINSIEME_LIBS_HOME=\"${THIRD_PARTY_LIBS_HOME}/\"")
add_definitions("-DINSIEME_BUILD_ROOT=\"${CMAKE_CURRENT_BINARY_DIR}/..\"")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DBOOST_NO_CXX11_EXPLICIT_CONVERSION_OPERATORS")

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
	# disable warning "symbol will be dynamically initialized (implementation limitation)" because MSVC 2015.1 is still buggy on that
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4592\"" )

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

	# insieme file completion
	#file(GLOB_RECURSE insieme_all_files *.h *.hpp *.inc *.def *.c *.cpp *.cc *.cxx *.ir *.y *.l *.dl *.hs)
	#add_custom_target(insieme_all_files  SOURCES ${insieme_all_files})

	# integration test file completion
	if(NOT TARGET insieme_integration_test_files)
		file(GLOB_RECURSE insieme_integration_test_files
			../test/*.h ../test/*.hpp ../test/*.inc ../test/*.def
			../test/*.c ../test/*.cpp ../test/*.cc ../test/*.cxx
			../test/*.ir
		)
		add_custom_target(insieme_integration_test_files SOURCES ${insieme_integration_test_files})
	endif()
endif()
