#
# This file sets up some general variables and include paths for the build environment
#

# Configuration:
#   $ENV{XERCES_HOME}           Both
#   $ENV{GLOG_HOME}             Linux
#   LINKING_TYPE                Linux
#   LLVM_HOME/$ENV{LLVM_HOME}   Both


# -------------------------------------------------------------- define some code locations

ENABLE_LANGUAGE(C)
ENABLE_LANGUAGE(CXX)

# setup std-include directories (to support some IDEs)
if (GCC_INCLUDE_DIR) 
	include_directories( ${GCC_INCLUDE_DIR} )
endif()


# get code root directory (based on current file name path)
get_filename_component( insieme_code_dir ${CMAKE_CURRENT_LIST_FILE} PATH )
get_filename_component( insieme_root_dir ${insieme_code_dir} PATH )

#custom findxxx modules
list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/cmake/")

include (${insieme_code_dir}/lookup_lib.cmake)
include (${insieme_code_dir}/add_unit_test.cmake)

set ( insieme_core_src_dir 	            	${insieme_code_dir}/core/src )
set ( insieme_core_include_dir 	         	${insieme_code_dir}/core/include )

set ( insieme_utils_src_dir 	         	${insieme_code_dir}/utils/src )
set ( insieme_utils_include_dir          	${insieme_code_dir}/utils/include )

set ( insieme_annotations_src_dir       ${insieme_code_dir}/annotations/src )
set ( insieme_annotations_include_dir       ${insieme_code_dir}/annotations/include )

set ( insieme_xml_src_dir            	${insieme_code_dir}/xml/src )
set ( insieme_xml_include_dir            	${insieme_code_dir}/xml/include )

set ( insieme_iwir_src_dir            	${insieme_code_dir}/iwir/src )
set ( insieme_iwir_include_dir            	${insieme_code_dir}/iwir/include )

set ( insieme_frontend_src_dir       	${insieme_code_dir}/frontend/src )
set ( insieme_frontend_include_dir       	${insieme_code_dir}/frontend/include )
set ( insieme_backend_src_dir       	${insieme_code_dir}/backend/src )
set ( insieme_backend_include_dir       	${insieme_code_dir}/backend/include )

set ( insieme_driver_src_dir         	${insieme_code_dir}/driver/src )
set ( insieme_driver_include_dir         	${insieme_code_dir}/driver/include )

set ( insieme_analysis_src_dir       	${insieme_code_dir}/analysis/src )
set ( insieme_analysis_include_dir       	${insieme_code_dir}/analysis/include )
set ( insieme_transform_src_dir       	${insieme_code_dir}/transform/src )
set ( insieme_transform_include_dir       	${insieme_code_dir}/transform/include )

set ( insieme_common_include_dir 		${insieme_code_dir}/common/include )

set ( insieme_runtime_src_dir 	        ${insieme_code_dir}/runtime/src )
set ( insieme_runtime_include_dir 	        ${insieme_code_dir}/runtime/include )

set ( insieme_machine_learning_src_dir  	${insieme_code_dir}/machine_learning/src )
set ( insieme_machine_learning_include_dir  	${insieme_code_dir}/machine_learning/include )

#FIXME ONLY IN EXT???
set ( insieme_plugins_include_dir  		${insieme_code_dir}/plugins/include )
set ( insieme_playground_include_dir       	${insieme_code_dir}/playground/include )
set ( insieme_experiments_include_dir       	${insieme_code_dir}/experiments/include )

# -------------------------------------------------------------- find location of utilities

find_program(TIME_EXECUTABLE time)
if(${TIME_EXECUTABLE} STREQUAL "TIME_EXECUTABLE-NOTFOUND" AND NOT MSVC) 
	message(FATAL_ERROR "Unable to locate time utility!")
endif()

# ------------------------------------------------------------- configuration for platforms
if(MSVC)
  # Windows Visual Studio
  # MSVC can compile insieme statical only
  if(NOT LINKING_TYPE)
	set(LINKING_TYPE STATIC)
  endif(NOT LINKING_TYPE)

  # Therefore Boost needs to be linked statically
  set(Boost_USE_STATIC_LIBS ON)
  # Use MT Boost
  set(Boost_USE_MULTITHREADED ON)
  set(DO_INSTALL FALSE)

else(MSVC) 
	# Linux or Cygwin/MinGW

    # Default is here: shared linking
    if(NOT LINKING_TYPE)
  	    set(LINKING_TYPE SHARED)
    endif(NOT LINKING_TYPE)

  set(DO_INSTALL TRUE)
  
endif(MSVC)


# --------------------------------------------------------------------- including libraries

# set up third-part library home
if (NOT THIRD_PARTY_LIBS_HOME )
	if ( DEFINED INSIEME_LIBS_HOME ) 
		set ( THIRD_PARTY_LIBS_HOME ${INSIEME_LIBS_HOME} CACHE PATH "Third part library home" )
	else()
		set ( THIRD_PARTY_LIBS_HOME $ENV{INSIEME_LIBS_HOME} CACHE PATH "Third part library home" )
	endif()
endif()

# - boost
if ( NOT DEFINED BOOST_ROOT )
	if ( NOT $ENV{BOOST_ROOT} EQUAL "" )
		set ( BOOST_ROOT $ENV{BOOST_ROOT} CACHE PATH "Boost installation directory." )
	else()
		set ( BOOST_ROOT "${THIRD_PARTY_LIBS_HOME}/boost-latest" CACHE PATH "Boost installation directory." )
	endif()
endif()
find_package( Boost 1.48 QUIET COMPONENTS program_options system filesystem regex serialization )
#everybody wants boost (except runtime...) so everybody gets it
include_directories(SYSTEM ${Boost_INCLUDE_DIRS})	 #we use SYSTEM to include boost-header as system header (-isystem) to suppress warnings found in boost code
#the linking should be done specifically
#link_directories(${Boost_LIBRARY_DIRS})

#profiling
IF (DO_GOOGLE_PROFILING)
	if(NOT DEFINED GPERFTOOLS_HOME)
		set (GPERFTOOLS_HOME $ENV{GPERFTOOLS_HOME})
	endif()
	include_directories( ${GPERFTOOLS_HOME}/include )
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -L${GPERFTOOLS_HOME}/lib -lprofiler")
ENDIF ()

# ------------------------------------------------------------- configuration for platforms
# Visual Studio customization
if(MSVC)
	# enable minimal rebuild
	add_definitions( /Gm )
	# disable optimizations (compilation speed)
	add_definitions( /Od )
	# disable some warnings
	add_definitions( /D_CRT_SECURE_NO_WARNINGS )
	# Boost: No auto-lib
	add_definitions( /DBOOST_ALL_NO_LIB )
	# disable warning "assignment operator could not be generated"
	add_definitions( /wd"4512" )
	# disable warning "nonstandard extension: enum '[EnumName::ENUM]' used in qualified name"	
	add_definitions( /wd"4482" )
	# statically link with runtime library (required for gtest)
	foreach(flag_var
		CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
		CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
		if(${flag_var} MATCHES "/MD")
			string(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
		endif(${flag_var} MATCHES "/MD")
	endforeach(flag_var)

	# enable warnings
	add_definitions( /W4 )
endif()

#--------------------------- GCC -------------------------
if (CMAKE_COMPILER_IS_GNUCXX)
	# add general flags
	add_definitions( -fshow-column )
	add_definitions( -fdiagnostics-show-option )
	add_definitions( -Wall )
	add_definitions( -fopenmp )
	# add_definitions( -Wextra )
	# add_definitions( -Werror )
	# add_definitions( -pedantic )

	# add flag allowing arbitrary library ordering (not default in newer distributions)
	set (CMAKE_CXX_FLAGS "-Wl,--no-as-needed ${CMAKE_CXX_FLAGS}")

	# add flags for debug mode
	set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g3 -O0")
  
	# add flags for release mode
	set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")

	# ENABLE PROFILING
	# add_definitions( -pg )
	# SET(CMAKE_EXE_LINKER_FLAGS -pg)


	include(CheckCXXCompilerFlag)
	# check for -std=c++0x
	check_cxx_compiler_flag( -std=c++0x CXX0X_Support )
	# check for -std=c++11
	check_cxx_compiler_flag( -std=c++11 CXX11_Support )
	if(CXX11_Support)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
	elseif(CXX0X_Support)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
	else()
		message( "WARNING: -std=c++0x or -std=c++11 not supported by your compiler!" )
	endif()
endif()

# enable C99 support within gcc
if (CMAKE_COMPILER_IS_GNUC)
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -rdynamic -fPIC")

	# add flags for debug mode
	set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g3 -O0 -fPIC")
  
	# add flags for release mode
	set (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -fPIC")

	include(CheckCCompilerFlag)
	check_c_compiler_flag( -std=c99 C99_Support )
	if(C99_Support)
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
	else()
		message( "WARNING: --std=c99 not supported by your compiler!" )
	endif()
endif()

#--------------------------- Intel Compiler -------------------------
if (${CMAKE_CXX_COMPILER} MATCHES "icpc")
	# add general flags
	add_definitions( -Wall )
	
  	set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")
	set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g3 -O0")
	
	include(CheckCXXCompilerFlag)
	# check for -std=c++0x
	check_cxx_compiler_flag( -std=c++0x CXX0X_Support )
	# check for -std=c++11
	check_cxx_compiler_flag( -std=c++11 CXX11_Support )
	if(CXX11_Support)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
	elseif(CXX0X_Support)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
	else()
		message( "WARNING: --std=c++0x not supported by your compiler!" )
	endif()
endif ()




# --------------------------------------------------------- Runtime
# -D_XOPEN_SOURCE=700 is required to get recent pthread features with -std=c99
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 -D_XOPEN_SOURCE=700")
# required for affinity-related macros
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_GNU_SOURCE")
# set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pg")

# -------------------------------------------------------------- determines insieme version
find_package(Git)
if(GIT_FOUND)
	# deduce the code version using git describe
	set ( insieme_version "`(cd ${insieme_code_dir}; ${GIT_EXECUTABLE} describe --dirty)`")
else()
	set ( insieme_version "unknown" )
endif()


# add insieme version definition (add_definitions escapes back-quotes)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DINSIEME_VERSION=\"\\\"${insieme_version}\\\"\"")

# --------------------------------------------------------- Valgrind / GTest testing suite
# avoid multiple import
if (NOT MEMORY_CHECK_SETUP)
	option(CONDUCT_MEMORY_CHECKS "Checks all test cases for memory leaks using valgrind if enabled." OFF)

	# add -all-valgrind target
	add_custom_target(valgrind)

	# mark as defined
	set(MEMORY_CHECK_SETUP OFF CACHE INTERNAL "Flag to avoid multiple setup" PARENT_SCOPE)
endif (NOT MEMORY_CHECK_SETUP)

# query the number of cores to control parallelism
execute_process(COMMAND getconf  _NPROCESSORS_ONLN
                OUTPUT_VARIABLE NB_PROCESSORS
		OUTPUT_STRIP_TRAILING_WHITESPACE
		)

