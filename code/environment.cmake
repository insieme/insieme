# This file sets up some general variables and paths to find libraries (modules have to lookup them
# selves) for the build environment

# Configuration:
#   LINKING_TYPE                Linux
#   LLVM_HOME/$ENV{LLVM_HOME}   Both

#	BOOST_ROOT    				as env-var or cmake-var
#	GTEST_ROOT    				as env-var or cmake-var
#	GMP_ROOT    				as env-var or cmake-var
#	MPFR_ROOT    				as env-var or cmake-var
#	XERCES_ROOT    				as env-var or cmake-var
#	CUDD_ROOT    				as env-var or cmake-var
#	SHARK_ROOT    				as env-var or cmake-var
#	KOMPEX_ROOT    				as env-var or cmake-var
#	ISL_ROOT    				as env-var or cmake-var
#	PAPI_ROOT    				as env-var or cmake-var

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

# -------------------------------------------------------------- define some code locations

#find them in CMAKE_MODULE_PATH
include(default_library_configuration)
include(insieme_find_package)
include(insieme_glob_headers)
include(add_unit_test)

#if CBA_JOBS option was given, we query the number of cores, if no -j was specified this is the
#uperlimit for parallel compile jobs
if(DEFINED CBA_JOBS)
	include(ProcessorCount)
	ProcessorCount(jobs)
	set_property(GLOBAL APPEND PROPERTY JOB_POOLS compile_job_pool=${jobs} link_job_pool=${jobs})
	get_property(job_pools GLOBAL PROPERTY JOB_POOLS)
	set(CMAKE_JOB_POOL_COMPILE compile_job_pool)
	set(CMAKE_JOB_POOL_LINK link_job_pool)
endif()

set ( insieme_core_src_dir 	            	${insieme_code_dir}/core/src )
set ( insieme_core_include_dir 	         	${insieme_code_dir}/core/include )

set ( insieme_utils_src_dir 	         	${insieme_code_dir}/utils/src )
set ( insieme_utils_include_dir          	${insieme_code_dir}/utils/include )

set ( insieme_annotations_src_dir			${insieme_code_dir}/annotations/src )
set ( insieme_annotations_include_dir       ${insieme_code_dir}/annotations/include )

set ( insieme_xml_src_dir					${insieme_code_dir}/xml/src )
set ( insieme_xml_include_dir            	${insieme_code_dir}/xml/include )

set ( insieme_iwir_src_dir					${insieme_code_dir}/iwir/src )
set ( insieme_iwir_include_dir            	${insieme_code_dir}/iwir/include )

set ( insieme_frontend_src_dir				${insieme_code_dir}/frontend/src )
set ( insieme_frontend_include_dir       	${insieme_code_dir}/frontend/include )
set ( insieme_backend_src_dir				${insieme_code_dir}/backend/src )
set ( insieme_backend_include_dir       	${insieme_code_dir}/backend/include )

set ( insieme_driver_src_dir				${insieme_code_dir}/driver/src )
set ( insieme_driver_include_dir         	${insieme_code_dir}/driver/include )

set ( insieme_analysis_src_dir				${insieme_code_dir}/analysis/src )
set ( insieme_analysis_include_dir       	${insieme_code_dir}/analysis/include )
set ( insieme_transform_src_dir				${insieme_code_dir}/transform/src )
set ( insieme_transform_include_dir       	${insieme_code_dir}/transform/include )

set ( insieme_common_include_dir			${insieme_code_dir}/common/include )

set ( insieme_runtime_src_dir				${insieme_code_dir}/runtime/src )
set ( insieme_runtime_include_dir 	        ${insieme_code_dir}/runtime/include )

set ( insieme_machine_learning_src_dir  	${insieme_code_dir}/machine_learning/src )
set ( insieme_machine_learning_include_dir  ${insieme_code_dir}/machine_learning/include )

# only in ext repo
set ( insieme_playground_include_dir       	${insieme_code_dir}/playground/include )

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
# set up insieme lib home either from THIRD_PARTY_LIBS_HOME or INSIEME_LIBS_HOME env var 
if ( DEFINED ENV{THIRD_PARTY_LIBS_HOME} ) 
	set(THIRD_PARTY_LIBS_HOME $ENV{THIRD_PARTY_LIBS_HOME} CACHE PATH "Third party library home" )
elseif ( DEFINED ENV{INSIEME_LIBS_HOME} ) 
	set(THIRD_PARTY_LIBS_HOME $ENV{INSIEME_LIBS_HOME} CACHE PATH "Third party library home" )
endif()

#------------------------------------------------------------- profiling
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
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fshow-column")
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-show-option")
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
	#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror")
	#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wextra")
	#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic")
	#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")

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
		message(WARNING "WARNING: -std=c++0x or -std=c++11 not supported by your compiler!" )
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
		message(WARNING  "WARNING: --std=c99 not supported by your compiler!" )
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
		message(WARNING  "WARNING: -std=c++0x not supported by your compiler!" )
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
	#set ( insieme_version "`(cd ${insieme_code_dir}; ${GIT_EXECUTABLE} describe --dirty)`")

	set (git_cmd "${GIT_EXECUTABLE}")
	set (git_arg "describe;--dirty")

	execute_process(COMMAND ${git_cmd} ${git_arg}
							  WORKING_DIRECTORY ${insieme_code_dir}
							  RESULT_VARIABLE git_result
							  OUTPUT_VARIABLE insieme_version)

	# git returns space and newline, remove them to keep commands clean
	string(REPLACE "\n" " " insieme_version ${insieme_version})
	string(REPLACE " " "" insieme_version ${insieme_version})
else()
	set ( insieme_version "unknown" )
endif()

# add insieme version definition (add_definitions escapes back-quotes)
#
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

# --------------------------------------------------------- Limit number of processors used in some generators
include(ProcessorCount)
ProcessorCount(NB_PROCESSORS)

if(MSVC)
	set(NB_PROCESSORS "1")
endif()

#ninja job pools setting
if( NOT DEFINED(CBA_JOBS) OR (CBA_JOBS) )
	set(CBA_JOBS "1" CACHE STRING "number of parallel cba unit test jobs")
endif()
math(EXPR ALL_JOBS "${NB_PROCESSORS} + 2")
set_property(GLOBAL PROPERTY JOB_POOLS cba_jobs=${CBA_JOBS} all_jobs=${ALL_JOBS} )
set(CMAKE_JOB_POOL_COMPILE all_jobs)
set(CMAKE_JOB_POOL_LINK all_jobs)
