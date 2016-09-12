# This file sets up some general variables and paths to find libraries (modules have to lookup them
# selves) for the build environment

# Configuration:
#   LINKING_TYPE                Linux
#   LLVM_HOME/$ENV{LLVM_HOME}   Both

#	BOOST_ROOT    				as env-var or cmake-var
#	GTEST_ROOT    				as env-var or cmake-var
#	CUDD_ROOT    				as env-var or cmake-var
#	PAPI_ROOT    				as env-var or cmake-var

ENABLE_LANGUAGE(C)
ENABLE_LANGUAGE(CXX)

#custom findxxx modules
list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/cmake/")

# -------------------------------------------------------------- define some code locations

#find them in CMAKE_MODULE_PATH
include(insieme_glob_headers)
include(add_unit_test)
include(insieme_cotire)
include(cotire)
include(insieme_fix_case_name)
include(add_souffle)

set ( insieme_core_src_dir 	            	${insieme_code_dir}/core/src )
set ( insieme_core_include_dir 	         	${insieme_code_dir}/core/include )

set ( insieme_utils_src_dir 	         	${insieme_code_dir}/utils/src )
set ( insieme_utils_include_dir          	${insieme_code_dir}/utils/include )

set ( insieme_annotations_src_dir			${insieme_code_dir}/annotations/src )
set ( insieme_annotations_include_dir       ${insieme_code_dir}/annotations/include )

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

set ( insieme_runtime_src_dir				${insieme_code_dir}/runtime/src )
set ( insieme_runtime_include_dir 	        ${insieme_code_dir}/runtime/include )

# only in ext repo
set ( insieme_playground_include_dir       	${insieme_code_dir}/playground/include )

# -------------------------------------------------------------- find location of utilities
find_program(TIME_EXECUTABLE time)
if(${TIME_EXECUTABLE} STREQUAL "TIME_EXECUTABLE-NOTFOUND" AND NOT MSVC)
	message(FATAL_ERROR "Unable to locate time utility!")
endif()

#------------------------------------------------------------- set linking type to shared

if(NOT LINKING_TYPE)
    set(LINKING_TYPE SHARED)
endif(NOT LINKING_TYPE)

#------------------------------------------------------------- profiling
IF (DO_GOOGLE_PROFILING)
	if(NOT DEFINED GPERFTOOLS_HOME)
		set (GPERFTOOLS_HOME $ENV{GPERFTOOLS_HOME})
	endif()
	include_directories( ${GPERFTOOLS_HOME}/include )
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -L${GPERFTOOLS_HOME}/lib -lprofiler")
ENDIF ()

# ------------------------------------------------------------- configuration for platforms

# --------------------------- MSVC -------------------------
if(MSVC)
	# set warning level to maximum (all warnings + informational stuff)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
endif()

# --------------------------- GCC -------------------------
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
	# check for -std=c++14
	check_cxx_compiler_flag( -std=c++14 CXX14_Support )
	if(CXX14_Support)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
	else()
		message(WARNING "WARNING: -std=c++14 not supported by your compiler!" )
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

# --------------------------- Clang Compiler -------------------------
if (${CMAKE_CXX_COMPILER} MATCHES "clang")
	# C flags
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fopenmp")
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -rdynamic -fPIC")
	set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g3 -O0 -fPIC")
	set (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -fPIC")
	# CPP flags
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -include ${insieme_common_include_dir}/insieme/common/utils/cxx14_workaround.h")
	# conform to the same template unfolding depth limit as GCC 6.2
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ftemplate-depth-900")
	set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g3 -O0")	
  	set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")
endif ()

# --------------------------- Intel Compiler -------------------------
if (${CMAKE_CXX_COMPILER} MATCHES "icpc")
	# add general flags
	add_definitions( -Wall )
	
	set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g3 -O0")
  	set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")
	
	include(CheckCXXCompilerFlag)
	# check for -std=c++14
	check_cxx_compiler_flag( -std=c++14 CXX14_Support )
	if(CXX14_Support)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
	else()
		message(WARNING  "WARNING: -std=c++14 not supported by your compiler!" )
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
	set (git_arg "rev-parse;--short;HEAD")

	execute_process(COMMAND ${git_cmd} ${git_arg}
	                WORKING_DIRECTORY ${insieme_code_dir}
	                RESULT_VARIABLE git_result
	                OUTPUT_VARIABLE insieme_version)

	# git returns space and newline, remove them to keep commands clean
	string(REPLACE "\n" " " insieme_version ${insieme_version})
	string(REPLACE " " "" insieme_version ${insieme_version})


	# deduce the name of the currently checked out branch
	set (git_arg "rev-parse;--abbrev-ref;HEAD")

	execute_process(COMMAND ${git_cmd} ${git_arg}
	                WORKING_DIRECTORY ${insieme_code_dir}
	                RESULT_VARIABLE git_result
	                OUTPUT_VARIABLE insieme_branch)

	# git returns space and newline, remove them to keep commands clean
	string(REPLACE "\n" " " insieme_branch ${insieme_branch})
	string(REPLACE " " "" insieme_branch ${insieme_branch})
else()
	set(insieme_version "unknown")
	set(insieme_branch "unknown")
endif()

# add insieme version definition (add_definitions escapes back-quotes)
#
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DINSIEME_VERSION=\"\\\"${insieme_version}\\\"\"")

# --------------------------------------------------------- Analysis Backends
option(ANALYSIS_DATALOG "Enable Datalog engine for analysis" ON)
if(ANALYSIS_DATALOG)
	add_definitions(-DINSIEME_ANALYSIS_DATALOG)
endif()

option(ANALYSIS_HASKELL "Enable Haskell engine for analysis" ON)
if(ANALYSIS_HASKELL)
	add_definitions(-DINSIEME_ANALYSIS_HASKELL)
endif()

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

# -------------------------------------------- Set backend compilers if specified, default to gcc/g++ otherwise
if( DEFINED ENV{INSIEME_C_BACKEND_COMPILER} )
	set(INSIEME_C_BACKEND_COMPILER $ENV{INSIEME_C_BACKEND_COMPILER})
else()
	message(WARNING "INSIEME_C_BACKEND_COMPILER environment variable not set, defaulting to gcc in PATH")
	set(INSIEME_C_BACKEND_COMPILER "gcc")
endif()
if( DEFINED ENV{INSIEME_CXX_BACKEND_COMPILER} )
	set(INSIEME_CXX_BACKEND_COMPILER $ENV{INSIEME_CXX_BACKEND_COMPILER})
else()
	message(WARNING "INSIEME_CXX_BACKEND_COMPILER environment variable not set, defaulting to g++ in PATH")
	set(INSIEME_CXX_BACKEND_COMPILER "g++")
endif()

# Define some colors for printing
if(NOT WIN32)
  string(ASCII 27 PrintEsc)
  set(PrintColorReset       "${PrintEsc}[m")
  set(PrintColorBold        "${PrintEsc}[1m")
  set(PrintColorRed         "${PrintEsc}[31m")
  set(PrintColorGreen       "${PrintEsc}[32m")
  set(PrintColorYellow      "${PrintEsc}[33m")
  set(PrintColorBlue        "${PrintEsc}[34m")
  set(PrintColorMagenta     "${PrintEsc}[35m")
  set(PrintColorCyan        "${PrintEsc}[36m")
  set(PrintColorWhite       "${PrintEsc}[37m")
  set(PrintColorBoldRed     "${PrintEsc}[1;31m")
  set(PrintColorBoldGreen   "${PrintEsc}[1;32m")
  set(PrintColorBoldYellow  "${PrintEsc}[1;33m")
  set(PrintColorBoldBlue    "${PrintEsc}[1;34m")
  set(PrintColorBoldMagenta "${PrintEsc}[1;35m")
  set(PrintColorBoldCyan    "${PrintEsc}[1;36m")
  set(PrintColorBoldWhite   "${PrintEsc}[1;37m")
endif()
