#
# This file sets up some general variables and include paths for the build environment
#

# Configuration:
#   $ENV{XERCES_HOME}           Both
#   $ENV{GLOG_HOME}             Linux
#   LINKING_TYPE                Linux
#   LLVM_HOME/$ENV{LLVM_HOME}   Both


# -------------------------------------------------------------- define some code locations

# SET(CMAKE_BUILD_TYPE "Debug")

# get code root directory (based on current file name path)
get_filename_component( insieme_code_dir ${CMAKE_CURRENT_LIST_FILE} PATH )

set ( insieme_core_src_dir 	            	${insieme_code_dir}/core/src )
set ( insieme_utils_src_dir 	         	${insieme_code_dir}/utils/src )

set ( insieme_core_include_dir 	         	${insieme_code_dir}/core/include )
set ( insieme_utils_include_dir          	${insieme_code_dir}/utils/include )
set ( insieme_c_info_include_dir         	${insieme_code_dir}/c_info/include )

set ( insieme_frontend_include_dir       	${insieme_code_dir}/frontend/include )
set ( insieme_backend_include_dir       	${insieme_code_dir}/backend/include )

set ( insieme_xml_include_dir            	${insieme_code_dir}/xml/include )
set ( insieme_driver_include_dir         	${insieme_code_dir}/driver/include )

set ( insieme_simple_backend_include_dir 	${insieme_code_dir}/simple_backend/include )
set ( insieme_opencl_backend_include_dir 	${insieme_code_dir}/opencl_backend/include )

set ( insieme_analysis_include_dir       	${insieme_code_dir}/analysis/include )
set ( insieme_transform_include_dir       	${insieme_code_dir}/transform/include )

# ------------------------------------------------------------- configuration for platforms
if(MSVC)   # Windows Visual Studio

  # MSVC can compile insieme statical only
  if(NOT LINKING_TYPE)
	set(LINKING_TYPE STATIC)
  endif(NOT LINKING_TYPE)

  # Therefore Boost needs to be linked statically
  set(Boost_USE_STATIC_LIBS ON)

  set(DO_INSTALL FALSE)

else(MSVC) # Linux or Cygwin/MinGW

    # Default is here: shared linking
    if(NOT LINKING_TYPE)
  	    set(LINKING_TYPE SHARED)
    endif(NOT LINKING_TYPE)

  set(DO_INSTALL TRUE)
  
endif(MSVC)


# --------------------------------------------------------------------- including libraries

# - boost
find_package( Boost COMPONENTS program_options )
find_package( Boost COMPONENTS system )
find_package( Boost COMPONENTS filesystem )
include_directories( ${Boost_INCLUDE_DIRS} )
link_directories(${Boost_LIBRARY_DIRS})

# - xerces
include_directories( $ENV{XERCES_HOME}/include )
find_library(xerces_LIB NAMES xerces-c PATHS $ENV{XERCES_HOME}/lib)

# lookup perl
find_package( Perl )

# lookup ISL library
#Fix LLVM path
if(NOT DEFINED ISL_HOME)
	set (ISL_HOME $ENV{ISL_HOME})
endif()
include_directories( ${ISL_HOME}/include )
find_library(isl_LIB NAMES isl PATHS ${ISL_HOME}/lib)
if(MSVC) 
	set (isl_LIB dummy)
endif(MSVC)

# lookup pthread library
find_library(pthread_LIB pthread)
# http://fedetft.wordpress.com/2010/03/07/cmake-part-3-finding-libraries/
#find_package(Threads REQUIRED)
#target_link_libraries(test ${CMAKE_THREAD_LIBS_INIT})


# -------------------------------------------------------------- LLVM / CLANG 2.8 libraries

#Fix LLVM path
if(NOT DEFINED LLVM_HOME)
	set (LLVM_HOME $ENV{LLVM_HOME})
endif()

# Full (?) list of clang libraries
# TODO: needs some cmake feature to gather "lib/libLLVM*.lib"
set(clang_LList
    clangBasic clangSema clangIndex clangDriver clangAST
    clangRewrite clangAnalysis clangLex clangFrontend clangFrontendTool 
    clangParse clangSerialization
)

if(MSVC)
    # Manual list - TODO: needs some cmake feature to gather "lib/libLLVM*.lib"
    set( llvm_LList
       LLVMSystem LLVMCore LLVMCodeGen LLVMSelectionDAG LLVMAsmPrinter LLVMBitReader 
       LLVMBitWriter LLVMTransformUtils LLVMX86AsmParser LLVMX86AsmPrinter LLVMX86CodeGen
       LLVMX86Info LLVMX86Disassembler LLVMInstrumentation LLVMInstCombine LLVMScalarOpts 
       LLVMipo LLVMLinker LLVMAnalysis LLVMipa LLVMExecutionEngine LLVMInterpreter 
       LLVMJIT LLVMTarget LLVMAsmParser LLVMArchive LLVMSupport LLVMSelectionDAG LLVMMC 
       LLVMMCDisassembler LLVMMCParser
    )
    set(clang_LList libclang ${clang_LList})

else(MSVC)
	# On Linux we have a .so file for all LLVM
    set(llvm_LList  LLVM-2.9 )
    set(clang_LList clang ${clang_LList})
endif(MSVC)


# Find all llvm libraries
foreach (name ${llvm_LList})
    find_library(llvm_${name}_LIB   NAMES ${name}   PATHS ${LLVM_HOME}/lib)
    set(llvm_LIBs ${llvm_${name}_LIB} ${llvm_LIBs})
endforeach(name)


# Find (all?) clang libraries
foreach (name ${clang_LList})
    find_library(clang_${name}_LIB   NAMES ${name}   PATHS ${LLVM_HOME}/lib)
    set(clang_LIBs ${clang_${name}_LIB} ${clang_LIBs})
endforeach(name)

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
endif()

# enable C++0x support within gcc (if supported)
if (CMAKE_COMPILER_IS_GNUCXX)

	# add general flags
	add_definitions( -fshow-column )
	add_definitions( -Wall )

	# add flags for debug mode
	set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0")
  
  # add flags for release mode
  set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")

	# ENABLE PROFILING
	# add_definitions( -pg )
	#SET(CMAKE_EXE_LINKER_FLAGS -pg)


	# check for -std=c++0x
	include(CheckCXXCompilerFlag)
	check_cxx_compiler_flag( -std=c++0x CXX0X_Support )
	if(CXX0X_Support)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
	else()
		message( "WARNING: --std=c++0x not supported by your compiler!" )
	endif()
endif()

# enable C99 support within gcc
if (CMAKE_COMPILER_IS_GNUC)
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -rdynamic -fPIC")

	# add flags for debug mode
	set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0 -fPIC")
  
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

# enable warnings
if(MSVC) 
	add_definitions( /W4 )
endif()

# --------------------------------------------------------- Runtime
# -D_XOPEN_SOURCE=700 is required to get recent pthread features with -std=c99
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 -D_XOPEN_SOURCE=700")
# required for affinity-related macros
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_GNU_SOURCE")
# set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pg")

# --------------------------------------------------------- Valgrind / GTest testing suite
# avoid multiple import
if (NOT MEMORY_CHECK_SETUP)
	option(CONDUCT_MEMORY_CHECKS "Checks all test cases for memory leaks using valgrind if enabled." OFF)

	# add -all-valgrind target
	add_custom_target(valgrind)

	# define macro for adding tests
	macro ( add_unit_test case_name )
		
		# take value from environment variable
		set(USE_VALGRIND ${CONDUCT_MEMORY_CHECKS})

		# check whether there was a 2nd argument
		if(${ARGC} GREATER 1)
			# use last argument as a valgrind flag
			set(USE_VALGRIND ${ARG2})
		endif(${ARGC} GREATER 1)
		
		# add test case
		if(USE_VALGRIND)
			# no valgrind support in MSVC 
			if(NOT MSVC)
				# add valgrind as a test
				add_test(NAME valgrind_${case_name} 
					COMMAND valgrind
						--leak-check=full
						--show-reachable=no
						--track-fds=yes
						--error-exitcode=1
						#--log-file=${CMAKE_CURRENT_BINARY_DIR}/valgrind.log.${case_name}
						${CMAKE_CURRENT_BINARY_DIR}/${case_name}
					WORKING_DIRECTORY
						${CMAKE_CURRENT_BINARY_DIR}
				)
			endif(NOT MSVC)
		else(USE_VALGRIND)
			# add normal test
			add_test(${case_name} ${case_name})

			# + valgrind as a custom target (only of not explicitly prohibited)
			if ((NOT MSVC) AND ((NOT (${ARGC} GREATER 1)) OR (${ARG2})))
				add_custom_target(valgrind_${case_name} 
					COMMAND valgrind
						--leak-check=full
						--show-reachable=no
						--track-fds=yes
						--error-exitcode=1
						#--log-file=${CMAKE_CURRENT_BINARY_DIR}/valgrind.log.${case_name}
						${CMAKE_CURRENT_BINARY_DIR}/${case_name}
					WORKING_DIRECTORY
						${CMAKE_CURRENT_BINARY_DIR}
				)
				add_dependencies(valgrind valgrind_${case_name})
			endif ((NOT MSVC) AND ((NOT (${ARGC} GREATER 1)) OR (${ARG2})))
		endif(USE_VALGRIND)
	endmacro(add_unit_test)
endif (NOT MEMORY_CHECK_SETUP)

# mark as defined
set(MEMORY_CHECK_SETUP OFF CACHE INTERNAL "Flag to avoid multiple setup" PARENT_SCOPE)


