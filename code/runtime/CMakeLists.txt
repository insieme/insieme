#
# Builds the insieme runtime
#
project(insieme_runtime C CXX)
cmake_minimum_required(VERSION 2.6)

#user configurable options
if (MSVC)
	option (USE_PTHREADS "Use the Pthread Win32 Library (requires PTHREAD_ROOT environment var)" OFF)
endif()
option (USE_OPENCL "Build Insieme Runtime with Opencl" OFF)


if (NOT third_part_libs_home )
	set ( third_part_libs_home $ENV{INSIEME_LIBS_HOME} CACHE PATH "Third part library home")
endif()


include(../lookup_lib.cmake)
include (../add_unit_test.cmake)

#add_subdirectory( pmlib )

# for win64 we need to add a assembly object file to each target, see below.
# For all other platforms this var is empty. This safes a lot of if clauses througout this file
set (WIN64ASM_OBJ "") 

if(MSVC)
	#you need to have environment variables PTHREAD_ROOT and GTEST_ROOT which point to the base dir of those libraries/headers
	set (gtest_LIB "$ENV{GTEST_ROOT}/build/Debug/gtest.lib")
	set (gtest_main_LIB "$ENV{GTEST_ROOT}/build/Debug/gtest_main.lib")
	set (GTEST_INCLUDE_DIRS "$ENV{GTEST_ROOT}/include")
	
	if (CMAKE_CL_64) #64 bit apps
		set (WIN64ASM_OBJ "${CMAKE_CURRENT_SOURCE_DIR}/asm/win64asm.obj")
		if (USE_PTHREADS)
			set (pthread_LIB "$ENV{PTHREAD_ROOT}/lib/x64/pthreadVC2.lib")
		endif()
	else () #32 bit apps
		if (USE_PTHREADS)
			set (pthread_LIB "$ENV{PTHREAD_ROOT}/lib/x86/pthreadVC2.lib")
		endif()
	endif()
	
	#set include dir if pthreads shall be used
	if (USE_PTHREADS)
		set (PTHREAD_INCLUDE_DIRS "$ENV{PTHREAD_ROOT}/include")	
	endif()
	
else() #only look for pthread lib on linux
	find_library(pthread_LIB pthread)
endif()

lookup_lib( PAPI papi )
	
# set compiler flags for gcc
if (CMAKE_COMPILER_IS_GNUCC)
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -rdynamic -fPIC")

	# add flags for debug mode
	set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0 -fPIC")
  
	# add flags for release mode
	set (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -fPIC")
	
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_XOPEN_SOURCE=700")
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_GNU_SOURCE")

	include(CheckCCompilerFlag)
	check_c_compiler_flag( --std=gnu99 C99_Support )

	if(C99_Support)
	       	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} --std=gnu99")
	else()
		message( "WARNING: --std=gnu99 not supported by your compiler!" )
	endif()
endif()

if(CMAKE_COMPILER_IS_ICC)
	#enable c99 for icc
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Qstd=c99")
endif()

#if pthreads shall be used under Windows the define IRT_USE_PTHREADS is set
if (MSVC AND USE_PTHREADS)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /D IRT_USE_PTHREADS") #with Visual Studio we compile code as C++ Code => set CXX Flag
endif()


#collect all source and header files
file(GLOB_RECURSE insieme_runtime_srcs 	src/*.c )
file(GLOB_RECURSE insieme_runtime_incs 	include/*.h )

#using MSVC we need to tell cmake to treat code as c++ code (also applies to testcases at the bottom)
if (MSVC)
	foreach (source_file ${insieme_runtime_srcs})
		set_source_files_properties(${source_file} PROPERTIES LANGUAGE CXX)
	endforeach(source_file)
endif()

# We also search for OpenCL in the SDK default location

if ( USE_OPENCL )
	find_library(OPENCL_LIBS NAMES OpenCL PATHS $ENV{OPENCL_ROOT}/lib/x86_64)
	if ( ${OPENCL_LIBS} STREQUAL "OPENCL_LIBS-NOTFOUND")
		set(USE_OPENCL OFF)
		message(WARNING "OPENCL DISABLED, LIBRARY NOT FOUND")	
	else ( ${OPENCL_LIBS} STREQUAL "OPENCL_LIBS-NOTFOUND")
		add_definitions(-DUSE_OPENCL)
		message(STATUS "Found At: " ${OPENCL_LIBS})
		message(STATUS "OPENCL ENABLED")
		message(STATUS "USING ${OPENCL_LIBS}")
		# list all include paths
		include_directories(include/ $ENV{OPENCL_ROOT}/include/)			
	endif(${OPENCL_LIBS} STREQUAL "OPENCL_LIBS-NOTFOUND")
	
else ( USE_OPENCL )
	message(STATUS "OPENCL DISABLED")
endif( USE_OPENCL )

# create the runtime target
add_executable(insieme_runtime ${insieme_runtime_srcs} ${insieme_runtime_incs} ${WIN64ASM_OBJ})

if ( USE_OPENCL )
	target_link_libraries(insieme_runtime ${OPENCL_LIBS})
endif ( USE_OPENCL )


#link libraries for the insieme_runtime target
if(NOT MSVC OR USE_PTHREADS)
	target_link_libraries(insieme_runtime ${pthread_LIB})
endif()
if(NOT MSVC)
	target_link_libraries(insieme_runtime dl rt m ${papi_LIB})
endif()

# lookup Google Test libraries
lookup_lib ( GTEST gtest )
lookup_lib ( GTEST_MAIN gtest_main )

#include_directories(include/ ${PTHREAD_INCLUDE_DIRS} ${GTEST_INCLUDE_DIRS} ${OPENCL_INCLUDE_DIR}) #what's this include/ thing? S. Widerin
include_directories(${PTHREAD_INCLUDE_DIRS} ${GTEST_INCLUDE_DIRS} ${OPENCL_INCLUDE_DIR})

# --------------------------------------------------------- Valgrind / GTest testing suite
# avoid multiple import
if (NOT MEMORY_CHECK_SETUP)
	option(CONDUCT_MEMORY_CHECKS "Checks all test cases for memory leaks using valgrind if enabled." OFF)

	# add -all-valgrind target
	add_custom_target(valgrind)

endif (NOT MEMORY_CHECK_SETUP)

# handle gtest unit tests
set ( ut_prefix  ut_rt_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file} ${WIN64ASM_OBJ})
	target_link_libraries(${case_name} ${pthread_LIB})
	if(NOT MSVC)
		target_link_libraries(${case_name} dl)
		target_link_libraries(${case_name} rt)
		target_link_libraries(${case_name} m)
		target_link_libraries(${case_name} ${papi_LIB})
	endif()
	# add dependency to google test libraries
	target_link_libraries(${case_name} ${gtest_LIB})
	target_link_libraries(${case_name} ${gtest_main_LIB})
		
	if ( USE_OPENCL )
    	target_link_libraries(${case_name} ${OPENCL_LIBS})
	endif ( USE_OPENCL )
	
	# disable valgrind for parallel test cases
	set(VFLAG ${TRUE})
	if(case_name MATCHES ".*_par")
		set(VFLAG FALSE)
	endif(case_name MATCHES ".*_par")
	
	#add_unit_test(${case_name} ${VFLAG})
	add_unit_test(${case_name} FALSE)
	
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp") #as one test uses openmp
	
endforeach(case_file)

# handle manual (library) test cases
file(GLOB test_cases test/manual_*.c)
foreach (case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_library(${case_name} SHARED ${case_file} ${WIN64ASM_OBJ})
	if(NOT MSVC)
		target_link_libraries(${case_name} ${pthread_LIB})
		target_link_libraries(${case_name} dl)
		target_link_libraries(${case_name} m)
		target_link_libraries(${case_name} ${papi_LIB})
	else()
		set_source_files_properties(${case_file} PROPERTIES LANGUAGE CXX)
	endif()
endforeach(case_file)

# handle standalone (executable) test cases
file(GLOB test_cases test/standalone_*.c)
foreach (case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	include_directories(include/ $ENV{OPENCL_ROOT}/include/)
	add_executable(${case_name} ${case_file} ${WIN64ASM_OBJ})
	target_link_libraries(${case_name} ${pthread_LIB})
	if(NOT MSVC)
		target_link_libraries(${case_name} dl)
		target_link_libraries(${case_name} rt)
		target_link_libraries(${case_name} m)
		target_link_libraries(${case_name} ${papi_LIB})
	else()
		set_source_files_properties(${case_file} PROPERTIES LANGUAGE CXX)
	endif()
	
	if ( USE_OPENCL )
    	target_link_libraries(${case_name} ${OPENCL_LIBS})
	endif ( USE_OPENCL )
endforeach(case_file)
