#
# Builds the insieme runtime
#
project(insieme_runtime C CXX)
message(STATUS "========== configure runtime =============")
cmake_minimum_required(VERSION 3.2)

# user configurable options
if (WIN32)
	option (USE_PTHREADS "Use the Pthread Win32 Library (requires PTHREAD_ROOT environment var)" OFF)
endif()
option (USE_OPENCL "Build Insieme Runtime with Opencl" OFF)

#to find the custom modules (for find_package)
#TODO better path
get_filename_component( insieme_code_dir ${CMAKE_CURRENT_LIST_FILE} PATH )

list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/../cmake/")

#find them in CMAKE_MODULE_PATH
include(default_library_configuration)
include(insieme_find_package)
include(add_unit_test)

# for win64 we need to add a assembly object file to each target, see below.
# For all other platforms this var is empty. This safes a lot of if clauses througout this file
set (WIN64ASM_OBJ "")

if(WIN32)
	#you need to have environment variables PTHREAD_ROOT and GTEST_ROOT which point to the base dir of those libraries/headers
	set (gtest_LIB "$ENV{GTEST_ROOT}/build/Debug/gtest.lib")
	set (gtest_main_LIB "$ENV{GTEST_ROOT}/build/Debug/gtest_main.lib")
	set (GTEST_INCLUDE_DIRS "$ENV{GTEST_ROOT}/include")
	
	if (CMAKE_CL_64) #64 bit apps
		message(STATUS "Creating WIN x64 project")
		set (WIN64ASM_OBJ "${CMAKE_CURRENT_SOURCE_DIR}/asm/win64asm.obj")
		if (USE_PTHREADS)
			set (CMAKE_THREAD_LIBS_INIT "$ENV{PTHREAD_ROOT}/lib/x64/pthreadVC2.lib")
		endif()
	else () #32 bit apps
		if (USE_PTHREADS)
			set (CMAKE_THREAD_LIBS_INIT "$ENV{PTHREAD_ROOT}/lib/x86/pthreadVC2.lib")
		endif()
	endif()
	
	#set include dir if pthreads shall be used
	if (USE_PTHREADS)
		set (PTHREAD_INCLUDE_DIRS "$ENV{PTHREAD_ROOT}/include")	
	endif()
	
else() 
	#only look for pthread lib on linux
	find_package(Threads REQUIRED)

	#look for papi
	if(USE_PAPI)
		insieme_find_package(NAME PAPI)
		add_definitions(-DIRT_USE_PAPI)
	endif()
endif()
	
# set compiler flags for gcc
if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_COMPILER_IS_GNUCC 
		OR CMAKE_C_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -rdynamic -fPIC")
	
	# add flags for debug mode
	set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0 -fPIC")
  
	# add flags for release mode
	set (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -fPIC")
	
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_XOPEN_SOURCE=700")
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_GNU_SOURCE")

	include(CheckCCompilerFlag)
	check_c_compiler_flag( -std=gnu99 C99_Support )
	check_c_compiler_flag( -std=c11   C11_Support )
	

	#Compile with C11 and all warnings enabled, pedantic and warnings treated as errors for GCC>=4.9
	#to avoid a problem with 4.8 and C11 we only want c11 with 4.9
	if (CMAKE_C_COMPILER_VERSION VERSION_GREATER 4.9 OR CMAKE_C_COMPILER_VERSION VERSION_EQUAL 4.9)
		set(WE_HAVE_GCC_49 TRUE)
	endif()

	if(C11_Support AND WE_HAVE_GCC_49)
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Werror -pedantic")
	elseif(C99_Support)
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu99")
	else()
		message(WARNING "WARNING: -std=gnu99 not supported by your compiler!" )
	endif()
endif()

if(CMAKE_COMPILER_IS_ICC)
	#enable c99 for icc
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Qstd=c99")
endif()

#if pthreads shall be used under Windows the define IRT_USE_PTHREADS is set
if (MSVC AND USE_PTHREADS)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /D IRT_USE_PTHREADS") #with Visual Studio we compile code as C++ Code => set CXX Flag
elseif (WIN32 AND USE_PTHREADS)
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /D IRT_USE_PTHREADS")
endif()

# find HWloc package
if (NOT MSVC)
	set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/code/cmake")
	insieme_find_package(NAME Hwloc)
	if (Hwloc_FOUND)
		add_definitions(-DIRT_USE_HWLOC)
		# I tried to do this as it is supposed to be done, but CMake sucks
		add_definitions("${Hwloc_CFLAGS}")
		# this would be the correct way, but it requires the user to set LD_LIBRARY_FLAGS
		# however, it would obviate the need for the link_hwloc function
		#string (REPLACE ";" " " Hwloc_LDFLAGS_STR "${Hwloc_LDFLAGS}")
		#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${Hwloc_LDFLAGS_STR}")
		#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${Hwloc_LDFLAGS_STR}")
	endif()

	function(link_hwloc target)
		if (Hwloc_FOUND)
			target_link_libraries(${target} ${HWLOC_LIBRARIES})
		endif()
	endfunction(link_hwloc)
else()
	function(link_hwloc target)
	endfunction(link_hwloc)
endif()

#collect all source and header files
file(GLOB_RECURSE insieme_runtime_srcs 	src/runtime.c )
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
		if ( USE_MPI_OPENCL )
			add_definitions(-DREMOTE_MODE)
		else ( USE_MPI_OPENCL )
			add_definitions(-DLOCAL_MODE)	
		endif ( USE_MPI_OPENCL )
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

target_link_libraries(insieme_runtime dl rt m)

link_hwloc(insieme_runtime)

if(USE_PAPI)
	#papi header+libraries
	target_include_directories(insieme_runtime PRIVATE ${PAPI_INCLUDE_DIRS})
	target_link_libraries(insieme_runtime ${PAPI_LIBRARIES})
endif()
	

if ( USE_OPENCL )
	target_link_libraries(insieme_runtime ${OPENCL_LIBS})
endif ( USE_OPENCL )

#link pthread libraries for the insieme_runtime target
if(NOT WIN32 OR USE_PTHREADS) 
	target_link_libraries(insieme_runtime ${CMAKE_THREAD_LIBS_INIT})
endif()


if ( USE_MPI_OPENCL )
	find_package(MPI REQUIRED)
	include_directories(${MPI_INCLUDE_PATH})
	set(CMAKE_C_COMPILE_FLAGS ${CMAKE_C_COMPILE_FLAGS} ${MPI_COMPILE_FLAGS})
	set(CMAKE_C_LINK_FLAGS ${CMAKE_C_LINK_FLAGS} ${MPI_LINK_FLAGS})
	set(CMAKE_CXX_COMPILE_FLAGS ${CMAKE_CXX_COMPILE_FLAGS} ${MPI_COMPILE_FLAGS})
	set(CMAKE_CXX_LINK_FLAGS ${CMAKE_CXX_LINK_FLAGS} ${MPI_LINK_FLAGS})

	target_link_libraries(insieme_runtime ${MPI_C_LIBRARIES} )
endif ( USE_MPI_OPENCL )

include_directories(${PTHREAD_INCLUDE_DIRS} ${OPENCL_INCLUDE_DIR})

#insieme_common is header-only library
target_link_libraries(insieme_runtime insieme_common)

# --------------------------------------------------------- Valgrind / GTest testing suite
# avoid multiple import
if(NOT MEMORY_CHECK_SETUP)
	option(CONDUCT_MEMORY_CHECKS "Checks all test cases for memory leaks using valgrind if enabled." OFF)

	if(NOT TARGET valgrind)
		# the valgrind target was not added before
		# happens when we build runtime only
		# add -all-valgrind target
		add_custom_target(valgrind)
	endif()
	# mark as defined
	set(MEMORY_CHECK_SETUP OFF CACHE INTERNAL "Flag to avoid multiple setup" PARENT_SCOPE)
endif()

# --------------------------------------------------------- Unit tests ---------------
# handle gtest unit tests
set(ut_prefix  ut_rt )
file(GLOB_RECURSE test_cases test/*.cc)
# get energy unit tests, to be removed if papi and msr module are not present
file(GLOB_RECURSE energy_test_cases test/energy*.cc)

include(CheckCXXCompilerFlag)
# check for -std=c++0x
check_cxx_compiler_flag( -std=c++0x CXX0X_Support )
# check for -std=c++11
check_cxx_compiler_flag( -std=c++11 CXX11_Support )

#carefull! the unit tests are compiled with g++ so the runtime code is compiled as well as c++
#this gives problems with anonymous structs as they are not supported by C++/C (before C11)
#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror -pedantic")

if(CXX11_Support)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
elseif(CXX0X_Support)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
elseif(MSVC_VERSION LESS 1900) # 1900 = Visual Studio 2015
	message(WARNING "WARNING: -std=c++0x or -std=c++11 not supported by your compiler!" )
endif()

# runtime unit tests use openmp
if(NOT WIN32)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")
endif()

foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}_${case_name} )

	add_executable(${case_name} ${case_file} ${WIN64ASM_OBJ})
	target_link_libraries(${case_name} insieme_common)
	if(NOT WIN32)
		target_link_libraries(${case_name} dl)
		target_link_libraries(${case_name} rt)
		target_link_libraries(${case_name} m)

		#papi header+libraries
		if(USE_PAPI)
			target_include_directories(${case_name} PRIVATE ${PAPI_INCLUDE_DIRS})
			target_link_libraries(${case_name} ${PAPI_LIBRARIES})
		endif()
		if ( USE_MPI_OPENCL )
			target_link_libraries(${case_name} ${MPI_C_LIBRARIES})
		endif ( USE_MPI_OPENCL )
	endif()
	link_hwloc(${case_name})
		
	if ( USE_OPENCL )
		target_link_libraries(${case_name} ${OPENCL_LIBS})
	endif ( USE_OPENCL )
	
	# disable valgrind for parallel test cases
	set(VFLAG true)
	if(case_name MATCHES ".*_par")
		set(VFLAG FALSE)
	endif(case_name MATCHES ".*_par")
	
	#disable valgrind for runtime unit tests
	add_unit_test(${case_name} ${ut_prefix} FALSE)
	
endforeach(case_file)

# handle manual (library) test cases
file(GLOB test_cases test/manual_*.c)
foreach (case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_library(${case_name} SHARED ${case_file} ${WIN64ASM_OBJ})
	target_link_libraries(${case_name} insieme_common)
	if(NOT WIN32)
		target_link_libraries(${case_name} ${CMAKE_THREAD_LIBS_INIT})
		target_link_libraries(${case_name} dl)
		target_link_libraries(${case_name} m)

		#papi header+libraries
		if(USE_PAPI)
			target_include_directories(${case_name} PRIVATE ${PAPI_INCLUDE_DIRS})
			target_link_libraries(${case_name} ${PAPI_LIBRARIES})
		endif()
		if ( USE_MPI_OPENCL )
			target_link_libraries(${case_name} ${MPI_C_LIBRARIES} )
		endif ( USE_MPI_OPENCL )
	else()
		set_source_files_properties(${case_file} PROPERTIES LANGUAGE CXX)
	endif()
	link_hwloc(${case_name})
endforeach(case_file)

# handle standalone (executable) test cases
file(GLOB test_cases test/standalone_*.c)
foreach (case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	include_directories(include/ $ENV{OPENCL_ROOT}/include/)
	add_executable(${case_name} ${case_file} ${WIN64ASM_OBJ})
	target_link_libraries(${case_name} insieme_common)
	target_link_libraries(${case_name} ${CMAKE_THREAD_LIBS_INIT})
	if(NOT WIN32)
		target_link_libraries(${case_name} dl)
		target_link_libraries(${case_name} rt)
		target_link_libraries(${case_name} m)
		
		#papi header+libraries
		if(USE_PAPI)
			target_include_directories(${case_name} PRIVATE ${PAPI_INCLUDE_DIRS})
			target_link_libraries(${case_name} ${PAPI_LIBRARIES})
		endif()
		if ( USE_MPI_OPENCL )
			target_link_libraries(${case_name} ${MPI_C_LIBRARIES} )
		endif ( USE_MPI_OPENCL )
	else()
		set_source_files_properties(${case_file} PROPERTIES LANGUAGE CXX)
	endif()
	link_hwloc(${case_name})
	
	if ( USE_OPENCL )
    	target_link_libraries(${case_name} ${OPENCL_LIBS})
	endif ( USE_OPENCL )
endforeach(case_file)
