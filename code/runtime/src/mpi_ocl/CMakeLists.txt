#
# Builds the mpi OpenCL worker 
#
project(worker C CXX)
cmake_minimum_required(VERSION 2.6)

option (USE_OPENCL "Build Insieme Runtime with Opencl" OFF)

if (NOT third_part_libs_home )
	set ( third_part_libs_home $ENV{INSIEME_LIBS_HOME} CACHE PATH "Third part library home")
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../../include/irt_ocl_mpi_worker.h.in 
			   ${CMAKE_CURRENT_SOURCE_DIR}/../../include/irt_ocl_mpi_worker.h)

find_library(pthread_LIB pthread)
	
# set compiler flags for gcc
if (CMAKE_COMPILER_IS_GNUCC)
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -rdynamic -fPIC -lrt")

	# add flags for debug mode
	set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0 -fPIC")
  
	# add flags for release mode
	set (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -fPIC")
	
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_XOPEN_SOURCE=700")
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_GNU_SOURCE")

	include(CheckCCompilerFlag)
	check_c_compiler_flag( --std=gnu99 C99_Support )
	
	if(NOT WIN32)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp") #as one gtest uses openmp
	endif()
	
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

#collect all source and header files
file(GLOB_RECURSE worker_srcs 	irt_ocl_mpi_worker.c )

# find MPI
find_package(MPI REQUIRED)

include_directories(${MPI_INCLUDE_PATH} ${PTHREAD_INCLUDE_DIRS} ${OPENCL_INCLUDE_DIR})

# We also search for OpenCL in the SDK default location
if ( USE_OPENCL )
	find_library(OPENCL_LIBS NAMES OpenCL PATHS $ENV{OPENCL_ROOT}/lib/x86_64)
	if ( ${OPENCL_LIBS} STREQUAL "OPENCL_LIBS-NOTFOUND")
		set(USE_OPENCL OFF)
		message(WARNING "OPENCL DISABLED, LIBRARY NOT FOUND")	
	else ( ${OPENCL_LIBS} STREQUAL "OPENCL_LIBS-NOTFOUND")
		add_definitions(-DUSE_OPENCL)
		add_definitions(-DLOCAL_MODE)
		message(STATUS "Found At: " ${OPENCL_LIBS})
		message(STATUS "OPENCL ENABLED")
		message(STATUS "USING ${OPENCL_LIBS}")
	endif(${OPENCL_LIBS} STREQUAL "OPENCL_LIBS-NOTFOUND")
else ( USE_OPENCL )
	message(STATUS "OPENCL DISABLED")
endif( USE_OPENCL )

# create the worker target
add_executable(worker ${worker_srcs} ${MPI_INCLUDE_PATH} ${WIN64ASM_OBJ})
target_link_libraries(worker ${OPENCL_LIBS} ${MPI_LIBRARIES} ${MPI_COMPILE_FLAGS} ${MPI_LINK_FLAGS} ${pthread_LIB})
