# Custom compile flags
if(CMAKE_C_COMPILER_ID STREQUAL "GNU" OR CMAKE_C_COMPILER_ID STREQUAL "Clang")
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -pedantic -std=c11 -fPIC")
	add_definitions(-D_XOPEN_SOURCE=700 -D_GNU_SOURCE)

	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -rdynamic")
	set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -rdynamic")
endif()

# Papi
if(USE_PAPI)
	find_package(PAPI REQUIRED)
	add_definitions(-DIRT_USE_PAPI)
endif()

# HWloc
if(NOT MSVC)
	find_package(Hwloc)
	if(Hwloc_FOUND)
		add_definitions(-DIRT_USE_HWLOC)

		# TODO
		# I tried to do this as it is supposed to be done, but CMake sucks
		add_definitions("${Hwloc_CFLAGS}")
		# this would be the correct way, but it requires the user to set LD_LIBRARY_FLAGS
		# however, it would obviate the need for the link_hwloc function
		#string (REPLACE ";" " " Hwloc_LDFLAGS_STR "${Hwloc_LDFLAGS}")
		#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${Hwloc_LDFLAGS_STR}")
		#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${Hwloc_LDFLAGS_STR}")
	endif()
endif()

glob_sources(runtime_srcs src)
glob_headers(runtime_incs include)

# Visual Studio should treat the sources as C++ code
if(MSVC)
	foreach(src ${runtime_srcs})
		set_source_files_properties(${src} PROPERTIES LANGUAGE CXX)
	endforeach(src)
	set(runtime_srcs "${runtime_srcs} ${runtime_incs}")
endif()

add_executable(runtime ${runtime_srcs})
target_link_libraries(runtime dl rt m)
target_link_libraries(runtime common)
target_include_directories(runtime PRIVATE include)

# pthread
find_package(Threads REQUIRED)
target_link_libraries(runtime ${CMAKE_THREAD_LIBS_INIT})

# Papi
if(USE_PAPI)
	target_link_libraries(runtime ${PAPI_LIBRARIES})
	target_include_directories(runtime PRIVATE ${PAPI_INCLUDE_DIRS})
endif()

# HWloc
if(Hwloc_FOUND)
	target_link_libraries(runtime ${HWLOC_LIBRARIES})
endif()

if(USE_MPI_OPENCL)
	find_package(MPI REQUIRED)
	target_link_libraries(runtime ${MPI_C_LIBRARIES} )
	target_include_directories(runtime PRIVATE ${MPI_INCLUDE_PATH})

	set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} ${MPI_COMPILE_FLAGS})
	set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} ${MPI_COMPILE_FLAGS})

	set(CMAKE_C_LINK_FLAGS ${CMAKE_C_LINK_FLAGS} ${MPI_LINK_FLAGS})
	set(CMAKE_CXX_LINK_FLAGS ${CMAKE_CXX_LINK_FLAGS} ${MPI_LINK_FLAGS})
endif()

# TODO check if this is needed
target_include_directories(runtime PRIVATE ${OPENCL_INCLUDE_DIR})

# Note that the unit tests are compiled with g++ so the runtime code is
# compiled as well as C++ this gives problems with anonymous structs as they
# are not supported by C++/C (before C11).

# OpenMP is required for unit tests
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")

# Also we ignore warnings about signed comparison and unused variables
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sign-compare")

glob_tests(runtime_tests test)
foreach(test ${runtime_tests})
	add_module_unittest(runtime ${test} TARGET_NAME tgt_name)

	if(BUILD_TESTS)
		target_include_directories(${test_name} PRIVATE include)

		target_link_libraries(${tgt_name} dl rt m)
		target_link_libraries(${tgt_name} common)

		if(USE_PAPI)
			target_link_libraries(${tgt_name} ${PAPI_LIBRARIES})
		endif()

		if(USE_MPI_OPENCL)
			target_link_libraries(${tgt_name} ${MPI_C_LIBRARIES})
		endif()

		if(Hwloc_FOUND)
			target_link_libraries(${tgt_name} ${HWLOC_LIBRARIES})
		endif()
	endif()
endforeach(test)

# handle manual (library) test cases
file(GLOB runtime_manual_tests test/manual_*.c)
foreach(test ${runtime_manual_tests})
	if(BUILD_TESTS)
		get_filename_component(test_name ${test} NAME_WE)

		add_library(${test_name} SHARED ${test})
		target_include_directories(${test_name} PRIVATE include)

		target_link_libraries(${test_name} dl rt m)
		target_link_libraries(${test_name} common)

		if(USE_PAPI)
			target_link_libraries(${test_name} ${PAPI_LIBRARIES})
		endif()

		if(USE_MPI_OPENCL)
			target_link_libraries(${test_name} ${MPI_C_LIBRARIES})
		endif()

		if(Hwloc_FOUND)
			target_link_libraries(${test_name} ${HWLOC_LIBRARIES})
		endif()

		if(MSVC)
			set_source_files_properties(${test} PROPERTIES LANGUAGE CXX)
		endif()
	endif()
endforeach(test)

# handle standalone (executable) test cases
file(GLOB runtime_standalone_tests test/standalone_*.c)
foreach(test ${runtime_standalone_tests})
	if(BUILD_TESTS)
		get_filename_component(test_name ${test} NAME_WE)

		add_executable(${test_name} ${test})
		target_include_directories(${test_name} PRIVATE include)

		# TODO fix OpenCL Path
		target_include_directories(${test_name} PRIVATE $ENV{OPENCL_ROOT}/include)

		target_link_libraries(${test_name} dl rt m)
		target_link_libraries(${test_name} common)

		target_link_libraries(${test_name} ${CMAKE_THREAD_LIBS_INIT})

		if(USE_PAPI)
			target_link_libraries(${test_name} ${PAPI_LIBRARIES})
		endif()

		if(USE_MPI_OPENCL)
			target_link_libraries(${test_name} ${MPI_C_LIBRARIES})
		endif()

		if(Hwloc_FOUND)
			target_link_libraries(${test_name} ${HWLOC_LIBRARIES})
		endif()

		if(MSVC)
			set_source_files_properties(${test} PROPERTIES LANGUAGE CXX)
		endif()
	endif()
endforeach(test)
