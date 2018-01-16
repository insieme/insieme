# This file has been overhauled completely and in the process Windows (and ICC)
# stuff was dropped. If you need them, take a look at the previous version of
# this file.
#
#     $ git show afa30ee:code/runtime/CMakeLists.txt

# Attach all needed dependencies to the targets defined in here
macro(add_runtime_dependencies target)
	target_include_directories(${target} PRIVATE include)

	target_link_libraries(${target} dl rt m)
	target_link_libraries(${target} common)

	if(USE_PAPI)
		target_link_libraries(${target} ${PAPI_LIBRARIES})
		target_include_directories(${target} PRIVATE ${PAPI_INCLUDE_DIRS})
	endif()

	# TODO revisit FindHwloc script
	if(Hwloc_FOUND)
		target_link_libraries(${target} ${Hwloc_LIBRARIES})
		target_include_directories(${target} PRIVATE ${Hwloc_INCLUDE_DIRS})
	endif()
endmacro()

# Custom compile flags
if(CMAKE_C_COMPILER_ID STREQUAL "GNU" OR CMAKE_C_COMPILER_ID STREQUAL "Clang")
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -fPIC")
	add_definitions(-D_XOPEN_SOURCE=700 -D_GNU_SOURCE)

	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -rdynamic")
	set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -rdynamic")
endif()

# Papi
if(USE_PAPI)
	add_definitions(-DIRT_USE_PAPI)
endif()

# HWloc
if(NOT MSVC)
	find_package(Hwloc REQUIRED)
	add_definitions(-DIRT_USE_HWLOC)
	add_definitions("${Hwloc_CFLAGS}")
endif()

glob_sources(runtime_srcs src)
glob_headers(runtime_incs include)

if(MSVC)
	set(runtime_srcs ${runtime_srcs} ${runtime_incs})
endif()

add_executable(runtime ${runtime_srcs})
add_runtime_dependencies(runtime)

set_target_properties(runtime PROPERTIES LINKER_LANGUAGE C)

# Fix source files for Visual Studio
if(MSVC)
	msvc_source_group("Source Files" "${runtime_srcs}" STRIP src)
	msvc_source_group("Header Files" "${runtime_incs}" STRIP include)
	set_target_properties(runtime PROPERTIES FOLDER "runtime")
endif()

# pthread
target_link_libraries(runtime Threads::Threads)

# Note that the unit tests are compiled with g++ so the runtime code is
# compiled as well as C++ this gives problems with anonymous structs as they
# are not supported by C++/C (before C11).

# OpenMP is required for unit tests
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")

# Also we ignore warnings about signed comparison and unused variables
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sign-compare -Wno-unused-function")

glob_tests(runtime_tests test)
foreach(test ${runtime_tests})
	add_module_unittest(runtime ${test} NO_VALGRIND NO_LIB OUTPUT_TARGET_NAME test_tgt)
	if(BUILD_TESTS)
		add_runtime_dependencies(${test_tgt})
	endif()
endforeach(test)

# handle manual (library) test cases
file(GLOB runtime_manual_tests test/manual_*.c)
foreach(test ${runtime_manual_tests})
	if(BUILD_TESTS)
		get_filename_component(test_name ${test} NAME_WE)

		add_library(${test_name} SHARED ${test})
		add_runtime_dependencies(${test_name})

		if(MSVC)
			set_target_properties(${test_name} PROPERTIES FOLDER "runtime/Tests")
		endif()
	endif()
endforeach(test)

# handle standalone (executable) test cases
file(GLOB runtime_standalone_tests test/standalone_*.c)
foreach(test ${runtime_standalone_tests})
	if(BUILD_TESTS)
		get_filename_component(test_name ${test} NAME_WE)

		add_executable(${test_name} ${test})
		add_runtime_dependencies(${test_name})

		if(OpenCL_FOUND)
			target_include_directories(${test_name} PRIVATE ${OpenCL_INCLUDE_DIR})
		endif()

		target_link_libraries(${test_name} Threads::Threads)

		if(MSVC)
			set_target_properties(${test_name} PROPERTIES FOLDER "runtime/Tests")
		endif()
	endif()
endforeach(test)
