#
# Builds the insieme core library
#
project(insieme_core CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB insieme_core_srcs 	src/*.cpp )
file(GLOB insieme_core_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )

# Build the core library
add_library(insieme_core ${insieme_core_srcs} ${insieme_core_incs} )


# Build dummy executable
add_executable( main src/main.cpp )

# register linked libraries
target_link_libraries( main insieme_core insieme_utils )



# enable testing for this project
find_package(CxxTest)
if(CXXTEST_FOUND)
	include_directories(${CXXTEST_INCLUDE_DIR})

	# search for all test cases and register them
	file(GLOB test_cases test/*.h)
	foreach( case_file ${test_cases}) 
		get_filename_component( case_name ${case_file} NAME_WE )
	
		cxxtest_add_test(ut_${case_name} ${case_name}.cc ${case_file})
		target_link_libraries(ut_${case_name} insieme_utils insieme_core )	
	endforeach(case_file)
endif()
