#Build the IWIR frontend
project(insieme_iwir CXX) 

#include(../environment.cmake)
# States that CMake required version must be >= 2.8
cmake_minimum_required(VERSION 2.8)

file(GLOB_RECURSE insieme_iwir_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_iwir_incs 	include/*.h )

list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/iwir/cmake")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/xsd_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/iwir/xsd_config.h)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/test_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/iwir/test_config.h)

# Build the iwir library
add_library(insieme_iwir ${LINKING_TYPE} ${insieme_iwir_srcs} ${insieme_iwir_incs})
target_include_directories(insieme_iwir PUBLIC ${insieme_iwir_include_dir})
target_link_libraries(insieme_iwir insieme_xml)
target_link_libraries(insieme_iwir insieme_core)
target_link_libraries(insieme_iwir insieme_annotations)
target_link_libraries(insieme_iwir insieme_utils)

# lookup Boost library
insieme_find_package(NAME Boost COMPONENTS program_options filesystem system)
target_include_directories(insieme_iwir SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(insieme_iwir ${Boost_PROGRAM_OPTIONS_LIBRARY})
target_link_libraries(insieme_iwir ${Boost_FILESYSTEM_LIBRARY})
target_link_libraries(insieme_iwir ${Boost_SYSTEM_LIBRARY})

#iwir unit tests
set ( ut_prefix  ut_iwir )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )

	set(case_name ${ut_prefix}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_xml)
	target_link_libraries(${case_name} insieme_iwir)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	
	add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)

#iwir driver
file(GLOB_RECURSE executables src/*.cxx)
foreach ( exec_file ${executables})
	get_filename_component( exec_name ${exec_file} NAME_WE )

	# Build driver executable
	add_executable(${exec_name} ${exec_file} ${insieme_iwir_incs} )

	target_link_libraries( ${exec_name} insieme_iwir)
	target_link_libraries( ${exec_name} insieme_xml)
	target_link_libraries( ${exec_name} insieme_utils)
	target_link_libraries( ${exec_name} insieme_core)
	target_link_libraries( ${exec_name} insieme_annotations)
	target_link_libraries( ${exec_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})

endforeach(exec_file)
