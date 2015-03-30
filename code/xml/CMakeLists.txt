#
# Builds the insieme xml library
#
project(insieme_xml CXX)

#include(../environment.cmake)

# States that CMake required version must be >= 2.8
cmake_minimum_required(VERSION 2.8)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_xml_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_xml_incs 	include/*.h )

#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/xsd_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/xml/xsd_config.h)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/test_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/xml/test_config.h)

# Build the xml library
add_library(insieme_xml ${LINKING_TYPE} ${insieme_xml_srcs} ${insieme_xml_incs} )
target_include_directories(insieme_xml PUBLIC ${insieme_xml_include_dir})

# lookup Xerces library
insieme_find_package(NAME Xerces)
target_include_directories(insieme_xml PUBLIC ${Xerces_INCLUDE_DIRS})
target_link_libraries(insieme_xml ${Xerces_LIBRARIES})

target_link_libraries(insieme_xml insieme_core)

# setup unit tests
set ( ut_prefix  ut_xml )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )

	set(case_name ${ut_prefix}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_xml)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_utils)
	
	add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_xml LIBRARY DESTINATION lib)
endif(DO_INSTALL)
