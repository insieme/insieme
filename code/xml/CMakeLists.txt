#
# Builds the insieme xml library
#
project(insieme_xml CXX)

# -------------------------------------------------------------- Xerces library
option (USE_XML "Build XML support in Insieme" OFF) 

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_xml_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_xml_incs 	include/*.h )

#to use FindXerces.cmake
list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/xml/cmake/")

#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/xsd_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/xml/xsd_config.h)
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/test_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/xml/test_config.h)

# Build the xml library
add_library(insieme_xml ${LINKING_TYPE} ${insieme_xml_srcs} ${insieme_xml_incs} )
target_include_directories(insieme_xml PUBLIC ${insieme_xml_include_dir} )

# lookup Boost library
#insieme_find_package(Boost) #TODO NOT NEEDED? program_options system filesystem regex serialization 
#target_include_directories(insieme_xml SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

# lookup Xerces library
insieme_find_package(Xerces)
target_link_libraries(insieme_xml ${Xerces_LIBRARIES})

target_link_libraries(insieme_xml insieme_core)

set ( ut_prefix  ut_xml_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )

	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_xml)
	
	add_unit_test(${case_name} off)
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_xml LIBRARY DESTINATION lib)
endif(DO_INSTALL)
