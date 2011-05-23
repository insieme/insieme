#
# Builds the insieme c_info library
#
project(insieme_xml CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB insieme_xml_srcs 	src/*.cpp )
file(GLOB insieme_xml_incs 	include/*.h )

# -------------------------------------------------------------- Xerces library
option (USE_XML "Build XML support in Insieme" ON) 

list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/xml/cmake")

if (USE_XML) 	
	find_package(Xerces)
	if ( NOT ${Xerces_FOUND} )
		message("Xerces library not found, XML support will be disabled!")
		set(USE_XML OFF)
	endif()
endif (USE_XML)

if (USE_XML) 
	# list all include paths
	include_directories( ${insieme_xml_include_dir} )
	include_directories( ${insieme_c_info_include_dir} )
	include_directories( ${insieme_utils_include_dir} )
	include_directories( ${insieme_core_include_dir} )
	include_directories( ${XERCES_INCLUDE_DIRS} )

	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/xsd_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/xml/xsd_config.h)

	# Build the core library
	add_library(insieme_xml ${LINKING_TYPE} ${insieme_xml_srcs} ${insieme_xml_incs} )

	# add dependencies
	add_dependencies( insieme_xml insieme_core insieme_utils )
	target_link_libraries(insieme_xml ${XERCES_LIBRARIES})

	# enable tests for this project
	find_package(GTest REQUIRED)
	include_directories(${GTEST_INCLUDE_DIRS})

	set ( ut_prefix  ut_xml_ )
	file(GLOB_RECURSE test_cases test/*.cc)
	foreach ( case_file ${test_cases})
		get_filename_component( case_name ${case_file} NAME_WE )
	
		set ( case_name ${ut_prefix}${case_name} )
		add_executable(${case_name} ${case_file})
		target_link_libraries(${case_name} ${GTEST_BOTH_LIBRARIES})
		target_link_libraries(${case_name} insieme_c_info)
		target_link_libraries(${case_name} insieme_core)
		target_link_libraries(${case_name} insieme_utils)
		target_link_libraries(${case_name} insieme_xml)
		target_link_libraries(${case_name} ${pthread_LIB})
		target_link_libraries(${case_name} ${XERCES_LIBRARIES})
		
		add_unit_test(${case_name} off)
	endforeach(case_file)

	# --- INSTALL --------------------------------------------------------------------
	INSTALL(TARGETS insieme_xml LIBRARY DESTINATION lib)
endif (USE_XML)


