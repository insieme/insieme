#
# Builds the insieme xml library
#
project(insieme_xml CXX)

# -------------------------------------------------------------- Xerces library
option (USE_XML "Build XML support in Insieme" ON) 

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB insieme_xml_srcs 	src/*.cpp )
file(GLOB insieme_xml_incs 	include/*.h )

list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/xml/cmake")

if (USE_XML) 

	# lookup Xerces library
	lookup_lib( XERCES xerces-c )

	# list all include paths
	include_directories( ${insieme_xml_include_dir} )
	include_directories( ${insieme_annotations_include_dir} )
	include_directories( ${insieme_utils_include_dir} )
	include_directories( ${insieme_core_include_dir} )

	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/xsd_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/xml/xsd_config.h)

	# Build the core library
	add_library(insieme_xml ${LINKING_TYPE} ${insieme_xml_srcs} ${insieme_xml_incs} )

	# add dependencies
	#add_dependencies( insieme_xml insieme_core insieme_utils )
	target_link_libraries(insieme_xml ${xerces_LIB})

	set ( ut_prefix  ut_xml_ )
	file(GLOB_RECURSE test_cases test/*.cc)
	foreach ( case_file ${test_cases})
		get_filename_component( case_name ${case_file} NAME_WE )
	
		set ( case_name ${ut_prefix}${case_name} )
		add_executable(${case_name} ${case_file})
		target_link_libraries(${case_name} insieme_annotations)
		target_link_libraries(${case_name} insieme_core)
		target_link_libraries(${case_name} insieme_utils)
		target_link_libraries(${case_name} insieme_xml)
		target_link_libraries(${case_name} ${pthread_LIB})
		target_link_libraries(${case_name} ${xerces_LIB})
		
		add_unit_test(${case_name} off)
	endforeach(case_file)

	# --- INSTALL --------------------------------------------------------------------
	if(DO_INSTALL)
		INSTALL(TARGETS insieme_xml LIBRARY DESTINATION lib)
	endif(DO_INSTALL)
endif (USE_XML)


