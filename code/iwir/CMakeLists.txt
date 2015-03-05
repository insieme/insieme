#Build the IWIR frontend
project(insieme_iwir CXX) 

option (USE_IWIR "Build IWIR support in Insieme" OFF) 

#if (USE_IWIR) 

#	set(USE_XML)

	include(../environment.cmake)
	# States that CMake required version must be >= 2.6
	cmake_minimum_required(VERSION 2.6)

	file(GLOB_RECURSE insieme_iwir_srcs 	src/*.cpp )
	file(GLOB_RECURSE insieme_iwir_incs 	include/*.h )

	list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/iwir/cmake")

	# list all include paths
	#include_directories( ${insieme_iwir_include_dir} )
	#	include_directories( ${insieme_xml_include_dir}  ) 
	#include_directories( ${insieme_core_include_dir} ) 
	#include_directories( ${insieme_annotations_include_dir}  ) 
	#include_directories( ${insieme_utils_include_dir}  ) 

	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/xsd_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/iwir/xsd_config.h)
	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/test_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/iwir/test_config.h)

	# Build the iwir library
	add_library(insieme_iwir ${LINKING_TYPE} ${insieme_iwir_srcs} ${insieme_iwir_incs} )
	target_include_directories(insieme_iwir PUBLIC ${insieme_iwir_include_dir} )
	target_link_libraries(insieme_iwir insieme_xml)
	target_link_libraries(insieme_iwir insieme_core)
	target_link_libraries(insieme_iwir insieme_annotations)
	target_link_libraries(insieme_iwir insieme_utils)

	#iwir unit tests
	set ( ut_prefix  ut_iwir_ )
	file(GLOB_RECURSE test_cases test/*.cc)
	foreach ( case_file ${test_cases})
		get_filename_component( case_name ${case_file} NAME_WE )

		set ( case_name ${ut_prefix}${case_name} )
		add_executable(${case_name} ${case_file})
		target_link_libraries(${case_name} insieme_iwir)
		target_link_libraries(${case_name} insieme_annotations)
		target_link_libraries(${case_name} insieme_core)
		target_link_libraries(${case_name} insieme_utils)
		target_link_libraries(${case_name} insieme_xml)
		target_link_libraries(${case_name} ${pthread_LIB})
		#target_link_libraries(${case_name} ${xerces_LIB})
		
		add_unit_test(${case_name} off)
	endforeach(case_file)

	#iwir driver
	set( insieme_libraries 
		insieme_utils 
		insieme_core 
		insieme_annotations 
		insieme_xml
		insieme_iwir
	)

	file(GLOB_RECURSE executables src/*.cxx)
	foreach ( exec_file ${executables})
		get_filename_component( exec_name ${exec_file} NAME_WE )

		# Build dummy executable
		add_executable( ${exec_name} ${exec_file} ${insieme_iwir_incs} )
		#add_dependencies( ${exec_name} ${insieme_libraries})

		target_link_libraries( ${exec_name} ${insieme_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY})
		#target_link_libraries( ${exec_name} ${xerces_LIB})

	endforeach(exec_file)
#endif (USE_IWIR)
