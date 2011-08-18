#
# Builds the insieme default driver
#
project(insieme_driver CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

configure_file(
		${CMAKE_CURRENT_SOURCE_DIR}/cmake/driver_config.h.in 
		${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/driver/driver_config.h 
)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_driver_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_driver_incs 	include/*.h )

# --------------------------------------------------------------------------------------
# We repeat the search for Xerces library here because adding this line to 
# the environment.cmake file will produce the same error message every time 
# the file is included (therefore for every module)

option (USE_XML "Build XML support in Insieme" ON) 
list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/xml/cmake")

if (USE_XML) 
	find_package(Xerces)
	if ( NOT ${Xerces_FOUND} )
		set(USE_XML OFF)
	endif()
endif (USE_XML)
# --------------------------------------------------------------------------------------

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_frontend_include_dir} )
include_directories( ${insieme_backend_include_dir} )
include_directories( ${insieme_analysis_include_dir} )
include_directories( ${insieme_transform_include_dir} )
include_directories( ${insieme_simple_backend_include_dir} )
include_directories( ${insieme_opencl_backend_include_dir} )
include_directories( ${insieme_annotations_include_dir} )
include_directories( ${insieme_driver_include_dir} )
if (USE_XML)
	include_directories( ${insieme_xml_include_dir} )
endif (USE_XML)


# Build dummy executable
add_executable( main ${insieme_driver_srcs} )

# add dependencies
add_dependencies( main insieme_core insieme_utils insieme_frontend insieme_backend 
					   insieme_simple_backend insieme_annotations insieme_transform
			    )

# register linked libraries
target_link_libraries( main insieme_utils )
target_link_libraries( main insieme_frontend )
target_link_libraries( main insieme_core )
target_link_libraries( main insieme_backend )
target_link_libraries( main insieme_simple_backend )
target_link_libraries( main insieme_opencl_backend ) 
target_link_libraries( main insieme_annotations )
target_link_libraries( main insieme_analysis )
target_link_libraries( main insieme_transform )

if (USE_XML)
	target_link_libraries(main insieme_xml)
endif (USE_XML)

# add boost_program_options library
target_link_libraries( main ${Boost_PROGRAM_OPTIONS_LIBRARY})

# enable tests for this project
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

set ( ut_prefix  ut_driver_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_frontend)
	target_link_libraries(${case_name} insieme_backend )
	target_link_libraries(${case_name} insieme_transform)
	target_link_libraries(${case_name} insieme_simple_backend)
	target_link_libraries(${case_name} insieme_opencl_backend)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_utils)
	
	if (USE_XML) 
		target_link_libraries(${case_name} insieme_xml)
	endif (USE_XML)

	target_link_libraries(${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${case_name} ${pthread_LIB})

	add_unit_test(${case_name} off)
endforeach(case_file)


# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS main DESTINATION bin)
endif(DO_INSTALL)
