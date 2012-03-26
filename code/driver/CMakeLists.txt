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
file(GLOB_RECURSE insieme_driver_defs 	include/*.def )


# Build the driver library
add_library(insieme_driver ${LINKING_TYPE} ${insieme_driver_srcs} ${insieme_driver_incs} ${insieme_driver_defs})


# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_frontend_include_dir} )
include_directories( ${insieme_backend_include_dir} )
include_directories( ${insieme_analysis_include_dir} )
include_directories( ${insieme_transform_include_dir} )
include_directories( ${insieme_simple_backend_include_dir} )
include_directories( ${insieme_annotations_include_dir} )
include_directories( ${insieme_driver_include_dir} )
include_directories( ${insieme_machine_learning_include_dir} )
if (USE_XML)
	include_directories( ${insieme_xml_include_dir} )
endif (USE_XML)

set( insieme_libraries 
	insieme_utils 
	insieme_core 
	insieme_frontend 
	insieme_backend 
	insieme_simple_backend
	insieme_annotations 
	insieme_analysis
	insieme_transform
	insieme_driver
	insieme_machine_learning
)

if (USE_XML)
	set( insieme_libraries ${insieme_libraries} insieme_xml )
endif (USE_XML)

# build executables
add_definitions( -fopenmp )
file(GLOB_RECURSE executables src/*.cxx)
foreach ( exec_file ${executables})

	get_filename_component( exec_name ${exec_file} NAME_WE )

	# Build dummy executable
	add_executable( ${exec_name} ${exec_file} ${insieme_driver_incs} ${insieme_driver_defs})
	add_dependencies( ${exec_name} ${insieme_libraries})

	target_link_libraries( ${exec_name} ${insieme_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY} ${shark_LIB} ${kompex_LIB} gomp dl)

endforeach(exec_file)

# add unit test cases
set ( ut_prefix  ut_driver_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} ${insieme_libraries})
	target_link_libraries(${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${case_name} ${pthread_LIB})
	target_link_libraries(${case_name} ${shark_LIB})
	target_link_libraries(${case_name} ${kompex_LIB})
	target_link_libraries(${case_name} dl)

	add_unit_test(${case_name} off)
endforeach(case_file)


# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS main DESTINATION bin)
endif(DO_INSTALL)
