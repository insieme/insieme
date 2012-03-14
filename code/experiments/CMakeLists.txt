#
# Builds the insieme default driver
#
project(insieme_experiments CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

#configure_file(
#		${CMAKE_CURRENT_SOURCE_DIR}/cmake/driver_config.h.in 
#		${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/driver/driver_config.h 
#)

# Create a variables containing all src, includes and definitions:
file(GLOB_RECURSE insieme_experiments_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_experiments_incs 	include/*.h )
file(GLOB_RECURSE insieme_experiments_defs 	include/*.def )

# Build the experiment library
add_library(insieme_experiments ${LINKING_TYPE} ${insieme_experiments_srcs} ${insieme_experiments_incs} ${insieme_experiments_defs})

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
include_directories( ${insieme_experiments_include_dir} )
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
	insieme_experiments
)

if (USE_XML)
	set( insieme_libraries ${insieme_libraries} insieme_xml )
endif (USE_XML)

# build executables
add_definitions( -fopenmp )
file(GLOB_RECURSE executables src/*.cxx)
foreach ( exec_file ${executables})

	get_filename_component( exec_name ${exec_file} NAME_WE )

	# Build executable
	add_executable( ${exec_name} ${exec_file} ${insieme_driver_incs} ${insieme_driver_defs})
	add_dependencies( ${exec_name} ${insieme_libraries} )

	target_link_libraries( ${exec_name} ${insieme_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY} ${shark_LIB} ${kompex_LIB} gomp dl)

endforeach(exec_file)


# add unit test cases
set ( ut_prefix  ut_expreriments_ )
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


# create common target for all experiments
add_custom_target(experiments)

# add experiments
set ( prefix  ex_ )

# collect stand-alone executables
file(GLOB_RECURSE standalone experiments/*.cxx)
file(GLOB_RECURSE gtestbased experiments/*.cc)

# those which are stand-alone executables ...
set( experiments "${standalone};${gtestbased}" )
foreach ( experiment ${experiments} )

	# extract file name components and build up target / executable name
	get_filename_component( exec_name ${experiment} NAME_WE )

	# fixes name of executable - TODO: fix this
	# set( OUTPUT_NAME ${exec_name} )	# not having any effect

	# compute target name
	set( trgt_name ${exec_name} )
	get_filename_component( dir ${experiment} PATH )
	get_filename_component( dir_name ${dir} NAME ) 
	while ( NOT( ${dir_name} STREQUAL "experiments" ) )
		set( trgt_name "${dir_name}_${trgt_name}" )
		get_filename_component( dir ${dir} PATH )
		get_filename_component( dir_name ${dir} NAME ) 
	endWhile()

	# complete target name by adding prefix
	set ( trgt_name "${prefix}${trgt_name}" )

	# Build executable (not part of build all)
	add_executable( ${trgt_name} EXCLUDE_FROM_ALL ${experiment} ${insieme_experiments_incs} ${insieme_experiments_defs})
	add_dependencies( ${trgt_name} ${insieme_libraries} )
	target_link_libraries( ${trgt_name} ${insieme_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY} ${gtest_LIB} ${shark_LIB} ${kompex_LIB} gomp dl)

	# add g-test main dependency if required
	get_filename_component( extension ${experiment} EXT )
	if ( ${extension} STREQUAL ".cc" )
		target_link_libraries(${trgt_name} ${gtest_main_LIB})
	endif()

	# add common target for all experiments
	add_dependencies(experiments ${trgt_name})

endforeach( experiment )


