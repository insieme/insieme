#
# Builds the various insieme driver
#
project(insieme_driver CXX)
message(STATUS "========== configure driver =============")
cmake_minimum_required(VERSION 3.2)
include(../environment_common.cmake)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_driver_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_driver_incs 	include/*.h )
file(GLOB_RECURSE insieme_driver_defs 	include/*.def )

# Build the driver library
add_library(insieme_driver ${LINKING_TYPE} ${insieme_driver_srcs} ${insieme_driver_incs} ${insieme_driver_defs})
target_include_directories(insieme_driver PUBLIC ${insieme_driver_include_dir} )

# lookup Boost library
insieme_find_package(NAME Boost COMPONENTS program_options) 
target_include_directories(insieme_driver SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(insieme_driver ${Boost_PROGRAM_OPTIONS_LIBRARY})

target_link_libraries(insieme_driver insieme_utils) 
target_link_libraries(insieme_driver insieme_frontend) 
target_link_libraries(insieme_driver insieme_backend) 
target_link_libraries(insieme_driver insieme_analysis) 
target_link_libraries(insieme_driver insieme_transform) 
target_link_libraries(insieme_driver insieme_annotations) 
target_link_libraries(insieme_driver insieme_machine_learning)

#custom target to build all drivers at once
add_custom_target(drivers)

# build executables
#some drivers use openmp
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")

file(GLOB_RECURSE executables src/*.cxx)
foreach ( exec_file ${executables})

	get_filename_component( exec_name ${exec_file} NAME_WE )

	# Build driver executable
	add_executable( ${exec_name} ${exec_file} ${insieme_driver_incs} ${insieme_driver_defs})
	#target drivers depends on all driver
	add_dependencies(drivers ${exec_name})

	target_link_libraries(${exec_name} insieme_driver) 
	target_link_libraries(${exec_name} insieme_utils) 
	target_link_libraries(${exec_name} insieme_core) 
	target_link_libraries(${exec_name} insieme_frontend) 
	target_link_libraries(${exec_name} insieme_backend) 
	target_link_libraries(${exec_name} insieme_analysis) 
	target_link_libraries(${exec_name} insieme_transform) 
	target_link_libraries(${exec_name} insieme_annotations) 
	target_link_libraries(${exec_name} insieme_machine_learning)
	target_link_libraries(${exec_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${exec_name} gomp)
	target_link_libraries(${exec_name} dl)

endforeach(exec_file)

# integrations_tests uses insiemecc - so there is a dependency
add_dependencies(integration_tests insiemecc)

# add unit test cases
set(ut_prefix  ut_driver )
file(GLOB_RECURSE test_cases test/*.cc)

insieme_find_package(NAME PAPI)
if(NOT PAPI_FOUND)
	#measure test run a runtime-thingy which sould use papi if available
	# if papi is not available the measure test will fail therefore exclude measuretest if papi is not
	# available
	message(STATUS "Excluding measure tests")

	# get measure unit tests, to be excluded if papi is not present
	file(GLOB_RECURSE measure_test_cases test/measure*.cc)
	list(REMOVE_ITEM test_cases ${measure_test_cases})
endif()

foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_driver) 
	target_link_libraries(${case_name} insieme_core) 
	target_link_libraries(${case_name} insieme_utils) 
	target_link_libraries(${case_name} insieme_frontend) 
	target_link_libraries(${case_name} insieme_backend) 
	target_link_libraries(${case_name} insieme_analysis) 
	target_link_libraries(${case_name} insieme_transform) 
	target_link_libraries(${case_name} insieme_annotations) 
	target_link_libraries(${case_name} insieme_machine_learning)
	target_link_libraries(${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${case_name} dl)

	add_unit_test(${case_name} ${ut_prefix} off)
endforeach(case_file)


# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insiemecc DESTINATION bin)
endif(DO_INSTALL)
