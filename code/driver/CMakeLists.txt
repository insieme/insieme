#
# Builds the various insieme driver
#
project(insieme_driver CXX)
message(STATUS "========== configure driver =============")
cmake_minimum_required(VERSION 3.5)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_driver_srcs	src/*.cpp )
insieme_glob_headers(insieme_driver_incs include)

# Build the driver library
add_library(insieme_driver ${LINKING_TYPE} ${insieme_driver_srcs} ${insieme_driver_incs})
target_include_directories(insieme_driver PUBLIC ${insieme_driver_include_dir} )

msvc_file_completion(insieme_driver_tests test c)

# lookup Boost library
insieme_find_package(NAME Boost COMPONENTS program_options)
target_include_directories(insieme_driver SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(insieme_driver ${Boost_PROGRAM_OPTIONS_LIBRARY})

target_link_libraries(insieme_driver insieme_common)
target_link_libraries(insieme_driver insieme_utils)
target_link_libraries(insieme_driver insieme_core)
target_link_libraries(insieme_driver insieme_annotations)
target_link_libraries(insieme_driver insieme_frontend)
target_link_libraries(insieme_driver insieme_backend)
target_link_libraries(insieme_driver insieme_analysis)
target_link_libraries(insieme_driver insieme_transform)

#custom target to build all drivers at once
add_custom_target(drivers)

# build executables
#some drivers use openmp
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")

file(GLOB_RECURSE executables src/*.cxx)
foreach(exec_file ${executables})

	get_filename_component(exec_name ${exec_file} NAME_WE)

	# skip datalog binaries
	if((NOT ANALYSIS_DATALOG) AND (exec_name MATCHES "datalog"))
		continue()
	endif()

	# Build driver executable
	add_executable(${exec_name} ${exec_file} ${insieme_driver_defs})
	#target drivers depends on all driver
	add_dependencies(drivers ${exec_name})

	target_link_libraries(${exec_name} insieme_driver)

endforeach(exec_file)

# integrations_tests uses insiemecc - so there is a dependency
add_dependencies(integration_tests insiemecc)

# =============================================  TESTING  =====================================
# add unit test cases
set(ut_prefix  ut_driver )
file(GLOB_RECURSE test_cases test/*.cc)

if(USE_PAPI)
	insieme_find_package(NAME PAPI)
	add_definitions(-DUSE_PAPI)
endif()

foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )

	insieme_fix_case_name( case_subdir ${case_file} )

	set ( case_name ${ut_prefix}${case_subdir}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_driver)

	if(${case_name} MATCHES ".*driver_integration.*" OR ${case_name} MATCHES ".*driver_measure.*")
		add_unit_test(${case_name} ${ut_prefix} DISABLE_VALGRIND RUN_PARALLEL)
	else()
		add_unit_test(${case_name} ${ut_prefix} DISABLE_VALGRIND)
	endif()
endforeach(case_file)

# =============================================  PREPROCESS  =====================================
add_custom_command(
	TARGET integration_tests
	POST_BUILD
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/integration_tests --preprocess
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	COMMENT "Run preprocessing for integration tests"
)
