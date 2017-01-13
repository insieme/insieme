add_module_library(driver)

target_link_libraries(driver analysis)
target_link_libraries(driver annotations)
target_link_libraries(driver backend)
target_link_libraries(driver common)
target_link_libraries(driver core)
target_link_libraries(driver frontend)
target_link_libraries(driver transform)
target_link_libraries(driver utils)

# Boost
find_package(Boost ${Boost_VERSION} EXACT REQUIRED COMPONENTS program_options)
target_link_libraries(driver ${Boost_LIBRARIES})
target_include_directories(driver SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

# Target for building all drivers at once
add_custom_target(drivers)

# Some drivers use openmp
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")

glob_executables(driver_exes src)
foreach(exe ${driver_exes})

	# TODO
	if(exe MATCHES "datalog")
		continue()
	endif()

	add_module_executable(driver ${exe})

	get_filename_component(exe_name ${exe} NAME_WE)
	add_dependencies(drivers driver_${exe_name})
endforeach(exe)

# integrations_tests requiresinsiemecc
add_dependencies(driver_integration_tests driver_insiemecc)

#if(USE_PAPI)
#	insieme_find_package(NAME PAPI)
#	add_definitions(-DUSE_PAPI)
#endif()

glob_tests(driver_tests test)
foreach(test ${driver_tests})
	string(REPLACE "${CMAKE_CURRENT_SOURCE_DIR}/test/" "" test_relpath ${test})
	if(${test_relpath} MATCHES ".*driver_integration.*" OR ${test_relpath} MATCHES ".*driver_measure.*")
		add_module_unittest(driver ${test} PARALLEL)
	else()
		add_module_unittest(driver ${test})
	endif()
endforeach(test)

# Run preprocessing for integration tests
#add_custom_command(
#	TARGET driver_integration_tests
#	POST_BUILD
#	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/integration_tests --preprocess
#	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#)
