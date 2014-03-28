project(insieme_integration_test NONE)

cmake_minimum_required(VERSION 2.6)

# create test script
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/integration_tests.py.in ${CMAKE_BINARY_DIR}/integration_tests.py @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test.cfg.in ${CMAKE_BINARY_DIR}/test.cfg)
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/perf_counters.dat ${CMAKE_BINARY_DIR}/perf_counters.dat COPYONLY)
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/compare_versions.py ${CMAKE_BINARY_DIR}/compare_versions.py @ONLY)

