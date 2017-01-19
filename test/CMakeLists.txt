project(insieme_integration_test CXX)
cmake_minimum_required(VERSION 3.5)

# create test script
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/perf_counters.dat ${CMAKE_BINARY_DIR}/perf_counters.dat COPYONLY)
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/compare_versions.py ${CMAKE_BINARY_DIR}/compare_versions.py @ONLY)

msvc_file_completion(insieme_integration_tests . c cpp h)
