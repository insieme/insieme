project(insieme_integration_test NONE)

cmake_minimum_required(VERSION 2.6)

# create test script
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/integration_tests.py.in ${CMAKE_BINARY_DIR}/integration_tests.py @ONLY)

