project(insieme_integration_test NONE)

cmake_minimum_required(VERSION 2.6)

# create test script
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/newtest.py.in ${CMAKE_BINARY_DIR}/newtest.py @ONLY)

