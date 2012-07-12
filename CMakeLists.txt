project(insieme NONE)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# enable ctest support for testing
enable_testing()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test/test.py.in ${CMAKE_BINARY_DIR}/test.py)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test/test.cfg.in ${CMAKE_BINARY_DIR}/test.cfg)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/newtest.py ${CMAKE_BINARY_DIR}/newtest.py)

add_subdirectory(docs)
add_subdirectory(code)
