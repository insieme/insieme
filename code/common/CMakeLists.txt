project(insieme_common CXX)
message(STATUS "========== configure common =============")
cmake_minimum_required(VERSION 3.2)

insieme_glob_headers(insieme_common_incs include)

set(insieme_common_include_dir ${CMAKE_CURRENT_LIST_DIR}/include)

# provide a target (header-only) to be used by other targets/libraris 
add_library(insieme_common INTERFACE)
target_include_directories(insieme_common INTERFACE ${insieme_common_include_dir})
target_sources(insieme_common INTERFACE ${insieme_common_incs})
