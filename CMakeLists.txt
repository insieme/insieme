project(insieme NONE)
cmake_minimum_required(VERSION 3.2)

# enable ctest support for testing - generate a CTest file in the ${build_dir}
enable_testing()

option(DOCS "Build Insieme documentation" OFF)
if(DOCS)
	add_subdirectory(docs)
endif()

add_subdirectory(code)

#integration tests
add_subdirectory(test)

