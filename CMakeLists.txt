project(insieme NONE)
cmake_minimum_required(Version 3.5)

# enable ctest support for testing - generate a CTest file in the ${build_dir}
enable_testing()

option(DOCS "Build Insieme documentation" OFF)
if(DOCS)
	add_subdirectory(docs)
endif()

add_subdirectory(code)

#integration tests
add_subdirectory(test)

if(BUILD_COMPILER)
	export(
		TARGETS
			insieme_analysis
			insieme_annotations
			insieme_backend
			insieme_common
			insieme_core
			insieme_driver
			insieme_frontend
			insieme_transform
			insieme_utils
			insiemecc
		FILE ${CMAKE_BINARY_DIR}/InsiemeConfig.cmake
	)
	export(PACKAGE Insieme)
endif()
