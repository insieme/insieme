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


# dummy target: Glob all files to show them in your IDE
file(GLOB_RECURSE list_of_all_files build/code/[^.]*[^o]  code/[^.]* test/* scripts/[^.]*)
add_custom_target(all_files_dummy SOURCES ${list_of_all_files})
