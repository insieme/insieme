#
# Builds the insieme annotations library
#
project(insieme_annotations CXX)
message(STATUS "========== configure annotations =============")
cmake_minimum_required(VERSION 3.2)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_annotations_srcs src/*.cpp)
insieme_glob_headers(insieme_annotations_incs include)

# Build the annotations library
add_library(insieme_annotations ${LINKING_TYPE} ${insieme_annotations_srcs} ${insieme_annotations_incs})
target_include_directories(insieme_annotations PUBLIC ${insieme_annotations_include_dir})
target_link_libraries(insieme_annotations insieme_core)
target_link_libraries(insieme_annotations insieme_utils)

msvc_file_completion(insieme_annotations_tests test h def)

#insieme_common is a header-only target
target_link_libraries(insieme_annotations insieme_common)

# lookup Boost library
insieme_find_package(NAME Boost)
target_include_directories(insieme_annotations SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

set(ut_prefix  ut_annotations )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	insieme_fix_case_name( case_subdir ${case_file} )

	set ( case_name ${ut_prefix}${case_subdir}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_annotations)
	
	add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)
