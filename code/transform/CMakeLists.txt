#
# Builds the insieme transform library
#
project(insieme_transform CXX)
message(STATUS "========== configure transform =============")
cmake_minimum_required(Version 3.5)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_transform_srcs 	src/*.cpp src/*.def src/*.inc )
insieme_glob_headers(insieme_transform_incs include)

# Build the transform library
add_library(insieme_transform ${LINKING_TYPE} ${insieme_transform_srcs} ${insieme_transform_incs})
target_include_directories(insieme_transform PUBLIC ${insieme_transform_include_dir})

# lookup Boost library
insieme_find_package(NAME Boost) 
target_include_directories(insieme_transform SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

target_link_libraries(insieme_transform insieme_core)
target_link_libraries(insieme_transform insieme_utils)

# =============================================  TESTING  =====================================
set ( ut_prefix  ut_transform )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	insieme_fix_case_name( case_subdir ${case_file} )
	
	set ( case_name ${ut_prefix}${case_subdir}_${case_name} )	
	add_executable(${case_name} ${case_file})
	
	target_link_libraries(${case_name} insieme_transform)
	
	add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)
