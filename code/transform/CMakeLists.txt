#
# Builds the insieme transform library
#
project(insieme_transform CXX)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_transform_srcs 	src/*.cpp src/*.def src/*.inc )
file(GLOB_RECURSE insieme_transform_incs 	include/*.h include/*.def include/*.inc )

# Build the transform library
add_library(insieme_transform ${LINKING_TYPE} ${insieme_transform_srcs} ${insieme_transform_incs} )
target_include_directories(insieme_transform PUBLIC ${insieme_transform_include_dir})

# lookup Boost library
insieme_find_package(NAME Boost) 
target_include_directories(insieme_transform SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

target_link_libraries(insieme_transform insieme_core)
target_link_libraries(insieme_transform insieme_utils)
target_link_libraries(insieme_transform insieme_annotations)
target_link_libraries(insieme_transform insieme_analysis)

set ( ut_prefix  ut_transform )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}_${case_name} )	
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_transform)
	target_link_libraries(${case_name} insieme_analysis)
	
	add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_transform LIBRARY DESTINATION lib)
endif(DO_INSTALL)
