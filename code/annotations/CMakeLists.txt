#
# Builds the insieme annotations library
#
project(insieme_annotations CXX)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_annotations_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_annotations_incs 	include/*.h )

# Build the annotations library
add_library(insieme_annotations ${LINKING_TYPE} ${insieme_annotations_srcs} ${insieme_annotations_incs} )
target_include_directories(insieme_annotations PUBLIC ${insieme_annotations_include_dir})
target_link_libraries(insieme_annotations insieme_core)
target_link_libraries(insieme_annotations insieme_utils)

#TODO how to handle insieme_common??
#target_link_libraries(insieme_annotations insieme_common)
#include_directories( ${insieme_common_include_dir} )
#target_include_directories(insieme_annotations PUBLIC ${insieme_common_include_dir})
#insieme_common is a header-only target
target_link_libraries(insieme_annotations insieme_common)

# lookup Boost library
insieme_find_package(NAME Boost)
target_include_directories(insieme_annotations SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

set(ut_prefix  ut_annotations )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	
	add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_annotations LIBRARY DESTINATION lib)
endif(DO_INSTALL)
