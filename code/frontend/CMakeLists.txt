#
# Builds the insieme frontend library
#
project(insieme_frontend CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# collect source files
file(GLOB_RECURSE insieme_frontend_srcs	src/*.cpp src/*.def src/*.inc )
file(GLOB_RECURSE insieme_frontend_incs		include/*.h include/*.def include/*.inc )

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/clang_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/frontend/clang_config.h)

# add include directory
include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${LLVM_HOME}/include
	${insieme_utils_include_dir}
	${insieme_core_include_dir}
	${insieme_annotations_include_dir} )

# build the frontend library
add_library(insieme_frontend ${LINKING_TYPE} ${insieme_frontend_srcs} ${insieme_frontend_incs} )

# add dependencies
#add_dependencies( insieme_frontend insieme_core insieme_utils insieme_annotations insieme_analysis )
target_link_libraries(insieme_frontend ${clang_LIBs} ${pthread_LIB} ${llvm_LIBs})

set ( ut_prefix  ut_fe_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	include_directories(${case_name} ${insieme_utils_include_dir})
	target_link_libraries(${case_name} insieme_frontend)
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${case_name} ${clang_LIBs})
	target_link_libraries(${case_name} ${pthread_LIB})
	target_link_libraries(${case_name} ${llvm_LIBs})
	
	if(${case_name} STREQUAL ut_fe_ocl_host_compiler_test)
		add_unit_test(${case_name} off)
	else()
		add_unit_test(${case_name})
	endif()
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	install (FILES include/insieme/frontend/frontend.h DESTINATION include/insieme/)
	INSTALL(TARGETS insieme_frontend LIBRARY DESTINATION lib)
endif(DO_INSTALL)

