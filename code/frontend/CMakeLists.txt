#
# Builds the insieme frontend library
#
project(insieme_frontend CXX)

message(STATUS "========== configure frontend =============")
cmake_minimum_required(VERSION 3.2)

# collect source files
file(GLOB_RECURSE insieme_frontend_srcs	src/*.cpp src/*.def src/*.inc )
insieme_glob_headers(insieme_frontend_incs include)

# automatically run the script for generating builtin header files
if(NOT MSVC)
	execute_process(WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMMAND ruby generate_builtins.rb --quiet --detect-via-compiler ${INSIEME_CXX_BACKEND_COMPILER} --output ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/frontend/builtin_headers RESULT_VARIABLE generate_builtins_success OUTPUT_VARIABLE generate_builtins_output ERROR_VARIABLE generate_builtins_output)

	if (NOT ${generate_builtins_success} EQUAL 0)
		message(FATAL_ERROR "Ruby generate_builtins.rb script failed to execute, reason:\n${generate_builtins_output}")
	endif()
endif()

# build the frontend library
add_library(insieme_frontend ${LINKING_TYPE} ${insieme_frontend_srcs} ${insieme_frontend_incs} )
target_include_directories(insieme_frontend PUBLIC ${insieme_frontend_include_dir})

# lookup Boost library
insieme_find_package(NAME Boost COMPONENTS serialization filesystem system program_options) #TODO NOT NEEDED? program_options system filesystem regex serialization 
target_include_directories(insieme_frontend SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(insieme_frontend ${Boost_SERIALIZATION_LIBRARY})
target_link_libraries(insieme_frontend ${Boost_FILESYSTEM_LIBRARY})
target_link_libraries(insieme_frontend ${Boost_SYSTEM_LIBRARY})
target_link_libraries(insieme_frontend ${Boost_PROGRAM_OPTIONS_LIBRARY})

# lookup LLVM/CLANG library+header
insieme_find_package(NAME LLVM)
target_link_libraries(insieme_frontend ${CLANG_LIBRARIES})
target_include_directories(insieme_frontend PUBLIC ${CLANG_INCLUDE_DIRS})
target_link_libraries(insieme_frontend ${LLVM_LIBRARIES})
target_include_directories(insieme_frontend PUBLIC ${LLVM_INCLUDE_DIRS})

# add dependencies
target_link_libraries(insieme_frontend insieme_core)
target_link_libraries(insieme_frontend insieme_common)
target_link_libraries(insieme_frontend insieme_utils)
target_link_libraries(insieme_frontend insieme_transform)
target_link_libraries(insieme_frontend insieme_annotations)

# required for llvm
add_definitions(-D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS)

# =============================================  TESTING  =====================================
# add custom target for ut_fe test suite
set ( ut_prefix  ut_fe )
add_custom_target(${ut_prefix})

file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	insieme_fix_case_name( case_subdir ${case_file} )
	
	set ( case_name ${ut_prefix}${case_subdir}_${case_name} )
	add_executable(${case_name} ${case_file})

	#dependency on test suit target
	add_dependencies(${ut_prefix} ${case_name})

	target_link_libraries(${case_name} insieme_frontend)
	target_link_libraries(${case_name} insieme_common)
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	#target_link_libraries(${case_name} insieme_driver)
	#target_link_libraries(${case_name} insieme_backend)
	#target_link_libraries(${case_name} insieme_transform)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} ${CLANG_LIBRARIES})
	target_link_libraries(${case_name} ${LLVM_LIBRARIES})
	
	add_unit_test(${case_name} ${ut_prefix}) 
endforeach(case_file)
