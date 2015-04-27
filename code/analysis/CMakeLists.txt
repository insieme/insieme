#
# Builds the insieme analysis library
#
project(insieme_analysis CXX)

#include(../environment.cmake)

# States that CMake required version must be >= 2.8
cmake_minimum_required(VERSION 2.8)

#ninja job pools setting
# used to limit maximum of parallel CBA jobs as they need a lot of memory
if( DEFINED CBA_JOBS)
	set_property(GLOBAL APPEND PROPERTY JOB_POOLS cba_jobs_compile=${CBA_JOBS} )
endif()

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_analysis_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_analysis_incs 	include/*.h )

# Build the analysis library
add_library(insieme_analysis ${LINKING_TYPE} ${insieme_analysis_srcs} ${insieme_analysis_incs} )
target_include_directories(insieme_analysis PUBLIC ${insieme_analysis_include_dir} )
target_link_libraries(insieme_analysis insieme_core )
target_link_libraries(insieme_analysis insieme_utils)
target_link_libraries(insieme_analysis insieme_annotations)

# lookup ISL library
insieme_find_package(NAME ISL)
target_include_directories(insieme_analysis PUBLIC ${ISL_INCLUDE_DIRS})
target_link_libraries(insieme_analysis ${ISL_LIBRARIES})

# lookup cloog library 
insieme_find_package(NAME CLOOG)
target_include_directories(insieme_analysis PUBLIC ${CLOOG_INCLUDE_DIRS})
target_link_libraries(insieme_analysis ${CLOOG_LIBRARIES}) 

# lookup Barvinok library 
insieme_find_package(NAME BARVINOK)
target_include_directories(insieme_analysis PUBLIC ${BARVINOK_INCLUDE_DIRS})
target_link_libraries(insieme_analysis ${BARVINOK_LIBRARIES})

# lookup GMP library
insieme_find_package(NAME GMP) 
target_include_directories(insieme_analysis PUBLIC ${GMP_INCLUDE_DIRS})
target_link_libraries(insieme_analysis ${GMP_LIBRARIES})

# lookup Boost library
insieme_find_package(NAME Boost) 
target_include_directories(insieme_analysis SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

set ( ut_prefix  ut_analysis )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )

	set ( case_name ${ut_prefix}_${case_name} )
	add_executable(${case_name} ${case_file})

	#ninja job pools
	if(${case_name} MATCHES "ut_analysis_cba_(.*)")
		set_property(TARGET ${case_name} PROPERTY JOB_POOL_COMPILE cba_jobs)
		set_property(TARGET ${case_name} PROPERTY JOB_POOL_LINK cba_jobs)
	endif()

	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_analysis)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_transform)
	target_link_libraries(${case_name} insieme_frontend)
	target_link_libraries(${case_name} insieme_backend)

	target_link_libraries(${case_name} ${ISL_LIBRARIES})
	target_link_libraries(${case_name} ${GMP_LIBRARIES})
	target_link_libraries(${case_name} ${GLOOG_LIBRARIES})
	target_link_libraries(${case_name} ${BARVINOK_LIBRARIES})

	if(${case_name} STREQUAL ut_analysis_dependence_test)
		add_unit_test(${case_name} ${ut_prefix} off)
	elseif((${case_name} STREQUAL ut_analysis_cba_full_scale_integration_test) 
		OR (${case_name} STREQUAL ut_analysis_cba_inputs_test))
		add_unit_test(${case_name} ${ut_prefix} off)
		target_link_libraries(${case_name} insieme_driver)
	elseif(${case_name} STREQUAL ut_analysis_cache_features_test)
		add_unit_test(${case_name} ${ut_prefix} off)
	else()
		add_unit_test(${case_name} ${ut_prefix})
	endif()

endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	install (DIRECTORY ${insieme_analysis_include_dir}/insieme DESTINATION include)
	INSTALL(TARGETS insieme_analysis LIBRARY DESTINATION lib)
endif(DO_INSTALL)
