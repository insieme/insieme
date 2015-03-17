#
# Builds the insieme analysis library
#
project(insieme_analysis CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

#ninja job pools setting
# used to limit maximum of parallel CBA jobs as they need a lot of memory
if( NOT DEFINED(CBA_JOBS) OR (CBA_JOBS) )
	set(CBA_JOBS "1" CACHE STRING "number of parallel cba unit test jobs")
endif()
set_property(GLOBAL PROPERTY JOB_POOLS cba_jobs=${CBA_JOBS} )

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
#lookup_lib( ISL isl )
#target_include_directories(insieme_analysis PUBLIC ${isl_INC})
#target_link_libraries(insieme_analysis ${isl_LIB})
insieme_find_package(ISL)
target_include_directories(insieme_analysis PUBLIC ${ISL_INCLUDE_DIRS})
target_link_libraries(insieme_analysis ${ISL_LIBRARIES})

# lookup cloog library 
#lookup_lib( CLOOG cloog-isl )
#target_include_directories(insieme_analysis PUBLIC ${cloog_INC})
#target_link_libraries(insieme_analysis ${cloog_LIB}) 
insieme_find_package(CLOOG)
target_include_directories(insieme_analysis PUBLIC ${CLOOG_INCLUDE_DIRS})
target_link_libraries(insieme_analysis ${CLOOG_LIBRARIES}) 

# lookup Barvinok library 
#lookup_lib( BARVINOK barvinok )
#target_include_directories(insieme_analysis PUBLIC ${barvinok_INC})
#target_link_libraries(insieme_analysis ${barvinok_LIB})
insieme_find_package(BARVINOK)
target_include_directories(insieme_analysis PUBLIC ${BARVINOK_INCLUDE_DIRS})
target_link_libraries(insieme_analysis ${BARVINOK_LIBRARIES})


# lookup GMP library
#lookup_lib( GMP gmp ) 
#target_include_directories(insieme_analysis PUBLIC ${gmp_INC})
#target_link_libraries(insieme_analysis ${gmp_LIB})
insieme_find_package(GMP) 
target_include_directories(insieme_analysis PUBLIC ${GMP_INCLUDE_DIRS})
target_link_libraries(insieme_analysis ${GMP_LIBRARIES})


set ( ut_prefix  ut_analysis_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )

	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})

	#ninja job pools
	if(${case_name} MATCHES "ut_analysis_cba_(.*)")
		set_property(TARGET ${case_name} PROPERTY JOB_POOL_COMPILE cba_jobs)
	endif()

	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_analysis)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_transform)
	target_link_libraries(${case_name} insieme_frontend)
	target_link_libraries(${case_name} insieme_backend)

	target_link_libraries(${case_name} ${isl_LIB})
	target_link_libraries(${case_name} ${gmp_LIB})
	target_link_libraries(${case_name} ${cloog_LIB})
	target_link_libraries(${case_name} ${barvinok_LIB})

	if(${case_name} STREQUAL ut_analysis_dependence_test)
		add_unit_test(${case_name} off)
	elseif(${case_name} STREQUAL ut_analysis_cba_full_scale_integration_test)
		add_unit_test(${case_name} off)
		target_link_libraries(${case_name} insieme_driver)
	elseif(${case_name} STREQUAL ut_analysis_cache_features_test)
		add_unit_test(${case_name} off)
	else()
		add_unit_test(${case_name})
	endif()

endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	install (DIRECTORY ${insieme_analysis_include_dir}/insieme DESTINATION include)
	INSTALL(TARGETS insieme_analysis LIBRARY DESTINATION lib)
endif(DO_INSTALL)
