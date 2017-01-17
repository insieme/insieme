# This module has a more complex setup so we don't use add_module_library here.

# Find common source files
file(GLOB analysis_cba_srcs src/cba/*.cpp src/cba/common/*.cpp)
file(GLOB_RECURSE analysis_features_srcs src/features/*.cpp)
set(analysis_srcs ${analysis_cba_srcs} ${analysis_features_srcs})

# Find common header files
file(GLOB analysis_incs include/insieme/analysis/cba/*.h include/insieme/analysis/cba/common/*.h)

# =============================================  SOUFFLE  =====================================
#if(ANALYSIS_DATALOG)
#
#    # Add soufflé specific source files
#    file(GLOB_RECURSE insieme_analysis_srcs_datalog src/cba/datalog/*.cpp src/cba/datalog/*.dl)
#    set(insieme_analysis_srcs ${insieme_analysis_srcs} ${insieme_analysis_srcs_datalog})
#    insieme_glob_headers(insieme_analysis_incs_datalog include/insieme/analysis/cba/datalog)
#    set(insieme_analysis_incs ${insieme_analysis_incs} ${insieme_analysis_incs_datalog})
#
#    # Generate soufflé files now so we can add them to the insieme_analysis library later
#    set(souffle_input_dir  ${insieme_code_dir}/analysis/src/cba/datalog)
#
#    # Create directories, soufflé-related variables etc
#    configure_souffle()
#
#    # Run the Dough preprocessor script on the datalog files
#    souffle_run_dough(${souffle_input_dir} ${souffle_tmp_dir})
#
#    # compile the preprocessed analysis files *.dl to CPP header files
#    file(GLOB datalog_analysises ${souffle_input_dir}/*.dl)
#    foreach(analysis_file ${datalog_analysises})
#        get_filename_component(analysis_name ${analysis_file} NAME_WE)
#        souffle_generate_cpp(${souffle_tmp_dir} ${analysis_name} "${souffle_tmp_dir}/include")
#    endforeach()
#
#    set(insieme_analysis_srcs ${insieme_analysis_srcs} ${souffle_output})
#
#endif()

# =============================================  HASKELL  =====================================
#if(ANALYSIS_HASKELL)
#
#    # Add Haskell specific source files
#    file(GLOB_RECURSE insieme_analysis_srcs_haskell src/cba/haskell/*.cpp)
#    set(insieme_analysis_srcs ${insieme_analysis_srcs} ${insieme_analysis_srcs_haskell})
#    insieme_glob_headers(insieme_analysis_incs_haskell include/insieme/analysis/cba/haskell)
#    set(insieme_analysis_incs ${insieme_analysis_incs} ${insieme_analysis_incs_haskell})
#
#    insieme_find_package(NAME GMP)
#
#    add_subdirectory(src/cba/haskell/insieme-hat)
#    link_directories(${CMAKE_CURRENT_BINARY_DIR}/src/cba/haskell/insieme-hat)
#
#    # add directories containing libffi, libHSrts and gmp
#    link_directories(${LIBHSRTS_DIR})
#    link_directories(${GMP_ROOT}/lib)
#
#    set(LIBHSRTS_DIR ${LIBHSRTS_DIR} PARENT_SCOPE)
#    set(LIBHSRTS ${LIBHSRTS} PARENT_SCOPE)
#
#endif()

if(MSVC)
	set(analysis_srcs ${analysis_srcs} ${analysis_incs})
endif()

# Build library
add_library(analysis ${analysis_srcs})
set_target_properties(analysis PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(analysis PUBLIC include)

# Output name
set_target_properties(analysis PROPERTIES OUTPUT_NAME ${PROJECT_NAME}_analysis)

# IDE structure
if(MSVC)
	msvc_source_group("Source Files" "${analysis_srcs}" STRIP src)
	msvc_source_group("Header Files" "${analysis_incs}" STRIP include/${PROJECT_NAME}/analysis)
	set_target_properties(analysis PROPERTIES FOLDER analysis)
endif()

# Dependencies
target_link_libraries(analysis core)
target_link_libraries(analysis utils)

# Boost
find_package(Boost ${BOOST_VERSION} EXACT REQUIRED)
target_link_libraries(analysis ${Boost_LIBRARIES})
target_include_directories(analysis SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

# Souffle dependencies
#if(ANALYSIS_DATALOG)
#	# Find SQLite3 includes and lib. Newer versions of Soufflé need this
#	insieme_find_package(NAME SQLITE)
#	target_include_directories(insieme_analysis SYSTEM PRIVATE ${SQLITE_INCLUDE_DIRS})
#	target_link_libraries(insieme_analysis ${SQLITE_LIBRARIES})
#endif()

# Haskell dependency
#if(ANALYSIS_HASKELL)
#    add_dependencies(insieme_analysis insieme_analysis_haskell)
#    target_include_directories(insieme_analysis SYSTEM PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/cba/haskell/insieme-hat/include)
#
#    # Gold linker does not automatically resolve libHSrts depdency from
#    # insieme_analysis lib, need to specify them explicitly.
#    target_link_libraries(insieme_analysis ${LIBHSRTS})
#
#    target_link_libraries(insieme_analysis ${insieme_analysis_haskell})
#endif()

# Find test cases base
file(GLOB test_cases test/*.cc)

file(GLOB_RECURSE test_cases_common test/cba/common/*.cc)
set(test_cases ${test_cases} ${test_cases_common})

file(GLOB_RECURSE test_cases_features test/features/*.cc)
set(test_cases ${test_cases} ${test_cases_features})

#if(ANALYSIS_DATALOG)
#    file(GLOB_RECURSE test_cases_datalog test/cba/datalog/*.cc)
#    set(test_cases ${test_cases} ${test_cases_datalog})
#endif()

#if(ANALYSIS_HASKELL)
#    file(GLOB_RECURSE test_cases_haskell test/cba/haskell/*.cc)
#    set(test_cases ${test_cases} ${test_cases_haskell})
#endif()

# Find common test case headers
glob_headers(test_common_incs test/cba/common)

foreach(test ${test_cases})
	add_module_unittest(analysis ${test} TARGET_NAME tgt_name)

	if(BUILD_TESTS)
		target_sources(${tgt_name} PRIVATE ${test_common_incs})
		target_link_libraries(${tgt_name} driver)
	endif()
endforeach(test)
