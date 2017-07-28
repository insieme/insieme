# This module has a more complex setup so we don't use add_module_library here.

# Find common source files
file(GLOB analysis_cba_srcs src/cba/*.cpp src/cba/common/*.cpp)
file(GLOB_RECURSE analysis_features_srcs src/features/*.cpp)
set(analysis_srcs ${analysis_cba_srcs} ${analysis_features_srcs})

# Find common header files
file(GLOB analysis_incs include/insieme/analysis/cba/*.h include/insieme/analysis/cba/common/*.h)

# =============================================  SOUFFLE  =====================================
if(ANALYSIS_DATALOG)

	# Add soufflé specific source files
	file(GLOB_RECURSE analysis_datalog_srcs src/cba/datalog/*.cpp src/cba/datalog/*.dl)
	set(analysis_srcs ${analysis_srcs} ${analysis_datalog_srcs})
	glob_headers(analysis_datalog_incs include/insieme/analysis/cba/datalog)
	set(analysis_incs ${analysis_incs} ${analysis_datalog_incs})

	# Generate soufflé files now so we can add them to the insieme_analysis library later
	set(souffle_input_dir  ${CMAKE_CURRENT_SOURCE_DIR}/src/cba/datalog)

	# Create directories, soufflé-related variables etc
	configure_souffle()

	# Run the Dough preprocessor script on the datalog files
	souffle_run_dough(${souffle_input_dir} ${souffle_tmp_dir})

	# compile the preprocessed analysis files *.dl to CPP header files
	file(GLOB datalog_analyses ${souffle_input_dir}/*.dl)
	foreach(analysis_file ${datalog_analyses})
	get_filename_component(analysis_name ${analysis_file} NAME_WE)
	souffle_generate_cpp(${souffle_tmp_dir} ${analysis_name} ${souffle_tmp_dir}/include)
	endforeach(analysis_file)

	set(analysis_srcs ${analysis_srcs} ${souffle_output})

endif()

# =============================================  HASKELL  =====================================
if(ANALYSIS_HASKELL)

	# Add Haskell specific source files
	file(GLOB_RECURSE analysis_haskell_srcs src/cba/haskell/*.cpp)
	set(analysis_srcs ${analysis_srcs} ${analysis_haskell_srcs})
	glob_headers(analysis_haskell_incs include/insieme/analysis/cba/haskell)
	set(analysis_incs ${analysis_incs} ${analysis_haskell_incs})

	if(NOT GMP_FOUND)
		message(WARNING "GMP NOT found")
	endif()

	add_subdirectory(src/cba/haskell/insieme-hat)
	link_directories(${CMAKE_CURRENT_BINARY_DIR}/src/cba/haskell/insieme-hat)
endif()

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
target_link_libraries(analysis ${Boost_LIBRARIES})
target_include_directories(analysis SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

# Souffle dependencies
if(ANALYSIS_DATALOG)
	# Find SQLite3 includes and lib. Newer versions of Soufflé need this
	find_package(SQLITE)
	if(NOT SQLITE_FOUND)
		message(WARNING "SQLite NOT found")
		set(SQLITE_LIBRARIES "")
		set(SQLITE_INCLUDE_DIRS "")
	endif()
	target_link_libraries(analysis ${SQLITE_LIBRARIES})
	target_include_directories(analysis SYSTEM PRIVATE ${SQLITE_INCLUDE_DIRS})
endif()

# Haskell dependency
if(ANALYSIS_HASKELL)
	add_dependencies(analysis insieme_hat)

	target_link_libraries(analysis ${insieme_hat})
endif()

# Find test cases base
file(GLOB test_cases test/*.cc)

file(GLOB_RECURSE test_cases_common test/cba/common/*.cc)
set(test_cases ${test_cases} ${test_cases_common})

file(GLOB_RECURSE test_cases_features test/features/*.cc)
set(test_cases ${test_cases} ${test_cases_features})

if(ANALYSIS_DATALOG)
	file(GLOB_RECURSE test_cases_datalog test/cba/datalog/*.cc)
	set(test_cases ${test_cases} ${test_cases_datalog})
endif()

if(ANALYSIS_HASKELL)
	file(GLOB_RECURSE test_cases_haskell test/cba/haskell/*.cc)
	set(test_cases ${test_cases} ${test_cases_haskell})
endif()

# Find common test case headers
glob_headers(test_common_incs test/cba/common)

foreach(test ${test_cases})
	add_module_unittest(analysis ${test} OUTPUT_TARGET_NAME test_tgt)

	if(BUILD_TESTS)
		target_sources(${test_tgt} PRIVATE ${test_common_incs})
		target_link_libraries(${test_tgt} driver)
	endif()
endforeach(test)
