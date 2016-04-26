#
# Builds the analysis framework which uses Haskell and Soufflé
#
project(insieme_analysis CXX)
message(STATUS "========== configure analysis =============")
cmake_minimum_required(VERSION 3.2)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_analysis_srcs src/haskell/*.cpp src/*.def src/*.inc src/*.dl)
insieme_glob_headers(insieme_analysis_incs include)


# =============================================  SOUFFLE  =====================================
#
## Generate soufflé files now so we can add them to the insieme_analysis library later
#set(souffle_input_dir  ${insieme_code_dir}/analysis/src/datalog)
#
#build_souffle()
#
## compile all analysis files *.dl to CPP header files
#file(GLOB datalog_analysis ${souffle_input_dir}/*.dl)
#foreach ( analysis_file ${datalog_analysis} )
#	get_filename_component( analysis_name ${analysis_file} NAME_WE )
#	souffle_generate_cpp(${souffle_input_dir} ${analysis_name} "${souffle_input_dir}/include" )
#endforeach()
#
#
# =============================================================================================

# Build the analysis library
add_library(insieme_analysis ${LINKING_TYPE} ${insieme_analysis_srcs} ${insieme_analysis_incs} ) #${souffle_output})
target_include_directories(insieme_analysis PUBLIC ${insieme_analysis_include_dir})

# lookup Boost library
insieme_find_package(NAME Boost)
target_include_directories(insieme_analysis SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

target_link_libraries(insieme_analysis insieme_core)
target_link_libraries(insieme_analysis insieme_utils)

cotire(insieme_analysis)


## =============================================  HASKELL  =====================================

add_subdirectory(src/haskell/haskell_inspire)
add_dependencies(insieme_analysis insieme_analysis_haskell)
target_link_libraries(insieme_analysis ${insieme_analysis_haskell})
link_directories(${CMAKE_CURRENT_BINARY_DIR}/src/haskell/haskell_inspire)

# directory containing libffi and libHSrts
link_directories(${LIBHSRTS_DIR})

# haskell stuff requires gmp
link_directories($ENV{INSIEME_LIBS_HOME}/gmp-latest/lib)

## =============================================  TESTING  =====================================

set ( ut_prefix  ut_analysis )
file(GLOB_RECURSE test_cases test/node_count_test.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )

	insieme_fix_case_name( case_subdir ${case_file} )

	set ( case_name ${ut_prefix}${case_subdir}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_analysis)

	add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)
