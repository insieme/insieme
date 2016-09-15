#
# Builds the analysis framework which uses Haskell and Soufflé
#
project(insieme_analysis CXX)
message(STATUS "========== configure analysis =============")
cmake_minimum_required(VERSION 3.2)

# Setup common source files
file(GLOB insieme_analysis_srcs src/*.cpp)
file(GLOB_RECURSE insieme_analysis_incs
    include/insieme/analysis/cba_interface.h
    include/insieme/analysis/cba_postprocessing.h
    include/insieme/analysis/common/*.h
)

# =============================================  SOUFFLE  =====================================
if(ANALYSIS_DATALOG)

    # Add soufflé specific source files
    file(GLOB_RECURSE insieme_analysis_srcs_datalog src/datalog/*.cpp src/datalog/*.dl)
    set(insieme_analysis_srcs ${insieme_analysis_srcs} ${insieme_analysis_srcs_datalog})
    insieme_glob_headers(insieme_analysis_incs_datalog include/insieme/analysis/datalog)
    set(insieme_analysis_incs ${insieme_analysis_incs} ${insieme_analysis_incs_datalog} include/insieme/analysis/datalog_interface.h)

    # Generate soufflé files now so we can add them to the insieme_analysis library later
    set(souffle_input_dir  ${insieme_code_dir}/analysis/src/datalog)

    # Create directories, soufflé-related variables etc
    configure_souffle()

    # Run the Dough preprocessor script on the datalog files
    souffle_run_dough(${souffle_input_dir} ${souffle_tmp_dir})

    # compile the preprocessed analysis files *.dl to CPP header files
    file(GLOB datalog_analysises ${souffle_input_dir}/*.dl)
    foreach(analysis_file ${datalog_analysises})
        get_filename_component(analysis_name ${analysis_file} NAME_WE)
        souffle_generate_cpp(${souffle_tmp_dir} ${analysis_name} "${souffle_tmp_dir}/include")
    endforeach()

    set(insieme_analysis_srcs ${insieme_analysis_srcs} ${souffle_output})

endif()


# =============================================  HASKELL  =====================================

if(ANALYSIS_HASKELL)

    # Add Haskell specific source files
    file(GLOB_RECURSE insieme_analysis_srcs_haskell src/haskell/*.cpp src/haskell/*.dl)
    set(insieme_analysis_srcs ${insieme_analysis_srcs} ${insieme_analysis_srcs_haskell})
    insieme_glob_headers(insieme_analysis_incs_haskell include/insieme/analysis/haskell)
    set(insieme_analysis_incs ${insieme_analysis_incs} ${insieme_analysis_incs_haskell} include/insieme/analysis/haskell_interface.h)

    insieme_find_package(NAME GMP)

    add_subdirectory(src/haskell/insieme-hat)
    link_directories(${CMAKE_CURRENT_BINARY_DIR}/src/haskell/insieme-hat)

    # add directories containing libffi, libHSrts and gmp
    link_directories(${LIBHSRTS_DIR})
    link_directories(${GMP_ROOT}/lib)

    set(LIBHSRTS_DIR ${LIBHSRTS_DIR} PARENT_SCOPE)
    set(LIBHSRTS ${LIBHSRTS} PARENT_SCOPE)

endif()


# =============================================================================================

# Build the analysis library
add_library(insieme_analysis ${LINKING_TYPE} ${insieme_analysis_srcs} ${insieme_analysis_incs})
set_target_properties(insieme_analysis PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(insieme_analysis PUBLIC ${insieme_analysis_include_dir})

# Haskell dependency
if(ANALYSIS_HASKELL)
    add_dependencies(insieme_analysis insieme_analysis_haskell)
    target_include_directories(insieme_analysis SYSTEM PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/haskell/insieme-hat/include)
    target_link_libraries(insieme_analysis ${insieme_analysis_haskell})
endif()

# lookup Boost library
insieme_find_package(NAME Boost)
target_include_directories(insieme_analysis SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

target_link_libraries(insieme_analysis insieme_core)
target_link_libraries(insieme_analysis insieme_utils)

#cotire(insieme_analysis)


# =============================================  TESTING  =====================================

set(ut_prefix ut_analysis)

# Find relevant source files
set(test_cases ${CMAKE_CURRENT_SOURCE_DIR}/test/interface_test.cc)
if(ANALYSIS_DATALOG)
    file(GLOB_RECURSE test_cases_datalog test/datalog/*.cc)
    set(test_cases ${test_cases} ${test_cases_datalog})
endif()
if(ANALYSIS_HASKELL)
    file(GLOB_RECURSE test_cases_haskell test/haskell/*.cc)
    set(test_cases ${test_cases} ${test_cases_haskell})
endif()

foreach(case_file ${test_cases})
    get_filename_component(case_name ${case_file} NAME_WE)

    insieme_fix_case_name(case_subdir ${case_file})

    set(case_name ${ut_prefix}${case_subdir}_${case_name})
    add_executable(${case_name} ${case_file})
    target_link_libraries(${case_name} insieme_analysis)
    target_link_libraries(${case_name} insieme_driver)

    if(ANALYSIS_HASKELL)
        target_include_directories(${case_name} SYSTEM PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/haskell/insieme-hat/include)

        # Gold linker does not automatically resolve libHSrts depdency from
        # insieme_analysis lib, need to specify them explicitly.
        target_link_libraries(${case_name} ${LIBHSRTS})
    endif()

    add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)
