#
# Builds the insieme machine_learning library
#
project(insieme_machine_learning CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# collect source files
file(GLOB_RECURSE insieme_machine_learning_srcs 	src/*.cpp)
file(GLOB_RECURSE insieme_machine_learning_incs	include/*.h)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/inputs.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/machine_learning/inputs.h)

# lookup shark library
#lookup_lib( SHARK shark )
find_package(SHARK)

# lookup kompex library
#lookup_lib( KOMPEX KompexSQLiteWrapper_Static_d )
find_package(KOMPEX)

# build the machine learning library
add_library(insieme_machine_learning ${LINKING_TYPE} ${insieme_machine_learning_srcs} ${insieme_machine_learning_incs} )
target_include_directories(insieme_machine_learning PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(insieme_machine_learning insieme_utils)
target_link_libraries(insieme_machine_learning dl)
#target_link_libraries(insieme_machine_learning ${shark_LIB})
target_include_directories(insieme_machine_learning PUBLIC ${SHARK_INCLUDE_DIRS})
target_link_libraries(insieme_machine_learning ${SHARK_LIBRARIES})
#target_link_libraries(insieme_machine_learning ${kompex_LIB})
target_include_directories(insieme_machine_learning PUBLIC ${KOMPEX_INCLUDE_DIRS})
target_link_libraries(insieme_machine_learning ${KOMPEX_LIBRARIES})

# Build machine learning executables
add_executable( train_ffnet src/train_ffnet.cxx ${insieme_machine_learning_incs})
target_link_libraries(train_ffnet insieme_machine_learning)
target_link_libraries(train_ffnet insieme_utils)
target_link_libraries(train_ffnet dl)
target_link_libraries(train_ffnet ${shark_LIB})
target_link_libraries(train_ffnet ${kompex_LIB})
target_link_libraries(train_ffnet ${Boost_PROGRAM_OPTIONS_LIBRARY})

add_executable(evaluate_model src/evaluate_model.cxx ${insieme_machine_learning_incs})
target_link_libraries(evaluate_model insieme_machine_learning)
target_link_libraries(evaluate_model insieme_utils)
target_link_libraries(evaluate_model dl)
target_link_libraries(evaluate_model ${shark_LIB})
target_link_libraries(evaluate_model ${kompex_LIB})
target_link_libraries(evaluate_model ${Boost_PROGRAM_OPTIONS_LIBRARY})

add_executable(train_svm src/train_svm.cxx ${insieme_machine_learning_incs})
target_link_libraries(train_svm insieme_machine_learning)
target_link_libraries(train_svm insieme_utils)
target_link_libraries(train_svm dl)
target_link_libraries(train_svm ${shark_LIB})
target_link_libraries(train_svm ${kompex_LIB})
target_link_libraries(train_svm ${Boost_PROGRAM_OPTIONS_LIBRARY})

add_executable(calculate_pcs src/calculate_pcs.cxx ${insieme_machine_learning_incs})
target_link_libraries(calculate_pcs insieme_machine_learning)
target_link_libraries(calculate_pcs insieme_utils)
target_link_libraries(calculate_pcs dl)
target_link_libraries(calculate_pcs ${shark_LIB})
target_link_libraries(calculate_pcs ${kompex_LIB})
target_link_libraries(calculate_pcs ${Boost_PROGRAM_OPTIONS_LIBRARY}) 

set ( ut_prefix  ut_ml_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_machine_learning)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} dl)
	target_link_libraries(${case_name} ${shark_LIB})
	target_link_libraries(${case_name} ${kompex_LIB})
	
	add_unit_test(${case_name} )
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_machine_learning LIBRARY DESTINATION lib)
endif(DO_INSTALL)

