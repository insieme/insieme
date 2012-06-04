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

# add include directory
include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${SHARK_HOME}/include
	${KOMPEX_HOME}/inc )


set( machine_learning_libraries 
	insieme_machine_learning
	insieme_utils
	dl
	${shark_LIB}
	${kompex_LIB}
)

# build the machine learning library
add_library(insieme_machine_learning ${LINKING_TYPE} ${insieme_machine_learning_srcs} ${insieme_machine_learning_incs} )

# Build machine learning executables
add_executable( train_ffnet src/train_ffnet.cxx ${insieme_machine_learning_incs})
add_dependencies( train_ffnet ${machine_learning_libraries} )
target_link_libraries( train_ffnet ${machine_learning_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY} ${pthread_LIB})

add_executable( evaluate_ffnet src/evaluate_ffnet.cxx ${insieme_machine_learning_incs})
add_dependencies( evaluate_ffnet ${machine_learning_libraries} )
target_link_libraries( evaluate_ffnet ${machine_learning_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY} ${pthread_LIB})

add_executable( train_svm src/train_svm.cxx ${insieme_machine_learning_incs})
add_dependencies( train_svm ${machine_learning_libraries} )
target_link_libraries( train_svm ${machine_learning_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY} ${pthread_LIB})

add_executable( calculate_pcs src/calculate_pcs.cxx ${insieme_machine_learning_incs})
add_dependencies( calculate_pcs ${machine_learning_libraries} )
target_link_libraries( calculate_pcs ${machine_learning_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY} ${pthread_LIB})

set ( ut_prefix  ut_ml_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	include_directories(${case_name} ${insieme_utils_include_dir})
	target_link_libraries(${case_name} insieme_machine_learning)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} dl)
	target_link_libraries(${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${case_name} ${shark_LIB})
	target_link_libraries(${case_name} ${kompex_LIB})
	target_link_libraries(${case_name} ${pthread_LIB})
	
	add_unit_test(${case_name} )
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_machine_learning LIBRARY DESTINATION lib)
endif(DO_INSTALL)

