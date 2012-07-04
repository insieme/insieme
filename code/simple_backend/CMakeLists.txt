#
# Builds the insieme simple backend
#
project(insieme_simple_backend CXX)

include(../environment.cmake)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_simple_backend_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_simple_backend_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_annotations_include_dir} )
# include_directories( ${insieme_backend_include_dir} )
include_directories( ${insieme_simple_backend_include_dir} )

# Build the core library
add_library(insieme_simple_backend ${LINKING_TYPE} ${insieme_simple_backend_srcs} ${insieme_simple_backend_incs} )

# add dependencies
#add_dependencies( insieme_simple_backend insieme_core insieme_annotations insieme_utils )
#target_link_libraries(insieme_simple_backend insieme_utils)
#target_link_libraries(insieme_simple_backend insieme_core)
#target_link_libraries(insieme_simple_backend insieme_annotations)
#target_link_libraries(insieme_simple_backend insieme_backend)
# target_link_libraries( insieme_simple_backend ${Boost_SYSTEM_LIBRARY})
# target_link_libraries( insieme_simple_backend ${Boost_FILESYSTEM_LIBRARY})

set ( ut_prefix  ut_sbe_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_analysis)
	target_link_libraries(${case_name} insieme_transform)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_backend)
	target_link_libraries(${case_name} insieme_simple_backend)
	target_link_libraries(${case_name} ${pthread_LIB})
	
	add_unit_test(${case_name})
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	install (FILES include/insieme/simple_backend/simple_backend.h DESTINATION include/insieme)
	INSTALL(TARGETS insieme_simple_backend LIBRARY DESTINATION lib)
endif(DO_INSTALL)

# ------------------------------------------------------------------------------------------------------------------

#
# Builds the insieme simple backend runtime
#
project(insieme_simple_backend_runtime C)

include(../environment.cmake)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_simple_backend_runtime_srcs 	src/runtime/*.c )
file(GLOB_RECURSE insieme_simple_backend_runtime_incs 	include/runtime/*.h )

# list all include paths

# Build the runtime library
add_library(insieme_simple_backend_runtime ${LINKING_TYPE} ${insieme_simple_backend_runtime_srcs} ${insieme_simple_backend_runtime_incs} )


