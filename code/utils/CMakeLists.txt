#
# Builds the insieme util library
#
project(insieme_utils CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.8
cmake_minimum_required(VERSION 2.8)

# configure config files
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/utils/config.h)

# collect source files 
file(GLOB_RECURSE insieme_utils_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_utils_incs	include/*.h )

# add include directory
#include_directories(include)

# build the utils library
add_library(insieme_utils ${LINKING_TYPE} ${insieme_utils_srcs} ${insieme_utils_incs} )
target_include_directories(insieme_utils PUBLIC include)

target_link_libraries(insieme_utils ${Boost_LIBRARIES})

# add boost_filesystem library
target_link_libraries( insieme_utils ${Boost_FILESYSTEM_LIBRARY})

# lookup LuaJIT library
#lookup_lib( LUAJIT luajit-5.1 )
## add LuaJIT library
#target_link_libraries(insieme_utils ${luajit_LIB})
#target_include_directories(insieme_utils PUBLIC ${luajit_INC})
find_package(LUAJIT)
target_include_directories(insieme_utils PUBLIC ${LUAJIT_INCLUDE_DIRS})
target_link_libraries(insieme_utils ${LUAJIT_LIBRARIES})

target_link_libraries(insieme_utils dl)

set ( ut_prefix  ut_util_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases} )
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} ${Boost_LIBRARIES})
	target_link_libraries(${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${case_name} ${Boost_FILESYSTEM_LIBRARY})
	target_link_libraries(${case_name} ${luajit_LIB} )

	# exclude lua tests from valgrind since it fails for versions newer than 3.7.0
	set(VFLAG ${TRUE})
	if(case_file MATCHES ".*lua.*")
		set(VFLAG FALSE)
	endif(case_file MATCHES ".*lua.*")
	add_unit_test(${case_name} ${VFLAG})
        
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	install (DIRECTORY ${insieme_utils_include_dir}/insieme DESTINATION include)
	INSTALL(TARGETS insieme_utils LIBRARY DESTINATION lib)
endif(DO_INSTALL)

