#
# Builds the insieme utils library
#
project(insieme_utils CXX)
message(STATUS "========== configure utils =============")
cmake_minimum_required(VERSION 3.2)

# collect source files 
file(GLOB_RECURSE insieme_utils_srcs 	src/*.cpp )
insieme_glob_headers(insieme_utils_incs include)

# add include directory
#include_directories(include)

# build the utils library
add_library(insieme_utils ${LINKING_TYPE} ${insieme_utils_srcs} ${insieme_utils_incs} )
target_include_directories(insieme_utils PUBLIC include)
# required for generating import libraries (".lib") in Windows for DLL linking
if(MSVC)
	include (GenerateExportHeader)
	GENERATE_EXPORT_HEADER(insieme_utils)
endif()

target_link_libraries(insieme_utils insieme_common)

# lookup Boost library
insieme_find_package(NAME Boost COMPONENTS filesystem system) 
target_include_directories(insieme_utils SYSTEM PUBLIC ${Boost_INCLUDE_DIRS})
target_link_libraries(insieme_utils ${Boost_FILESYSTEM_LIBRARY})
target_link_libraries(insieme_utils ${Boost_SYSTEM_LIBRARY})

# lookup LuaJIT library
insieme_find_package(NAME LUAJIT)
target_include_directories(insieme_utils PUBLIC ${LUAJIT_INCLUDE_DIRS})
target_link_libraries(insieme_utils ${LUAJIT_LIBRARIES})

# =============================================  TESTING  =====================================
set ( ut_prefix  ut_utils )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases} )
	get_filename_component( case_name ${case_file} NAME_WE )

	insieme_fix_case_name( case_subdir ${case_file} )
	
	set ( case_name ${ut_prefix}${case_subdir}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_utils)

	add_unit_test(${case_name} ${ut_prefix})
        
endforeach(case_file)
