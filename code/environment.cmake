#
# This file sets up some general variables and include paths for the build environment
#

# - define some code locations

# get code root directory (based on current file name path)
get_filename_component( insieme_code_dir ${CMAKE_CURRENT_LIST_FILE} PATH )

set ( insieme_core_src_dir 	${insieme_code_dir}/core/src )
set ( insieme_core_include_dir 	${insieme_code_dir}/core/include )

set ( insieme_utils_src_dir 	${insieme_code_dir}/utils/src )
set ( insieme_utils_include_dir ${insieme_code_dir}/utils/include )

set ( CXXTEST_INCLUDE_DIR ${insieme_code_dir}/../thirdparty/cxxtest )

# include boost headers
find_package( Boost )
include_directories( ${Boost_INCLUDE_DIRS} )
link_directories(${Boost_LIBRARY_DIRS})

# lookup perl
find_package( Perl )

# Visual Studio customization
if(MSVC) 
	# disable some warnings
	add_definitions( /D_CRT_SECURE_NO_WARNINGS )
	# statically link with runtime library (required for gtest)
	foreach(flag_var
		CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
		CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
		if(${flag_var} MATCHES "/MD")
			string(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
		endif(${flag_var} MATCHES "/MD")
	endforeach(flag_var)
endif()

# enable C++0x support within gcc (if supported)
if (CMAKE_COMPILER_IS_GNUCXX)
	include(CheckCXXCompilerFlag)
	check_cxx_compiler_flag( -std=c++0x CXX0X_Support )
	if(CXX0X_Support)
		add_definitions( -std=c++0x )
	else()
		message( "WARNING: --std=c++0x not supported by your compiler!" )
	endif()
endif()

# enable warnings
if(MSVC) 
	add_definitions( /W4 )
endif()
if (CMAKE_COMPILER_IS_GNUCXX)
	add_definitions( -Wall )
endif()