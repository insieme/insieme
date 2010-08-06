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


# include boost headers
find_package( Boost )
include_directories(BOOST_INCLUDE_DIRS)


# disable some warnings within visual studio
if(MSVC) 
	set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} /D "_CRT_SECURE_NO_WARNINGS")
endif()


# enable C++0x support within gcc (if supported)
if (CMAKE_COMPILER_IS_GNUCXX)
	include(CheckCXXCompilerFlag)
	check_cxx_compiler_flag( --std=c++0x CXX0X_Support )
	if(CXX0X_Support)
		add_definitions( --std=c++0x )
	else()
		message( "WARNING: --std=c++0x not supported by your compiler!" )
	endif()
endif()
