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
set ( insieme_frontend_include_dir ${insieme_code_dir}/frontend/include )

set ( CXXTEST_INCLUDE_DIR ${insieme_code_dir}/../thirdparty/cxxtest )

# include boost headers
find_package( Boost )
include_directories( ${Boost_INCLUDE_DIRS} )
link_directories(${Boost_LIBRARY_DIRS})

# glog
if(MSVC)
	include_directories( $ENV{GLOG_HOME}/src/windows )
	set(glog_LIB $ENV{GLOG_HOME}/Release/libglog_static.lib)
else()
	include_directories( $ENV{GLOG_HOME}/include )
	find_library(glog_LIB NAMES glog PATHS $ENV{GLOG_HOME}/lib)
endif()

# lookup perl
find_package( Perl )

# lookup pthread library
find_library(pthread_LIB pthread)

# Visual Studio customization
if(MSVC) 
	# disable some warnings
	add_definitions( /D_CRT_SECURE_NO_WARNINGS )
	# disable warning "assignment operator could not be generated"
	add_definitions( /wd"4512" )
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

	add_definitions( -fshow-column )

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

# Add debug symbols
add_definitions( -g )

# avoid multiple import
if (NOT MEMORY_CHECK_SETUP)
	option(CONDUCT_MEMORY_CHECKS "Checks all test cases for memory leaks using valgrind if enabled." OFF)

	# add -all-valgrind target
	# if(NOT CONDUCT_MEMORY_CHECKS)
		add_custom_target(valgrind)
	#endif(NOT CONDUCT_MEMORY_CHECKS)

	# define macro for adding tests
	macro ( add_unit_test case_name )
		if(CONDUCT_MEMORY_CHECKS)
			# no valgrind support in MSVC 
			if(NOT MSVC)
				# add valgrind as a test
				add_test(NAME valgrind_${case_name} 
					COMMAND valgrind
						--leak-check=full
						--show-reachable=yes
						--track-fds=yes
						--error-exitcode=1
						#--log-file=${CMAKE_CURRENT_BINARY_DIR}/valgrind.log.${case_name}
						${CMAKE_CURRENT_BINARY_DIR}/ut_${case_name}
					WORKING_DIRECTORY
						${CMAKE_CURRENT_BINARY_DIR}
				)
			endif(NOT MSVC)
		else(CONDUCT_MEMORY_CHECKS)
			# add normal test
			add_test(ut_${case_name} ut_${case_name})

			# no valgrind support in MSVC
			if(NOT MSVC)
				# add valgrind as seperated target
				add_custom_target(valgrind_${case_name})
				add_custom_command(TARGET valgrind_${case_name} 
					COMMAND valgrind
						--leak-check=full
						--show-reachable=yes
						--track-fds=yes
						--error-exitcode=1
						#--log-file=${CMAKE_CURRENT_BINARY_DIR}/valgrind.log.${case_name}
						${CMAKE_CURRENT_BINARY_DIR}/ut_${case_name}
					WORKING_DIRECTORY
						${CMAKE_CURRENT_BINARY_DIR}
				)
				add_dependencies(valgrind valgrind_${case_name})
			endif(NOT MSVC)
		endif(CONDUCT_MEMORY_CHECKS)
	endmacro(add_unit_test)


endif (NOT MEMORY_CHECK_SETUP)

# mark as defined
set(MEMORY_CHECK_SETUP OFF CACHE INTERNAL "Flag to avoid multiple setup" PARENT_SCOPE)


