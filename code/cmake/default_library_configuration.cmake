####
#
#
#
# The default versions for the different libraries used by insieme
# can be overwritten by the user with an env-variable
# we rely on capitalized variables: LIBNAME_VERSION
#
#
#
####

#LLVM/CLANG
set(LLVM_VERSION 3.6.2)

#PAPI - used by: driver, runtime
set(PAPI_VERSION 5.4.0)

#HWLOC - used by: runtime
set(HWLOC_VERSION 1.10.1)

#ISL - used by: analysis
set(ISL_VERSION 0.10)

#CLOOG - used by: analysis
set(CLOOG_VERSION 0.17.0)

#BARVINOK - used by: analysis
set(BARVINOK_VERSION 0.35)

#MPFR - used by: analysis
set(MPFR_VERSION 3.1.1)

#GMP - used by: analysis
set(GMP_VERSION 6.0.0)

#CUDD - used by: core
set(CUDD_VERSION 2.4.2)

#LUAJIT - used by: utils
set(LUAJIT_VERSION 2.0.3)

#KOMPEX - used by: machine_learning
set(KOMPEX_VERSION 1.7.9)

#GTEST - used for unit tests
set(GTEST_VERSION 1.7.0)

#BOOST - used everywhere except for runtime...
set(BOOST_VERSION 1.50.0)

#BISON - used in core
set(BISON_VERSION 3.0.0)

#FLEX - used in core
set(FLEX_VERSION 2.5.35)

#TODO: currently this happens everytime we include this cmake some where...
#if the user provides a env{lib_version} for one of the libraries
# we overwrite the default
list(APPEND LIB_VERSIONS LLVM_VERSION PAPI_VERSION ISL_VERSION CLOOG_VERSION
	BARVINOK_VERSION MPFR_VERSION GMP_VERSION CUDD_VERSION LUAJIT_VERSION
	KOMPEX_VERSION GTEST_VERSION BOOST_VERSION BISON_VERSION FLEX_VERSION HWLOC_VERSION)

foreach(libversion ${LIB_VERSIONS})
	#get ${lib_NAME}_VERSION from library_default_version_file
	if( DEFINED ENV{${libversion}} )
		# overwrite if user specifies otherwise 
		set(${libversion} $ENV{${libversion}})
		message(STATUS "Overwriting default lib version: ${libversion} = ${${libversion}}")
	endif()
endforeach()
