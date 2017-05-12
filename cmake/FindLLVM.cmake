# Try to find LLVM headers and libraries.
#
# Usage of this module as follows:
#
# find_package(LLVM)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# - LLVM_ROOT Set this variable to the root installation of
# LLVM if the module has problems finding the proper installation path.
# - LLVM_VERSION needs to be set as LLVM generates a .so with version in the name
# - CLANG_ROOT Set this variable to the root installation of
# CLANG - typically the same as LLVM_ROOT
# - CLANG_VERSION typically the same as LLVM_VERSION
#
# Variables defined by this module:
#
# LLVM_FOUND System has LLVM libraries and headers
# LLVM_LIBRARIES The LLVM library
# LLVM_INCLUDE_DIRS The location of LLVM headers
#
# CLANG_FOUND System has CLANG libraries and headers
# CLANG_LIBRARIES The CLANG libraries
# CLANG_INCLUDE_DIRS The location of CLANG headers

# -- LLVM

# Get hint from environment variable (if any)
if(NOT LLVM_ROOT AND DEFINED ENV{LLVM_ROOT})
	set(LLVM_ROOT "$ENV{LLVM_ROOT}" CACHE PATH "LLVM base directory location (optional, used for nonstandard installation paths)")
endif()

if(LLVM_ROOT)
	set(LLVM_LIBRARY_PATH PATHS ${LLVM_ROOT}/lib NO_DEFAULT_PATH)
endif()

find_library(LLVM_LIBRARIES LLVM-${LLVM_VERSION} ${LLVM_LIBRARY_PATH})

if(NOT LLVM_ROOT)
	get_filename_component(LLVM_ROOT ${LLVM_LIBRARIES} DIRECTORY)
	get_filename_component(LLVM_ROOT ${LLVM_ROOT} DIRECTORY)
endif()

set(LLVM_INCLUDE_DIRS ${LLVM_ROOT}/include)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LLVM DEFAULT_MSG LLVM_LIBRARIES LLVM_INCLUDE_DIRS)

# -- CLANG

if(NOT CLANG_ROOT)
	if(DEFINED ENV{CLANG_ROOT})
		set(CLANG_ROOT "$ENV{CLANG_ROOT}" CACHE PATH "CLANG base directory location (optional, used for nonstandard installation paths)")
	else()
		# assume clang and llvm share the same folder
		set(CLANG_ROOT ${LLVM_ROOT} CACHE PATH "CLANG base directory location (optional, used for nonstandard installation paths)")
	endif()
endif()

find_library(CLANG_LIBRARIES clang PATHS ${CLANG_ROOT}/lib NO_DEFAULT_PATH)

if(NOT CLANG_LIBRARIES STREQUAL CLANG_LIBRARIES-NOTFOUND)
	set(CLANG_LIB_LIST
		libclangAnalysis.a libclangARCMigrate.a libclangAST.a
		libclangASTMatchers.a libclangBasic.a libclangCodeGen.a
		libclangDriver.a libclangEdit.a libclangFrontend.a
		libclangFrontendTool.a libclangLex.a libclangParse.a libclangRewrite.a
		libclangRewriteFrontend.a libclangSema.a libclangSerialization.a
		libclangStaticAnalyzerCheckers.a libclangStaticAnalyzerCore.a
		libclangStaticAnalyzerFrontend.a libclangTooling.a
	)
	foreach(lib ${CLANG_LIB_LIST})
		find_library(CLANG_${lib} ${lib} PATHS ${CLANG_ROOT}/lib NO_DEFAULT_PATH)
		set(CLANG_LIBRARIES ${CLANG_${lib}} ${CLANG_LIBRARIES})
	endforeach(lib)
endif()

set(CLANG_INCLUDE_DIRS ${CLANG_ROOT}/include)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CLANG DEFAULT_MSG CLANG_LIBRARIES CLANG_INCLUDE_DIRS)

# TODO: Insieme does not compile on windows
if(MSVC)
	set(LLVM_LIBRARIES "dummy")
	set(CLANG_LIBRARIES "dummy")
endif()
