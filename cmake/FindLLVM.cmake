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

# Get hint from environment variable (if any)
if(NOT LLVM_ROOT AND DEFINED ENV{LLVM_ROOT})
	set(LLVM_ROOT "$ENV{LLVM_ROOT}" CACHE PATH "LLVM base directory location (optional, used for nonstandard installation paths)")
	mark_as_advanced(LLVM_ROOT)
endif()

# Search path for nonstandard locations
if(LLVM_ROOT)
	set(LLVM_INCLUDE_PATH PATHS "${LLVM_ROOT}/include" NO_DEFAULT_PATH)
	set(LLVM_LIBRARY_PATH PATHS "${LLVM_ROOT}/lib" NO_DEFAULT_PATH)
endif()

# Find all llvm libraries
foreach (name ${llvm_LList})
	if(MSVC)
		set (llvm_${name}_LIB dummy)
	else()
		find_library(llvm_${name}_LIB NAMES ${name} HINTS ${LLVM_ROOT}/lib)
		set(llvm_LIBS ${llvm_${name}_LIB} ${llvm_LIBS})
	endif()
endforeach(name)

if(MSVC)
	# Manual list - TODO needs some cmake feature to gather "lib/libLLVM*.lib"
	set(llvm_LList
		LLVMSystem LLVMCore LLVMCodeGen LLVMSelectionDAG LLVMAsmPrinter LLVMBitReader
		LLVMBitWriter LLVMTransformUtils LLVMX86AsmParser LLVMX86AsmPrinter LLVMX86CodeGen
		LLVMX86Info LLVMX86Disassembler LLVMInstrumentation LLVMInstCombine LLVMScalarOpts
		LLVMipo LLVMLinker LLVMAnalysis LLVMipa LLVMExecutionEngine LLVMInterpreter
		LLVMJIT LLVMTarget LLVMAsmParser LLVMArchive LLVMSupport LLVMSelectionDAG LLVMMC
		LLVMMCDisassembler LLVMMCParser
	)
else()
	# find include path with the used headers
	find_path(LLVM_INCLUDE_DIRS
		NAMES
		llvm/ADT/IntrusiveRefCntPtr.h    # used in: fe/compiler.cpp
		llvm/Support/Host.h              # used in: fe/compiler.cpp
		llvm/Config/config.h             # used in: fe/compiler.cpp
		llvm/Support/raw_os_ostream.h    # used in: fe/compiler.cpp, pragma/matcher.cpp, pragma/handler.cpp
		llvm/Support/FileSystem.h        # used in: fe/translation_unit.cpp
		llvm/Support/Casting.h           # used in: fe/compiler.cpp, fe/name_manager.cpp, pragma/matcher.cpp
		llvm/ADT/PointerUnion.h          # used in: pragma/matcher.cpp
		HINTS ${LLVM_INCLUDE_PATH}
	)

	# On Linux we have a .so file for all LLVM (LLVM-${LLVM_VERSION}.so)
	find_library(LLVM_LIBRARIES NAMES LLVM-${LLVM_VERSION} HINTS ${LLVM_LIBRARY_PATH})
endif()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(LLVM DEFAULT_MSG LLVM_LIBRARIES LLVM_INCLUDE_DIRS)

mark_as_advanced(LLVM_ROOTS LLVM_LIBRARIES LLVM_INCLUDE_DIRS)

if(NOT CLANG_ROOT AND DEFINED ENV{CLANG_ROOT})
	set(CLANG_ROOT "$ENV{CLANG_ROOT}" CACHE PATH "CLANG base directory location (optional, used for nonstandard installation paths)")
	mark_as_advanced(CLANG_ROOT)
endif()

# clang-root is typically the same as llvm-root
# unless the user sets something completley different we use llvm-root for clang-root
if(NOT CLANG_ROOT AND NOT DEFINED ENV{CLANG_ROOT})
	set(CLANG_ROOT ${LLVM_ROOT} CACHE PATH "CLANG base directory location (optional, used for nonstandard installation paths)")
	mark_as_advanced(CLANG_ROOT)
endif()

# Search path for nonstandard locations
if(CLANG_ROOT)
	set(CLANG_INCLUDE_PATH PATHS "${CLANG_ROOT}/include" NO_DEFAULT_PATH)
	set(CLANG_LIBRARY_PATH PATHS "${CLANG_ROOT}/lib" NO_DEFAULT_PATH)
endif()

set(CLANG_LIB_LIST
	#libclang.a
	#libclang.so
	libclangAnalysis.a
	libclangARCMigrate.a
	libclangAST.a
	libclangASTMatchers.a
	libclangBasic.a
	libclangCodeGen.a
	libclangDriver.a
	libclangEdit.a
	libclangFrontend.a
	libclangFrontendTool.a
	libclangLex.a
	libclangParse.a
	libclangRewrite.a
	libclangRewriteFrontend.a
	libclangSema.a
	libclangSerialization.a
	libclangStaticAnalyzerCheckers.a
	libclangStaticAnalyzerCore.a
	libclangStaticAnalyzerFrontend.a
	libclangTooling.a
)

if(NOT MSVC)
	# find include path for clang headers - look for some headers used by insieme
	# listing all would be a bit verbose...
	find_path(CLANG_INCLUDE_DIRS
		NAMES
			clang/AST/Decl.h
			clang/AST/Expr.h
			clang/Sema/Sema.h
		HINTS ${CLANG_INCLUDE_PATH}
	)

	# clang provides a shared library libclang.so
	find_library(CLANG_LIBRARIES NAMES clang HINTS ${CLANG_LIBRARY_PATH})
endif()

# Find (all?) clang libraries
foreach(name ${CLANG_LIB_LIST})
	if(MSVC)
		set (clang_${name}_LIB dummy)
	else()
		find_library(CLANG_${name}_LIB NAMES ${name} HINTS ${LLVM_ROOT}/lib)
		set(CLANG_LIBRARIES ${CLANG_${name}_LIB} ${CLANG_LIBRARIES})
	endif()
endforeach(name)

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(CLANG DEFAULT_MSG CLANG_LIBRARIES CLANG_INCLUDE_DIRS)

mark_as_advanced(CLANG_ROOTS CLANG_LIBRARIES CLANG_INCLUDE_DIRS)

