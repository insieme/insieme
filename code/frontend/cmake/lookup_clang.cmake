# -------------------------------------------------------------- LLVM / CLANG  3.4 libraries
#Fix LLVM path
if(NOT DEFINED LLVM_HOME)
	if (NOT $ENV{LLVM_HOME} STREQUAL "")
		set (LLVM_HOME $ENV{LLVM_HOME})
	else()
		set (LLVM_HOME ${THIRD_PARTY_LIBS_HOME}/llvm-latest)
	endif()
endif()

# FIXME: select only the needed libraries
set(clang_LList
	#libclang.a
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
	libclangRewriteCore.a
	libclangRewriteFrontend.a
	libclangSema.a
	libclangSerialization.a
	#libclang.so
	libclangStaticAnalyzerCheckers.a
	libclangStaticAnalyzerCore.a
	libclangStaticAnalyzerFrontend.a
	libclangTooling.a
)
	#clangBasic clangSema clangIndex clangDriver clangAST
	#clangRewrite clangAnalysis clangLex clangFrontend clangFrontendTool 

if(MSVC)
    # Manual list - TODO: needs some cmake feature to gather "lib/libLLVM*.lib"
    set( llvm_LList
       LLVMSystem LLVMCore LLVMCodeGen LLVMSelectionDAG LLVMAsmPrinter LLVMBitReader 
       LLVMBitWriter LLVMTransformUtils LLVMX86AsmParser LLVMX86AsmPrinter LLVMX86CodeGen
       LLVMX86Info LLVMX86Disassembler LLVMInstrumentation LLVMInstCombine LLVMScalarOpts 
       LLVMipo LLVMLinker LLVMAnalysis LLVMipa LLVMExecutionEngine LLVMInterpreter 
       LLVMJIT LLVMTarget LLVMAsmParser LLVMArchive LLVMSupport LLVMSelectionDAG LLVMMC 
       LLVMMCDisassembler LLVMMCParser
    )
    #set(clang_LList libclang ${clang_LList})

else(MSVC)
	# On Linux we have a .so file for all LLVM
	set(llvm_LList  LLVM-3.4 )
    set(clang_LList clang ${clang_LList})
endif(MSVC)


# Find all llvm libraries
foreach (name ${llvm_LList})
    if(MSVC) 
        set (llvm_${name}_LIB dummy)
    else()
		find_library(llvm_${name}_LIB  NAMES ${name}  HINTS ${LLVM_HOME}/lib)
        set(llvm_LIBS ${llvm_${name}_LIB} ${llvm_LIBS})
    endif()
endforeach(name)


# Find (all?) clang libraries
foreach (name ${clang_LList})
    if(MSVC) 
        set (clang_${name}_LIB dummy)
    else()
		find_library(clang_${name}_LIB  NAMES ${name}  HINTS ${LLVM_HOME}/lib)
        set(clang_LIBS ${clang_${name}_LIB} ${clang_LIBS})
    endif()
endforeach(name)
