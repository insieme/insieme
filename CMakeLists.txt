project(insieme)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

set(LD_LIBRARY_PATH /software/lib-gcc450/:${LD_LIBRARY_PATH})
set(CMAKE_CXX_COMPILER /software/gcc450/bin/g++)
set(CMAKE_C_COMPILER   /software/gcc450/bin/gcc)

set(LLVM_HOME 		 /home/spellegrini/shared/llvm-2.7/)
set(CLANG_SRC_HOME /home/spellegrini/shared/llvm-2.7-src/tools/clang)

include_directories( 
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${LLVM_HOME}/include 
	${CLANG_SRC_HOME} )

set(CMAKE_CXX_FLAGS "-g -Wall -fmessage-length=0 -fno-rtti")

find_library(clang_CompilerDriver_LIB NAMES CompilerDriver 	PATHS ${LLVM_HOME}/lib)
find_library(clang_Basic_LIB 		  		NAMES clangBasic 			PATHS ${LLVM_HOME}/lib)
find_library(clang_Sema_LIB 		  		NAMES clangSema 			PATHS ${LLVM_HOME}/lib)
find_library(clang_CIndex_LIB 		  	NAMES CIndex 					PATHS ${LLVM_HOME}/lib)
find_library(clang_Driver_LIB 		  	NAMES clangDriver 		PATHS ${LLVM_HOME}/lib)
find_library(clang_AST_LIB 			  		NAMES clangAST 				PATHS ${LLVM_HOME}/lib)
find_library(clang_Rewrite_LIB 		  	NAMES clangRewrite 		PATHS ${LLVM_HOME}/lib)
find_library(clang_Analysis_LIB 	  	NAMES clangAnalysis 	PATHS ${LLVM_HOME}/lib)
find_library(clang_Lex_LIB 			  		NAMES clangLex 				PATHS ${LLVM_HOME}/lib)
find_library(clang_Frontend_LIB 	  	NAMES clangFrontend 	PATHS ${LLVM_HOME}/lib)
find_library(clang_Parse_LIB 		  		NAMES clangParse 			PATHS ${LLVM_HOME}/lib)
find_library(clang_Index_LIB 		  		NAMES clangIndex 			PATHS ${LLVM_HOME}/lib)

set(clang_LIBs  ${clang_CompilerDriver_LIB} ${clang_CIndex_LIB} ${clang_Sema_LIB} ${clang_Driver_LIB} 		
							  ${clang_AST_LIB} ${clang_Rewrite_LIB} ${clang_Analysis_LIB} ${clang_Lex_LIB} 
							 	${clang_Frontend_LIB} ${clang_Parse_LIB} ${clang_Basic_LIB} ${clang_Index_LIB}
   )
				 
find_library(llvm_Core_LIB 			  NAMES LLVMCore 			PATHS ${LLVM_HOME}/lib)
find_library(llvm_MMC_LIB 			  NAMES LLVMMC 				PATHS ${LLVM_HOME}/lib)
find_library(llvm_System_LIB 		  NAMES LLVMSystem 		PATHS ${LLVM_HOME}/lib)
find_library(llvm_Support_LIB 	  NAMES LLVMSupport 	PATHS ${LLVM_HOME}/lib)
find_library(llvm_Core_LIB 		 	  NAMES LLVMCore 			PATHS ${LLVM_HOME}/lib)

set(llvm_LIBs ${llvm_Core_LIB} ${LLVMMC} ${llvm_System_LIB} ${llvm_Support_LIB} ${llvm_Core_LIB}) 

find_library(pthread_LIB pthread)

# The macro below forces the build directory to be different from source directory:
# include(MacroOutOfSourceBuild)
# macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")

# Create a variable called helloworld_SOURCES containing all .cpp files:
file(GLOB insieme_SOURCES 	src/*.cpp )

# Create an executable file called helloworld from sources:
add_executable(insieme ${insieme_SOURCES})

target_link_libraries( insieme ${clang_LIBs} ${llvm_LIBs} ${pthread_LIB} )

