#
# Builds the insieme frontend library
#
project(insieme_frontend CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# collect source files 
file(GLOB insieme_frontend_srcs 	src/*.cpp src/omp/*.cpp src/utils/*.cpp)
file(GLOB insieme_frontend_incs	include/*.h)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/clang_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/clang_config.h)

if(NOT DEFINED LLVM_HOME) 
	set (LLVM_HOME $ENV{LLVM_HOME})
endif()

# add include directory
include_directories( 
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${LLVM_HOME}/include 
	${insieme_utils_include_dir} 
	${insieme_core_include_dir} 
	${insieme_c_info_include_dir} )
	

if(NOT MSVC)
	find_library(clang_CompilerDriver_LIB 	NAMES CompilerDriver 		PATHS ${LLVM_HOME}/lib)
	find_library(clang_Basic_LIB 		  		NAMES clangBasic 				PATHS ${LLVM_HOME}/lib)
	find_library(clang_Checker_LIB 		 	NAMES clangChecker 			PATHS ${LLVM_HOME}/lib)
	find_library(clang_Sema_LIB 		  		NAMES clangSema 				PATHS ${LLVM_HOME}/lib)
	find_library(clang_clang_LIB 		  		NAMES clang 					PATHS ${LLVM_HOME}/lib)
	find_library(clang_Index_LIB 		  		NAMES clangIndex				PATHS ${LLVM_HOME}/lib)
	find_library(clang_Driver_LIB 		  	NAMES clangDriver 			PATHS ${LLVM_HOME}/lib)
	find_library(clang_AST_LIB 			  	NAMES clangAST 				PATHS ${LLVM_HOME}/lib)
	find_library(clang_Rewrite_LIB 		  	NAMES clangRewrite 			PATHS ${LLVM_HOME}/lib)
	find_library(clang_Analysis_LIB 	  		NAMES clangAnalysis 			PATHS ${LLVM_HOME}/lib)
	find_library(clang_Lex_LIB 			  	NAMES clangLex 				PATHS ${LLVM_HOME}/lib)
	find_library(clang_Frontend_LIB 	  		NAMES clangFrontend 			PATHS ${LLVM_HOME}/lib)
	find_library(clang_FrontendTool_LIB 	NAMES clangFrontendTool 	PATHS ${LLVM_HOME}/lib)
	find_library(clang_Parse_LIB 		  		NAMES clangParse 				PATHS ${LLVM_HOME}/lib)
	find_library(clang_Serialization_LIB   NAMES clangSerialization 	PATHS ${LLVM_HOME}/lib)
	
	set(clang_LIBs    ${clang_clang_LIB}
							${clang_Index_LIB} 
							${clang_Checker_LIB}
							${clang_FrontendTool_LIB}	
							${clang_Frontend_LIB}  
							${clang_Driver_LIB}  
							${clang_Basic_LIB}
							${clang_Lex_LIB} 		
							${clang_Parse_LIB}  
							${clang_Sema_LIB} 
							${clang_AST_LIB}
						 	${clang_Rewrite_LIB}   
						 	${clang_Serialization_LIB}
						 	${clang_Analysis_LIB} 	
						 	${clang_Basic_LIB}
	)
	
	find_library(llvm_LIB   NAMES LLVM-2.8rc 	PATHS ${LLVM_HOME}/lib)
						 
else()
	set(clang_LIBs )
endif() 

# build the frontend library
add_library(insieme_frontend SHARED ${insieme_frontend_srcs} ${insieme_frontend_incs} )
target_link_libraries(insieme_frontend insieme_core ${clang_LIBs} ${pthread_LIB})

# add dependencies
add_dependencies( insieme_frontend insieme_core insieme_utils insieme_c_info )

# enable tests for this project
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

file(GLOB test_cases test/*.cc)
foreach ( case_file ${test_cases})
 	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_executable(ut_${case_name} ${case_file})
	include_directories(ut_${case_name} ${insieme_utils_include_dir})
	target_link_libraries(ut_${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(ut_${case_name} insieme_frontend)
	target_link_libraries(ut_${case_name} insieme_core)
	target_link_libraries(ut_${case_name} insieme_utils)
	target_link_libraries(ut_${case_name} insieme_c_info)
	target_link_libraries(ut_${case_name} boost_program_options)
	target_link_libraries(ut_${case_name} ${clang_LIBs})
	target_link_libraries(ut_${case_name} ${glog_LIB})
	target_link_libraries(ut_${case_name} ${pthread_LIB})
	target_link_libraries(ut_${case_name} ${llvm_LIB})
	
	set(CONDUCT_MEMORY_CHECKS off)
	
	add_unit_test(${case_name})	
endforeach(case_file)
 