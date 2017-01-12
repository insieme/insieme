add_module_library(core)

target_link_libraries(core common)
target_link_libraries(core utils)

# Boost
find_package(Boost ${Boost_VERSION} EXACT REQUIRED COMPONENTS filesystem system)
target_link_libraries(core ${Boost_LIBRARIES})
target_include_directories(core SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

# CUDD
find_package(CUDD ${CUDD_VERSION} EXACT REQUIRED)
target_link_libraries(core ${CUDD_LIBRARIES})
target_include_directories(core SYSTEM PUBLIC ${CUDD_INCLUDE_DIRS})

# Generate IRBuilder interface
# apparently CMake's find_package for Ruby is broken, see https://github.com/NREL/OpenStudio/issues/1271
set(builderbuilder_output_dir ${CMAKE_CURRENT_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${builderbuilder_output_dir})
execute_process(
	COMMAND ruby ${CMAKE_CURRENT_SOURCE_DIR}/scripts/builderbuilder.rb ${builderbuilder_output_dir}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	RESULT_VARIABLE builderbuilder_result
	OUTPUT_VARIABLE builderbuilder_stdout
	ERROR_VARIABLE  builderbuilder_stdout
)
if(NOT ${builderbuilder_result} EQUAL 0)
	message(FATAL_ERROR "Ruby builder builder script failed to execute:\n${builderbuilder_stdout}")
endif()
file(GLOB_RECURSE builderbuilder_output ${builderbuilder_output_dir}/*)
target_include_directories(core PUBLIC ${builderbuilder_output_dir}/include)
target_sources(core PRIVATE ${builderbuilder_output})

# Generate Inspire Parser
if(NOT MSVC)
	find_package(FLEX REQUIRED)
	find_package(BISON REQUIRED)

	file(GLOB_RECURSE lexer_input src/*.l src/*.ll)
	file(GLOB_RECURSE parser_input src/*.y src/*.yy)

	set(parser_output_dir ${CMAKE_CURRENT_BINARY_DIR}/inspire_parser)

	set(lexer_output ${parser_output_dir}/inspire_scanner.cpp)
	set(parser_output ${parser_output_dir}/inspire_parser.cpp)

	file(MAKE_DIRECTORY ${parser_output_dir})

	add_custom_command(
		COMMAND ${FLEX_EXECUTABLE} -o ${lexer_output} ${lexer_input}
		DEPENDS ${lexer_input}
		OUTPUT ${lexer_output}
	)

	add_custom_command(
		COMMAND ${BISON_EXECUTABLE} -d -v --report=all --graph -o ${parser_output} ${parser_input}
		DEPENDS ${parser_input}
		OUTPUT ${parser_output}
	)

	target_include_directories(core PRIVATE ${FLEX_INCLUDE_DIRS})
	target_include_directories(core PRIVATE ${parser_output_dir})
	target_sources(core PRIVATE ${lexer_output} ${parser_output})
endif()


glob_tests(core_tests test)
foreach(test ${core_tests})
	add_module_unittest(core ${test} TARGET_NAME tgt_name)

	if(BUILD_TESTS)
		target_include_directories(${tgt_name} PUBLIC test/include)

		if(tgt_name MATCHES ".*parser.*")
			target_include_directories(${tgt_name} PRIVATE ${FLEX_INCLUDE_DIRS})
			target_include_directories(${tgt_name} PRIVATE ${parser_output_dir})
		endif()
	endif()
endforeach(test)
