add_module_library(core)

target_link_libraries(core common)
target_link_libraries(core utils)

# Boost
target_link_libraries(core ${Boost_LIBRARIES})
target_include_directories(core SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

# CUDD
target_link_libraries(core ${CUDD_LIBRARIES})
target_include_directories(core SYSTEM PUBLIC ${CUDD_INCLUDE_DIRS})

# Generate IRBuilder interface
# apparently CMake's find_package for Ruby is broken, see https://github.com/NREL/OpenStudio/issues/1271
set(builderbuilder_output_dir ${CMAKE_CURRENT_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${builderbuilder_output_dir})
run_ruby(${CMAKE_CURRENT_SOURCE_DIR}/scripts/builderbuilder.rb ${builderbuilder_output_dir})
file(GLOB_RECURSE builderbuilder_output ${builderbuilder_output_dir}/*)
target_include_directories(core PUBLIC ${builderbuilder_output_dir}/include)
target_sources(core PRIVATE ${builderbuilder_output})

# Generate Inspire Parser
file(GLOB_RECURSE lexer_input src/*.l src/*.ll)
file(GLOB_RECURSE parser_input src/*.y src/*.yy)
if(NOT MSVC)
	find_package(FLEX REQUIRED)
	find_package(BISON REQUIRED)

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
else()
	target_sources(core PRIVATE ${lexer_input} ${parser_input})
	set_target_properties(core PROPERTIES RESOURCE "${lexer_input} ${parser_input}")
	msvc_source_group("Parser Input" "${lexer_input}" STRIP src)
	msvc_source_group("Parser Input" "${parser_input}" STRIP src)
endif()


glob_tests(core_tests test)
foreach(test ${core_tests})
	add_module_unittest(core ${test} OUTPUT_TARGET_NAME test_tgt)

	if(BUILD_TESTS)
		target_include_directories(${test_tgt} PUBLIC test/include)

		if(test_tgt MATCHES "parser")
			target_include_directories(${test_tgt} PRIVATE ${FLEX_INCLUDE_DIRS})
			target_include_directories(${test_tgt} PRIVATE ${parser_output_dir})
		endif()
	endif()
endforeach(test)
