
macro(configure_souffle)

	# Settings
	set(souffle_tmp_dir       ${CMAKE_CURRENT_BINARY_DIR}/souffle_tmp)
	set(souffle_dough_done    ${souffle_tmp_dir}/preprocessor_done.txt)
	set(souffle_output_base   ${CMAKE_CURRENT_BINARY_DIR}/souffle_gen)
	set(souffle_output_path   ${souffle_output_base}/souffle/gen)

	# Define preprocessor macro for embedded soufflé
	add_definitions(-D__EMBEDDED_SOUFFLE__)

	# Create tmp and output directory
	file(MAKE_DIRECTORY ${souffle_tmp_dir})
	file(MAKE_DIRECTORY ${souffle_output_path})

	# Generated files will also be included as header files
	include_directories(SYSTEM ${souffle_output_base})

	# Find the Dough script, a preprocessor script
	set(souffle_dough ${CMAKE_SOURCE_DIR}/code/analysis/src/souffle_dough.sh)

	# Find Soufflé installation directory
	set(souffle_home $ENV{INSIEME_LIBS_HOME}/souffle-latest CACHE PATH "Souffle Home Directory")	# TODO: use a FindSouffle to locate it

	# Find Soufflé static header files
	set(souffle_header_files ${souffle_home}/include)
	include_directories(SYSTEM ${souffle_header_files})

	# Find the Soufflé binary
	set(souffle_binary ${souffle_home}/bin/souffle)

endmacro(configure_souffle)



macro(souffle_run_dough input_dir output_dir)

	# Glob the dl-files to add them as a dependency
	file(GLOB souffle_original_dls         ${input_dir}/*.dl)
	file(GLOB souffle_original_dl_includes ${input_dir}/include/*.dl)

	# Execute on given input dir
	add_custom_command(
		COMMAND ${souffle_dough}
		ARGS ${input_dir} ${output_dir}
		COMMENT "todo"
		DEPENDS ${souffle_original_dls} ${souffle_original_dl_includes}
		WORKING_DIRECTORY ${input_dir}
		OUTPUT ${souffle_dough_done}
	)

endmacro(souffle_run_dough)



#
# Macro to compile a datalog file to a CPP file using the
# Soufflé executable that we built above
#
# Parameters:
# input path e.g.  '${CMAKE_SOURCE_DIR}/code/src/datalog'
# DL target e.g.  'constant_analysis' (without file extension!)
# souffle_include_path (optional)
#
macro(souffle_generate_cpp souffle_input_path souffle_dl_target souffle_include_path )
	# Convenience variables
	set(souffle_input_file    ${souffle_input_path}/${souffle_dl_target}.dl)
	set(souffle_output_file   ${souffle_output_path}/${souffle_dl_target})

	# add -I to include path
	set(include_argument "${souffle_include_path}")
	if (include_argument)
		set(include_argument "--include-dir=${include_argument}")
		file(GLOB include_dl_dependencies ${souffle_include_path}/*.dl)
	endif()

	# Custom command to compile DL files into CPP files using Soufflé
	add_custom_command(
		SOURCE ${souffle_input_file}
		COMMAND ${souffle_binary}
		ARGS -g ${souffle_output_file}.h ${souffle_input_file} ${include_argument}
		COMMENT "Generating compiled soufflé datalog: ${souffle_dl_target}.h"
		DEPENDS ${souffle_dough_done} ${souffle_input_file} ${include_dl_dependencies}
		IMPLICIT_DEPENDS C ${souffle_input_file}
		WORKING_DIRECTORY ${souffle_output_path}
		OUTPUT ${souffle_output_file}.h
	)

	# Add generated file to list of generated files ( = '${souffle_output}' )
	#
	# You have to add '${souffle_output}' to your 'add_library(...)' call
	# later on in order to actually make CMake have a dependency
	set(souffle_output ${souffle_output} ${souffle_output_file}.h)

endmacro(souffle_generate_cpp)
