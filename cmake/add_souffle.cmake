#
# Macro to configure souffle-related variables and paths, as well as
# souffle-code defines, directory creation, etc...
#
macro(configure_souffle)
	
	# TODO find_package(SOUFFLE REQUIRED)
	if(NOT SOUFFLE_ROOT AND DEFINED $ENV{SOUFFLE_ROOT})
		set(SOUFFLE_ROOT "$ENV{SOUFFLE_ROOT}" CACHE PATH "Souffle base directory location")
	endif()
	if(NOT SOUFFLE_ROOT)
		message(WARNING "Souffle NOT found")
	endif()

	# Settings
	set(souffle_tmp_dir       ${CMAKE_CURRENT_BINARY_DIR}/souffle_tmp)
	set(souffle_output_base   ${CMAKE_CURRENT_BINARY_DIR}/souffle_gen)
	set(souffle_output_path   ${souffle_output_base}/souffle/gen)

	# Define preprocessor macro for embedded soufflé
	add_definitions(-D__EMBEDDED_SOUFFLE__)

	# Create tmp and output directory
	file(MAKE_DIRECTORY ${souffle_tmp_dir})
	file(MAKE_DIRECTORY ${souffle_output_path})

	# Generated files will also be included as header files
	# TODO attach to relevant targets
	include_directories(SYSTEM ${souffle_output_base})

	# Find the Dough script, a preprocessor script
	set(souffle_dough ${CMAKE_CURRENT_SOURCE_DIR}/src/cba/datalog/scripts/souffle_dough.rb)

	# Find Soufflé static header files
	set(souffle_header_files ${SOUFFLE_ROOT}/include)
	# TODO attach to relevant targets
	include_directories(SYSTEM ${souffle_header_files})

	# Find the Soufflé binary
	set(souffle_binary ${SOUFFLE_ROOT}/bin/souffle)

endmacro(configure_souffle)



#
# Macro to execute the Dough preprocessor, a script which name-mangles
# the datalog files to provide some sort of namespacing in the dl header files.
#
# Parameters:
# input dir e.g. '${CMAKE_CURRENT_SOURCE_DIR}/src/cba/datalog'
# output dir e.g. '${CMAKE_CURRENT_BINARY_DIR}/souffle_tmp'
#
macro(souffle_run_dough input_dir output_dir)

	# Glob all the dl files to provide correct dependency creation
	file(GLOB_RECURSE raw_datalog_files ${input_dir}/*.dl)

	# Generate "dough output files", i.e. replace the input_dir with the output_dir path
	foreach(analysis_file ${raw_datalog_files})
		string(REPLACE ${input_dir} ${output_dir} next_dough_output_file ${analysis_file})
		set(dough_output_files ${dough_output_files} ${next_dough_output_file})
	endforeach()

	# Execute on given input dir
	# TODO find_package(Ruby)
	add_custom_command(
		COMMAND ruby ${souffle_dough} ${input_dir} ${output_dir}
		WORKING_DIRECTORY ${input_dir}
		DEPENDS ${raw_datalog_files}
		OUTPUT ${dough_output_files}
		COMMENT "Executing Dough, the Soufflé preprocessor"
	)

	# Debug stuff -- uncomment to check if generated file lists are correct
	# message("XXX RAW DATALOG FILES: ${raw_datalog_files}")
	# message("XXX DOUGH OUTPUT FILES: ${dough_output_files}")

endmacro(souffle_run_dough)



#
# Macro to compile a datalog file to a CPP file using the
# Soufflé executable that we built above
#
# Parameters:
# input path e.g.  '${CMAKE_SOURCE_DIR}/code/src/cba/datalog'
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
	# TODO link souffle binaries with rpath
	add_custom_command(
		COMMAND
			${CMAKE_COMMAND} -E env
			"LD_LIBRARY_PATH=${Boost_LIBRARY_DIRS}:$ENV{LD_LIBRARY_PATH}"
			${souffle_binary} -g ${souffle_output_file}.h ${souffle_input_file} ${include_argument}
		WORKING_DIRECTORY ${souffle_output_path}
		SOURCE ${souffle_input_file}
		DEPENDS ${souffle_input_file} ${dough_output_files}
		IMPLICIT_DEPENDS C ${souffle_input_file}
		OUTPUT ${souffle_output_file}.h
		COMMENT "Generating compiled soufflé datalog: ${souffle_dl_target}.h"
	)

	# Add generated file to list of generated files ( = '${souffle_output}' )
	#
	# You have to add '${souffle_output}' to your 'add_library(...)' call
	# later on in order to actually make CMake have a dependency
	set(souffle_output ${souffle_output} ${souffle_output_file}.h)

endmacro(souffle_generate_cpp)
