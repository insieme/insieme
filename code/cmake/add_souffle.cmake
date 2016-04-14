#
# Macro to build a Soufflé executable
#
macro(build_souffle)

	# Settings
	set(souffle_prefix        ${CMAKE_BINARY_DIR}/ep-souffle)
	set(souffle_output_base   ${CMAKE_BINARY_DIR}/souffle_gen)
	set(souffle_output_path   ${souffle_output_base}/souffle/gen)

	# Create output dir
	file(MAKE_DIRECTORY ${souffle_output_path})

	# Load SSL libraries needed for HTTPS
	find_package(PkgConfig REQUIRED)
	pkg_search_module(OPENSSL REQUIRED openssl)

	message(STATUS "Using OpenSSL ${OPENSSL_VERSION}")
	include_directories(${OPENSSL_INCLUDE_DIRS})

	# Download + build instructions for Soufflé
	include(ExternalProject)

	ExternalProject_Add(
		souffle
		URL https://github.com/souffle-lang/souffle/archive/master.zip
		PATCH_COMMAND ./bootstrap
		CONFIGURE_COMMAND ${souffle_prefix}/src/souffle/configure
		BUILD_COMMAND make
		INSTALL_COMMAND ""
		PREFIX ${souffle_prefix}
		DOWNLOAD_NO_PROGRESS 1
	)

	# Add Soufflé header files directory to include paths
	set(souffle_header_files ${souffle_prefix}/src/souffle-build/include)
	include_directories(SYSTEM ${souffle_header_files})

	# Generated files will also be included as header files
	include_directories(SYSTEM ${souffle_output_base})

	# Define preprocessor macro for embedded soufflé
	add_definitions(-D__EMBEDDED_SOUFFLE__)

endmacro(build_souffle)



#
# Macro to compile a datalog file to a CPP file using the
# Soufflé executable that we built above
#
# Parameters:
# input path e.g.  '${CMAKE_SOURCE_DIR}/code/src/datalog'
# DL target e.g.  'constant_analysis' (without file extension!)
#
macro(souffle_generate_cpp souffle_input_path souffle_dl_target)
	# Convenience variables
	set(souffle_input_file    ${souffle_input_path}/${souffle_dl_target}.dl)
	set(souffle_output_file   ${souffle_output_path}/${souffle_dl_target})

	# Get path for soufflé that we built before
	ExternalProject_Get_Property(souffle binary_dir)

	# Custom command to compile DL files into CPP files using Soufflé
	add_custom_command(
		SOURCE ${souffle_input_file}
		COMMAND ${binary_dir}/src/souffle
		ARGS -g ${souffle_output_file}.h ${souffle_input_file}
		COMMENT "Generating compiled soufflé datalog"
		DEPENDS souffle ${souffle_input_file}
		WORKING_DIRECTORY ${souffle_output_path}
		OUTPUT ${souffle_output_file}.h
	)

	# Add generated file to list of generated files ( = '${souffle_output}' )
	#
	# You have to add '${souffle_output}' to your 'add_library(...)' call
	# later on in order to actually make CMake have a dependency
	set(souffle_output ${souffle_output} ${souffle_output_file}.h)

endmacro(souffle_generate_cpp)
