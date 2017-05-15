macro(file_name_parts filepath strip_dir output_prefix)
	get_filename_component(${output_prefix}_name ${filepath} NAME_WE)

	get_filename_component(${output_prefix}_dir ${filepath} DIRECTORY)
	string(LENGTH ${CMAKE_CURRENT_SOURCE_DIR}/${strip_dir} strip_length)
	string(SUBSTRING ${${output_prefix}_dir} ${strip_length} -1 ${output_prefix}_subdir)

	# move leading slash to back
	if(NOT ${output_prefix}_subdir STREQUAL "")
		string(SUBSTRING ${${output_prefix}_subdir}/ 1 -1 ${output_prefix}_subdir)
	endif()

	string(REPLACE "/" "_" ${output_prefix}_subdir_ "${${output_prefix}_subdir}")

	#message(
	#	"== filepath: ${filepath}"
	#	" | name: ${${output_prefix}_name}"
	#	" | dir: ${${output_prefix}_dir}"
	#	" | subdir: ${${output_prefix}_subdir}"
	#	" | subdir_: ${${output_prefix}_subdir_}"
	#)
endmacro()

macro(add_module_library module)
	set(options HEADER_ONLY EXCLUDE_FROM_ALL)
	cmake_parse_arguments(ARG "${options}" "" "" ${ARGN})

	glob_sources(${module}_srcs src)

	if(MSVC OR ARG_HEADER_ONLY)
		glob_headers(${module}_incs include)
		set(${module}_srcs ${${module}_srcs} ${${module}_incs})
	endif()

	if(ARG_HEADER_ONLY)
		add_library(${module} INTERFACE)
		target_include_directories(${module} INTERFACE include)
		target_sources(${module} INTERFACE ${${module}_srcs})

		if(MSVC)
			add_custom_target(${module}_vs SOURCES ${${module}_srcs})
			set_target_properties(${module}_vs PROPERTIES
				FOLDER ${module}
				PROJECT_LABEL ${module}
			)
		endif()
	else()
		if(ARG_EXCLUDE_FROM_ALL)
			set(library_options EXCLUDE_FROM_ALL)
		endif()

		add_library(${module} ${library_options} ${${module}_srcs})
		target_include_directories(${module} PUBLIC include)

		# The project name will be prefixed to the output file since your
		# library should be named libinsieme_frontend.so rather than
		# libfrontend.so. This is especially helpful for installing this
		# target.
		set_target_properties(${module} PROPERTIES OUTPUT_NAME ${PROJECT_NAME}_${module})

		if(ARG_HEADER_ONLY)
			set_target_properties(${module} PROPERTIES LINKER_LANGUAGE CXX)
		endif()

		if(MSVC)
			set_target_properties(${module} PROPERTIES FOLDER ${module})
		endif()
	endif()

	if(MSVC)
		msvc_source_group("Source Files" "${${module}_srcs}" STRIP src)
		msvc_source_group("Header Files" "${${module}_incs}" STRIP include/${PROJECT_NAME}/${module})
	endif()
endmacro()

macro(add_module_executable module exe)
	set(options NO_LIB EXCLUDE_FROM_ALL)
	set(one_value_args OUTPUT_TARGET_NAME)
	cmake_parse_arguments(ARG "${options}" "${one_value_args}" "" ${ARGN})

	file_name_parts(${exe} src exe)

	# setup target name
	set(exe_tgt ${module}_${exe_subdir_}${exe_name})

	# output generated target name
	if(ARG_OUTPUT_TARGET_NAME)
		set(${ARG_OUTPUT_TARGET_NAME} ${exe_tgt})
	endif()

	if(ARG_EXCLUDE_FROM_ALL)
		set(executable_options EXCLUDE_FROM_ALL)
	endif()

	# The target name is prefixed with the module name so you can have two
	# equally named executables in different modules.
	add_executable(${exe_tgt} ${executable_options} ${exe})

	# link with module library
	if(NOT ARG_NO_LIB)
		target_link_libraries(${exe_tgt} ${module})
	endif()

	# The output name does not contain the module prefix, this is fine
	# inside the build directory since everything is organized in
	# subfolders. Just be aware when installing the executable, it may be
	# overwritten by one with the same output name.
	set_target_properties(${exe_tgt} PROPERTIES OUTPUT_NAME ${exe_subdir}${exe_name})

	# ensure subdir exists
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${exe_subdir})

	if(MSVC)
		set_target_properties(${exe_tgt} PROPERTIES FOLDER ${module})
	endif()
endmacro()

macro(add_module_unittest module test)
	if(BUILD_TESTS)
		set(options NO_VALGRIND PARALLEL NO_LIB)
		set(one_value_args OUTPUT_TARGET_NAME)
		cmake_parse_arguments(ARG "${options}" "${one_value_args}" "" ${ARGN})

		file_name_parts(${test} test test)

		if(USE_VALGRIND AND NOT ARG_NO_VALGRIND)
			set(test_prefix valgrind_)
		endif()

		# setup target name
		set(test_tgt ut_${test_prefix}${module}_${test_subdir_}${test_name})

		# output generated target name
		if(ARG_OUTPUT_TARGET_NAME)
			set(${ARG_OUTPUT_TARGET_NAME} ${test_tgt})
		endif()

		# build executable
		add_executable(${test_tgt} ${test})

		# link with module library
		if(NOT ARG_NO_LIB)
			target_link_libraries(${test_tgt} ${module})
		endif()

		# add gtest
		target_link_libraries(${test_tgt} gtest)
		target_link_libraries(${test_tgt} gtest_main)

		# set command for running the test
		set(test_cmd $<TARGET_FILE:${test_tgt}>)
		if(USE_VALGRIND AND NOT ARG_NO_VALGRIND)
			set(test_cmd ${Valgrind_EXECUTABLE} ${Valgrind_FLAGS} ${test_cmd})
		endif()
		if(ARG_PARALLEL)
			string(REPLACE ";" " " test_cmd "${test_cmd}")
			set(test_cmd ruby ${PROJECT_SOURCE_DIR}/../scripts/gtest/run_parallel.rb -w ${NPROC_HALF} ${test_cmd})
		endif()

		# register test
		add_test(NAME ${test_tgt} COMMAND ${test_cmd} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

		if(MSVC)
			set_target_properties(${test_tgt} PROPERTIES FOLDER ${module}/Tests)
		endif()

		# unit test required for code coverage
		if(BUILD_COVERAGE)
			add_dependencies(coverage ${test_tgt})
		endif()
	endif()
endmacro()
