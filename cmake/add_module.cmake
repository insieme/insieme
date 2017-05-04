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
		set(library_options "")
		if(ARG_EXCLUDE_FROM_ALL)
			set(library_options "EXCLUDE_FROM_ALL")
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
	cmake_parse_arguments(ARG "${options}" "" "" ${ARGN})

	get_filename_component(exe_name ${exe} NAME_WE)

	set(executable_options "")
	if(ARG_EXCLUDE_FROM_ALL)
		set(executable_options "EXCLUDE_FROM_ALL")
	endif()

	# The target name will be prefixed with the module name so you can have
	# two main executables in different modules.
	add_executable(${module}_${exe_name} ${executable_options} ${exe})

	# link with module library
	if(NOT ARG_NO_LIB)
		target_link_libraries(${module}_${exe_name} ${module})
	endif()

	# But the output name will not contain the module prefix, this is fine
	# inside the build directory since everything is organized in
	# subfolders. Just be aware when installing the executable, it may be
	# overwritten by one with the same output name.
	set_target_properties(${module}_${exe_name} PROPERTIES OUTPUT_NAME ${exe_name})

	if(MSVC)
		set_target_properties(${module}_${exe_name} PROPERTIES FOLDER ${module})
	endif()
endmacro()

macro(add_module_unittest module test)
	if(BUILD_TESTS)
		set(options NO_VALGRIND PARALLEL NO_LIB)
		set(one_value_args OUTPUT_TARGET_NAME)
		cmake_parse_arguments(ARG "${options}" "${one_value_args}" "" ${ARGN})

		# subdirectory list
		get_filename_component(test_dir ${test} DIRECTORY)
		string(CONCAT current_dir ${CMAKE_CURRENT_SOURCE_DIR} "/test")
		string(LENGTH ${current_dir} current_dir_length)
		string(SUBSTRING ${test_dir} ${current_dir_length} -1 test_stripped_dir)
		string(REPLACE "/" "_" test_subdir "${test_stripped_dir}")

		if(USE_VALGRIND AND NOT ARG_NO_VALGRIND)
			set(test_prefix "valgrind_")
		endif()

		# setup full name
		get_filename_component(test_name ${test} NAME_WE)
		set(test_name "ut_${test_prefix}${module}${test_subdir}_${test_name}")

		# output generated target name
		if(ARG_OUTPUT_TARGET_NAME)
			set(${ARG_OUTPUT_TARGET_NAME} ${test_name})
		endif()

		# build executable
		add_executable(${test_name} ${test})

		# link with module library
		if(NOT ARG_NO_LIB)
			target_link_libraries(${test_name} ${module})
		endif()

		# add gtest
		target_link_libraries(${test_name} gtest)
		target_link_libraries(${test_name} gtest_main)

		# set command for running the test
		set(test_cmd ${CMAKE_CURRENT_BINARY_DIR}/${test_name})
		if(USE_VALGRIND AND NOT ARG_NO_VALGRIND)
			set(test_cmd ${Valgrind_EXECUTABLE} ${Valgrind_FLAGS} ${test_cmd})
		endif()
		if(ARG_PARALLEL)
			string(REPLACE ";" " " test_cmd "${test_cmd}")
			set(test_cmd ruby ${PROJECT_SOURCE_DIR}/../scripts/gtest/run_parallel.rb -w ${NPROC_HALF} ${test_cmd})
		endif()

		# register test
		add_test(NAME ${test_name} COMMAND ${test_cmd} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

		if(MSVC)
			set_target_properties(${test_name} PROPERTIES FOLDER "${module}/Tests")
		endif()

		# unit test required for code coverage
		if(BUILD_COVERAGE)
			add_dependencies(coverage ${test_name})
		endif()
	endif()
endmacro()
