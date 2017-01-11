if(BUILD_TESTS)
	include(googletest)
endif()

if(MSVC)
	include(msvc_source_group)
endif()

macro(add_module_library module)
	set(options HEADER_ONLY C_LINKAGE)
	cmake_parse_arguments(ARG "${options}" "" "" ${ARGN})

	glob_sources(${module}_srcs src)

	if(MSVC OR ARG_HEADER_ONLY)
		glob_headers(${module}_incs include)
		set(${module}_srcs ${${module}_srcs} ${${module}_incs})
	endif()

	add_library(${module} ${${module}_srcs})
	target_include_directories(${module} PUBLIC include)

	# The project name will be prefixed to the output file since your
	# library should be named libinsieme_frontend.so rather than
	# libfrontend.so. This is especially helpful for installing this
	# target.
	set_target_properties(${module} PROPERTIES OUTPUT_NAME ${PROJECT_NAME}_${module})

	if(ARG_C_LINKAGE)
		set_target_properties(${module} PROPERTIES LINKER_LANGUAGE C)
	else()
		set_target_properties(${module} PROPERTIES LINKER_LANGUAGE CXX)
	endif()

	if(MSVC)
		msvc_source_group("Source Files" "${${module}_srcs}" STRIP src)
		msvc_source_group("Header Files" "${${module}_incs}" STRIP include/${PROJECT_NAME}/${module})
		set_target_properties(${module} PROPERTIES FOLDER ${module})
	endif()
endmacro()

macro(add_module_executable module exe)
	get_filename_component(exe_name ${exe} NAME_WE)

	# The target name will be prefixed with the module name so you can have
	# two main executables in different modules.
	add_executable(${module}_${exe_name} ${exe})
	target_link_libraries(${module}_${exe_name} ${module})

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
		# subdirectory list
		get_filename_component(test_dir ${test} DIRECTORY)
		string(CONCAT current_dir ${CMAKE_CURRENT_SOURCE_DIR} "/test")
		string(LENGTH ${current_dir} current_dir_length)
		string(SUBSTRING ${test_dir} ${current_dir_length} -1 test_stripped_dir)
		string(REPLACE "/" "_" test_subdir "${test_stripped_dir}")

		# setup full name
		get_filename_component(test_name ${test} NAME_WE)
		set(test_name "ut_${module}${test_subdir}_${test_name}")

		# build executable
		add_executable(${test_name} ${test})
		target_link_libraries(${test_name} ${module})

		# add gtest
		target_link_libraries(${test_name} gtest)
		target_link_libraries(${test_name} gtest_main)
		target_include_directories(${test_name} SYSTEM PRIVATE ${GTEST_INCLUDE_PATH})

		# gtest requires pthread
		find_package(Threads REQUIRED)
		target_link_libraries(${test_name} ${CMAKE_THREAD_LIBS_INIT})

		# register executable as test
		add_test(NAME ${test_name} COMMAND ${test_name})

		if(MSVC)
			set_target_properties(${test_name} PROPERTIES FOLDER "${module}/Tests")
		endif()
	endif()
endmacro()
