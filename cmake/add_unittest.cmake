if(BUILD_TESTS)
	include(googletest)
endif()

macro(add_unittest module test)
	if(BUILD_TESTS)
		# subdirectory list
		get_filename_component(test_dir ${test} DIRECTORY)
		string(CONCAT current_dir ${CMAKE_CURRENT_SOURCE_DIR} "/test")
		string(LENGTH ${current_dir} current_dir_length)
		string(SUBSTRING ${test_dir} ${current_dir_length} -1 test_stripped_dir)
		string(REPLACE "/" "_" test_subdir "${test_stripped_dir}")

		# setup full name
		get_filename_component(test_name ${test} NAME_WE)
		set(test_name "ut_${module}_${test_subdir}${test_name}")

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
	endif()
endmacro()
