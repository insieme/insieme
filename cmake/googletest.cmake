if(BUILD_TESTS AND NOT TARGET googletest)
	include(ExternalProject)

	# gtest should be build with the same compiler as the project using it
	ExternalProject_Add(
		googletest
		URL http://insieme-compiler.org/ext_libs/gtest-1.8.0.tar.gz
		URL_HASH SHA256=58a6f4277ca2bc8565222b3bbd58a177609e9c488e8a72649359ba51450db7d8
		INSTALL_COMMAND ""
		CMAKE_ARGS ${CMAKE_EXTERNALPROJECT_FORWARDS}
		DOWNLOAD_NO_PROGRESS 1
		EXCLUDE_FROM_ALL 1
	)
	ExternalProject_Get_Property(googletest source_dir binary_dir)

	if(BUILD_SHARED_LIBS)
		set(_prefix ${CMAKE_SHARED_LIBRARY_PREFIX})
		set(_suffix ${CMAKE_SHARED_LIBRARY_SUFFIX})
		set(_linking SHARED)
	else()
		set(_prefix ${CMAKE_STATIC_LIBRARY_PREFIX})
		set(_suffix ${CMAKE_STATIC_LIBRARY_SUFFIX})
		set(_linking STATIC)
	endif()

	set(GTEST_LIBRARY_PATH ${binary_dir}/googlemock/gtest/${LIBRARY_OUTPUT_DIRECTORY})

	# import libgtest
	add_library(gtest ${_linking} IMPORTED)
	set(gtest ${_prefix}gtest${_suffix})
	set_target_properties(gtest PROPERTIES IMPORTED_LOCATION ${GTEST_LIBRARY_PATH}/${gtest})
	add_dependencies(gtest googletest)

	# import libgtest_main
	add_library(gtest_main ${_linking} IMPORTED)
	set(gtest_main ${_prefix}gtest_main${_suffix})
	set_target_properties(gtest_main PROPERTIES IMPORTED_LOCATION ${GTEST_LIBRARY_PATH}/${gtest_main})
	add_dependencies(gtest_main googletest)

	# cannot attach include path to gtest target, must be added manually
	set(GTEST_INCLUDE_PATH ${source_dir}/googletest/include)
endif()
