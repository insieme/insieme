# define macro for adding tests
macro ( add_unit_test case_name ut_prefix )

	#check if target for test suite already exists
	if(NOT TARGET ${ut_prefix})
		add_custom_target(${ut_prefix})
	endif()

	add_dependencies(${ut_prefix} ${case_name})

	#rely on CMAKE_MODULE_PATH being set correctly
    include(insieme_find_package)

	# lookup pthread library
	find_package(Threads REQUIRED)

	#With CMAKE 3.2 external projets support byproducts -> this is needed to support ninja as
	#generator
	if(NOT TARGET googletest) 
		include(ExternalProject)
		set(GTEST_PREFIX ${CMAKE_BINARY_DIR}/ep-gtest-${GTEST_VERSION})
		#set(GTEST_PREFIX ${THIRD_PARTY_LIBS_HOME}/ep-gtest-${GTEST_VERSION}/)
		#ugly but necessary, in future versions one can use ${BINARY_DIR} in BUILD_BYPRODUCTS
		set(gtest_lib
			${GTEST_PREFIX}/src/googletest-build/libgtest.a)
		set(gtest_main_lib
			${GTEST_PREFIX}/src/googletest-build/libgtest_main.a)

		ExternalProject_Add(googletest
			URL http://googletest.googlecode.com/files/gtest-${GTEST_VERSION}.zip
			PREFIX ${GTEST_PREFIX} 
			INSTALL_COMMAND "" #make gtest gtest_main
			CMAKE_ARGS -DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}
			BUILD_BYPRODUCTS
				${gtest_lib}
				${gtest_main_lib}
			DOWNLOAD_NO_PROGRESS 1
		)
	
		ExternalProject_Get_Property(googletest source_dir binary_dir)
		set(GTEST_LIBRARY_PATH ${binary_dir}/${CMAKE_FIND_LIBRARY_PREFIXES}gtest.a)
		set(GTEST_MAIN_LIBRARY_PATH ${binary_dir}/${CMAKE_FIND_LIBRARY_PREFIXES}gtest_main.a)

		set(GTEST_LIBRARY gtest)
		set(GTEST_MAIN_LIBRARY gtest_main)
		add_library(${GTEST_LIBRARY} STATIC IMPORTED)
		add_library(${GTEST_MAIN_LIBRARY} STATIC IMPORTED)
		set_property(TARGET ${GTEST_LIBRARY} PROPERTY IMPORTED_LOCATION ${GTEST_LIBRARY_PATH})
		set_property(TARGET ${GTEST_MAIN_LIBRARY} PROPERTY IMPORTED_LOCATION ${GTEST_MAIN_LIBRARY_PATH})
		add_dependencies(${GTEST_LIBRARY} googletest) 
		add_dependencies(${GTEST_MAIN_LIBRARY} googletest) 
	else()
		ExternalProject_Get_Property(googletest source_dir binary_dir)
		set(GTEST_LIBRARY_PATH ${binary_dir}/${CMAKE_FIND_LIBRARY_PREFIXES}gtest.a)
		set(GTEST_MAIN_LIBRARY_PATH ${binary_dir}/${CMAKE_FIND_LIBRARY_PREFIXES}gtest_main.a)
		#set(GTEST_LIBRARY gtest)
		#set(GTEST_MAIN_LIBRARY gtest_main)
	endif()

	add_dependencies(${case_name} googletest)
	target_include_directories(${case_name} SYSTEM PRIVATE ${source_dir}/include)
	# add dependency to google test libraries
	target_link_libraries(${case_name} ${GTEST_LIBRARY_PATH})
	target_link_libraries(${case_name} ${GTEST_MAIN_LIBRARY_PATH})

	# add dependency to pthread (TODO check gtest if depends on pthread?)
	target_link_libraries(${case_name} ${CMAKE_THREAD_LIBS_INIT})

	# take value from environment variable
	set(USE_VALGRIND ON)

	# check whether there was a optional 2nd argument 
	# which disables use of valgrind for this particular test
	if(${ARGC} GREATER 1)
		# use (optional) 2nd argument as a valgrind flag
		set(USE_VALGRIND ${ARGV2})
		if(NOT ${USE_VALGRIND})
			message(STATUS "Disabling Valgrind for ${case_name}")
		endif()
	endif()
	
	# add test case
	if(CONDUCT_MEMORY_CHECKS AND USE_VALGRIND)
		# no valgrind support in MSVC 
		if(NOT MSVC)
			# add valgrind as a test
			add_test(NAME valgrind_${case_name} 
				COMMAND valgrind
					--leak-check=full
					--show-reachable=no
					--track-fds=yes
					--error-exitcode=1
					--track-origins=no
					#--log-file=${CMAKE_CURRENT_BINARY_DIR}/valgrind.log.${case_name}
					${CMAKE_CURRENT_BINARY_DIR}/${case_name}
				WORKING_DIRECTORY
					${CMAKE_CURRENT_BINARY_DIR}
			)
		endif(NOT MSVC)
	else()
		include(ProcessorCount)
		# use half the NB_PROCESSORS count to parallelize tests
		ProcessorCount(NB_PROCESSORS)
		if(NOT NB_PROCESSORS)
			# default = 8 if system query failed
			set(NB_PROCESSORS 8)
		endif(NOT NB_PROCESSORS)
		math(EXPR NB_PROCESSOR_PART "${NB_PROCESSORS} / 4")
		
		# add normal test
		# parallelize integration tests
		if(${case_name} MATCHES ".*driver_integration.*")
		add_test(NAME ${case_name} 
			COMMAND ${insieme_root_dir}/code/gtest-parallel.rb 
				-w ${NB_PROCESSOR_PART}
				${CMAKE_CURRENT_BINARY_DIR}/${case_name}
			WORKING_DIRECTORY
				${CMAKE_CURRENT_BINARY_DIR}
			)
		else()
			add_test(${case_name} ${case_name})
		endif(${case_name} MATCHES ".*driver_integration.*")

		# + valgrind as a custom target (only if not explicitly prohibited)
		if ((NOT MSVC))
			add_custom_target(valgrind_${case_name} 
				COMMAND valgrind
					--leak-check=full
					--show-reachable=no
					--track-fds=yes
					--error-exitcode=1
					--track-origins=no
					#--log-file=${CMAKE_CURRENT_BINARY_DIR}/valgrind.log.${case_name}
					${CMAKE_CURRENT_BINARY_DIR}/${case_name}
				WORKING_DIRECTORY
					${CMAKE_CURRENT_BINARY_DIR}
			)
			add_dependencies(valgrind valgrind_${case_name})
			add_dependencies(valgrind_${case_name} ${case_name})

			#check if target for test suite already exists
			if(NOT TARGET valgrind_${ut_prefix})
				add_custom_target(valgrind_${ut_prefix})
			endif()

			add_dependencies(valgrind_${ut_prefix} valgrind_${case_name})
		endif()
	endif()
endmacro(add_unit_test)

