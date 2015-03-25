# define macro for adding tests
macro ( add_unit_test case_name ut_prefix )

	#check if target for test suite already exists
	if(NOT TARGET ${ut_prefix})
		add_custom_target(${ut_prefix})
	endif()

	add_dependencies(${ut_prefix} ${case_name})

	#rely on CMAKE_MODULE_PATH being set correctly
    include(insieme_find_package)

	# lookup Google Test libraries
	#find_package(GTest REQUIRED)
	insieme_find_package(GTest)

	# add dependency to google test libraries
	target_link_libraries(${case_name} ${GTEST_LIBRARIES})
	target_link_libraries(${case_name} ${GTEST_MAIN_LIBRARIES})

	# lookup pthread library
	find_package(Threads REQUIRED)
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
		# use half the NB_PROCESSORS count to parallelize tests
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
		if ((NOT MSVC) AND USE_VALGRIND)
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
		endif()
	endif()
endmacro(add_unit_test)

