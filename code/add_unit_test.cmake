# define macro for adding tests
macro ( add_unit_test case_name )

	# add dependency to google test libraries
	target_link_libraries(${case_name} ${gtest_LIB})
	target_link_libraries(${case_name} ${gtest_main_LIB})

	# take value from environment variable
	set(USE_VALGRIND ${CONDUCT_MEMORY_CHECKS})

	# check whether there was a 2nd argument
	if(${ARGC} GREATER 1)
		# use last argument as a valgrind flag
		set(USE_VALGRIND ${ARG2})
	endif(${ARGC} GREATER 1)
	
	# add test case
	if(USE_VALGRIND)
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
	else(USE_VALGRIND)
		# use half the NB_PROCESSORS count to parallelize tests
		if(NOT NB_PROCESSORS)
			# default = 8 if system query failed
			set(NB_PROCESSORS 8)
		endif(NOT NB_PROCESSORS)
		math(EXPR NB_PROCESSOR_PART "${NB_PROCESSORS} / 4")
		
		# add normal test
		# parallelize integration tests
		if(${case_name} MATCHES ".*integration.*")
		add_test(NAME ${case_name} 
			COMMAND ${CMAKE_SOURCE_DIR}/code/gtest-parallel.py 
				-w ${NB_PROCESSOR_PART}
				${CMAKE_CURRENT_BINARY_DIR}/${case_name}
			WORKING_DIRECTORY
				${CMAKE_CURRENT_BINARY_DIR}
			)
		else()
			add_test(${case_name} ${case_name})
		endif(${case_name} MATCHES ".*integration.*")

		# + valgrind as a custom target (only if not explicitly prohibited)
		if ((NOT MSVC) AND ((NOT (${ARGC} GREATER 1)) OR (${ARG2})))
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
		endif ((NOT MSVC) AND ((NOT (${ARGC} GREATER 1)) OR (${ARG2})))
	endif(USE_VALGRIND)
endmacro(add_unit_test)

