
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
			${GTEST_PREFIX}/src/googletest-build/${CMAKE_CFG_INTDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}gtest${CMAKE_STATIC_LIBRARY_SUFFIX})
		set(gtest_main_lib
			${GTEST_PREFIX}/src/googletest-build/${CMAKE_CFG_INTDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}gtest_main${CMAKE_STATIC_LIBRARY_SUFFIX})

		# for now, always dynamically link in Windows
		if(MSVC)
			set(googletest_BUILD_SHARED_LIBS ON)
		endif()

		# NOTE: We need to "forward" the C(XX) compiler _and_ it's first argument here in CMAKE_ARGS in order to compile googletest with the same compiler and settings.
		# This is rather hacky as maybe we should already forward some more variables which we don't know of yet until they might actually be needed and things will break.
		# For now this works and we haven't really found a nicer way to do this.
		ExternalProject_Add(googletest
			URL http://googletest.googlecode.com/files/gtest-${GTEST_VERSION}.zip
			URL_HASH SHA256=247ca18dd83f53deb1328be17e4b1be31514cedfc1e3424f672bf11fd7e0d60d
			PREFIX ${GTEST_PREFIX} 
			INSTALL_COMMAND "" #make gtest gtest_main
			CMAKE_ARGS -DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_C_COMPILER_ARG1=${CMAKE_C_COMPILER_ARG1} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER} -DCMAKE_CXX_COMPILER_ARG1=${CMAKE_CXX_COMPILER_ARG1} -Dgtest_force_shared_crt=${MSVC_SHARED_RUNTIME} -DBUILD_SHARED_LIBS=${googletest_BUILD_SHARED_LIBS}
			BUILD_BYPRODUCTS
				${gtest_lib}
				${gtest_main_lib}
			DOWNLOAD_NO_PROGRESS 1
		)
	
		ExternalProject_Get_Property(googletest source_dir binary_dir)
		set(GTEST_LIBRARY_PATH ${gtest_lib})
		set(GTEST_MAIN_LIBRARY_PATH ${gtest_main_lib})

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
		set(GTEST_LIBRARY_PATH ${binary_dir}/${CMAKE_CFG_INTDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}gtest${CMAKE_STATIC_LIBRARY_SUFFIX})
		set(GTEST_MAIN_LIBRARY_PATH ${binary_dir}/${CMAKE_CFG_INTDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}gtest_main${CMAKE_STATIC_LIBRARY_SUFFIX})
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

	# set USE_VALGRIND=ON as fallback and disable only if asked to do so
	set(USE_VALGRIND ON)

	# check whether there was a optional 2nd argument 
	# which disables use of valgrind for this particular test
	if(${ARGC} GREATER 2)
		# use (optional) 2nd argument as a valgrind flag
		set(USE_VALGRIND ${ARGV2})
		if(NOT ${USE_VALGRIND})
			message(STATUS "Disabling Valgrind for ${case_name}")
		endif()
	endif()

	set(valgrind_cmd "valgrind --leak-check=full --show-reachable=no --track-fds=yes --error-exitcode=1 --track-origins=no")
		#--log-file=${CMAKE_CURRENT_BINARY_DIR}/valgrind.log.${case_name}
		
	# parallelize integration test if required
	function(add_test_conditionally_parallel case_name cmd insieme_root_dir current_binary_dir)
		include(ProcessorCount)
		# use half the NB_PROCESSORS count to parallelize test
		ProcessorCount(NB_PROCESSORS)
		if(NOT NB_PROCESSORS)
			# default = 8 if system query failed
			set(NB_PROCESSORS 8)
		endif(NOT NB_PROCESSORS)
		math(EXPR NB_PROCESSOR_PART "${NB_PROCESSORS} / 2")

		if(${case_name} MATCHES ".*driver_integration.*" OR ${case_name} MATCHES ".*snippets.*")
			add_test(NAME ${case_name} 
			COMMAND ${insieme_root_dir}/code/gtest-parallel.rb 
				-w ${NB_PROCESSOR_PART}
				"${cmd}"
			WORKING_DIRECTORY
				${current_binary_dir}
			)
		else()
			add_test(${case_name} ${case_name})
		endif()
	
	endfunction()

	# add test case
	if(CONDUCT_MEMORY_CHECKS AND USE_VALGRIND)
		# no valgrind support in MSVC 
		if(NOT MSVC)
			# add valgrind as a test
			add_test_conditionally_parallel(valgrind_${case_name} "${valgrind_cmd} ${CMAKE_CURRENT_BINARY_DIR}/${case_name}" ${insieme_root_dir} ${CMAKE_CURRENT_BINARY_DIR})
		endif(NOT MSVC)
	else()
		# add normal test
		add_test_conditionally_parallel(${case_name} "${CMAKE_CURRENT_BINARY_DIR}/${case_name}" ${insieme_root_dir} ${CMAKE_CURRENT_BINARY_DIR})
		# + valgrind as a custom target (only if not explicitly prohibited)
		if(NOT MSVC)
			add_custom_target(valgrind_${case_name}
				COMMAND ${valgrind-cmd}	${CMAKE_CURRENT_BINARY_DIR}/${case_name}
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

