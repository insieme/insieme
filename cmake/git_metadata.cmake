find_package(Git)

if(GIT_FOUND)
	# query reference of current commit
	execute_process(
		COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
		WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
		OUTPUT_VARIABLE GIT_REF
	)

	execute_process(
		COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
		WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
		RESULT_VARIABLE _cmd_result
		OUTPUT_VARIABLE GIT_BRANCH
	)

	# sanitize output variables
	string(REPLACE "\n" "" GIT_REF ${GIT_REF})
	string(REPLACE " "  "" GIT_REF ${GIT_REF})
	string(REPLACE "\n" "" GIT_BRANCH ${GIT_BRANCH})
	string(REPLACE " "  "" GIT_BRANCH ${GIT_BRANCH})
endif()

if(NOT GIT_FOUND OR NOT ${_cmd_result} EQUAL 0)
	set(GIT_REF "unknown")
	set(GIT_BRANCH "unknown")
endif()
