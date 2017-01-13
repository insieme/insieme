# apparently CMake's find_package for Ruby is broken, see
# https://github.com/NREL/OpenStudio/issues/1271, therefore we assume Ruby is
# available in PATH

macro(run_ruby)
	execute_process(
		COMMAND ruby ${ARGN}
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
		RESULT_VARIABLE result
		OUTPUT_VARIABLE output
		ERROR_VARIABLE output
	)
	if(NOT ${result} EQUAL 0)
		string(REPLACE ";" " " args "${ARGN}")
		message(FATAL_ERROR "Failed to execute `ruby ${args}`:\n${output}")
	endif()
endmacro()
