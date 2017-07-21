find_program(CABAL_EXECUTABLE cabal
  PATHS "${CABAL_ROOT}/bin"
  DOC "path to the cabal executable"
)

if(CABAL_EXECUTABLE)
  execute_process(COMMAND ${CABAL_EXECUTABLE} --numeric-version
    OUTPUT_VARIABLE CABAL_version_output
    ERROR_VARIABLE CABAL_version_error
    RESULT_VARIABLE CABAL_version_result
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  if(NOT ${CABAL_version_result} EQUAL 0)
    if(CABAL_FIND_REQUIRED)
      message(SEND_ERROR "Command \"${CABAL_EXECUTABLE} --numeric-version\" failed with output:\n${CABAL_version_output}\n${CABAL_version_error}")
    else()
      message("Command \"${CABAL_EXECUTABLE} --version\" failed with output:\n${CABAL_version_output}\n${CABAL_version_error}\nCABAL_VERSION will not be available")
    endif()
  else()
	set(CABAL_VERSION ${CABAL_version_output})
  endif()

  include(FindPackageHandleStandardArgs)
  FIND_PACKAGE_HANDLE_STANDARD_ARGS(CABAL REQUIRED_VARS CABAL_EXECUTABLE
    VERSION_VAR CABAL_VERSION)
endif()