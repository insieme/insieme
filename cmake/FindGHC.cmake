find_program(GHC_EXECUTABLE ghc DOC "path to the ghc executable")

if(GHC_EXECUTABLE)
  execute_process(COMMAND ${GHC_EXECUTABLE} --numeric-version
    OUTPUT_VARIABLE GHC_version_output
    ERROR_VARIABLE GHC_version_error
    RESULT_VARIABLE GHC_version_result
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  if(NOT ${GHC_version_result} EQUAL 0)
    if(GHC_FIND_REQUIRED)
      message(SEND_ERROR "Command \"${GHC_EXECUTABLE} --numeric-version\" failed with output:\n${GHC_version_output}\n${GHC_version_error}")
    else()
      message("Command \"${GHC_EXECUTABLE} --version\" failed with output:\n${GHC_version_output}\n${GHC_version_error}\nGHC_VERSION will not be available")
    endif()
  else()
	set(GHC_VERSION ${GHC_version_output})
  endif()
endif()

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(GHC
	REQUIRED_VARS GHC_EXECUTABLE
    VERSION_VAR GHC_VERSION)
