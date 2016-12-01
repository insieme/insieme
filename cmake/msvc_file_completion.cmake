# requires file_globs

macro(msvc_header_completion target_name input_dir)
	if(MSVC)
		glob_headers(_files ${input_dir})
		add_custom_target(${target_name} SOURCES ${_files})
	endif()
endmacro()
