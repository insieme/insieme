# This macro adds a custom target in case a Visual Solution is generated. Files
# with the given extension(s) are searched in `input_dir` and added as sources
# to `target_name`.
#
# Example: msvc_file_completion(my_target include h hpp def inc)

macro(msvc_file_completion target_name input_dir)
	if(MSVC)
		foreach(ext IN LIST ${argn})
			list(APPEND _globs "${input_dir}/*.${ext}")
		endforeach(ext)
		file(GLOB_RECURSE _files ${_globs})
		add_custom_target(${target_name} SOURCES ${_files})
	endif()
endmacro()
