macro(msvc_source_group label files)
	if(MSVC)
		set(one_value_args STRIP STRIP_ABS)
		cmake_parse_arguments(ARG "" "${one_value_args}" "" ${ARGN})

		foreach(file ${files})
			message("== ${file} | ${ARG_STRIP}")
			get_filename_component(file_dir ${file} DIRECTORY)

			# strip
			if(STRIP_ABS)
				string(LENGTH ${ARG_STRIP_ABS} strip_dir_length)
			else()
				string(LENGTH ${CMAKE_CURRENT_SOURCE_DIR}/${ARG_STRIP} strip_dir_length)
			endif()
			string(SUBSTRING ${file_dir} ${strip_dir_length} -1 file_dir)

			string(REPLACE "/" "\\" file_dir "${file_dir}")

			source_group("${label}\\${file_dir}" FILES ${file})
		endforeach(file)
	endif()
endmacro()
