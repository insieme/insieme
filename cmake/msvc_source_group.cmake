macro(msvc_source_group label files)
	set(one_value_args STRIP)
	cmake_parse_arguments(ARG "" "${one_value_args}" "" ${ARGN})

	foreach(file ${files})
		get_filename_component(file_dir ${file} DIRECTORY)

		# strip
		string(LENGTH ${CMAKE_CURRENT_SOURCE_DIR}/${ARG_STRIP} strip_dir_length)
		string(SUBSTRING ${file_dir} ${strip_dir_length} -1 file_dir)

		string(REPLACE "/" "\\" file_dir "${file_dir}")

		source_group("${label}\\${file_dir}" FILES ${file})
	endforeach(file)
endmacro()
