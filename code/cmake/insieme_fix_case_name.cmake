# utility that encodes the sub-path (starting from CMAKE_CURRENT_SOURCE_DIR) into the name of targets
macro( insieme_fix_case_name output case_file )

        get_filename_component( case_dir ${case_file} DIRECTORY )
        string( CONCAT current_dir ${CMAKE_CURRENT_SOURCE_DIR} "/test" )
        string( LENGTH ${current_dir} current_dir_length)
        string( SUBSTRING ${case_dir} ${current_dir_length} -1 case_stripped_dir )
        string( REPLACE "/" "_" case_stripped_dir "${case_stripped_dir}" )

	set(output ${case_stripped_dir} PARENT_SCOPE )

endmacro( insieme_fix_case_name )
