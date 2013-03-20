# a macro for looking up library dependencies
macro ( lookup_lib lib_name lib_file_name)

	string( TOLOWER ${lib_name} lib_name_lower_case )

	if (DEFINED ${lib_name}_HOME)
		set (CUR_HOME ${${lib_name}_HOME})
	else()
		if( DEFINED ENV{${lib_name}_HOME} )
			set (CUR_HOME $ENV{${lib_name}_HOME})
		else()
			if( DEFINED third_part_libs_home )
				set (CUR_HOME ${third_part_libs_home}/${lib_name_lower_case}-latest)
			else()
				message(FATAL_ERROR "No path to ${lib_name} set!")
			endif()
		endif()
	endif()
	
	set ( ${lib_name}_HOME ${CUR_HOME} ) # CACHE PATH "Home of ${lib_name} library" )

	include_directories( SYSTEM ${${lib_name}_HOME}/include )

	if(MSVC) 
		set (${lib_name_lower_case}_LIB dummy)
	else()
		find_library(${lib_name_lower_case}_LIB NAMES ${lib_file_name} HINTS ${${lib_name}_HOME} ${${lib_name}_HOME}/lib)

		if ( ${${lib_name_lower_case}_LIB} STREQUAL "${lib_name_lower_case}_LIB-NOTFOUND" ) 
			message(FATAL_ERROR "Required third-party library ${lib_name} not found!")
		#else()
		#	message( "Found third-party library ${lib_name} at ${${lib_name_lower_case}_LIB}" )
		endif()

	endif(MSVC)			

endmacro(lookup_lib)

