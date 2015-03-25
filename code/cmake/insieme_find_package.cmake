#####
#
#	insieme wrapper to find_package
#
#	user has several options to install/find libraries:
#	1 setting ${lib_name_uc}_ROOT (as environment var, or cmake option) to custom libray directory
#	2 having them installed in ${THIRD_PARTY_LIBS_HOME} aka. "~/libs"
#	3 force to install (custom, not system) library ${lib_name} to ${THIRD_PARTY_LIBS_HOME} (with ${lib_name}_FORCE_INSTALL)
#	4 use the system one (found by cmakes find_package) if this fails we install our custom one at ${THIRD_PARTY_LIBS_HOME}
#
#	User can specify certain version of a given library "lib" with "LIB_VERSION" otherwise the
#	version provided in default_library_configuration.cmake is taken
#
#####
macro(insieme_find_package lib_name )

	#finds the defualt_library_configuratio in the CMAKE Modules path -- needs to be set in before
	include(default_library_configuration)

	#example: Boost offers components as libraries
    set(lib_components "${ARGN}")
   
	string( TOLOWER ${lib_name} lib_name_lc) #lower case
	string( TOUPPER ${lib_name} lib_name_uc) #upper case
    
    #get ${lib_name}_VERSION from library_default_version_file
	if( DEFINED ENV{${lib_name_uc}_VERSION} )
        # overwrite if user specifies otherwise 
        set(${lib_name_uc}_VERSION $ENV{${lib_name_uc}_VERSION})
    endif()
    
	if( NOT DEFINED ${lib_name_uc}_ROOT)
        #user did not provide a CMAKE variable
        if( DEFINED ENV{${lib_name_uc}_ROOT} )
            #user did specify a custom ${lib_name}_ROOT as Environment variable
			set(${lib_name_uc}_ROOT $ENV{${lib_name_uc}_ROOT} CACHE PATH "${lib_name} installation directory")
        endif()
    endif()

 
    if( NOT DEFINED ${${lib_name_uc}_ROOT})
        #user did NOT specify a custom ${lib_name}_ROOT via a ENV variable of a CMAKE variable
		#check if path exists: ${THIRD_PARTY_LIBS_HOME}/${lib_name}-${${lib_name_uc}_VERSION}
		if(EXISTS "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION}" AND IS_DIRECTORY "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION}")
			set(${lib_name_uc}_ROOT "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION}" CACHE PATH "${lib_name_lc} installation directory")
		endif()
	endif()

	#we test if we find the requested library (at the suggested lib_ROOT) if we do not find it we install it our own
	find_package(${lib_name} ${${lib_name_uc}_VERSION} COMPONENTS ${lib_components})
   
	#we didn't find library at any of the given options:
	#	${lib_name}_ROOT
	#	${THIRD_PARTY_LIBS_HOME}/${lib_name}
	#	system
	# this means we install it at ${THIRD_PARTY_LIBS_HOME}/${lib_name}
	if( NOT (${lib_name}_FOUND OR ${lib_name_uc}_FOUND))
        #call install of ${lib_name}
		message(FATAL_ERROR "\n\n${lib_name} (Version ${${lib_name_uc}_VERSION}) not found \n Either provide :\n - an environment variable ${lib_name_uc}_ROOT pointing to the directory of the library\n - an -D${lib_name_uc}_ROOT argument to cmake call pointing to the directory of the library\n - install it at ${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION}\n ")
			
	endif()
endmacro()
