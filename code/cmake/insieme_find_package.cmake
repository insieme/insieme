include(CMakeParseArguments)
#####
#
#	insieme wrapper to find_package
#
#	user has several options to install/find libraries:
#	1 setting ${lib_name_uc}_ROOT (as environment var, or cmake option) to custom libray directory
#	2 having them installed in ${THIRD_PARTY_LIBS_HOME} aka. "~/libs"
#	3 force to install (custom, not system) library ${lib_NAME} to ${THIRD_PARTY_LIBS_HOME} (with ${lib_NAME}_FORCE_INSTALL)
#	4 use the system one (found by cmakes find_package) 
#
#	User can specify certain version of a given library "lib" with "LIB_VERSION" otherwise the
#	version provided in default_library_configuration.cmake is taken
#
#####
macro(insieme_find_package)
	set(options SUPERBUILD)		#if we use this from the superbuild script, disables the fatal_error in
								#case a library is not found -> needed to install it
	set(singleValueArgs NAME) 
	set(multipleValueArgs COMPONENTS) 

	cmake_parse_arguments(lib "${options}" "${singleValueArgs}" "${multipleValueArgs}" ${ARGN})

	#finds the defualt_library_configuratio in the CMAKE Modules path -- needs to be set in before

	#example: Boost offers components as libraries
	#set(lib_COMPONENTS "${ARGN}")
   
	string(TOLOWER ${lib_NAME} lib_name_lc) #lower case
	string(TOUPPER ${lib_NAME} lib_name_uc) #upper case
    
	if( NOT DEFINED ${lib_name_uc}_ROOT)
        #user did not provide a CMAKE variable
        if( DEFINED ENV{${lib_name_uc}_ROOT} )
            #user did specify a custom ${lib_NAME}_ROOT as Environment variable
			set(${lib_name_uc}_ROOT $ENV{${lib_name_uc}_ROOT} CACHE PATH "${lib_NAME} installation directory")
        endif()
    endif()

    if( NOT DEFINED ${${lib_name_uc}_ROOT})
        #user did NOT specify a custom ${lib_NAME}_ROOT via a ENV variable of a CMAKE variable
		#check if path exists: ${THIRD_PARTY_LIBS_HOME}/${lib_NAME}-${${lib_name_uc}_VERSION}
		if(EXISTS "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION}" AND IS_DIRECTORY "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION}")
			set(${lib_name_uc}_ROOT "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION}" CACHE PATH "${lib_name_lc} installation directory")
		elseif(EXISTS "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-latest" AND IS_DIRECTORY "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-latest")
			#if this failed check if path exists: ${THIRD_PARTY_LIBS_HOME}/${lib_NAME}-latest
			message(WARNING "${lib_NAME} was not found in ${lib_name_lc}-${${lib_name_uc}_VERSION} but ${lib_name_lc}-latest was found\nEither update default_library_configuration.cmake or install the correct library version (${lib_name_lc}-${${lib_name_uc}_VERSION}) !")
			set(${lib_name_uc}_ROOT "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-latest" CACHE PATH "${lib_name_lc} installation directory")
		endif()
	endif()

	#we test if we find the requested library (at the suggested lib_ROOT) if we do not find it we install it our own
	find_package(${lib_NAME} ${${lib_name_uc}_VERSION} COMPONENTS ${lib_COMPONENTS})
   
	#we didn't find library at any of the given options:
	#	${lib_NAME}_ROOT
	#	${THIRD_PARTY_LIBS_HOME}/${lib_NAME}
	#	system
	# this means we install it at ${THIRD_PARTY_LIBS_HOME}/${lib_NAME}
	if( NOT (${lib_NAME}_FOUND OR ${lib_name_uc}_FOUND) AND (NOT lib_SUPERBUILD))
		message(FATAL_ERROR "\n\n${lib_NAME} (Version ${${lib_name_uc}_VERSION}) not found \n Either provide :\n - an environment variable ${lib_name_uc}_ROOT pointing to the directory of the library\n - an -D${lib_name_uc}_ROOT argument to cmake call pointing to the directory of the library\n - install it at ${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION}\n ")
			
	endif()
endmacro()
