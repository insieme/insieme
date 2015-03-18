#####
#
#	insiem wrapper to find_pakckage
#
#	user has several options to install/find libraries:
#	1 setting ${lib_name}_ROOT (as environment var, or cmake option) to custom libray directory
#	2 having them installed in ${THIRD_PARTY_LIBS_HOME} aka. "~/libs"
#	3 force to install (custom, not system) library ${lib_name} to ${THIRD_PARTY_LIBS_HOME} (with ${lib_name}_FORCE_INSTALL)
#	4 use the system one (found by cmakes find_package) if this fails we install our custom one at ${THIRD_PARTY_LIBS_HOME}
#
#####
macro(insieme_find_package lib_name )

	#example: Boost offers components as libraries
    set(lib_components "${ARGN}")
    
	string( TOLOWER ${lib_name} lib_name_lc) #lower case
	string( TOUPPER ${lib_name} lib_name_uc) #upper case
    option(${lib_name}_FORCE_INSTALL "Force to install ${lib_name} into ${THIRD_PARTY_LIBS_HOME}" OFF)
    
	#include(../cmake/default_library_configuration.cmake)
    
    #get ${lib_name}_VERSION from library_default_version_file
	if( DEFINED ENV{${lib_name_uc}_VERSION} )
        # overwrite if user specifies otherwise 
        set(${lib_name}_VERSION $ENV{lib_name_uc}_VERSION)
    endif()
    
	if( NOT DEFINED ${${lib_name}_ROOT})
        #user did not provide a CMAKE variable
        if( DEFINED ENV{${lib_name_uc}_ROOT} )
            #user did specify a custom ${lib_name}_ROOT as Environment variable
			set(${lib_name_uc}_ROOT $ENV{${lib_name_uc}_ROOT} CACHE PATH "${lib_name} installation directory")
        endif()
    endif()
 
    if( NOT DEFINED ${${lib_name}_ROOT})
        #user did NOT specify a custom ${lib_name}_ROOT via a ENV variable of a CMAKE variable
        #check if path exists: ${THIRD_PARTY_LIBS_HOME}/${lib_name}
        if(EXISTS "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-latest" AND IS_DIRECTORY "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-latest")
            #set lib_name_ROOT to found path
			set(${lib_name_uc}_ROOT "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-latest" CACHE PATH "${lib_name_lc} installation directory")
        elseif()
            #not installed by user in insieme libs
                    
            if(${lib_name}_FORCE_INSTALL)
                #call install of ${lib_name}
                
                #set ${lib_name}_ROOT
				set(${lib_name_uc}_ROOT "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-latest" CACHE PATH "${lib_name} installation directory") 
            endif()
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
		message(STATUS "Installing ${lib_name} at ${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-${${lib_name_uc}_VERSION} (+${lib_name_lc}-latest link)")
               
        #set ${lib_name}_ROOT
		set(${lib_name_uc}_ROOT "${THIRD_PARTY_LIBS_HOME}/${lib_name_lc}-latest" CACHE PATH "${lib_name} installation directory")
        
        find_package(${lib_name} ${${lib_name_uc}_VERSION} REQUIRED COMPONENTS ${lib_components})
    endif()
endmacro()
