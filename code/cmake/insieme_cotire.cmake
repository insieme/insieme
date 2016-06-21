macro(insieme_cotire target)

	get_target_property(old_include_path ${target} COTIRE_PREFIX_HEADER_INCLUDE_PATH)
	get_target_property(old_include_priority_path ${target} COTIRE_PREFIX_HEADER_INCLUDE_PRIORITY_PATH)
	set_target_properties(${target} PROPERTIES
		COTIRE_PREFIX_HEADER_INCLUDE_PATH "${old_include_path};${insieme_common_include_dir}/insieme/common/utils"
	        COTIRE_PREFIX_HEADER_INCLUDE_PRIORITY_PATH "${old_include_priority_path};${insieme_common_include_dir}/insieme/common/utils/"
	)
	cotire(${target})

endmacro()
