#####
#
#	insieme function to find all headers and header-like files in the given directory
#      and its sub-directories. Includes .h, .def, .inc etc.
#
#####
macro(insieme_glob_headers output_var input_path)
	file(GLOB_RECURSE ${output_var} ${input_path}/*.h  ${input_path}/*.inc  ${input_path}/*.def)
endmacro()
