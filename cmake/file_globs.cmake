macro(glob_headers output_var input_path)
	file(GLOB_RECURSE ${output_var} ${input_path}/*.h ${input_path}/*.hpp ${input_path}/*.def ${input_path}/*.inc)
endmacro()

macro(glob_sources output_var input_path)
	file(GLOB_RECURSE ${output_var} ${input_path}/*.c ${input_path}/*.cpp)
endmacro()

macro(glob_tests output_var input_path)
	file(GLOB_RECURSE ${output_var} ${input_path}/*.cc)
endmacro()

macro(glob_executables output_var input_path)
	file(GLOB_RECURSE ${output_var} ${input_path}/*.cxx)
endmacro()
