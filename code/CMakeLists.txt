project(insieme_code CXX)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# include modules
add_subdirectory( core )
add_subdirectory( utils )

# add dependencies
add_dependencies( insieme_core insieme_utils )


