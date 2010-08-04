project(insieme)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)


# building documentation
SUBDIRS( docs )
SUBDIRS( compiler )
SUBDIRS( utils )
SUBDIRS( dummy )

