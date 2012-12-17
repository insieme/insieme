project(insieme_docs NONE)

cmake_minimum_required(VERSION 2.6)

include(UseLATEX.cmake)

add_subdirectory(insieme_dev_doc)

add_subdirectory(coding_standards)
add_subdirectory(ir_language_specification)
add_subdirectory(ir++_language_specification)
add_subdirectory(ir_translation)

add_subdirectory(insieme_runtime)
add_subdirectory(dataflow_framework)
add_subdirectory(polyhedral)
add_subdirectory(transformation_framework)

add_subdirectory(optimization)

