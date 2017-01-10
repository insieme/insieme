#
# Just build local document ...
#
# For more information see: http://www.cmake.org/Wiki/CMakeUserUseLATEX
#
project(insieme_dev_doc NONE)

cmake_minimum_required(VERSION 3.5)

include(../UseLATEX.cmake)

file(GLOB_RECURSE insieme_dev_doc_pics RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} pics/*.pdf)
file(GLOB_RECURSE insieme_dev_doc_tex RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tex)

ADD_LATEX_DOCUMENT(
	insieme_doc.tex
	BIBFILES biblio.bib
	INPUTS ${insieme_dev_doc_tex}
	IMAGES ${insieme_dev_doc_pics}
	USE_INDEX
	DEFAULT_PDF
	MANGLE_TARGET_NAMES
)

