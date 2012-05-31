#
# Just build local document ...
#
# For more information see: http://www.cmake.org/Wiki/CMakeUserUseLATEX
#
project(insieme_dev_doc NONE)

cmake_minimum_required(VERSION 2.6)

include(../UseLATEX.cmake)

file(GLOB_RECURSE insieme_dev_doc_pics RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} pics/*.pdf)

ADD_LATEX_DOCUMENT(
	insieme_doc.tex
	BIBFILES biblio.bib
	INPUTS 
		01_introduction.tex
		02_overview.tex
		03_compiler.tex
		04_runtime.tex
		05_infrastructure.tex
		06_credits.tex
	IMAGES ${insieme_dev_doc_pics}
	USE_INDEX
	DEFAULT_PDF
	MANGLE_TARGET_NAMES
)

