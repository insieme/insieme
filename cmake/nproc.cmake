include(ProcessorCount)

ProcessorCount(NPROC)
if(NOT NPROC)
	set(NPROC 8)
endif()

math(EXPR NPROC_HALF "${NPROC} / 2")
