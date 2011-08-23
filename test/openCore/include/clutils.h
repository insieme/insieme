#pragma once

#include <CL/cl.h>
#include "CLElemQMHS4.h"

void buildProgram(cl_program program, cl_device_id device, const char* options) {
	cl_int errVal = clBuildProgram(program, 1, &device, options, NULL, NULL);

	// handle non build failure errors
	if(errVal != CL_SUCCESS && errVal != CL_BUILD_PROGRAM_FAILURE) CHKERRQ(errVal);

	cl_build_status buildStatus;
	CHKERRQ(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_STATUS, sizeof(cl_build_status), &buildStatus, NULL));
	if(buildStatus == CL_SUCCESS) return;

	char *buildLog;
	size_t logSize;
	CHKERRQ(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &logSize));
	buildLog = (char*)malloc(sizeof(char) * (logSize+1));
	CHKERRQ(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, logSize, buildLog, NULL));
	buildLog[logSize] = '\0';

	printf("Kernel build failure:\n%s\n", buildLog);
	free(buildLog);
}
