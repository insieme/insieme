#ifndef _LIB_ICL_GL_
#define _LIB_ICL_GL_

/* 
	This file is used in Insieme for handling OpenGL call with OpenCL.
	There are also useful functions to handle raw IO.
	XXX current status is DRAFT
*/



// this macro should be set up externally 
#ifdef INOGL
// If INOGL flag is set up, there would be a void implementation of GL functions 
// of course, this is only a small list of function, a subset of the one available in OpenGL
// the ones used in our test cases.

// glFinish	


#else
	#include <GL/GL.h>
#endif


/*
XXX what to do with these? CL call for GL support and data sharing
	clCreateFromGLBuffer
	clCreateFromGLTexture
	clCreateFromGLRenderbuffer
	clGetGLObjectInfo
	clGetGLTextureInfo
	clEnqueueAcquireGLObjects
	clEnqueueReleaseGLObjects
	clCreateEventFromGLsyncKHR
	clGetGLContextInfoKHR
*/




#endif