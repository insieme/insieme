/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#ifndef _STHREAD_H_
#define _STHREAD_H_

#include "Runnable.h"
#include <cassert>
#include <memory>
#include <iostream>
#include <errno.h>



#ifdef _WIN32

#include <Windows.h>
#include <process.h>

#else 

#include <pthread.h>
#include <time.h>

#endif

/*
 * Java like thread class.
 *
 * Instances of this class can be used to run Runnable instances.
 *
 * For more informations about that, see "Runnable.h".
 *
 * Subclasses should override the run method as the default implementation
 * simply returns 0;
 *
 */
class SThread {

public:
	SThread(std::auto_ptr<Runnable> runnable_);
	SThread();

	virtual ~SThread();

	/*
	 * Starts to run the SThread (or the runnable inside the thread)
	 */
	void start(); // starts a suspended thread

	/*
	 * Wait for the thread to complete, and get the
	 * return value of void * run(), if there is any.
	 *
	 * Example for casting return value:
	 * int result2= reinterpret_cast<int>(thread->join());
	 */
	void* join(); // wait for thread to complete


protected:

	/*
	 *"Processing" method of the thread.
	 *
	 *Can return a value using: return reinterpret_cast<void*>(myReturn);
	 *
	 *The returned value can be get from other threads by calling join()
	 *
	 */
	virtual void* run();



private:

	SThread(const SThread&);
	const SThread& operator=(const SThread&);
	void setCompleted(); // called when run() completes
	void* result; // stores value returned by run()
	

#ifdef _WIN32
	HANDLE hThread;
	HANDLE completionEvent;
	unsigned int winThreadID; // Win32 thread ID
	std::auto_ptr<Runnable> runnable;

	static unsigned WINAPI startThreadRunnable(LPVOID pVoid);
	static unsigned WINAPI startThread(LPVOID pVoid);
	
	void PrintError(const char* lpszFunction, const char* fileName, const int lineNumber);



#else 
	
	pthread_t PthreadThreadID; // thread ID
	pthread_attr_t threadAttribute;
	std::auto_ptr<Runnable> runnable;


	static void* startThreadRunnable(void* pVoid);
	static void* startThread(void* pVoid);	

	void PrintError(const char* lpszFunction, const char* fileName, const int lineNumber, const int status);

#endif
};

#endif

