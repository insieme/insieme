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

#include "SThread.h"
#include <string.h>

void* SThread::run() {
	return 0;
}

#ifdef _WIN32

SThread::SThread(std::auto_ptr<Runnable> runnable_) : runnable(runnable_) {
	if (runnable.get() == NULL) {
		PrintError("Thread(std::auto_ptr<Runnable> runnable_) failed at ",__FILE__,__LINE__);
	}

	completionEvent = CreateEvent(NULL, 1, 0, NULL);

	// create a new suspended thread an store the handle to it 
	hThread = (HANDLE)_beginthreadex(NULL,0, SThread::startThreadRunnable,(LPVOID)this, CREATE_SUSPENDED, &winThreadID );
	if (!hThread) {
		PrintError("_beginthreadex failed at ",__FILE__,__LINE__);
	}
}

SThread::SThread(): runnable(NULL) {

	completionEvent = CreateEvent(NULL, 1, 0, NULL);

	// create a new suspended thread an store the handle to it 
	hThread = (HANDLE)_beginthreadex(NULL,0,SThread::startThread,
			(LPVOID)this, CREATE_SUSPENDED, &winThreadID );

	if (!hThread) {
		PrintError("_beginthreadex failed at ",__FILE__,__LINE__);
	}
}

SThread::~SThread() {
	if (winThreadID != GetCurrentThreadId()) {
		DWORD rc = CloseHandle(hThread);
		if (!rc) {
			PrintError("Closing handle failed at ",__FILE__,__LINE__);
		}
	} // note that the runnable object (if any) is automatically deleted by auto_ptr.
}

void* SThread::join() {
	DWORD rc = WaitForSingleObject(completionEvent, INFINITE);

	if (!(rc==WAIT_OBJECT_0)) {
		PrintError("WaitForSingleObject failed at ",__FILE__,__LINE__);
	}
	return result;
}

void SThread::setCompleted() {
	DWORD rc = SetEvent(completionEvent);
	if (!rc) {
		PrintError("SetEvent failed at ",__FILE__,__LINE__);
	}
}

unsigned WINAPI SThread::startThread(LPVOID pVoid) {
	SThread* aThread = static_cast<SThread*> (pVoid);
	assert_true(aThread);
	aThread->result = aThread->run();
	aThread->setCompleted();
	return reinterpret_cast<unsigned>(aThread->result);
}

unsigned WINAPI SThread::startThreadRunnable(LPVOID pVoid) {
	SThread* runnableThread = static_cast<SThread*> (pVoid);
	runnableThread->result = runnableThread->runnable->run();
	runnableThread->setCompleted();
	return reinterpret_cast<unsigned>(runnableThread->result);
}

void SThread::start() {
	assert(hThread != NULL);
	DWORD rc = ResumeThread(hThread);
	// thread was created in suspended state so this starts it running
	if (!rc) {
		PrintError("ResumeThread failed at ",__FILE__,__LINE__);
	}
}

void SThread::PrintError(const char* lpszFunction, const char* fileName, const int lineNumber) {
	std::cerr << lpszFunction << ' ' << fileName << ":" << lineNumber
	<< " - " << strerror(errno) << std::endl;
}

#else
void SThread::PrintError(const char* lpszFunction, const char* fileName, const int lineNumber,
		int status) {
	std::cerr << lpszFunction << ' ' << fileName << ":" << lineNumber << " - "
			<< strerror(status) << std::endl;
}

SThread::SThread(std::auto_ptr<Runnable> runnable_) :
	runnable(runnable_) {
	if (runnable.get() == NULL) {
		PrintError("Thread::Thread(auto_ptr<Runnable> runnable_) failed at",
				__FILE__, __LINE__, errno);
	}
}

SThread::SThread() :
	runnable(NULL) {
}

void* SThread::startThreadRunnable(void* pVoid) {
	// thread start function when a Runnable is involved
	SThread* runnableThread = static_cast<SThread*> (pVoid);
	assert_true(runnableThread);
	runnableThread->result = runnableThread->runnable->run();
	runnableThread->setCompleted();
	return runnableThread->result;
}
void* SThread::startThread(void* pVoid) {
	// thread start function when no Runnable is involved
	SThread* aThread = static_cast<SThread*> (pVoid);
	assert_true(aThread);
	aThread->result = aThread->run();
	aThread->setCompleted();
	return aThread->result;
}

SThread::~SThread() {
}

void* SThread::join() {

	void ** thread_return = NULL;
	int status = pthread_join(PthreadThreadID, thread_return);
	// result was saved by thread start functions


	if (status != 0) {
		PrintError("pthread_join failed at ", __FILE__, __LINE__, status);
	}

	return this->result;
}
void SThread::setCompleted() {
	/* completion was handled by pthread_join() */
}

void SThread::start() {
	// initialize the attribute object
	int status = pthread_attr_init(&threadAttribute);

	pthread_attr_setdetachstate(&threadAttribute, PTHREAD_CREATE_JOINABLE);

	if (status != 0) {
		PrintError("pthread_attr_init failed at ", __FILE__, __LINE__, status);
	}

	// linux only supports PTHREAD_SCOPE_SYSTEM here
	status = pthread_attr_setscope(&threadAttribute, PTHREAD_SCOPE_SYSTEM);
	if (status != 0) {
		PrintError("pthread_attr_setscope failed at ", __FILE__, __LINE__,
				status);
	}

	if (runnable.get() == NULL) {
		int status = pthread_create(&PthreadThreadID, &threadAttribute,
				SThread::startThread, (void*) this);


		if (status != 0) {
			PrintError("pthread_create failed at ", __FILE__, __LINE__, status);
		}
	} else {

		int status = pthread_create(&PthreadThreadID, &threadAttribute,
				SThread::startThreadRunnable, (void*) this);
		if (status != 0) {
			PrintError("pthread_create failed at ", __FILE__, __LINE__, status);
		}
	}

	// destroy the attribute object
	status = pthread_attr_destroy(&threadAttribute);
	if (status != 0) {
		PrintError("pthread_attr_destroy failed at ", __FILE__, __LINE__,
				status);
	}
}

#endif
