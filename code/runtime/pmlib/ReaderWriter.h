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

#ifndef _READERWRITER_H_
#define _READERWRITER_H_

#ifdef _WIN32

#include<Windows.h>

#pragma warning( disable : 4290 )

#else

#include<pthread.h>

#endif

/*
 * This class offers methods to handle a reader/writer lock.
 *
 * The implementation works on systems using POSIX (pthreads) or on Windows
 *  (Minimum supported client: Windows Vista, Minimum supported server: Windows Server 2008).
 */
class ReaderWriter {
public:
	virtual ~ReaderWriter(void);

	/*
	 * Must be called before using the lock.
	 *
	 * It is suggested to call it in the constructor of the subclass
	 */
	void initLock();

	/*
	 * Trys to obtain a read lock, if lock is not available it blocks.
	 */
	void readLock();

	/*
	 * Trys to obtain a write lock, if lock is not available it blocks.
	 */
	void writeLock();

	/*
	 * Unlocks a held read lock.
	 */
	void readUnlock();

	/*
	 * Unlocks a held write lock.
	 */
	void writeUnlock();

	/*
	 * Destroys the reader/writer lock item.
	 *
	 */
	void destroyLock();

protected:
#ifdef _WIN32

	SRWLOCK rwLock;

#else

	pthread_rwlock_t rwLock;

#endif

};
#endif
