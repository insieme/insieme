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

#ifndef _WIN32

#ifndef INOTIFYMANAGER_H_
#define INOTIFYMANAGER_H_

#include "AbstractFileObserver.h"
#include <sys/inotify.h>
#include <stdexcept>
#include <queue>

using std::queue;
using std::string;

/*
 * Instances of this class can be used to observe events on a file and react corresponding to those events.
 *
 * To change the behavior upon events, subclasses have to overwrite the function: void handleEvent(inotify_event event);
 *
 */
class LinuxFileObserver : public AbstractFileObserver {
public:
	/*
	 * @param dirname - path to directory/file which should be monitored
	 * @param mask - bitmask to identify the events which shall be caught
	 *               (identical to the bitmaks of inotify -> see http://linux.die.net/man/7/inotify;
	 *
	 * Creates a new InotifyManager monitoring the file/directory identified by dirname and
	 * reacting on events corresponding to mask.
	 */
	LinuxFileObserver(string dirname, unsigned long mask)
			throw (std::runtime_error);
	virtual ~LinuxFileObserver() throw (std::runtime_error);

	/*
	 * Start an infinite loop waiting and processing events on the file.
	 */
	void observeFile();



protected:
	/*
	 * Handles the given event. The basic implementation of this method simply prints out the
	 * type of the event to the console.
	 */
	virtual void handleEvent(inotify_event event);

	/*
	 * Reads the occurred events and stores them into the eventQueue.
	 *
	 * Returns the number of read events.
	 */
	int readEvents();



private:
	int fd;
	int wd;
	queue<inotify_event>* eventQueue;

	/*
	 * Handles all events inside the eventQueue by calling handleEvent(inotify_event event);
	 * for every single event.
	 */
	void handleEvents();

	/*
	 * Check if there is an event.
	 *
	 * Wait until an event happens or gets interrupted
	 * by a signal that was caught
	 *
	 * Returns a value >= 0 if there's an event, otherwise -1
	 */
	int eventCheck();
};

#endif /* INOTIFYMANAGER_H_ */
#endif //_WIN32
