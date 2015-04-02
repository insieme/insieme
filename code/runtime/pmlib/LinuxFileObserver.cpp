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

/*
 * InotifyManager.cpp
 *
 *  Created on: Aug 10, 2011
 *      Author: eiter
 */
#ifndef _WIN32

#include "LinuxFileObserver.h"
#include <sys/select.h>
#include <stdio.h>

LinuxFileObserver::LinuxFileObserver(string dirname, unsigned long mask) throw (std::runtime_error) {
	this->fd = inotify_init();

	if (fd < 0) {
		throw std::runtime_error("InotifyManager(): inotify_init() failed");
	}

	wd = inotify_add_watch(fd, dirname.c_str(), mask);

	if (wd < 0) {
		throw std::runtime_error(
				"InotifyManager(): Cannot add watch for " + dirname);
	}

	eventQueue = new queue<inotify_event> ();

}

LinuxFileObserver::~LinuxFileObserver() throw (std::runtime_error) {

	delete eventQueue;

	if (close(fd) < 0) {
		throw std::runtime_error("~InotifyManager(): close() failed");
	}

}

int LinuxFileObserver::readEvents() {
	char buffer[16384];
	ssize_t buffer_i;
	inotify_event *pevent;
	ssize_t r;

	r = read(fd, buffer, 16384);
	if (r <= 0)
		return r;
	buffer_i = 0;
	while (buffer_i < r) {
		/* Parse events and queue them. */
		pevent = (inotify_event *) &buffer[buffer_i];
		eventQueue->push(*pevent);
		buffer_i += sizeof(*pevent);

	}

	return eventQueue->size();
}

void LinuxFileObserver::handleEvents() {
	inotify_event event;
	while (!this->eventQueue->empty()) {
		event = eventQueue->front();
		handleEvent(event);
		eventQueue->pop();

	}
}

void LinuxFileObserver::handleEvent(inotify_event event) {
	/* If the event was associated with a filename, we will store it here */
	char *cur_event_filename = NULL;
	char cur_event_file[] = "File";
	char cur_event_dir[] = "Dir";
	char *cur_event_file_or_dir = NULL;
	/* This is the watch descriptor the event occurred on */
	int cur_event_wd = event.wd;
	int cur_event_cookie = event.cookie;
	unsigned long flags;

	if (event.len) {
		cur_event_filename = event.name;
	}
	if (event.mask & IN_ISDIR) {
		//cur_event_file_or_dir = "Dir";
		cur_event_file_or_dir = cur_event_file;
	} else {
		//cur_event_file_or_dir = "File";
		cur_event_file_or_dir = cur_event_dir;
	}
	flags = event.mask & ~(IN_ALL_EVENTS | IN_UNMOUNT | IN_Q_OVERFLOW
			| IN_IGNORED);

	/* Perform event dependent handler routines */
	/* The mask is the magic that tells us what file operation occurred */
	switch (event.mask & (IN_ALL_EVENTS | IN_UNMOUNT | IN_Q_OVERFLOW
			| IN_IGNORED)) {
	/* File was accessed */
	case IN_ACCESS:
		printf("ACCESS: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* File was modified */
	case IN_MODIFY:
		printf("MODIFY: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* File changed attributes */
	case IN_ATTRIB:
		printf("ATTRIB: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* File open for writing was closed */
	case IN_CLOSE_WRITE:
		printf("CLOSE_WRITE: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* File open read-only was closed */
	case IN_CLOSE_NOWRITE:
		printf("CLOSE_NOWRITE: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* File was opened */
	case IN_OPEN:
		printf("OPEN: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* File was moved from X */
	case IN_MOVED_FROM:
		printf("MOVED_FROM: %s \"%s\" on WD #%i. Cookie=%d\n",
				cur_event_file_or_dir, cur_event_filename, cur_event_wd,
				cur_event_cookie);
		break;

		/* File was moved to X */
	case IN_MOVED_TO:
		printf("MOVED_TO: %s \"%s\" on WD #%i. Cookie=%d\n",
				cur_event_file_or_dir, cur_event_filename, cur_event_wd,
				cur_event_cookie);
		break;

		/* Subdir or file was deleted */
	case IN_DELETE:
		printf("DELETE: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* Subdir or file was created */
	case IN_CREATE:
		printf("CREATE: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* Watched entry was deleted */
	case IN_DELETE_SELF:
		printf("DELETE_SELF: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* Watched entry was moved */
	case IN_MOVE_SELF:
		printf("MOVE_SELF: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* Backing FS was unmounted */
	case IN_UNMOUNT:
		printf("UNMOUNT: %s \"%s\" on WD #%i\n", cur_event_file_or_dir,
				cur_event_filename, cur_event_wd);
		break;

		/* Too many FS events were received without reading them
		 some event notifications were potentially lost.  */
	case IN_Q_OVERFLOW:
		printf("Warning: AN OVERFLOW EVENT OCCURRED: \n");
		break;

		/* Watch was removed explicitly by inotify_rm_watch or automatically
		 because file was deleted, or file system was unmounted.  */
	case IN_IGNORED:

		printf("IGNORED: WD #%d\n", cur_event_wd);
		break;

		/* Some unknown message received */
	default:
		printf("UNKNOWN EVENT \"%X\" OCCURRED for file \"%s\" on WD #%i\n",
				event.mask, cur_event_filename, cur_event_wd);
		break;
	}
	/* If any flags were set other than IN_ISDIR, report the flags */
	if (flags & (~IN_ISDIR)) {
		flags = event.mask;
		printf("Flags=%lX\n", flags);
	}
}

int LinuxFileObserver::eventCheck() {
	fd_set rfds;
	FD_ZERO (&rfds);
	FD_SET (fd, &rfds);
	/* Wait until an event happens or we get interrupted
	 by a signal that we catch */
	return select(FD_SETSIZE, &rfds, NULL, NULL, NULL);
}

void LinuxFileObserver::observeFile() {
	while (this->getKeepRunning() && (fd >= 0)) {
		if (eventCheck() > 0) {
			int r;
			r = readEvents();
			if (r < 0) {
				break;
			} else {
				handleEvents();
			}
		}
	}
}

#endif

