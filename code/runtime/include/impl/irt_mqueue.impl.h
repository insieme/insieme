/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once
#ifndef __GUARD_IMPL_IRT_MQUEUE_IMPL_H
#define __GUARD_IMPL_IRT_MQUEUE_IMPL_H

#include "irt_mqueue.h"

#include <stdlib.h>
#include <errno.h>

#include "impl/error_handling.impl.h"

#ifndef IRT_MIN_MODE

void irt_mqueue_init() {
	struct mq_attr attr;
	attr.mq_flags = O_NONBLOCK;
	attr.mq_maxmsg = IRT_MQUEUE_MAXMSGS;
	attr.mq_msgsize = IRT_MQUEUE_MAXMSGSIZE;
	irt_g_message_queue = mq_open(IRT_MQUEUE_NAME, O_RDWR | O_CREAT | O_NONBLOCK | O_EXCL, 0777, &attr); // TODO fix rights
	if(irt_g_message_queue == -1 && errno == EEXIST) { // MQ still exists, close and reopen (to purge messages)
		IRT_WARN("Message queue %s exists, trying to unlink and reopen. Make sure that you haven't started multiple instances of the IR.", IRT_MQUEUE_NAME);
		irt_g_message_queue = mq_open(IRT_MQUEUE_NAME, O_RDWR | O_CREAT | O_NONBLOCK, 0666, &attr);
		IRT_ASSERT(mq_unlink(IRT_MQUEUE_NAME) == 0, IRT_ERR_IO, "Could not unlink existing message queue " IRT_MQUEUE_NAME ".\nError string: %s\n",
		           strerror(errno));
		irt_g_message_queue = mq_open(IRT_MQUEUE_NAME, O_RDWR | O_CREAT | O_NONBLOCK | O_EXCL, 0666, &attr);
	}
	IRT_ASSERT(irt_g_message_queue != -1, IRT_ERR_IO, "Could not open message queue %s.\nError string: %s\n", IRT_MQUEUE_NAME, strerror(errno));
}
void irt_mqueue_cleanup() {
	IRT_ASSERT(mq_unlink(IRT_MQUEUE_NAME) == 0, IRT_ERR_IO, "Could not unlink message queue " IRT_MQUEUE_NAME ".\n");
}

void irt_mqueue_send(const irt_mqueue_msg* msg) {
	IRT_ASSERT(mq_send(irt_g_message_queue, (char*)msg, msg->size, 0) == 0, IRT_ERR_IO, "Could not send posix message.\nError string: %s\n", strerror(errno));
}
irt_mqueue_msg* irt_mqueue_receive() {
	char buffer[IRT_MQUEUE_MAXMSGSIZE];
	mqd_t retcode = mq_receive(irt_g_message_queue, buffer, IRT_MQUEUE_MAXMSGSIZE, NULL);
	if(retcode == -1 && errno == EAGAIN) { return NULL; }
	IRT_ASSERT(retcode != -1, IRT_ERR_IO, "Could not retrieve posix message.\nError string: %s\n", strerror(errno));
	irt_mqueue_msg* msg = (irt_mqueue_msg*)buffer;
	irt_mqueue_msg* retval = (irt_mqueue_msg*)malloc(msg->size);
	memcpy(retval, buffer, msg->size);
	return retval;
}

void irt_mqueue_send_new_app(const char* appname) {
	irt_mqueue_msg_new_app msg;
	msg.type = IRT_MQ_NEW_APP;
	msg.size = sizeof(irt_mqueue_msg_new_app);
	memcpy(msg.app_name, appname, sizeof(msg.app_name));
	msg.app_name[sizeof(msg.app_name) - 1] = '\0';
	irt_mqueue_send((irt_mqueue_msg*)&msg);
}

#else

void irt_mqueue_init() {}
void irt_mqueue_cleanup() {}

void irt_mqueue_send(const irt_mqueue_msg* msg) {}
irt_mqueue_msg* irt_mqueue_receive() {
	return NULL;
}

void irt_mqueue_send_new_app(const char* appname) {}

#endif

#endif // ifndef __GUARD_IMPL_IRT_MQUEUE_IMPL_H
