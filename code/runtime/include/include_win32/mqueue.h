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

/*****************************************************************************
 *
 * POSIX Message Queue library implemented using memory mapped files
 *
 *****************************************************************************/
#ifndef __mqueue_h
#define __mqueue_h
#include <sys/types.h>
#include "abstraction/threads.h"
#include "abstraction/impl/threads.impl.h"

#ifdef _MSC_VER
	#include "include_win32/missing_sys_types.h"
#endif

#include <fcntl.h>
#if !defined(LINUX)
union sigval
{
  int sival_int;                /* integer value */
  void *sival_ptr;              /* pointer value */
};
struct sigevent
{
  int sigev_notify;             /* notification type */
  int sigev_signo;              /* signal number */
  union sigval sigev_value;     /* signal value */
};

#else
#include <signal.h>
#endif

/*****************************************************************************/

typedef struct mq_info *mqd_t;  /* opaque datatype */

struct mq_attr
{
  long mq_flags;                /* message queue flag: O_NONBLOCK */
  long mq_maxmsg;               /* max number of messages allowed on queue */
  long mq_msgsize;              /* max size of a message (in bytes) */
  long mq_curmsgs;              /* number of messages currently on queue */
};

/* one mq_hdr{} per queue, at beginning of mapped file */
struct mq_hdr
{
  struct mq_attr mqh_attr;      /* the queue's attributes */
  long mqh_head;                /* index of first message */
  long mqh_free;                /* index of first free message */
  long mqh_nwait;               /* #threads blocked in mq_receive() */
  pid_t mqh_pid;                /* nonzero PID if mqh_event set */
  struct sigevent mqh_event;    /* for mq_notify() */
#if !defined(WIN32) && !defined (UNDER_CE)
  irt_lock_obj mqh_lock;     /* mutex lock */
  irt_cond_var mqh_wait;      /* and condition variable */
#endif
};

/* one msg_hdr{} at the front of each message in the mapped file */
struct msg_hdr
{
  long msg_next;                /* index of next on linked list */
  /* msg_next must be first member in struct */
  ssize_t msg_len;              /* actual length */
  unsigned int msg_prio;        /* priority */
};

/* one mq_info{} malloc'ed per process per mq_open() */
struct mq_info
{
#if defined(WIN32)
  /*
   * try not to polute the namespace.
   * typedef void* HANDLE;
  */
  void* lock;
  void* wait;
  void* signal;
  void* mqi_fmap;              /* file mapping object */
#endif
  struct mq_hdr *mqi_hdr;       /* start of mmap'ed region */
  long mqi_magic;               /* magic number if open */
  int mqi_flags;                /* flags for this process */
};
#define MQI_MAGIC  0x98765432

/* size of message in file is rounded up for alignment */
#define MSGSIZE(i) ((((i) + sizeof(long)-1) / sizeof(long)) * sizeof(long))

/* message queue functions */
extern int mq_msgcnt(mqd_t);
extern int mq_close(mqd_t);
extern int mq_getattr(mqd_t, struct mq_attr *);
extern int mq_notify(mqd_t, const struct sigevent *);
extern int mq_signal(mqd_t, int);
extern mqd_t mq_open(const char *, int, ...);
extern ssize_t mq_receive(mqd_t, char *, size_t, unsigned int *);
extern int mq_send(mqd_t, const char *, size_t, unsigned int);
extern int mq_setattr(mqd_t, const struct mq_attr *, struct mq_attr *);
extern int mq_unlink(const char *name);

#endif
