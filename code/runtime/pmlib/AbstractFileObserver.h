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
 * GenericFileObserver.h
 *
 *  Created on: Aug 11, 2011
 *      Author: eiter
 */

#ifndef GENERICFILEOBSERVER_H_
#define GENERICFILEOBSERVER_H_

/*
 * Abstract base class for platform depended file observers, which handle
 * events on a monitored file or directory.
 */
class AbstractFileObserver {
public:
	AbstractFileObserver();
	virtual ~AbstractFileObserver();

	/*
	 * Start an infinite loop waiting and processing events on the file.
	 */
	virtual void observeFile()=0;

	/*
	 * Sets keepRunning to false, and so, stops loop of processEvents (if running).
	 *
	 * If this method gets called before processEvents() the loop will never start.
	 */
	void stop();

	/*
	 * Returns keepRunning
	 */
	bool getKeepRunning();

private:
	bool keepRunning;
};

#endif /* GENERICFILEOBSERVER_H_ */
