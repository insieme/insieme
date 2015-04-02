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

#ifndef LOGFILECREATOR_H_
#define LOGFILECREATOR_H_

#include "SimpleFileCreator.h"
#include <fstream>
#include <sstream>

#ifdef _WIN32
#error "LogFileCreator currently doesn't run on Windows."
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif


using std::ofstream;

/*
 * This subclass of SimpleFileCreator creates a new csv file and adds
 * received measurements to it.
 */
class LogFileCreator : public SimpleFileCreator {
public:
	/*
	 * @param dirPath - path to the directory where the logfile shall be created
	 *
	 * Creates a new instance of this class.
	 *
	 * Throws invalud_argument if dirPath does not point to a valid directory.
	 */
	LogFileCreator(string dirPath) throw(std::invalid_argument);

	virtual ~LogFileCreator();

	virtual void addValueLine(string & value) throw (std::invalid_argument);

	/*
	 * Not supported by this class.
	 */
	virtual void addHeader(string & header) throw (std::invalid_argument,
			unsupported_operation);

	virtual void addHeader(time_t& time, string& resultTypeList)
				throw (unsupported_operation, std::runtime_error);



protected:
	/**
	 * Allows to perform additional operations after the header has been added
	 * by calling addHeater(time_t&, string&);
	 */
	virtual void addHeaderHook();

	/*
	 * Returns the number of added lines.
	 */
	int getResultCounter();


private:
	int resultCounter;

};

#endif /* LOGFILECREATOR_H_ */
