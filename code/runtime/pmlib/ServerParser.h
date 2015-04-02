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

#ifndef _SERVERPARSER_H_
#define _SERVERPARSER_H_

#include "AbstractParser.h"
#include "ReaderWriter.h"
#include "EthernetLogDisposer.h"
#include "unsupported_operation.h"
#include <vector>

using std::vector;
using std::stringstream;

/*
 * Instances of this class can be used to parse the content received from the logfile monitoring server.
 *
 * To add the informations received from the server, this class offers the methods addValueLine(string &) addHeader(string & ).
 *
 * Access and modification operations on the valueVector member are synchronized using a reader/writer lock.
 */
class ServerParser : public AbstractParser, public ReaderWriter, public EthernetLogDisposer {
public:

	/*
	 * @param maxSize - the maximum number of stored lines
	 * 
	 * Creates a new instance of this class using the given parameter.
	 *
	 */
	ServerParser( unsigned int maxSize);

	virtual ~ServerParser(void);
	
	
	virtual int getLine(unsigned int lineNumber, string &result)
			throw (std::invalid_argument);

	virtual int getMultipleFieldsFromLine(unsigned int lineNumber,
			const bool lowerCase, map<unsigned int, string> &resultMap,
			unsigned int numargs, ...) throw (std::invalid_argument);

	virtual int getColumnOverRange(unsigned int columnNumber, const bool lowerCase,
			list<string> &resultList, unsigned int startLine,
			unsigned int endLine) throw (std::invalid_argument);

	virtual int getColumnsOverRange(const bool lowerCase,
			list<map<int, string> > &resultList, unsigned int startLine,
			unsigned int endLine, unsigned int numargs, ...) throw (std::invalid_argument);

	virtual int countLines();

	/*
	* @param value - value string to add
	*
	* Splits the given string into the single lines and adds 
    * each single linte to valueVector, if the max number of
	* entries gets exceeded by adding the new elements, 
	* the first elements get deleted.
	*
	* @Throws an invalid_argument exception if no header was set 
	* before.
	*
	* @see addHeader(string & header);
	*/
	virtual void addValueLine(string & value) throw (std::invalid_argument);

	/*
	 * @param value - value string to add
	 *
	 * Splits given string into single lines and adds those lines
	 * to the headerVector.
	 * 
	 * If it's determined that the given string is not a valid header string 
	 * an invalid_argument exception get's thrown
	 *
	 * An example for a valid header:
	 * "Time: ","14:15:50"
     * "Date: ","11-August-2011"
     * "Time","Vrms (V)","Arms (A)","Watt (W)","VA (V)"
	 */
	virtual void addHeader(string & header) throw (std::invalid_argument, unsupported_operation);

	/*
	 * Resets this parser to its initial (empty) state.
	 */
	virtual void clear();

protected:

	/*
	 * At which line the header ends.
	 */
	int offset;

	bool headerSet;

	vector<string>  * valueVector;

	unsigned int maxSize;

	/* @param lineCounter - number of line inside the header string.
	 * @param content - line of the header string.
	 *
	 * Allows to check the input of header before adding it.
	 */
	virtual void checkInput(int lineCounter, string & content) throw (std::invalid_argument);

	//const static char* LINEBREAK = "\n";



};

#endif

