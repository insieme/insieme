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

#ifndef ABSTRACTPARSER_H_
#define ABSTRACTPARSER_H_

#include <string.h>
#include <stdexcept>
#include <map>
#include <list>



using std::string;
using std::list;
using std::map;

#ifdef _WIN32
#pragma warning( disable : 4290 )
#endif

/*
 * Baseclass for parsers used to get the measurements logged by the
 * Voltech PM1000+ Power Analyzer.
 *
 */
class AbstractParser {

public:
	AbstractParser(void);

	virtual ~AbstractParser(void) = 0;

	/*
	 * @param lineNumber - number of line to read
	 * @param fieldNumber - number of field/value in line to read
	 * @param result - where the result string gets stored.
	 * Stores the field defined by lineNumber and fieldNumber to result.
	 *
	 *
	 * Returns 0 if operation was completed successfully,
	 * or -1 if an error occurred.
	 *
	 * Throws invalid_argument if lineNumber or fieldNumber equals 0;
	 */
	virtual int getField(unsigned int lineNumber, unsigned int fieldNumber, std::string & result) throw(std::invalid_argument);

	/*
	 * @param lineNumber - number of line to read
	 * @param result - where the resulting string gets stored
	 *
	 * Reads and stores the line of the CSV file to the given string.
	 *
	 * Returns 0 if operation was completed successfully,
	 * or -1 if an error occurred (e.g. lineNumber where out of range);
	 *
	 * Throws invalid_argument if lineNumber equals 0;
	 */
	virtual int getLine(unsigned int lineNumber, string & result) throw (std::invalid_argument) =0;

	/*
	 * @param lineNumber - number of line to read
	 * @param list - where the results gets stored
	 * @param lowerCase - if true the values get transformed to lower case
	 *
	 * Reads the values of the line corresponding to lineNumber and stores
	 * the values to the given list.
	 *
	 * Returns 0 if operation was completed successfully,
	 * or -1 if an error occurred (e.g. lineNumber is out of range);
	 *
	 * Throws invalid_argument if lineNumber equals 0;
	 */
	virtual int getLineValues(unsigned int lineNumber, list< string > & l,const bool lowerCase) throw (std::invalid_argument);

	/*
	 * @param lineNumber - number of line from where to obtain the field values
	 * @param lowerCase - if true the values get transformed to lower case
	 * @param resultMap - map where the results get stored
	 * @param numargs - number of variable arguments (column numbers)
	 * @param ... - the numbers of the columns (in the line) whose values get returned (unsigned int)
	 *
	 * Reads the values corresponding to the given field numbers from the line according to lineNumber and stores
	 * the values to the given map, where the value is the value of the column and the key is the column number.
	 *
	 * Column numbers greater than the max number of columns will be ignored.
	 *
	 * Returns 0 if operation was completed successfully,
	 * or -1 if an error occurred (e.g. lineNumber is out of range);
	 *
	 * Throws invalid_argument if lineNumber or one of the column numbers equals 0;
	 *
	 * 	@see counLines():int
	 */
	virtual int getMultipleFieldsFromLine(unsigned int lineNumber,
			const bool lowerCase, map<unsigned int, string> &resultMap,
			unsigned int numargs, ...) throw (std::invalid_argument)=0;

	/*
	 * @param columnNumber - number of the row from which to obtain the field values
	 * @param lowerCase - if true the values get transformed to lower case
	 * @param resultList - list where the results get stored
	 * @param startLine - row from where to start (incl.), 0 (or 1) to start from first row.
	 * @param endLine - row where to end (incl.), 0 to end with last row.
	 *
	 * Reads the values corresponding to the given row numbers from the column according to start and end, and stores
	 * the values to the given map, where the value is the value of the row and the key is the row number.
	 *
	 * Row numbers greater than the max number of rows will be ignored.
	 *
	 * Returns 0 if operation was completed successfully,
	 * or -1 if an error occurred.
	 *
	 * Throws invalid_argument if columnNumber equals 0 or startLine > endLine (&& endLine != 0);
	 *
	 * @see counLines():int
	 */
	virtual int getColumnOverRange(unsigned int columnNumber,
			const bool lowerCase, list<string> &resultList,
			unsigned int startLine, unsigned int endLine)
			throw (std::invalid_argument)=0;

	/*
	 * @param lowerCase - if true the values get transformed to lower case
	 * @param resultList - list where the results get stored
	 * @param startLine - row from where to start (incl.), 0 (or 1) to start from first row.
	 * @param endLine - row where to end (incl.), 0 to end with last row.
	 * @param numargs - number of variable arguments (column numbers)
	 * @param ... - the numbers of the columns whose values get returned (unsigned int)
	 *
	 * Reads the values corresponding to the given column numbers from the lines according to startLine and endLine, and
	 * stores the values to the list, where the value of a list item is a map where the key is the column number and the
	 * value the corresponding value of this line.
	 *
	 * Row or column numbers greater than the max number of rows or colums will be ignored.
	 *
	 * Returns 0 if operation was completed successfully,
	 * or -1 if an error occurred.
	 *
	 * Throws invalid_argument if one of the column numbers equals 0 or startLine > endLine (&& endLine != 0);
	 *
	 * @see counLines():int
	 */

	virtual int getColumnsOverRange(const bool lowerCase,
			list<map<int, string> > &resultList, unsigned int startLine,
			unsigned int endLine, unsigned int numargs, ...)
			throw (std::invalid_argument)= 0;

	/*
	 * @param force - if true, a recomputation of the number of lines gets enforced.
	 * Returns the number of lines in the CSV file
	 *
	 */
	virtual int countLines(bool force) =0;

protected:

	/*
	 * Delimiter used for separate fields of the CSV file.
	 */
	const static char DELIMITER = ',';
};
#endif
