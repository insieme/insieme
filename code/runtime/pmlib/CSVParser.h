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
 * CSVParser.h
 *
 *  Created on: Aug 4, 2011
 *      Author: eiter
 */

#ifndef CSVPARSER_H_
#define CSVPARSER_H_


#include "AbstractParser.h"
#include <fstream>
#include <stdexcept>


using std::string;
using std::ifstream;
using std::list;
using std::map;

/*
 * Instances of this class can be used to parse content of a CSV file generated from the
 * Voltechs PM1000+ USB logging feature.
 *
 * It offers methods to access specific lines and values of the CSV file.
 */
class CSVParser : public AbstractParser{
public:

	/*
	 * @param filePath - path to the csv file which gets parsed.
	 */
	CSVParser(const string & filePath);
	virtual ~CSVParser();

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
	int getLine(unsigned int lineNumber, string &result)
			throw (std::invalid_argument);

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
	int getMultipleFieldsFromLine(unsigned int lineNumber,
			const bool lowerCase, map<unsigned int, string> &resultMap,
			unsigned int numargs, ...) throw (std::invalid_argument);

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
	int getColumnOverRange(unsigned int columnNumber, const bool lowerCase,
			list<string> &resultList, unsigned int startLine,
			unsigned int endLine) throw (std::invalid_argument);

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

	int getColumnsOverRange(const bool lowerCase,
			list<map<int, string> > &resultList, unsigned int startLine,
			unsigned int endLine, unsigned int numargs, ...) throw (std::invalid_argument);

	/*
	 * @param force - if true, a recomputation of the number of lines gets enforced.
	 * Returns the number of lines in the CSV file
	 *
	 */
	int countLines(bool force);

protected:

	/*
	 * Delimiter used for separate fields of the CSV file.
	 */
	const static char DELIMITER = ',';

	string path;
	int count;


};

#endif /* CSVPARSER_H_ */
