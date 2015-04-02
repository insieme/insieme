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
 * CSVParser.cpp
 *
 *  Created on: Aug 4, 2011
 *      Author: eiter
 */

#include "CSVParser.h"
#include "UtilityClass.h"

#include<iostream>
#include<exception>
#include<sstream>
#include<stdarg.h>
#include<cstdio>
#include<set>

using std::ifstream;
using std::exception;
using std::stringstream;
using std::set;

CSVParser::CSVParser(const string & filePath) :
	path(filePath), count(-1) {

}

CSVParser::~CSVParser() {

}

int CSVParser::countLines(bool force) {

	if (force || this->count == -1) {

		try {

			ifstream inStream(this->path.c_str(), ifstream::in);

			string line;


			if (inStream.is_open()) {

				int counter = 0;

				while (getline(inStream, line)) {
					if(line.length()>0)
					counter++;
				}

				inStream.close();

				this->count = counter;

				return this->count;
			} else {
				return -1;
			}
		} catch (exception &ex) {
			std::cerr << ex.what() << std::endl;
			return -1;
		}
	} else {
		return this->count;
	}

}

int CSVParser::getLine(unsigned int lineNumber, string &result)
		throw (std::invalid_argument) {

	if (lineNumber == 0) {
		throw std::invalid_argument("getLine(): lineNumber  must be > 0");
	}

	try {

		ifstream inStream(this->path.c_str(), ifstream::in);

		if (inStream.is_open()) {

			string line;

			// Searching for the desired line
			for (unsigned int counter = 1; getline(inStream, line); ++counter) {

				if (counter == lineNumber) {
					result = line;
					break;
				}
			}

			inStream.close();

			return 0;
		} else {
			return -1;
		}
	} catch (exception &ex) {
		std::cerr << ex.what() << std::endl;
		return -1;
	}
}

int CSVParser::getColumnsOverRange(const bool lowerCase,
		list<map<int, string> > &resultList, unsigned int startLine,
		unsigned int endLine, unsigned int numargs, ...)
		throw (std::invalid_argument) {

	va_list listPointer;

	va_start( listPointer, numargs );

	set<unsigned int> fieldNumberSet = set<unsigned int> ();

	unsigned int max = 0;

	// Checking all column numbers and putting them into a set
	for (unsigned int i = 0; i < numargs; i++) {

		unsigned int arg = va_arg( listPointer, unsigned int );

		if (arg == 0) {

			throw std::invalid_argument(
					"getColumnsOverRange(): field numbers must be > 0");

		} else {

			if (max < arg) {

				max = arg;
			}
			fieldNumberSet.insert(arg);

		}
	}

	va_end( listPointer );

	try {

		ifstream inStream(this->path.c_str(), ifstream::in);

		if (inStream.is_open()) {

			string line;

			bool endTest = !(endLine == 0);

			for (unsigned int counter = 1; getline(inStream, line); ++counter) {

				if (counter >= startLine) {

					stringstream stream(line, stringstream::in);
					string content;

					map<int, string> lineResults = map<int, string> ();

					for (unsigned int innerCounter = 1; getline(stream,
							content, CSVParser::DELIMITER) && innerCounter
							<= max; ++innerCounter) {

						if (fieldNumberSet.find(innerCounter)
								!= fieldNumberSet.end()) {
							if (lowerCase) {
								UtilityClass::toLowerCase(content);
							}
							lineResults[innerCounter] = content;
						}
					}

					resultList.push_back(lineResults);

					if (endTest && counter == endLine) {
						break;
					}
				}

			}

			inStream.close();

			return 0;
		}
	} catch (exception &ex) {
		std::cerr << ex.what() << std::endl;
		return -1;
	}

	return -1;

}

int CSVParser::getColumnOverRange(unsigned int columnNumber,
		const bool lowerCase, list<string> &resultList, unsigned int startLine,
		unsigned int endLine) throw (std::invalid_argument) {
	if (columnNumber == 0 || ((endLine != 0) && (startLine > endLine))) {

		throw std::invalid_argument(
				"getColumnOverRange(): columnNumber must be > 0 and start =< end");
	}

	try {

		ifstream inStream(this->path.c_str(), ifstream::in);

		if (inStream.is_open()) {

			string line;

			bool endTest = !(endLine == 0);

			for (unsigned int counter = 1; getline(inStream, line); ++counter) {

				if (counter >= startLine) {

					stringstream stream(line, stringstream::in);
					string content;

					for (unsigned int innerCounter = 1; getline(stream,
							content, CSVParser::DELIMITER); ++innerCounter) {

						if (innerCounter == columnNumber) {
							if (lowerCase) {
								UtilityClass::toLowerCase(content);
							}
							resultList.push_back(content);

							break;
						}
					}

					if (endTest && counter == endLine) {
						break;
					}
				}

			}

			inStream.close();

			return 0;
		} else {
			return -1;
		}
	} catch (exception &ex) {
		std::cerr << ex.what() << std::endl;
		return -1;
	}

	return -1;
}

int CSVParser::getMultipleFieldsFromLine(unsigned int lineNumber,
		const bool lowerCase, map<unsigned int, string> &resultMap,
		unsigned int numargs, ...) throw (std::invalid_argument) {

	va_list listPointer;

	va_start( listPointer, numargs );

	set<unsigned int> fieldNumberSet = set<unsigned int> ();

	unsigned int max = 0;

	// Checking all column numbers and putting them into a set
	for (unsigned int i = 0; i < numargs; i++) {

		unsigned int arg = va_arg( listPointer, unsigned int );

		if (arg == 0) {

			throw std::invalid_argument(
					"getMultipleMeasurementsFromLine(): field numbers must be > 0");

		} else {

			if (arg > max) {
				max = arg;
			}
			fieldNumberSet.insert(arg);

		}
	}

	va_end( listPointer );

	string line;

	int resultCode = this->getLine(lineNumber, line);

	if (resultCode != -1) {

		stringstream stream(line, stringstream::in);
		string content;

		// Storing the values and their position (used as key) into resultMap
		for (unsigned int counter = 1; getline(stream, content,
				CSVParser::DELIMITER) && counter <= max; ++counter) {

			if (fieldNumberSet.find(counter) != fieldNumberSet.end()) {

				resultMap[counter] = content;

			}

		}

		return 0;

	}

	return -1;

}



