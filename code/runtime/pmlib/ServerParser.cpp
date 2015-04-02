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

#include "ServerParser.h"
#include "UtilityClass.h"

#include<sstream>
#include<iostream>
#include<stdarg.h>
#include<set>

using std::set;



ServerParser::ServerParser(unsigned int maxSize) : AbstractParser(), maxSize(maxSize){

	this->offset = 3;

	this->valueVector = new vector<string>();
	this->initLock();
	this->headerSet = false;
}


ServerParser::~ServerParser(void){	
	this->writeLock();
	delete valueVector;
	this->writeUnlock();
}


void ServerParser::clear(){
		this->headerSet = false;

		this->writeLock();
		valueVector->clear();
		this->writeUnlock();

}

int ServerParser::getLine(unsigned int lineNumber, string &result)
	throw (std::invalid_argument) {

		if (lineNumber == 0) {
			throw std::invalid_argument("getLine(): lineNumber  must be > 0");
		}

		this->readLock();
		int ret;
		if(valueVector->empty() || lineNumber > valueVector->size()){
			ret =-1;
		} else {
			result = valueVector->at(lineNumber-1);			
			ret = 0;
		}

		this->readUnlock();
		return ret;
}



int ServerParser::getMultipleFieldsFromLine(unsigned int lineNumber,
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
				ServerParser::DELIMITER) && counter <= max; ++counter) {

					if (fieldNumberSet.find(counter) != fieldNumberSet.end()) {

						resultMap[counter] = content;

					}

			}
			return 0;

		}

		return -1;

}




int ServerParser::getColumnOverRange(unsigned int columnNumber,
	const bool lowerCase, list<string> &resultList, unsigned int startLine,
	unsigned int endLine) throw (std::invalid_argument) {



		if (columnNumber == 0 || ((endLine != 0) && (startLine > endLine))) {

			throw std::invalid_argument(
				"getColumnOverRange(): columnNumber must be > 0 and start =< end");
		}

		int ret = -1;

		try {

			string line;

			bool endTest = !(endLine == 0);

			this->readLock();
			
			for(unsigned int counter = 1; counter <= valueVector->size(); ++counter) {

				if (counter >= startLine) {

					line = valueVector->at(counter-1);
					stringstream stream(line, stringstream::in);
					string content;

					for (unsigned int innerCounter = 1; getline(stream,
						content, ServerParser::DELIMITER); ++innerCounter) {

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

			ret = 0;

		} catch (std::exception &ex) {
			std::cerr << ex.what() << std::endl;
			ret = -1;
		}
		this->readUnlock();
		return ret;
}




int ServerParser::getColumnsOverRange(const bool lowerCase,
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
					"getMultipleMeasurementsFromLine(): field numbers must be > 0");

			} else {

				if (max < arg) {

					max = arg;
				}
				fieldNumberSet.insert(arg);

			}
		}

		va_end( listPointer );
		int ret = -1;
		try {


			string line;

			bool endTest = !(endLine == 0);

			this->readLock();

			for (unsigned int counter = 1; counter < valueVector->size(); ++counter) {

				if (counter >= startLine) {

					line = valueVector->at(counter-1);
					stringstream stream(line, stringstream::in);
					string content;

					map<int, string> lineResults = map<int, string> ();

					for (unsigned int innerCounter = 1; getline(stream,
						content, ServerParser::DELIMITER) && innerCounter
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

			ret = 0;

		} catch (std::exception &ex) {
			std::cerr << ex.what() << std::endl;
			ret = -1;
		}
		this->readUnlock();
		return ret;

}

int ServerParser::countLines(){
	this->readLock();
	int ret = valueVector->size();
	this->readUnlock();

	return ret;
}


void ServerParser::addValueLine(string & value) throw (std::invalid_argument){

	if(!headerSet){
		throw std::invalid_argument(
					"addValueLine(): no lines accepted, set header before.");
	}

	stringstream stream(value, stringstream::in);

	string content;

	this->writeLock();
	while(getline(stream, content)) {

		
		valueVector->push_back(content);		

		if(valueVector->size() > maxSize){

			valueVector->erase(valueVector->begin()+this->offset);

		}
	}

	this->writeUnlock();

}


void ServerParser::addHeader(string & header) throw (std::invalid_argument, unsupported_operation){

	stringstream stream(header, stringstream::in);

	string content;

	for(int counter = 0;getline(stream, content); counter++ ) {

		checkInput(counter, content);

		this->writeLock();
		valueVector->push_back(content);	
		this->writeUnlock();
	}

	this->headerSet = true;
	

}


void ServerParser::checkInput(int lineCounter, string & content) throw (std::invalid_argument){


		switch(lineCounter){

		case 0:
		case 2:
			if(content.find("\"Time") != 0){
				throw std::invalid_argument(
					"addHeader(): Invalid header.");
			} break;
		case 1:		
			if(content.find("\"Date") != 0){
				throw std::invalid_argument(
					"addHeader(): Invalid header.");
			} break;
		default: 
			throw std::invalid_argument(
				"addHeader(): Invalid header.");
			break;
		}	

}
