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
* WinCSVAnalyzer.cpp
*
*  Created on: Aug 11, 2011
*      Author: eiter
*/

#include "WinCSVAnalyzer.h"
#include <iostream>
#include <ctime>
#include <cstdlib>

using std::cout;
using std::endl;

unsigned int WinCSVAnalyzer::OFFSET = 3;

WinCSVAnalyzer::WinCSVAnalyzer(AbstractParser * parser) :
CSVAnalyzer(parser) {
}

WinCSVAnalyzer::WinCSVAnalyzer(ServerParser * parser) :
CSVAnalyzer(parser) {
}

WinCSVAnalyzer::~WinCSVAnalyzer() {

}

list<string>* WinCSVAnalyzer::getMeasurementTypeList() {

	list<string>*  measurementList = this->getMeasurementTypeList();

	if (measurementList->empty()) {

		this->parser->getLineValues(this->getOffset(),
			*(measurementList), true);

		std::list<string>::iterator it;

		for(it = measurementList->begin(); it != measurementList->end(); it++){

			it->replace(0,1, "");

			int pos = it->find("(");

			if(pos >= 1){
				it->replace(pos-1, it->length()-pos+1, "");
			} else {
				it->replace(it->length()-1, 1, "");
			}



		}

	}

	return measurementList;
}



int WinCSVAnalyzer::getOffset() {

	return WinCSVAnalyzer::OFFSET;

}

double WinCSVAnalyzer::convertStringToValue(string str) {

	str.replace(0, 1, "");
	str.replace(str.length()-1,1, "");
	return atof(str.c_str());
}

time_t WinCSVAnalyzer::getStartTime() {

	string date;
	string time;

	int dateRes = this->parser->getField(2, 2, date);
	int timeRes = this->parser->getField(getOffset()+1, 1, time);

	if (!dateRes && !timeRes) {

		
		this->startTime = createTimeFromStrings(date, time);

	}

	return this->startTime;

}

time_t WinCSVAnalyzer::getEndTime() {

	string date;
	string time;

	int dateRes = this->parser->getField(2, 2, date);
	int timeRes = this->parser->getField(parser->countLines(true), 1, time);

	
	if (!dateRes && !timeRes) {

		
		this->endTime = createTimeFromStrings(date, time);

	}

	return this->endTime;

}


time_t WinCSVAnalyzer::createTimeFromStrings(string date, string time){

	std::tm timeStruct;

	timeStruct.tm_isdst = -1;
	timeStruct.tm_wday = 0;

	string hour = time.substr(1, 2);
	string min = time.substr(4, 2);
	string sec =time.substr(7, 2);
	timeStruct.tm_hour = atoi(hour.c_str());
	timeStruct.tm_min = atoi(min.c_str());
	timeStruct.tm_sec = atoi(sec.c_str());

	int length = date.length();
	string day = date.substr(1, 2);
	timeStruct.tm_mday = atoi(day.c_str());
	string month = date.substr(4, length - 10);

	if (month.compare("January") == 0) {
		timeStruct.tm_mon = 0;
	} else if (month.compare("February") == 0) {
		timeStruct.tm_mon = 1;
	} else if (month.compare("March") == 0) {
		timeStruct.tm_mon = 2;
	} else if (month.compare("April") == 0) {
		timeStruct.tm_mon = 3;
	} else if (month.compare("May") == 0) {
		timeStruct.tm_mon = 4;
	} else if (month.compare("June") == 0) {
		timeStruct.tm_mon = 5;
	} else if (month.compare("July") == 0) {
		timeStruct.tm_mon = 6;
	} else if (month.compare("August") == 0) {
		timeStruct.tm_mon = 7;
	} else if (month.compare("September") == 0) {
		timeStruct.tm_mon = 8;
	} else if (month.compare("October") == 0) {
		timeStruct.tm_mon = 9;
	} else if (month.compare("November") == 0) {
		timeStruct.tm_mon = 10;
	} else if (month.compare("December") == 0) {
		timeStruct.tm_mon = 11;
	}

	string year = date.substr(length-5, 4);
	timeStruct.tm_year = atoi(year.c_str()) - 1900;

	return mktime(&timeStruct);
}



