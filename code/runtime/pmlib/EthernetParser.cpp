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

#include "EthernetParser.h"

#include<sstream>
#include<time.h>
#include<ctime>

using std::endl;


EthernetParser::EthernetParser(unsigned int max) : ServerParser(max), indexCounter(0){

	this->offset = 7;

}


EthernetParser::~EthernetParser(void){
}


void EthernetParser::clear(){
		ServerParser::clear();
		this->indexCounter = 0;

}

void EthernetParser::addValueLine(string & value) throw (std::invalid_argument){

	stringstream s(value, stringstream::in);
	
	stringstream helper(stringstream::in | stringstream::out);
	string content;

	this->writeLock();
		
	while(getline(s, content)) {

		indexCounter++;

		helper << indexCounter << ", " << content;

		content = helper.str();
		valueVector->push_back(content);

		if(valueVector->size() > maxSize){

			valueVector->erase(valueVector->begin()+this->offset);
		}		
		helper.str(std::string());
	}	

	this->writeUnlock();

}

void EthernetParser::addHeader(string & header) throw (unsupported_operation){
	throw new unsupported_operation("addHeader(string &) is not supported by EthernetParser, use addHeader(time_t&, string&) instead.");
}



void EthernetParser::addHeader(time_t& time, string& resultTypeList)throw (unsupported_operation){


	this->writeLock();	

	size_t found = resultTypeList.find_last_of("\n");

	if(found != string::npos){		
		resultTypeList.replace(found,1, "");
	}

	valueVector->push_back("Voltech Instruments PM1000+");
	valueVector->push_back("Serial Number:,?");
	valueVector->push_back("Firmware Version,?");

	char buffer[12];

	std::tm * timeinfo = localtime(&time);

	stringstream sst(stringstream::in | stringstream::out);

	strftime (buffer,12,"%Y-%m-%d",timeinfo);
	sst << "Start Date (YYYYMMDD):," << buffer;

	valueVector->push_back(sst.str());
	sst.str(std::string());

	strftime (buffer,12,"%H:%M:%S",timeinfo);
	sst << "Start Time (24hr):," << buffer;

	valueVector->push_back(sst.str());

	sst.str(std::string());
	valueVector->push_back(""); // adding empty line

	sst << "Index," << resultTypeList;
	valueVector->push_back(sst.str());

	this->writeUnlock();	

}
