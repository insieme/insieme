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

#include "SimpleFileCreator.h"
#include <iostream>
#include <ctime>

SimpleFileCreator::~SimpleFileCreator() {

}

SimpleFileCreator::SimpleFileCreator(string dirPath)
		throw (std::invalid_argument) :
	EthernetLogDisposer(), filePath(""), dirPath(dirPath) {

	if (!UtilityClass::isValidDirPath(this->dirPath)) {

		throw std::invalid_argument("dirPath is not a valid directory path.");
	}

}

void SimpleFileCreator::addValueLine(string & value)
		throw (std::invalid_argument) {

	if (this->filePath == "") {
		throw std::invalid_argument(
				"addValueLine(): filePath has to be set, call addHeader(time, string) before.");

	}

	this->outFile.open(this->filePath.c_str(), std::ofstream::app);

	if (outFile.is_open()) {

		outFile << value;

		outFile.close();

	} else {

		throw std::runtime_error("Could not open/create " + this->filePath);
	}

}

void SimpleFileCreator::createFilePath() {

	time_t t = std::time(NULL);
	srand(t);

	std::stringstream strStream(std::stringstream::in | std::stringstream::out);

	strStream << dirPath << t << "_" << (rand() % 100) << ".csv";

	this->filePath = strStream.str();

}

void SimpleFileCreator::addHeader(string & header)
		throw (std::invalid_argument, unsupported_operation) {

	if (this->filePath == "") {

		this->createFilePath();
		this->addValueLine(header);
	}
}

void SimpleFileCreator::addHeader(time_t t, string& resultTypeList)
		throw (unsupported_operation, std::runtime_error) {

	if (this->filePath == "") {

		this->createFilePath();

		this->outFile.open(filePath.c_str());

		if (outFile.is_open()) {

			outFile << "Voltech Instruments PM1000+" << std::endl;
			outFile << "Serial Number:,?" << std::endl;
			outFile << "Firmware Version,?" << std::endl;

			char buffer[12];

			std::tm * timeinfo = localtime(&t);

			strftime(buffer, 12, "%Y-%m-%d", timeinfo);
			outFile << "Start Date (YYYYMMDD):," << buffer << std::endl;

			strftime(buffer, 12, "%H:%M:%S", timeinfo);
			outFile << "Start Time (24hr):," << buffer << std::endl;

			outFile << std::endl;

			outFile << "Index," << resultTypeList;

			outFile.close();

		} else {

			throw std::runtime_error("Could not open/create " + this->filePath);
		}
	}

}

string SimpleFileCreator::getFilePath() {
	return this->filePath;
}

string SimpleFileCreator::getDirPath() {

	return this->dirPath;

}

void SimpleFileCreator::setFileName(string & filename) {

	this->filePath = dirPath + filename;
}

