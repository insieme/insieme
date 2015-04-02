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

#include "CSVAnalyzer.h"
#include "UtilityClass.h"
#include <time.h>

#include <limits>
#include <cfloat>
#include <ctime>
#include <cstdlib>
#include <cmath>
#include <iostream>

unsigned int CSVAnalyzer::OFFSET = 7;

CSVAnalyzer::CSVAnalyzer(AbstractParser * parser) :
	AbstractAnalyzer(parser), startTime(-1), endTime(-1) {

}

CSVAnalyzer::~CSVAnalyzer() {

}

unsigned int CSVAnalyzer::getNumberOfMeasurements(bool force) {
	return this->parser->countLines(force) - this->getOffset();
}

list<string>* CSVAnalyzer::getMeasurementTypeList() {

	list<string> * resultList = new list<string> ();

	this->parser->getLineValues(this->getOffset(), *resultList, true);

	return resultList;
}

double CSVAnalyzer::getMeasurementHook(const unsigned int &measurementNumber,
		string measurementType) throw (std::invalid_argument) {

	int position = this->mapMeasurementTypeToColumn(measurementType);

	if (position == -1) {

		throw std::invalid_argument(
				"CSVAnalyzer::getMeasurementHook(): measurementType not available");

	} else {

		string result;
		this->parser->getField(measurementNumber + this->getOffset(), position,
				result);

		return convertStringToValue(result);

	}
}

int CSVAnalyzer::mapMeasurementTypeToColumn(string & measurementType) {

	list<string> * measurementList = this->getMeasurementTypeList();

	std::list<string>::const_iterator it = measurementList->begin();

	int ret = -1;

	UtilityClass::toLowerCase(measurementType);

	for (unsigned int counter = 1; it != measurementList->end(); it++) {

		if (measurementType.compare(*it) == 0) {
			ret = counter;
			break;
		}

		counter++;
	}
	delete measurementList;
	return ret;

}

time_t CSVAnalyzer::getStartTime() {

	if (this->startTime == -1) {

		string date;
		string time;

		int dateRes = this->parser->getField(4, 2, date);
		int timeRes = this->parser->getField(5, 2, time);

		if (!dateRes && !timeRes) {

			std::tm timeStruct;

			// TODO
			//strptime(time.c_str(), "%T", &timeStruct);
			//strptime(date.c_str(), "%Y/%m/%e", &timeStruct);

			string hour = time.substr(0, 2);
			string min = time.substr(3, 2);
			string sec = time.substr(6, 2);

			timeStruct.tm_isdst = -1;
			timeStruct.tm_wday = 0;

			timeStruct.tm_hour = atoi(hour.c_str());
			timeStruct.tm_min = atoi(min.c_str());
			timeStruct.tm_sec = atoi(sec.c_str());

			string year = date.substr(0, 4);
			string month = date.substr(5, 2);
			string day = date.substr(8, 2);

			timeStruct.tm_year = atoi(year.c_str()) - 1900;
			timeStruct.tm_mon = atoi(month.c_str()) - 1;
			timeStruct.tm_mday = atoi(day.c_str());

			this->startTime = mktime(&timeStruct);

		}
	}

	return this->startTime;

}

time_t CSVAnalyzer::getEndTime() {

	if (this->endTime == -1) {

		// just adding number of measurements - 1 to startTime as results get logged every second
		this->endTime = this->getStartTime() + this->getNumberOfMeasurements(
				true) - 1;

	}

	return this->endTime;

}

list<string> CSVAnalyzer::getResultList(unsigned int resultFrom,
		unsigned int resultTo, int & position) {
	list<string> resultList = list<string> ();
	resultFrom += this->getOffset() + 1;
	resultTo = (resultTo == 0) ? 0 : resultTo + this->getOffset() + 1;
	this->parser->getColumnOverRange(position, false, resultList, resultFrom,
			resultTo);
	return resultList;
}

int CSVAnalyzer::calculateMinMaxAvg(unsigned int resultFrom,
		unsigned int resultTo, string measurementType, double & max,
		double & min, double & avg) {

	int position = this->mapMeasurementTypeToColumn(measurementType);

	min = std::numeric_limits<double>::signaling_NaN();
	max = std::numeric_limits<double>::signaling_NaN();
	avg = std::numeric_limits<double>::signaling_NaN();

	if (position != -1) {

		list<string> resultList = getResultList(resultFrom, resultTo, position);

		if (!resultList.empty()) {
			std::list<string>::const_iterator it;

			double sum = 0;
			double currentMax = DBL_MIN;
			double currentMin = DBL_MAX;
			double value;

			double counter = 0;
			for (it = resultList.begin(); it != resultList.end(); it++) {

				value = convertStringToValue((string) *it);
				sum += value;

				if (value > currentMax) {
					currentMax = value;
				}

				if (value < currentMin) {
					currentMin = value;
				}

				counter++;
			}

			avg = sum / resultList.size();
			max = currentMax;
			min = currentMin;

			return 0;
		}
	}
	return -1;

}



int CSVAnalyzer::evaluateMeasurementTrends_Percentage(unsigned int resultFrom,
		unsigned int resultTo, string measurementType,
		double ditherTolerancePercentage,
		map<std::pair<double, double>, double> & resultMap) {

	int position = this->mapMeasurementTypeToColumn(measurementType);

	if (position != -1) {

		list<string> valueList = getResultList(resultFrom, resultTo, position);

		double f = fabs(ditherTolerancePercentage / 100);
		double lowerBound = 1 - f;
		double upperBound = 1 + f;

		if (!valueList.empty()) {
			std::list<string>::const_iterator it = valueList.begin();

			double currentValue;

			double valueBefore = convertStringToValue((string) (*it));
			double sectorStartValue = valueBefore;
			double factor;
			double sectorFactor;
			int sectorStart = resultFrom;
			int sector = 1;

			short directionFlag = 0;
			short flagBefore = -2;

			it++;

			int counter = resultFrom;

			for (; it != valueList.end(); it++) {

				currentValue = convertStringToValue(*it);

				factor = fabs(currentValue / valueBefore);
				sectorFactor = fabs(currentValue / sectorStartValue);

				bool outOfRange = sectorFactor > upperBound || sectorFactor
						< lowerBound;

				if (factor > 1 && outOfRange) {
					directionFlag = 1;
				} else if (factor < 1 && outOfRange) {
					directionFlag = -1;
				} else {
					directionFlag = 0;
				}

				if (flagBefore == -2) {
					flagBefore = directionFlag;
				}

				if (directionFlag != flagBefore) {

					resultMap[std::pair<double, double>(sectorStart + 1,
							counter + 1)] = valueBefore - sectorStartValue;

					sectorStartValue = valueBefore;
					sectorStart = counter;
					sector++;
				}

				flagBefore = directionFlag;
				valueBefore = currentValue;
				counter++;
			}

			resultMap[std::pair<double, double>(sectorStart + 1, counter + 1)]
					= currentValue - sectorStartValue;

			return 0;

		}
	}

	return -1;
}

int CSVAnalyzer::evaluateMeasurementTrends(unsigned int resultFrom,
		unsigned int resultTo, string measurementType, double ditherTolerance,
		map<std::pair<double, double>, double> & resultMap) {

	int position = this->mapMeasurementTypeToColumn(measurementType);

	if (position != -1) {

		list<string> valueList = getResultList(resultFrom, resultTo, position);

		ditherTolerance = fabs(ditherTolerance);
		double lowerBound = -ditherTolerance;
		double upperBound = ditherTolerance;

		if (!valueList.empty()) {
			std::list<string>::const_iterator it = valueList.begin();

			double currentValue;
			double valueBefore = convertStringToValue((string) *it);
			double sectorStartValue = valueBefore;
			double dither;
			double sectorDither;
			int sectorStart = resultFrom;
			int sector = 1;

			short directionFlag = 0;
			short flagBefore = -2;

			it++;

			int counter = resultFrom;

			for (; it != valueList.end(); it++) {

				currentValue = convertStringToValue((string) *it);

				dither = currentValue - valueBefore;
				sectorDither = currentValue > sectorStartValue;

				if ((dither > 0 && directionFlag == 1) || dither > upperBound) {
					directionFlag = 1;
				} else if ((dither < 0 && directionFlag == -1) || dither
						< lowerBound) {
					directionFlag = -1;
				} else {
					directionFlag = 0;
				}

				if (flagBefore == -2) {
					flagBefore = directionFlag;
				}

				if (directionFlag != flagBefore) {

					resultMap[std::pair<double, double>(sectorStart + 1,
							counter + 1)] = valueBefore - sectorStartValue;

					sectorStartValue = valueBefore;
					sectorStart = counter;
					sector++;

				}

				flagBefore = directionFlag;

				valueBefore = currentValue;
				counter++;
			}

			resultMap[std::pair<double, double>(sectorStart + 1, counter + 1)]
					= currentValue - sectorStartValue;

			return 0;

		}
	}

	return -1;
}

int CSVAnalyzer::sumOfColumn(unsigned int resultFrom, unsigned int resultTo,
		string measurementType, double & result, int & counter) {

	int position = this->mapMeasurementTypeToColumn(measurementType);

	if (position != -1) {

		list<string> resultList = getResultList(resultFrom, resultTo, position);

		std::list<string>::const_iterator it;

		counter = resultList.size();

		double sum = 0;

		for (it = resultList.begin(); it != resultList.end(); it++) {
			sum += convertStringToValue((string) *it);
		}

		result = sum;

		return 0;

	}

	return -1;

}

int CSVAnalyzer::getValueList(unsigned int resultFrom, unsigned int resultTo,
		string measuremenType, list<double> &resultList) {

	int pos;

	pos = this->mapMeasurementTypeToColumn(measuremenType);

	if (pos > -1) {

		list<string> stringList =
				this->getResultList(resultFrom, resultTo, pos);

		std::list<string>::const_iterator it = stringList.begin();

		resultList.clear();

		for (; it != stringList.end(); it++) {

			resultList.push_back(this->convertStringToValue(*it));
		}

		return 0;

	} else {

		return -1;
	}

}

int CSVAnalyzer::getOffset() {

	return CSVAnalyzer::OFFSET;

}

double CSVAnalyzer::convertStringToValue(string str) {
	return atof(str.c_str());
}

