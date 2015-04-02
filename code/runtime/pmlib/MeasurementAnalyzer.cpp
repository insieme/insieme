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

#include "MeasurementAnalyzer.h"

MeasurementAnalyzer::MeasurementAnalyzer(PMFileParser * parser) :
	CSVAnalyzer(parser) {

}
MeasurementAnalyzer::MeasurementAnalyzer(PMMemoryParser * parser) :
	CSVAnalyzer(parser) {

}

MeasurementAnalyzer::~MeasurementAnalyzer() {

}

unsigned int MeasurementAnalyzer::getNumberOfMeasurements() {

	return CSVAnalyzer::getNumberOfMeasurements(true);
}

list<MeasurementInfo>* MeasurementAnalyzer::getMeasurementParameterList() {

	list<string>* stringList = CSVAnalyzer::getMeasurementTypeList();

	list<MeasurementInfo>* paramList =
			ParamUtility::getMeasurementParameterList();

	list<string>::iterator it = stringList->begin();
	list<MeasurementInfo>::iterator innerIt;

	list<MeasurementInfo>* resultList = new list<MeasurementInfo> ();

	for (; it != stringList->end(); it++) {

		innerIt = paramList->begin();

		for (; innerIt != paramList->end(); innerIt++) {

			UtilityClass::toLowerCase(*it);

			if (it->compare(innerIt->title) == 0) {

				resultList->push_back((*innerIt));

			}
		}
	}

	delete stringList;

	return resultList;

}

int MeasurementAnalyzer::calculateMinMaxAvg(unsigned int resultFrom,
		unsigned int resultTo, MEASUREMENT_PARAMS param, double & min,
		double & max, double & avg) {

	string resultType = this->translateFromParamToString(param);

	return CSVAnalyzer::calculateMinMaxAvg(resultFrom, resultTo, resultType,
			max, min, avg);
}

int MeasurementAnalyzer::calculateDiff(MEASUREMENT_PARAMS param,
		unsigned int firstIndex, unsigned int secondIndex, double* result) {

	string resultType = this->translateFromParamToString(param);

	return CSVAnalyzer::calculateDiff(resultType, firstIndex, secondIndex,
			result);

}

int MeasurementAnalyzer::evaluateMeasurementTrends_Percentage(
		unsigned int resultFrom, unsigned int resultTo,
		MEASUREMENT_PARAMS param, double ditherIgnorePercentage,
		map<std::pair<double, double>, double> & resultMap) {

	string resultType = this->translateFromParamToString(param);

	if (resultType.compare("") == 0) {
		return -1;
	}

	return CSVAnalyzer::evaluateMeasurementTrends_Percentage(resultFrom,
			resultTo, resultType, ditherIgnorePercentage, resultMap);
}

int MeasurementAnalyzer::evaluateMeasurementTrends(unsigned int resultFrom,
		unsigned int resultTo, MEASUREMENT_PARAMS param,
		double ditherTolerance,
		map<std::pair<double, double>, double> & resultMap) {

	string resultType = this->translateFromParamToString(param);

	if (resultType.compare("") == 0) {
		return -1;
	}

	return CSVAnalyzer::evaluateMeasurementTrends(resultFrom, resultTo,
			resultType, ditherTolerance, resultMap);
}

int MeasurementAnalyzer::sumOfColumn(unsigned int resultFrom,
		unsigned int resultTo, MEASUREMENT_PARAMS param, double & result,
		int & counter) {

	string resultType = this->translateFromParamToString(param);

	if (resultType.empty()) {
		return -1;
	}

	int ret = CSVAnalyzer::sumOfColumn(resultFrom, resultTo, resultType,
			result, counter);

	return ret;
}

int MeasurementAnalyzer::getValueList(unsigned int resultFrom,
		unsigned int resultTo, MEASUREMENT_PARAMS param,
		list<double> &resultList) {

	string resultType = this->translateFromParamToString(param);

	if (resultType.empty()) {
		return -1;
	}

	return CSVAnalyzer::getValueList(resultFrom, resultTo, resultType,
			resultList);
}

int MeasurementAnalyzer::mapMeasurementTypeToColumn(string & measurementType) {

	list<string> stringList;

	UtilityClass::toLowerCase(measurementType);

	if (this->parser->getLineValues(this->getOffset(), stringList, true) != -1) {

		list<string>::iterator it = stringList.begin();

		for (int pos = 1; it != stringList.end(); it++) {

			if (measurementType.compare(*it) == 0) {
				return pos;
			}
			pos++;
		}
	}

	return -1;

}

string MeasurementAnalyzer::translateFromParamToString(MEASUREMENT_PARAMS param) {

	list<MeasurementInfo>* paramList =
			ParamUtility::getMeasurementParameterList();

	list<MeasurementInfo>::iterator it = paramList->begin();

	for (; it != paramList->end(); it++) {


		if (it->param == param) {
			return it->title;

		}
	}

	return "";
}
