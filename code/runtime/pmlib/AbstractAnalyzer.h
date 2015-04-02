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

#ifndef GENERICANALYZER_H_
#define GENERICANALYZER_H_

#ifdef _WIN32
#pragma warning( disable : 4290 )
#endif

#include "AbstractParser.h"
#include <string>
#include <list>
#include <map>
#include <stdexcept>
#include <iostream>

using std::list;
using std::string;
using std::map;

/*
 * Abstract base class which defines the basic methods which must be implemented by
 * a class used for analyzing the measurements of the Voltech PM1000+ Power Analyzer.
 */
class AbstractAnalyzer {
public:

	/*
	 *@param parser - from which to receive/obtain the measurements.
	 *
	 * Creates a new instance of this class operating on the given parser.
	 */
	AbstractAnalyzer(AbstractParser * parser);
	virtual ~AbstractAnalyzer();

	/*
	 * @param force - force recomputation true/false.
	 *
	 * Returns the number of available measurement results.
	 */
	virtual unsigned int getNumberOfMeasurements(bool force)=0;

	/*
	 * Returns a list which contains the names of the available measurement result types.
	 */
	virtual list<string>* getMeasurementTypeList()=0;

	/*
	 * @param measurementNumber - position of the wanted result, (index starts at 0).
	 * @param measurementType - name/type of the wanted measurement.
	 *
	 * Returns the value of the desired measurement at the given position, or throws an Exception if the
	 * measurementNumber is out of range or the measurementName is not available.
	 *
	 * @see getNumberOfMeasureMents()
	 * @see getMeasurementTypeList()
	 */
	double getMeasurement(const unsigned int &measurementNumber,
			string measurementType) throw (std::invalid_argument);

	/*
	 * Returns the time of the first available log entry.
	 */
	virtual time_t getStartTime()=0;

	/*
	 * Returns the time of the last available log entry.
	 */
	virtual time_t getEndTime()=0;

	/*
	 * @param resultFrom - number of starting row  from which (incl.)
	 * 					   the results shall be taken into account for this calculation (index starts at 0)
	 * @param resultTo -  number of ending row from which (incl.)
	 * 					   the results shall be taken into account for this calculation,
	 * 					   (0 alternatively addresses the last result)
	 * @param measurementType - name/type of the wanted measurement.
	 * @param result - where the result gets stored.
	 * @param counter - counts how many results where processed
	 *
	 * Sums up the values from resultFrom to resultTo stores the sum into result and the number of processed
	 * values into counter.
	 *
	 * Returns 0 if processing was successful, or -1 otherwise.
	 */
	virtual int sumOfColumn(unsigned int resultFrom, unsigned int resultTo,
			string measuremenType, double & result, int & counter)=0;

	/*
	 * @param resultFrom - number of starting row  from which (incl.)
	 * 					   the results shall be taken into account for this calculation (index starts at 0)
	 * @param resultTo -  number of ending row from which (incl.)
	 * 					   the results shall be taken into account for this calculation,
	 * 					   (0 alternatively addresses the last result)
	 *
	 * @param measurementType - type of measurement for which the calculations shall be performed.
	 * @param max - to store the maximum value
	 * @param min - to store the minimum value
	 * @param avg - to store the average value
	 *
	 * Calculates min, max and average for the given range for the fields of the given measurement type and stores
	 * the values to the corresponding parameters.
	 *
	 * Sets min, max and avg to nan if an error occurred and the results are not valid.
	 *
	 * Returns 0 if computation was successful, or -1 otherwise.
	 */
	virtual int calculateMinMaxAvg(unsigned int resultFrom,
			unsigned int resultTo, string measurementType, double & max,
			double & min, double & avg) = 0;

	/*
	 * @param measurementType- type of measurement for which the calculations shall be performed.
	 * @param firstIndex - index of the value from which the second value shall be subtracted,
	 * 						(or 0 for the last available measurement)
	 * @param secondIndex - index of the value which shall be subtracted from the first value.
	 * 						(or 0 for the first available measurement)
	 * @param result - to store the result [ v(firstIndex)-v(secondIndex) ]
	 *
	 *
	 * Calculates the difference between the values given by first- and secondIndex of
	 * the given measurementType.
	 *
	 * returns 0 if operation was successful, or -1 otherwise
	 */
	virtual int calculateDiff(string measurementType, unsigned int firstIndex,
			unsigned int secondIndex, double*  result);

	/*
	 * @param resultFrom - number of starting row  from which (incl.)
	 * 					   the results shall be taken into account for this calculation (index starts at 0)
	 * @param resultTo -  number of ending row from which (incl.)
	 * 					   the results shall be taken into account for this calculation,
	 * 					   (0 alternatively addresses the last result)
	 *
	 * @param measurementType - type of measurement for which the evaluation shall be performed.
	 * @param ditherIgnorePercentage - maximum deviation in percentage within which 2 values are considered to be the
	 * 							same/neutral (within a neutral sector)
	 * @param min - to store the minimum value
	 * @param avg - to store the average value
	 *
	 * This function divides the measured values into sectors. How this works is explained using the following example:
	 *
	 * Assuming ditherIgnorePercentage = 10.0, which means the max. tolerated deviation is 10 %.
	 *  _______________
	 * | Index | Value |    First sector: Index 1 - 3 : value +0.05;
	 *  ---------------     As 1.10 is within 1+-10% the two values are assumed to be the same/neutral, the same with 1.05.
	 * |   1   |  1.00 |    So the first sector is a neutral sector. As 1.50 > (1.00 + 10%) first sector stops at index 3.
	 * |   2   |  1.10 |
	 * |   3   |  1.05 |    Second sector: Index 3 - 4 : value +0.50
	 * |   4   |  1.50 |    (1.05 + 10 %) < 1.50. As 1.05 and 1.50 are not assumed to be neutral according to our ditherIgnorePercentage
	 * |   5   |  0.90 |    this sector is an ascending sector. Ending at Index 4 cause the value at index 5 (0.90) is smaller than 1.50
	 * |   6   |  0.89 |
	 * |   7   |  0.92 |    Third sector:  Index 4 - 6 : value -0.61
	 * |   8   |  0.87 |   	From index 4 (1.50) to index 5 (0.90) we have a drop off (-0.6). Index 6 is also smaller than index 5, so
	 *  ---------------		it also belongs to this negative sector. Value at index 7 is greater than the one of index 6, so the sector
	 *  					stops at 6.
	 *
	 *  					Fourth sector: Index 6 - 8 : value -0.02
	 *  					Due to our ditherIgnorePercentage all this values are assumed to be neutral/the same (within 0.89 +- 10 %)
	 *
	 * Stores the values assigned to each sector, and the corresponding starting and ending index, into the given resultMap.
	 *
	 * Returns 0 if computation was successful, or -1 otherwise.
	 */
	virtual int evaluateMeasurementTrends_Percentage(unsigned int resultFrom,
			unsigned int resultTo, string measurementType,
			double ditherIgnorePercentage,
			map<std::pair<double, double>, double> & resultMap) = 0;

	/*
	 * @param resultFrom - number of starting row  from which (incl.)
	 * 					   the results shall be taken into account for this calculation (index starts at 0)
	 * @param resultTo -  number of ending row from which (incl.)
	 * 					   the results shall be taken into account for this calculation,
	 * 					   (0 alternatively addresses the last result)
	 *
	 * @param measurementType - type of measurement for which the evaluation shall be performed.
	 * @param ditherTolerance -  maximum deviation within which 2 values are considered to be the
	 * 							same/neutral (within a neutral sector)
	 * @param min - to store the minimum value
	 * @param avg - to store the average value
	 *
	 * This function divides the measured values into sectors. How this works is explained using the following example:
	 *
	 * Assuming ditherTolerance = 0.1
	 *  _______________
	 * | Index | Value |    First sector: Index 1 - 3 : value +0.05;
	 *  ---------------     As 1.10 is within 1+-0.1 the two values are assumed to be the same/neutral, the same with 1.05.
	 * |   1   |  1.00 |    So the first sector is a neutral sector. As 1.50 > (1.00 + 0.1) first sector stops at index 3.
	 * |   2   |  1.10 |
	 * |   3   |  1.05 |    Second sector: Index 3 - 4 : value +0.50
	 * |   4   |  1.50 |    (1.05 + 0.1) < 1.50. As 1.05 and 1.50 are not assumed to be neutral according to our ditherIgnorePercentage
	 * |   5   |  0.90 |    this sector is an ascending sector. Ending at Index 4 cause the value at index 5 (0.90) is smaller than 1.50
	 * |   6   |  0.89 |
	 * |   7   |  0.92 |    Third sector:  Index 4 - 6 : value -0.61
	 * |   8   |  0.87 |   	From index 4 (1.50) to index 5 (0.90) we have a drop off (-0.6). Index 6 is also smaller than index 5, so
	 *  ---------------		it also belongs to this negative sector. Value at index 7 is greater than the one of index 6, so the sector
	 *  					stops at 6.
	 *
	 *  					Fourth sector: Index 6 - 8 : value -0.02
	 *  					Due to our ditherIgnorePercentage all this values are assumed to be neutral/the same (within 0.89 +- 0.10)
	 *
	 * Stores the values assigned to each sector, and the corresponding starting and ending index, into the given resultMap.
	 *
	 * Returns 0 if computation was successful, or -1 otherwise.
	 */
	virtual int evaluateMeasurementTrends(unsigned int resultFrom,
			unsigned int resultTo, string measurementType,
			double ditherTolerance,
			map<std::pair<double, double>, double> & resultMap) = 0;

	/*
	 * @param resultFrom - number of starting row  from which (incl.)
	 * 					   the results shall be taken into account for this calculation (index starts at 0)
	 * @param resultTo -  number of ending row from which (incl.)
	 * 					   the results shall be taken into account for this calculation,
	 * 					   (0 alternatively addresses the last result)
	 *
	 * @param measurementType - type of measurement for which the evaluation shall be performed.
	 * @param resultList - list to store the measurements
	 *
	 * Retrieves and stores the values from resultFrom to resultTo for the given
	 * measurement type to resultList.
	 *
	 * Returns 0 if operation was completed successfully, otherwise -1.
	 */
	virtual int getValueList(unsigned int resultFrom, unsigned int resultTo,
			string measuremenType, list<double> &resultList) =0;

protected:

	/*
	 * Hook method which gets called from getMeasurement(unsigned int, string) method after it has been checked if
	 * the given number is within range. No checks on the measurementType have been made.
	 *
	 * This method returns the desired value corresponding to the concrete used subclass;
	 *
	 * @throws invalud_argument if the measurementType is not available.
	 */
	virtual double getMeasurementHook(const unsigned int &measurementNumber,
			string measurementType) throw (std::invalid_argument) =0;

	AbstractParser * parser;

};

#endif /* GENERICANALYZER_H_ */
