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

#ifndef MEASUREMENTINFO_H_
#define MEASUREMENTINFO_H_

/*
 * This header contains the definition of the MEASUREMENT_PARAMS enum, the MeasurementInfo struct
 * as well as the ParamUtility class.
 *
 */

#include <string>
#include <list>

//using std::std::string;
//.using std::list;

/*
 * Enumeration assigning/representing a unique value to/for the
 * available measurement parameters of the Voltech PM1000+
 *
 */
enum MEASUREMENT_PARAMS {
	Vrms = 10,
	Arms = 11,
	F = 12,
	W = 13,
	PF = 14,
	VA = 15,
	VAr = 16,
	Vcf = 17,
	Acf = 18,
	Vthd = 19,
	Athd = 20,
	Z = 21,
	Vdc = 22,
	Adc = 23,
	R = 24,
	X = 25,
	Vrng = 26,
	Arng = 27,
	Vpk_pos = 28,
	Vpk_neg = 29,
	Apk_pos = 30,
	Apk_neg = 31,
	Whr = 32
};


struct measurementInfo {
	MEASUREMENT_PARAMS param;
	std::string title;
	std::string description;
	std::string unit;
	std::string formula;
	std::string pmCommand;
};

/*
 * Struct containing extended informations about the measurement
 * parameters of the Voltech PM1000+
 */
typedef struct measurementInfo MeasurementInfo;


/**
 * This utility class offers static methods to access the available
 * measurement parameters (MeasurementInfo) of the Voltech PM1000+
 */
class ParamUtility {

public:
	~ParamUtility();
	const static unsigned int paramMin = 10;
	const static unsigned int paramMax = 32;
	const static unsigned int params = paramMax - paramMin + 1;


	/**
	 * @param param
	 *
	 * returns the MeasurementInfo corresponding to the given parameter.
	 */
	static MeasurementInfo* getMeasurementInfo(MEASUREMENT_PARAMS param);

	/*
	 * returns a list of all available measurement parameters.
	 */
	static std::list<MeasurementInfo>* getMeasurementParameterList();

private:
	// No instances of this class needed
	ParamUtility();
	static std::list<MeasurementInfo> infoList;
	static void initInfoList();

};



#endif /* MEASUREMENTINFO_H_ */
