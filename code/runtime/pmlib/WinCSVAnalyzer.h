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

#ifndef WINCSVANALYZER_H_
#define WINCSVANALYZER_H_

#include "CSVAnalyzer.h"


/*
 * Subclass of CSVAnalyzer which analyzes the static CSV files generated
 * by the data logging feature of the IEC 62031 Software for the
 * Voltech PM 1000+ Power Analyzer.
 */
class WinCSVAnalyzer : public CSVAnalyzer{
public:
	WinCSVAnalyzer(AbstractParser * parser);
	WinCSVAnalyzer(ServerParser * parser);
	virtual ~WinCSVAnalyzer();
	virtual time_t getStartTime();
	virtual time_t getEndTime();
	virtual list<string>* getMeasurementTypeList();

protected:

	virtual int getOffset();
	virtual double convertStringToValue(string str);
	


private:

	/*
	 * OFFSET in lines inside the CSV until the header of the measured results starts.
	 */
	static unsigned int OFFSET;

	/*
	 *@param date - string in the format "DD-MMMM-YYYY", e.g. "11-August-2011"
	 *@param time - string in the format "hh-mm-ss", e.g. "14:01:53"
	 *
	 * Creates and returns a time_t out of the given parameters
	 */
	time_t createTimeFromStrings(string date, string time);

};

#endif /* WINCSVANALYZER_H_ */
