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

#ifndef ETHERNETPARSER2_H_
#define ETHERNETPARSER2_H_

#include "CSVParser.h"
#include "LogFileCreator.h"

/*
 * Instances of this class can be used to parse the content received from the Voltech PM1000+ Power Analyzer via ethernet.
 *
 * To add the informations received from the Power Analyzer, this class offers the methods addValueLine(string &) addHeader(string & ).
 *
 * The measurements are stored in a csv file.
 *
 * "EthernetParser.h" offers the same functionality, but stores received measurements to an stl container (vector).
 *
 */
class EthernetParser2: public CSVParser, public LogFileCreator {
public:
	EthernetParser2(string dirPath) throw(std::invalid_argument);

	virtual ~EthernetParser2();

	virtual void addHeader(time_t& time, string& resultTypeList)
			throw (unsupported_operation);


	//virtual void addValueLine(string & value) throw (std::invalid_argument);

};

#endif /* ETHERNETPARSER2_H_ */
