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

#ifndef _ETHERNETPARSER_H_
#define _ETHERNETPARSER_H_


#include "ServerParser.h"
#include "unsupported_operation.h"

/*
 * Instances of this class can be used to parse the content received from the Voltech PM1000+ Power Analyzer via ethernet.
 *
 * To add the informations received from the Power Analyzer, this class offers the methods addValueLine(string &) addHeader(string & ).
 *
 * The measurements are stored inside a vector (valueVector)
 *
 * Access and modification operations on the valueVector member are synchronized using a reader/writer lock.
 *
 * "EthernetParser2.h" offers the same functionality, but logs received measurements to a csv file.
 *
 */
class EthernetParser : public ServerParser {

public:
	EthernetParser(unsigned int max);
	~EthernetParser(void);
	
	virtual void addValueLine(string & value) throw (std::invalid_argument);
	
	/*
	 * Will throw an unsupported_operation exception.
	 *
	 * use addHeader(time_t&, string&) instead.
	 */
	virtual void addHeader(string & header) throw (unsupported_operation);

	/*
	 * @param time - start time (of the logging) to use
	 * @paran resultTypeList - string of available measurement types, separated by
	 * a ","; at the front of the string "Index," will be attached. 
	 *
	 * Creates and adds a header out of the given parameters.
	 *
	 * The resulting header using time: 1314266512 , resultTypeList: "Vrms,Arms,Watt,VA,VAr"
	 * will look like:
	 * Voltech Instruments PM1000+
	 * Serial Number:,?
     * Firmware Version,?
     * Start Date (YYYYMMDD):,2011-08-25
     * Start Time (24hr):,12:02:52
	 *
     * Index,Vrms,Arms,Watt,VA,VAr, 
	 */
	virtual void addHeader(time_t& time, string& resultTypeList) throw (unsupported_operation);

	/*
	 * Resets this parser to its initial (empty) state.
	 */
	virtual void clear();
	
protected:

	//virtual void checkInput(int lineCounter, string & content) throw (std::invalid_argument);

	unsigned int indexCounter;
};

#endif

