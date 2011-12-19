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
 * EthernetLogDisposer.h
 *
 *  Created on: Aug 29, 2011
 *      Author: eiter
 */

#ifndef ETHERNETLOGDISPOSER_H_
#define ETHERNETLOGDISPOSER_H_

#include <string>
#include <stdexcept>
#include "unsupported_operation.h"

using std::string;

/*
 * This abstract base class defines methods which have to be implemented by subclasses
 * used to store/process the input received from an EthernetReceiver.
 */
class EthernetLogDisposer {
public:
	EthernetLogDisposer();
	virtual ~EthernetLogDisposer();

	/*
	 * @param value - value string to add
	 *
	 * Splits the given string into the single lines and adds
	 * each line to the content.
	 *
	 * @Throws an invalid_argument exception if no header was set
	 * before.
	 *
	 * @see addHeader(string & header);
	 */
	virtual void addValueLine(string & value) throw (std::invalid_argument) =0;

	/*
	 * @param value - value string to add
	 *
	 * Splits given string into single lines and adds those lines
	 * to the headerVector.
	 *
	 * If it's determined that the given string is not a valid header string
	 * an invalid_argument exception get's thrown
	 *
	 */
	virtual void addHeader(string & header) throw (std::invalid_argument, unsupported_operation)=0;

	/*
	 * @param time - start time (of the logging) to use
	 * @paran resultTypeList - string of available measurement types, separated by
	 * a ",";
	 *
	 * Creates and adds a header out of the given parameters.
	 *
	 */
	virtual void addHeader(time_t& time, string& resultTypeList) throw (unsupported_operation, std::runtime_error)=0;
};

#endif /* ETHERNETLOGDISPOSER_H_ */
