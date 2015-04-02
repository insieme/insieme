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

#ifndef PMMEMORYPARSER_H_
#define PMMEMORYPARSER_H_

#include "EthernetParser.h"
#include <string>
#include <list>
#include <sstream>

using std::list;
using std::string;
using std::stringstream;

/*
 * This class extends EthernetParser.
 *
 * Unlike EthernetParser on adding values no indexes are assigned to them.
 *
 * Values get added just like they are, without changes.
 *
 */
class PMMemoryParser: public EthernetParser {
public:

	PMMemoryParser();

	virtual ~PMMemoryParser();

	/*
	 * @param value - value string to add
	 *
	 * Splits the given string into the single lines and adds
	 * each single linte to valueVector, if the max number of
	 * entries gets exceeded by adding the new elements,
	 * the first elements get deleted.
	 *
	 * @Throws an invalid_argument exception if no header was set
	 * before.
	 *
	 * @see addHeader(string & header);
	 */
	virtual void addValueLine(string & value) throw (std::invalid_argument);

	int countLines(bool force);

};

#endif /* PMMEMORYPARSER_H_ */
