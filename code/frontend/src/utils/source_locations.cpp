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

#include "insieme/frontend/utils/source_locations.h"

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/SourceManager.h>
#include <sstream>

using namespace std;
using namespace clang;

namespace insieme {
namespace frontend {
namespace utils {

string FileName(SourceLocation const& l, SourceManager const& sm) {
	PresumedLoc pl = sm.getPresumedLoc(l);
	if (pl.isValid())
		return string(pl.getFilename());
	else
		return string("UNKNOWN FILE");
}

string FileId(SourceLocation const& l, SourceManager const& sm) {
	string fn = FileName(l, sm);
	for(size_t i=0; i<fn.length(); ++i)
		switch(fn[i]) {
			case '/':
			case '\\':
			case '>':
			case '.':
				fn[i] = '_';
		}
	return fn;
}

unsigned Line(SourceLocation const& l, SourceManager const& sm) {
	PresumedLoc pl = sm.getPresumedLoc(l);
	return pl.getLine();
}

std::pair<unsigned, unsigned> Line(clang::SourceRange const& r, SourceManager const& sm) {
	return std::make_pair(Line(r.getBegin(), sm), Line(r.getEnd(), sm));
}

unsigned Column(SourceLocation const& l, SourceManager const& sm) {
	PresumedLoc pl = sm.getPresumedLoc(l);
	return pl.getColumn();
}

std::pair<unsigned, unsigned> Column(clang::SourceRange const& r, SourceManager const& sm) {
	return std::make_pair(Column(r.getBegin(), sm), Column(r.getEnd(), sm));
}

std::string location(clang::SourceLocation const& l, clang::SourceManager const& sm) {
	std::ostringstream ss;
	ss << FileName(l, sm) << ":" << Line(l,sm) << ":" << Column(l,sm);
	return ss.str();
}

} // End util namespace
} // End frontend namespace
} // End insieme namespace
