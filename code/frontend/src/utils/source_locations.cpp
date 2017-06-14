/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/core/ir_builder.h"

#include "insieme/frontend/clang.h"

#include <sstream>
#include <iostream>

using namespace std;
using namespace clang;

namespace insieme {
namespace frontend {
namespace utils {

	string FileName(SourceLocation const& l, SourceManager const& sm) {
		if(l.isValid()) {
			return l.printToString(sm);
			//	if (l.isMacroID()){
			//		PresumedLoc pl = sm.getPresumedLoc(l);
			//		if (pl.isValid()){
			//			return string(pl.getFilename());
			//		}
			//	}
			//	return string(l.getFilename());
		}
		return string("UNKNOWN FILE");
	}

	string FileId(SourceLocation const& l, SourceManager const& sm) {
		string fn = FileName(l, sm);
		for(size_t i = 0; i < fn.length(); ++i)
			switch(fn[i]) {
			case '/':
			case '\\':
			case '>':
			case '.': fn[i] = '_';
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
		if(l.isValid()) {
			if(l.isFileID()) {
				// if (sm.isLoadedFileID (sm.getFileID(l))) return "PRELOADED MODULE";
				if(sm.isLoadedSourceLocation(l)) { return "PRELOADED MODULE"; }

				return l.printToString(sm);
			}

			if(l.isMacroID()) {
				// FIXME: what do we do here? Somehow clang fails
                std::cerr << "WARNING: we are not quite confident with source locations in expanded macros.\n" <<
                             " please take note of the current source file to improve expanded macros source location retrieval" <<
                             std::endl;
				/*
				std::cout << "SLoc isMacroID\n";

				auto sl = sm.getSpellingLoc(l);
				if (sm.isLoadedSourceLocation(sl) ) { return "PRELOADED MODULE"; }
				if(sl.isValid()) {
				    return sl.printToString(sm);
				}

				PresumedLoc pl = sm.getPresumedLoc(l);
				if (pl.isValid()){
				    return string(pl.getFilename());
				}
				*/
			}
			return string("UNKNOWN FILE");
		}
		return string("INVALID LOC");
	}

	clang::SourceLocation getExpansionLoc(const clang::SourceManager& sm, clang::SourceLocation loc) {
		if(sm.isMacroArgExpansion(loc)) { loc = sm.getExpansionLoc(loc); }
		return loc;
	}

	core::annotations::Location convertClangSrcLoc(core::NodeManager& man, const clang::SourceManager& sm, clang::SourceLocation start,
	                                               clang::SourceLocation end) {
		// check file validity
		FileID&& fileId = sm.getFileID(start);
		if(fileId.isInvalid()) return core::annotations::Location::getShared();
		const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
		// if we cannot get the file entry, lets try to get the source filename directly
		std::string filename;
		if(!fileEntry) {
			StringRef s = sm.getFilename(start);
			filename = s.str();
		} else {
			assert_true(fileEntry);
			filename = fileEntry->getName();
		}
		// update macro locations, if required
		start = getExpansionLoc(sm, start);
		end = getExpansionLoc(sm, end);
		// generate location object
		core::IRBuilder builder(man);
		return core::annotations::Location(builder.stringValue(filename.c_str()),
		                                   core::annotations::TextPosition(sm.getSpellingLineNumber(start), sm.getSpellingColumnNumber(start)),
		                                   core::annotations::TextPosition(sm.getSpellingLineNumber(end), sm.getSpellingColumnNumber(end)));
	}

	const core::NodePtr& attachLocationFromClang(const core::NodePtr& node, const clang::SourceManager& sm, clang::SourceLocation start,
	                                             clang::SourceLocation end) {
		core::annotations::Location l = convertClangSrcLoc(node.getNodeManager(), sm, start, end);
		return core::annotations::attachLocation(node, l);
	}

} // End util namespace
} // End frontend namespace
} // End insieme namespace
