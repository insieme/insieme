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

#include "rewrite.h"
#include "logging.h"

#include <fstream>
#include <limits>

namespace {

using namespace insieme::backend;

// Applies modification hints to the target file.
void rewriterFile(Rewriter::CodeModificationList::const_iterator& modIt, const Rewriter::CodeModificationList::const_iterator& end) {

	const std::string currFile = modIt->getFileName();
	std::fstream in(currFile.c_str(), std::fstream::in);
	// path srcFile(currFile);
	// std::string insimeSrc = srcFile.filename() + ".insieme" + srcFile.extension();
	std::string insiemeSrc = currFile + ".insieme";
	std::fstream out(insiemeSrc.c_str(), std::fstream::out | std::fstream::trunc);

	char line[32768]; // FIXME: is 32768 this enough?

	auto copyLine = [&in, &out, &line] () {
		in.getline(line, std::numeric_limits<std::streamsize>::max());
		out.write(line, in.gcount()-1); // the -1 represent the new-line characther
		out << std::endl;
	};

	size_t lineNo = 1;
	auto insertCode = [ &out ](const std::string& code) {
		out.write(code.c_str(), code.size());
		out << std::endl;
	};

	auto skipLines = [ &in, &lineNo ] (size_t end) {
		in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		while(lineNo++ <= end) {
			in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		}
	};

	while(modIt != end && modIt->getFileName() == currFile) {
		while(lineNo < modIt->getStartLoc().getLine()) {
			// copy input file lines to output file
			copyLine();
			lineNo++;
		}
		// todo: copy until the column
		// do the modification
		switch(modIt->getType()) {
		case Rewriter::CodeModification::REMOVE:
			skipLines( modIt->getEndLoc().getLine() );
			break;
		case Rewriter::CodeModification::INSERT:
			insertCode( modIt->getCode().c_str() );
			break;
		case Rewriter::CodeModification::REPLACE:
			insertCode(modIt->getCode().c_str());
			skipLines(modIt->getEndLoc().getLine());
			break;
		}
		++modIt; // get the next modification
	}
	// finish to write back the current opened file
	while(!in.eof())
		copyLine();

	in.close();
	out.close();
}

} // end anonymous namespace

namespace insieme {
namespace backend {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							CodeModification
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Rewriter::CodeModification::CodeModification(const SourceLocation& locStart, const SourceLocation& locEnd,
	const std::string& code, const ModificationType& type):
	fileName(locStart.getFileName()), locStart(locStart), locEnd(locEnd), code(code), type(type) {
	assert(locStart.isValid());
	if(locEnd.isValid())
		assert(locStart.getFileName() == fileName && locEnd.getFileName() == fileName);
}

bool Rewriter::CodeModification::operator<(const CodeModification& other) const {
	if(fileName == other.fileName) {
		assert((locEnd <= other.locStart || locStart >= other.locEnd) && "Overlapping code modifications.");
		return locStart < other.locStart;
	}
	// order the modification hints accordingly to the file name
	return fileName < other.fileName;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								Rewriter
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void Rewriter::writeBack(const ProgramPtr& program, const std::string& insiemeFileName) {
//	using namespace boost::filesystem;

	CodeModificationList list;
	std::for_each(program->getEntryPoints().begin(),  program->getEntryPoints().end(),
		[ &list ](const core::ExpressionPtr& curr) {
			// we expect entry point to have annotations with corresponding source locations
			std::shared_ptr<insieme::c_info::CLocAnnotation>&& locAnn = curr.getAnnotation(insieme::c_info::CLocAnnotation::KEY);
			assert(locAnn && "Entry point has not source location annotation, impossible to rewrite it back in the correct location");

			// in case of function decl we wipe out the old definition from the source file
			list.insert( CodeModification::createRemoval(locAnn->getStartLoc(), locAnn->getEndLoc()) );

			if( locAnn->isFunctionDefinition() ) {
				// the removed function definition has to be defined as extern now because it will be written in the insieme file
				list.insert( CodeModification::createInsertion( locAnn->getStartLoc(), "extern f();") );
				return;
			}
			// else we are replacing a code region, a function call has to be created
//			std::shared_ptr<insieme::c_info::CNameAnnotation>&& nameAnn = curr.getAnnotation(insieme::c_info::CNameAnnotation::KEY);
//			assert(nameAnn && "No name associated to this lambda expr");
//			name->get
	});

	if(list.empty()) {
		LOG(WARNING) << "No entry point has been written (check for missing source location annotations).";
		return;
	}

	CodeModificationList::const_iterator it = list.begin();
	while(it != list.end()) {
		DVLOG(1) << "Rewriting compilation unit: " << it->getFileName();
		rewriterFile(it, list.end());
	}
}

} // end backend namespace
} // end insieme namespace
