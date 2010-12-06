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

#include "insieme/simple_backend/rewrite.h"
#include "insieme/utils/logging.h"

#include <algorithm>
#include <fstream>
#include <limits>

#include "insieme/simple_backend/backend_convert.h"

namespace {
using namespace insieme::utils::log;
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

	auto copyLine = [&in, &out, &line] () -> size_t {
		in.getline(line, std::numeric_limits<std::streamsize>::max());
		size_t readCount = in.gcount()-1;
		out.write(line, readCount); // the -1 represent the new-line characther
		out << std::endl;
		return readCount;
	};

	size_t lineNo = 1;
	auto insertCode = [ &out ](const std::string& code) {
		out.write(code.c_str(), code.size());
		out << std::endl;
	};

	// skip lines until line end, while end-1 lines are ignored, the last line
	// is actually read in order to handle insertion in the center of a line
	auto skipLines = [ &in, &lineNo, &line ] (size_t end) -> size_t {
		// in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		while(lineNo++ < end)
			in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');

		in.getline(line, std::numeric_limits<std::streamsize>::max());
		return in.gcount()-1;
	};

	while(modIt != end && modIt->getFileName() == currFile) {
		size_t columns=0;
		while(lineNo < modIt->getStartLoc().getLine()) {
			// copy input file lines to output file
			columns = copyLine();
			lineNo++;
		}
		// copy until column
		if(columns)
			out.write(line, modIt->getStartLoc().getColumn()-1);

		// todo: copy until the column
		// do the modification
		switch(modIt->getType()) {
		case Rewriter::CodeModification::REMOVE:
			columns = skipLines( modIt->getEndLoc().getLine() );
			break;
		case Rewriter::CodeModification::INSERT:
			insertCode( modIt->getCode() );
			break;
		case Rewriter::CodeModification::REPLACE:
			insertCode( modIt->getCode() );
			columns = skipLines( modIt->getEndLoc().getLine() );
			break;
		}

		// copy from column to the end of the line
		if(columns) {
			size_t endLoc = std::min(modIt->getEndLoc().getColumn() + 1, columns);
			insertCode( std::string(&line[endLoc], columns-endLoc) );
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

	CodeModificationList list;
	const Program::EntryPointSet& entryPoints = program->getEntryPoints();
	std::for_each(entryPoints.begin(), entryPoints.end(),
		[ &list ](const core::ExpressionPtr& curr) {
			// we expect entry point to have annotations with corresponding source locations
			std::shared_ptr<insieme::c_info::CLocAnnotation>&& locAnn = curr->getAnnotation(insieme::c_info::CLocAnnotation::KEY);
			assert(locAnn && "Entry point has not source location annotation, impossible to rewrite it back in the correct location");

			// in case of function decl we wipe out the old definition from the source file
			list.insert( CodeModification::createRemoval(locAnn->getStartLoc(), locAnn->getEndLoc()) );

			if( locAnn->isFunctionDefinition() ) {
				// the removed function definition has to be defined as extern now because it will be written in the insieme file
				list.insert( CodeModification::createInsertion( locAnn->getStartLoc(), "extern f();") ); // FIXME
				return;
			}
			// else we are replacing a code region, a function call has to be created
//			std::shared_ptr<insieme::c_info::CNameAnnotation>&& nameAnn = curr.getAnnotation(insieme::c_info::CNameAnnotation::KEY);
//			assert(nameAnn && "No name associated to this lambda expr");
	});

	if(list.empty()) {
		LOG(WARNING) << "No entry point has been written (check for missing source location annotations).";
		return;
	}

	// Rewrite the original files with the new modifications
	CodeModificationList::const_iterator it = list.begin();
	while(it != list.end()) {
		VLOG(1) << "==== Rewriting compilation unit: " << it->getFileName() << " =================";
		rewriterFile(it, list.end());
	}

	VLOG(1) << "==== Writing insieme file : " << insiemeFileName << " =================";
	// Write the insieme file contaning the insieme handled code
	insieme::simple_backend::ConversionContext convCtx(program);
	auto converted = convCtx.convert(program);
	std::fstream outFile(insiemeFileName.c_str(), std::fstream::out | std::fstream::trunc);
	outFile << converted;

	VLOG(1) << "Write back completed!";
}

} // end backend namespace
} // end insieme namespace
