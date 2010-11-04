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

#pragma once

#include "location.h"
#include "program.h"

#include <fstream>
#include <limits>

// #include <boost/filesystem.hpp>

namespace insieme {
namespace backend {

using namespace insieme::c_info;
using namespace insieme::core;

class CodeModification: public boost::less_than_comparable<SourceLocation, SourceLocation> {
public:
	enum ModificationType { INSERT, REMOVE, REPLACE };

	// this operation is needed by the sort algorithm in order to order the modification hints
	// accordingly with the file name and their location.
	bool operator<(const CodeModification& other) const {
		if(fileName == other.fileName) {
			assert((locEnd <= other.locStart || locStart >= other.locEnd) && "Overlapping code modification hints.");
			return locStart < other.locStart;
		}
		// order the modification hints accordingly to the file name
		return fileName < other.fileName;
	}

	bool operator==(const CodeModification& other) const {
		return fileName == other.fileName && locStart == other.locStart && locEnd == other.locEnd && type == other.type;
	}

	const std::string getFileName() const { return fileName; }
	const SourceLocation getStartLoc() const { return locStart; }
	const SourceLocation getEndLoc() const { return locEnd; }
	const ModificationType getType() const { return type; }

	const std::string getCode() const { return code; }

	static CodeModification createInsertion(const SourceLocation& locStart, const std::string& insertionCode) {
		return CodeModification(locStart, SourceLocation(), insertionCode, INSERT);
	}
	static CodeModification createRemoval(const SourceLocation& locStart, const SourceLocation& locEnd) {
		return CodeModification(locStart, locEnd, std::string(), REMOVE);
	}
	static CodeModification createReplacement(const SourceLocation& locStart, const SourceLocation& locEnd, const std::string& insertionCode);

private:
	const std::string 		fileName;
	const SourceLocation 	locStart;
	const SourceLocation 	locEnd;
	const std::string 		code;
	const ModificationType	type;

	CodeModification(const SourceLocation& locStart, const SourceLocation& locEnd, const std::string& code, const ModificationType& type) :
			fileName(locStart.getFileName()), locStart(locStart), locEnd(locEnd), code(code), type(type) {
		assert(locStart.isValid());
		if(locEnd.isValid())
			assert(locStart.getFileName() == fileName && locEnd.getFileName() == fileName);
	}
};

typedef std::set<CodeModification> CodeModificationList;

class Rewriter {
	const ProgramPtr program;

	// rewrite code modifications belonging to the same file
	void rewriterFile(CodeModificationList::const_iterator& modIt, const CodeModificationList::const_iterator& end) {
		const std::string currFile = modIt->getFileName();
		std::fstream in(currFile.c_str(), std::fstream::in);
		// path srcFile(currFile);
		// std::string insimeSrc = srcFile.filename() + ".insieme" + srcFile.extension();
		std::string insiemeSrc = currFile + ".insieme";
		std::fstream out(insiemeSrc.c_str(), std::fstream::out | std::fstream::trunc);
		char line[32768]; // FIXME: is this enough
		size_t lineNo = 1;
		while(modIt != end && modIt->getFileName() == currFile) {
			while(lineNo < modIt->getStartLoc().getLine()) {
				// copy input file lines to output file
				in.getline(line, std::numeric_limits<std::streamsize>::max());
				out.write(line, in.gcount()-1); // the -1 represent the new-line characther
				out << std::endl;
				lineNo++;
			}
			// todo: copy until the column
			// do the modification
			if(modIt->getType() == CodeModification::REMOVE) {
				// SKIP LINES
				in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
				while(lineNo++ <= modIt->getEndLoc().getLine())
					in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
			} else if (modIt->getType() == CodeModification::INSERT) {
				// INSERT CODE
				out.write(modIt->getCode().c_str(), modIt->getCode().size());
				out << std::endl;
			}
			++modIt; // get the next modification
		}
		// finish to write back the current opened file
		while(!in.eof()) {
			in.getline(line, std::numeric_limits<std::streamsize>::max());
			out.write(line, in.gcount()-1); // the -1 represent the new-line characther
			out << std::endl;
		}
		in.close();
		out.close();
	}

public:
	Rewriter(const ProgramPtr& program, const std::string& insiemeFileName = "insieme.c") : program(program) { }

	void operator()() {
//		using namespace boost::filesystem;

		CodeModificationList list;

		std::for_each(program->getEntryPoints().begin(),  program->getEntryPoints().end(),
			[ &list ](const core::ExpressionPtr& curr) {
				// we expect entry point to have annotations with corresponding source locations
				std::shared_ptr<insieme::c_info::CLocAnnotation>&& locAnn = curr.getAnnotation(insieme::c_info::CLocAnnotation::KEY);
				assert(locAnn && "Entry point has not source location annotation, impossible to rewrite it back in the correct location");

				// in case of function decl we wipe out the old definition from the source file
				list.insert( CodeModification::createRemoval(locAnn->getStartLoc(), locAnn->getEndLoc()) );

				// the removed function definition has to be defined as extern now because it will be written in the insieme file
				list.insert( CodeModification::createInsertion( SourceLocation(locAnn->getStartLoc().getFileName(), 15, 0), "extern f();") );
		});

		if(list.empty())
			return;

		CodeModificationList::const_iterator it = list.begin();
		while(it != list.end())
			rewriterFile(it, list.end());
	}

};



} // End backend namespace
} // End insieme namespace
