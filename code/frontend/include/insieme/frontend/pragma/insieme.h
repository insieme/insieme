/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <string>
#include "insieme/frontend/pragma/handler.h"
#include "insieme/core/ir_node.h"

namespace clang {
class Preprocessor;
}

namespace insieme {
namespace frontend {
namespace conversion {

class Converter;

} // end convert namespace

/**
 * Custom pragma used for testing purposes;
 *
 * #pragma test "insieme-IR"
 * C stmt
 *
 * checks if the conversion of the C statement matches the one specified by the user
 */
class TestPragma: public pragma::Pragma {
	std::string expected;

public:
	TestPragma(const clang::SourceLocation& startLoc, 
			   const clang::SourceLocation& endLoc, 
			   const std::string&			type, 	
			   const pragma::MatchMap& 		mmap);

	std::string getExpected() const { return expected; }

	static void registerPragmaHandler(clang::Preprocessor& pp);
};

unsigned extractIntegerConstant(const pragma::ValueUnionPtr& val);

typedef std::vector<std::string> StrValueVect;

void attach(const clang::SourceLocation& startLoc,
			const clang::SourceLocation endLoc,
			unsigned id,
		    const StrValueVect& values,
			const core::NodePtr& node, 
			conversion::Converter& fact);

struct InsiemeInfo : public pragma::Pragma, public pragma::AutomaticAttachable {
	
	typedef StrValueVect::const_iterator iterator;

	InsiemeInfo(const clang::SourceLocation&  startLoc, 
				const clang::SourceLocation&  endLoc, 
				const std::string& 				type, 
				const pragma::MatchMap& 		mmap) : 
		pragma::Pragma(startLoc, endLoc, type) 
	{ 
		{
			auto fit = mmap.find("id");
			assert(fit != mmap.end() && fit->second.size() == 1 && "Id not present in pragma info");
			id = extractIntegerConstant(fit->second.front());
		}
		{
			auto fit = mmap.find("values");
			for_each(fit->second, [&](const pragma::ValueUnionPtr& cur) {
				this->values.push_back( *cur->get<std::string*>() );
			});
		}
	}
	
	virtual stmtutils::StmtWrapper attachTo(const stmtutils::StmtWrapper& node, conversion::Converter& fact) const {
        for(auto element : node) {
    		attach(getStartLocation(), getEndLocation(), id, values, element, fact);
        }
		return node;
	};

	iterator begin() const { return values.begin(); }
	iterator end() const { return values.end(); }

private:
	unsigned id;
	StrValueVect values;
};

} // End frontend namespace
} // End insieme namespace
