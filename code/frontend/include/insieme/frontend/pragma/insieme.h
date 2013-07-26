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

/**
 * The pragma 'insieme mark' is used to mark code regions (i.e. function definitions and code blocks)
 * that will be parsed by the compiler.
 */
class InsiemePragma: public pragma::Pragma {
public:
	InsiemePragma(const clang::SourceLocation& 	startLoc, 
				  const clang::SourceLocation& 	endLoc, 
				  const std::string& 			type, 
				  const pragma::MatchMap& 		mmap);

	static void registerPragmaHandler(clang::Preprocessor& pp);

};


class InsiemeMark: public InsiemePragma {
public:
	InsiemeMark(const clang::SourceLocation& 	startLoc, 
				const clang::SourceLocation& 	endLoc, 
				const std::string& 				type, 
				const pragma::MatchMap& 		mmap)
	: InsiemePragma(startLoc, endLoc, type, mmap) { }

};

class InsiemeIgnore: public InsiemePragma {
public:
	InsiemeIgnore(const clang::SourceLocation&  startLoc, 
				  const clang::SourceLocation&  endLoc, 
				  const std::string& 			type, 
				  const pragma::MatchMap& 		mmap)
		: InsiemePragma(startLoc, endLoc, type, mmap) { }

};

struct InsiemeKernelFile: public InsiemePragma {

    InsiemeKernelFile(const clang::SourceLocation& 	startLoc, 
					  const clang::SourceLocation& 	endLoc, 
					  const std::string& 			type, 
					  const pragma::MatchMap& 		mmap)
		: InsiemePragma(startLoc, endLoc, type, mmap), mmap(mmap) { }

	const std::string getPath() const {
    	assert(mmap.size() == 1 && "Insieme KernelPath pragma cannot have more than one argument");
        return *mmap.begin()->second.front()->get<std::string*>();
	}
private:
	pragma::MatchMap mmap;
};


struct InsiemeDatarange: public InsiemePragma {

    InsiemeDatarange(const clang::SourceLocation& 	startLoc,
					  const clang::SourceLocation& 	endLoc,
					  const std::string& 			type,
					  const pragma::MatchMap& 		mmap)
		: InsiemePragma(startLoc, endLoc, type, mmap), mmap(mmap) { }

    const pragma::MatchMap& getMatchMap() const { return mmap; }

private:
	pragma::MatchMap mmap;
};

void attatchDatarangeAnnotation(const core::StatementPtr& irNode, const clang::Stmt* clangNode,
        frontend::conversion::Converter& convFact);



struct InsiemeLoop: public InsiemePragma {

    InsiemeLoop(const clang::SourceLocation& 	startLoc,
					  const clang::SourceLocation& 	endLoc,
					  const std::string& 			type,
					  const pragma::MatchMap& 		mmap)
		: InsiemePragma(startLoc, endLoc, type, mmap), mmap(mmap) { }

    const pragma::MatchMap& getMatchMap() const { return mmap; }

private:
	pragma::MatchMap mmap;
};

void attatchLoopAnnotation(const core::StatementPtr& irNode, const clang::Stmt* clangNode,
        frontend::conversion::Converter& convFact);



/**
 * InsiemeTransformation: This pragma is utilizied by the user to give transformation hints to the
 * compiler. It can be placed anywhere and node marked with such pragmas will be marked with an
 * annotation which is handled later in the driver right after the frontend is completed. 
 */

unsigned extractIntegerConstant(const pragma::ValueUnionPtr& val);

enum TransformationType { 
	INTERCHANGE,
	STRIP,
	TILE, 
	UNROLL,
	FUSE, 
	SPLIT, 
	STAMP,
	RESCHEDULE,
	PARALLELIZE,
	RSTRIP,
	REC_FUN_UNROLL
};

typedef std::vector<unsigned> ValueVect;

void attach(const clang::SourceLocation& startLoc,
			const clang::SourceLocation endLoc,
			const TransformationType& trans, 
		    const ValueVect& values,
			const core::NodePtr& node, 
			conversion::Converter& fact);

template <TransformationType TransTy>
struct InsiemeTransform : public pragma::Pragma, public pragma::AutomaticAttachable {
	
	typedef ValueVect::const_iterator iterator;
	
	InsiemeTransform(const clang::SourceLocation&  startLoc, 
					 const clang::SourceLocation&  endLoc, 
					 const std::string& 			type, 
					 const pragma::MatchMap& 		mmap) : 
		pragma::Pragma(startLoc, endLoc, type) 
	{ 
		auto fit = mmap.find("values");

		for_each(fit->second, [&](const pragma::ValueUnionPtr& cur) {
			this->values.push_back( extractIntegerConstant(cur) );
		});	
	}
	
	virtual core::NodePtr attachTo(const core::NodePtr& node, conversion::Converter& fact) const {
		attach(getStartLocation(), getEndLocation(), TransTy, values, node, fact);
		return node;
	};

	iterator begin() const { return values.begin(); }
	iterator end() const { return values.end(); }

	TransformationType getTransformationType() const { return TransTy; }

private:
	ValueVect values;
};


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
	
	virtual core::NodePtr attachTo(const core::NodePtr& node, conversion::Converter& fact) const {
		attach(getStartLocation(), getEndLocation(), id, values, node, fact);
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
