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

#include "insieme/core/types.h"
#include "insieme/core/expressions.h"

#include "clang/AST/DeclVisitor.h"
#include "clang/AST/RecursiveASTVisitor.h"

#include <set>
#include <map>
#include <stack>

namespace clang {
namespace idx {
class Indexer;
} // end idx namespace
}

namespace insieme {
namespace frontend {

namespace conversion {
class ConversionFactory;
}

namespace analysis {

class GlobalVarCollector : public clang::RecursiveASTVisitor<GlobalVarCollector> {
public:
	typedef std::map<const clang::VarDecl*, std::pair<bool, bool>> GlobalVarVect;
	typedef std::set<const clang::FunctionDecl*> VisitedFuncSet;

	typedef std::stack<const clang::FunctionDecl*> FunctionStack;
	typedef std::set<const clang::FunctionDecl*> UseGlobalFuncMap;

	GlobalVarCollector(clang::idx::Indexer& indexer, UseGlobalFuncMap& globalFuncMap) : indexer(indexer), usingGlobals(globalFuncMap) { }
	bool VisitVarDecl(clang::VarDecl* decl);
	bool VisitDeclRefExpr(clang::DeclRefExpr* decl);
	bool VisitCallExpr(clang::CallExpr* callExpr);

	void operator()(const clang::Decl* decl);

	const GlobalVarVect& getGlobals() const { return globals; }
	const UseGlobalFuncMap& getUsingGlobals() const { return usingGlobals; }

	void dump(std::ostream& out) const ;

	std::pair<core::StructTypePtr, core::StructExprPtr> createGlobalStruct(const conversion::ConversionFactory& fact) const;

private:
	GlobalVarVect globals;
	VisitedFuncSet visited;
	FunctionStack	  funcStack;

	clang::idx::Indexer& indexer;
	UseGlobalFuncMap& usingGlobals;
};

} // end analysis namespace
} // end frontend namespace
} // end insieme namespace

namespace std {
std::ostream& operator<<(std::ostream& out, const insieme::frontend::analysis::GlobalVarCollector& globals);
} // end std namespace
