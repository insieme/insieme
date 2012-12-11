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

#include "insieme/frontend/pragma/handler.h"

#include "insieme/frontend/convert.h"

#include "clang/AST/Stmt.h"
#include <llvm/Support/raw_ostream.h>
#include <clang/AST/Expr.h>

using namespace clang;
using namespace insieme::frontend;

namespace {

std::string loc2string(const clang::SourceLocation& loc, const clang::SourceManager& sm) {
	std::string str;
	llvm::raw_string_ostream ss(str);
	loc.print(ss,sm);
	return ss.str();
}

} // End empty namespace

namespace insieme {
namespace frontend {
namespace pragma {

void Pragma::setStatement(clang::Stmt const* stmt) {
	assert(mTargetNode.isNull() && "Pragma already associated with an AST node");
	mTargetNode = stmt;
}

void Pragma::setDecl(clang::Decl const* decl) {
	assert(mTargetNode.isNull() && "Pragma already associated with an AST node");
	mTargetNode = decl;
}

clang::Stmt const* Pragma::getStatement() const {
	assert(!mTargetNode.isNull() && isStatement());
	return mTargetNode.get<clang::Stmt const*> ();
}

clang::Decl const* Pragma::getDecl() const {
	assert(!mTargetNode.isNull() && isDecl());
	return mTargetNode.get<clang::Decl const*> ();
}

std::string Pragma::toStr(const clang::SourceManager& sm) const {
	std::ostringstream ss;
	ss << "(" << loc2string(getStartLocation(), sm) << ", " << loc2string(getEndLocation(), sm) << "),\n\t";
	
	ss << (isStatement() ? "Stmt -> " : "Decl -> ") << "(";

	if(isStatement() && getStatement())
		ss << loc2string(getStatement()->getLocStart(), sm) << ", " <<
			  loc2string(getStatement()->getLocEnd(), sm);
	else if(isDecl() && getDecl())
		ss << loc2string(getDecl()->getLocStart(), sm) << ", " <<
			  loc2string(getDecl()->getLocEnd(), sm);
	ss << ")";
	return ss.str();
}

void Pragma::dump(std::ostream& out, const clang::SourceManager& sm) const {
	out << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" <<
		   "|~> Pragma: " << getType() << " -> " << toStr(sm) << "\n";
}


core::NodePtr attachPragma( const core::NodePtr& 			node, 
						   const clang::Stmt* 				clangNode, 
						   conversion::ConversionFactory& 	fact ) 
{
	const PragmaStmtMap::StmtMap& pragmaStmtMap = fact.getPragmaMap().getStatementMap();

	typedef PragmaStmtMap::StmtMap::const_iterator PragmaStmtIter; 

	// Get the list of pragmas attached to the clang node
	std::pair<PragmaStmtIter, PragmaStmtIter>&& iter = pragmaStmtMap.equal_range(clangNode);

	core::NodePtr ret = node;
	std::for_each(iter.first, iter.second,
		[&] (const PragmaStmtMap::StmtMap::value_type& curr) {
			if(const AutomaticAttachable* pragma = dynamic_cast<const AutomaticAttachable*>( &*(curr.second) )) {
				ret = pragma->attachTo(node, fact);
				return;
			}
	});

	return ret;
}

core::NodePtr attachPragma(const core::NodePtr& 			node, 
						   const clang::Decl* 				clangDecl, 
						   conversion::ConversionFactory& 	fact ) 
{
	const PragmaStmtMap::DeclMap& pragmaDeclMap = fact.getPragmaMap().getDeclarationMap();

	typedef PragmaStmtMap::DeclMap::const_iterator PragmaDeclIter; 

	// Get the list of pragmas attached to the clang node
	std::pair<PragmaDeclIter, PragmaDeclIter>&& iter = pragmaDeclMap.equal_range(clangDecl);

	core::NodePtr ret = node;
	std::for_each(iter.first, iter.second,
		[&] (const PragmaStmtMap::DeclMap::value_type& curr) {
			if(const AutomaticAttachable* pragma = dynamic_cast<const AutomaticAttachable*>( &*(curr.second) )) {
				ret = pragma->attachTo(node, fact);
				return;
			}
	});

	return ret;
}

} // End pragma namespace
} // End frontend namespace
} // End insieme namespace
