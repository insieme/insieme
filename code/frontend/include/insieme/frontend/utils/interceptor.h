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

#include "insieme/core/ir_builder.h"
#include "insieme/utils/map_utils.h"

#include "insieme/frontend/utils/indexer.h"

#include "clang/AST/Decl.h"
#include "clang/AST/StmtVisitor.h"

#include <boost/regex.hpp>
#include <map>

namespace insieme {
namespace frontend { 

namespace conversion {
class ConversionFactory;
}

namespace utils {

class Interceptor {
public:
	Interceptor(
			insieme::core::NodeManager& mgr,
			insieme::frontend::utils::Indexer& indexer)
		: indexer(indexer), builder(mgr)
	{}

	typedef std::set<const clang::Decl*> InterceptedDeclSet;
	typedef std::set<const clang::TypeDecl*> InterceptedTypeDeclSet;

	typedef std::map<const clang::FunctionDecl*, std::string> InterceptedFuncMap;
	typedef std::map<const clang::FunctionDecl*, insieme::core::ExpressionPtr> InterceptedExprCache;
	typedef std::map<const clang::Type*, insieme::core::TypePtr> InterceptedTypeCache;

	InterceptedDeclSet& getInterceptedDecls() { return interceptedDecls; }
	InterceptedFuncMap& getInterceptedFuncMap() { return interceptedFuncMap; }

	void loadConfigFile(std::string fileName);	
	void loadConfigSet(std::set<std::string> toIntercept);	
	void intercept();
	
	bool isIntercepted(const clang::Decl* decl) const { return interceptedDecls.find(decl) != interceptedDecls.end(); }

	InterceptedExprCache buildInterceptedExprCache(insieme::frontend::conversion::ConversionFactory& convFact);
	Interceptor::InterceptedTypeCache buildInterceptedTypeCache(insieme::frontend::conversion::ConversionFactory& convFact);

private:
	insieme::frontend::utils::Indexer& indexer;
	insieme::core::IRBuilder builder;
	InterceptedDeclSet interceptedDecls;
	InterceptedFuncMap interceptedFuncMap;
	InterceptedTypeDeclSet interceptedTypes;

	std::set<std::string> toIntercept;
};

struct InterceptVisitor : public clang::StmtVisitor<InterceptVisitor> {

	insieme::frontend::utils::Interceptor::InterceptedDeclSet& interceptedDecls;
	insieme::frontend::utils::Interceptor::InterceptedFuncMap& interceptedFuncMap;
	insieme::frontend::utils::Interceptor::InterceptedTypeDeclSet& interceptedTypes;
	std::set<std::string>& toIntercept;
	boost::regex rx;

	InterceptVisitor(
			insieme::frontend::utils::Interceptor::InterceptedDeclSet& interceptedDecls, 
			insieme::frontend::utils::Interceptor::InterceptedFuncMap& interceptedFuncMap,
			insieme::frontend::utils::Interceptor::InterceptedTypeDeclSet& interceptedTypes,
			std::set<std::string>& toIntercept) 
		: interceptedDecls(interceptedDecls), 
		interceptedFuncMap(interceptedFuncMap), 
		interceptedTypes(interceptedTypes),
		toIntercept(toIntercept)
	{}

	void intercept(const clang::FunctionDecl* d, boost::regex rx);
	
	void VisitStmt(clang::Stmt* stmt);

	void VisitDeclStmt(const clang::DeclStmt* declStmt);

	//void VisitCallExpr(const clang::CallExpr* callExpr) {};

	//void VisitDeclRefExpr(const clang::DeclRefExpr* declRefExpr) {};
}

;} // end utils namespace
} // end frontend namespace
} // end insieme namespace
