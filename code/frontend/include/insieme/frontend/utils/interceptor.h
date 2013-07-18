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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include <clang/AST/Decl.h>
#include <clang/AST/StmtVisitor.h>
#include <clang/AST/TypeVisitor.h>
#include <clang/AST/DeclTemplate.h>
#pragma GCC diagnostic pop

#include <boost/filesystem.hpp>

#include "insieme/core/ir_builder.h"
#include "insieme/utils/map_utils.h"

#include "insieme/frontend/utils/indexer.h"

#include <boost/regex.hpp>
#include <map>

namespace insieme {
namespace frontend { 

namespace conversion {
class Converter;
}

namespace utils {

class Interceptor {
public:
	Interceptor(
			insieme::core::NodeManager& mgr,
			insieme::frontend::utils::Indexer& indexer,
			const vector<boost::filesystem::path>& stdLibDirs)
		: indexer(indexer), builder(mgr), stdLibDirs(stdLibDirs), 
		
		// by default intercept std:: and __gnu_cxx:: namespaces
		// __gnu_cxx is needed for the iterator of std::vector for example
		toIntercept( { "std::.*", "__gnu_cxx::.*" } ), rx("("+toString(join(")|(", toIntercept))+")")
	{}

	void loadConfigFile(std::string fileName);	
	void loadConfigSet(std::set<std::string> toIntercept);	
	
	bool isIntercepted(const string& name) const;
	bool isIntercepted(const clang::Type* type) const;
	bool isIntercepted(const clang::FunctionDecl* decl) const;

	insieme::core::TypePtr intercept(const clang::Type* type, insieme::frontend::conversion::Converter& convFact);
	insieme::core::ExpressionPtr intercept(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& convFact);

	insieme::frontend::utils::Indexer& getIndexer() const {
		return indexer;
	}

	const vector<boost::filesystem::path>& getStdLibDirectories() const {
		return stdLibDirs;
	}

private:
	insieme::frontend::utils::Indexer& indexer;
	insieme::core::IRBuilder builder;

	const vector<boost::filesystem::path>& stdLibDirs;

	std::set<std::string> toIntercept;
	boost::regex rx;
};

struct InterceptTypeVisitor : public clang::TypeVisitor<InterceptTypeVisitor, core::TypePtr> {

	insieme::frontend::conversion::Converter& convFact;
	const insieme::core::IRBuilder& builder;
	const Interceptor& interceptor;
	
	InterceptTypeVisitor(insieme::frontend::conversion::Converter& convFact, const Interceptor& interceptor);

	//core::TypePtr VisitTypedefType(const clang::TypedefType* typedefType);
	core::TypePtr VisitTagType(const clang::TagType* tagType);
	core::TypePtr VisitTemplateSpecializationType(const clang::TemplateSpecializationType* templTy);
	core::TypePtr VisitTemplateTypeParmType(const clang::TemplateTypeParmType* templParmType);
	core::TypePtr Visit(const clang::Type* type);
};

} // end utils namespace
} // end frontend namespace
} // end insieme namespace
