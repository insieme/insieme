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

#include <map>
#include <vector>
#include <string>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_program.h"

namespace clang {
    class Expr;
    class Stmt;
    class Decl;
    class Type;
}

namespace stmtutils {
    class StmtWrapper;
}

namespace insieme {
namespace frontend {

namespace tu {
    class IRTranslationUnit;
}

namespace conversion {
    class Converter;
}

namespace extensions {

    /**
     *  This class is the base class for user provided clang stage plugins
     *  It basically consists of three visitor methods. The user can implement one
     *  or more of the methods to provide a visitor for clang types,
     *  expression or statements.
     */
	class FrontendPlugin {
    protected:
        typedef std::map<std::string,std::string> macroMap;
        macroMap macros;
        typedef std::vector<std::string> headerVec;
        headerVec injectedHeaders;
        headerVec kidnappedHeaders;

	public:
		virtual ~FrontendPlugin(){}
		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& convFact);
        virtual insieme::core::TypePtr Visit(const clang::Type* type, insieme::frontend::conversion::Converter& convFact);
		virtual stmtutils::StmtWrapper Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& convFact);
		virtual bool Visit(const clang::Decl* decl, insieme::frontend::conversion::Converter& convFact);
		const macroMap& getMacroList() const;
		const headerVec& getInjectedHeaderList() const;
		const headerVec& getKidnappedHeaderList() const;
		virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog);
		virtual insieme::frontend::tu::IRTranslationUnit IRVisit(insieme::frontend::tu::IRTranslationUnit& tu);
	};

}
}
}
