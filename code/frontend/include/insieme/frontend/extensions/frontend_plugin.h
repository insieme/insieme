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
    class FunctionDecl;
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
     *  This class is the base class for user provided frontend plugins
     *  It basically consists of four stages. The pre clang stage contains
     *  three methods that are called by the insieme frontend to receive
     *  user provided macros, injected headers and headers that should be
     *  kidnapped. The clang stage provides visitors for statement, expressions,
     *  types or declarations. Post clang stage is used to modify the program or
     *  translation unit after the conversion is done. Pragmas can be registered
     *  to support user provided pragma handling.
     */
	class FrontendPlugin {
    protected:
        typedef std::map<std::string,std::string> macroMap;
        typedef std::vector<std::string> headerVec;
        macroMap macros;
        headerVec injectedHeaders;
        headerVec kidnappedHeaders;

	public:
        virtual ~FrontendPlugin(){}
        // ############ PRE CLANG STAGE ############ //
        const macroMap& getMacroList() const;
        const headerVec& getInjectedHeaderList() const;
        const headerVec& getKidnappedHeaderList() const;
        // ############ CLANG STAGE ############ //
        virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& convFact);
        virtual insieme::core::TypePtr Visit(const clang::Type* type, insieme::frontend::conversion::Converter& convFact);
        virtual stmtutils::StmtWrapper Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& convFact);
        virtual bool Visit(const clang::Decl* decl, insieme::frontend::conversion::Converter& convFact);

        virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
                                                       insieme::frontend::conversion::Converter& convFact);
        virtual insieme::core::TypePtr PostVisit(const clang::Type* type, const insieme::core::TypePtr& irType,
                                                 insieme::frontend::conversion::Converter& convFact);
        virtual stmtutils::StmtWrapper PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmt,
                                                 insieme::frontend::conversion::Converter& convFact);
        virtual void PostVisit(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& convFact);

        // ############ POST CLANG STAGE ############ //
		virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog);
		virtual insieme::frontend::tu::IRTranslationUnit IRVisit(insieme::frontend::tu::IRTranslationUnit& tu);
	};

}
}
}
