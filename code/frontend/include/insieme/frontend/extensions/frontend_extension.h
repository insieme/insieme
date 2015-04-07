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

#include <map>
#include <vector>
#include <string>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_program.h"

#include "insieme/frontend/clang_forward.h"
#include "insieme/frontend/utils/stmt_wrapper.h"


namespace insieme {

namespace driver {
namespace cmd {
namespace detail {
    class OptionParser;
}
}
}

namespace frontend {

namespace stmtutils {
    class StmtWrapper;
}

namespace pragma {
    class MatchObject;
    struct node;
}

namespace tu {
    class IRTranslationUnit;
}

namespace conversion {
    class Converter;
}

class ConversionJob;

namespace extensions {

    class PragmaHandler {
    protected:
        typedef std::function<stmtutils::StmtWrapper (const insieme::frontend::pragma::MatchObject&, stmtutils::StmtWrapper)> pragmaHandlerFunction;
        pragmaHandlerFunction f;
        const std::string name;
        const std::string keyw;
        insieme::frontend::pragma::node* tok;
    public:
        PragmaHandler( const std::string& pragmaNamespace,
                        const std::string& keyword,
                        insieme::frontend::pragma::node const& re,
                        std::function<stmtutils::StmtWrapper (const insieme::frontend::pragma::MatchObject&, stmtutils::StmtWrapper)> lambda);
        PragmaHandler( PragmaHandler& pragma );
        PragmaHandler( const PragmaHandler& pragma );
        ~PragmaHandler();
        const std::function<stmtutils::StmtWrapper (const insieme::frontend::pragma::MatchObject&, stmtutils::StmtWrapper)>  getFunction() {
            return f;
        }
        const std::string& getName() const {
            return name;
        }
        const std::string& getKeyword() const {
            return keyw;
        }
        insieme::frontend::pragma::node* getToken();
    };

    /**
     *  This class is the base class for user provided frontend extensions
     *  It basically consists of four stages. The pre clang stage contains
     *  three methods that are called by the insieme frontend to receive
     *  user provided macros, injected headers and headers that should be
     *  kidnapped. The clang stage provides visitors for statement, expressions,
     *  types or declarations. Post clang stage is used to modify the program or
     *  translation unit after the conversion is done. Pragmas can be registered
     *  to support user provided pragma handling.
     */
	class FrontendExtension {
    protected:
        typedef std::map<std::string,std::string> macroMap;
        typedef std::vector<std::string> headerVec;
        typedef std::vector<std::shared_ptr<PragmaHandler>> pragmaHandlerVec;
        pragmaHandlerVec pragmaHandlers;
        macroMap macros;
        headerVec injectedHeaders;
        headerVec kidnappedHeaders;

	public:
        typedef std::function<bool(const ConversionJob&)> flagHandler;

        virtual ~FrontendExtension(){}

        /*****************DRIVER STAGE*****************/
        /**
         *  This method registers the flags in the OptionParser
         *  that are needed to activate and configures the plugin.
         *  @param optParser Reference to the OptionParser
         *  @return lambda that is called after the insiemecc call was parsed
         */
         virtual flagHandler registerFlag(insieme::driver::cmd::detail::OptionParser& optParser);

        /*****************PRE CLANG STAGE*****************/
        /**
         *  Returns the list with user defined macros.
         *  @return macro list
         */
        const macroMap& getMacroList() const;

        /**
         *  Returns the list with headers that should be injected.
         *  @return injected header list
         */
        const headerVec& getInjectedHeaderList() const;

        /**
         *  Returns the list with headers that should be kidnapped
         *  @return kidnapped header list
         */
        const headerVec& getKidnappedHeaderList() const;


        /*****************CLANG STAGE*****************/
        /**
         *  User provided clang expr visitor. Will be called before clang expression
         *  is visited by the insieme visitor. If non nullptr is returned the clang expression
         *  won't be visited by the insieme converter anymore.
         *  @param expr clang expression
         *  @param convFact insieme conversion factory
         *  @return converted clang expression or nullptr if not converted
         */
        virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided clang type visitor. Will be called before clang type
         *  is visited by the insieme visitor. If non nullptr is returned the clang type
         *  won't be visited by the insieme converter anymore.
         *  @param type clang type
         *  @param convFact insieme conversion factory
         *  @return converted clang type or nullptr if not converted
         */
        virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided clang stmt visitor. Will be called before clang stmt
         *  is visited by the insieme visitor. If non empty IR stmt list is
         *  returned the clang stmt won't be visited by the insieme converter anymore.
         *  @param stmt clang stmt
         *  @param convFact insieme conversion factory
         *  @return converted clang stmt or empty stmt list if not converted
         */
        virtual stmtutils::StmtWrapper Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided clang decl visitor. Will be called before clang decl
         *  is visited by the insieme decl visitors. If the extension returns an IR expression
         *  or an IR type the standard visitors are not called. This method mustn't be
         *  overriden, because this method delegates the declaration to the right extension decl visitor.
         *  @param decl clang decl
         *  @param convFact insieme conversion factory
         *  @return NodePtr that can either be an expression or a type
         */
        insieme::core::NodePtr Visit(const clang::Decl* decl, insieme::frontend::conversion::Converter& convFact, bool symbolic=false);

        /**
         *  User provided clang type decl visitor. Will be called before clang type decl
         *  is visited by the insieme type decl visitor. If the extension returns an IR type
         *  the standard visitor is not called.
         *  @param decl clang type decl
         *  @param convFact insieme conversion factory
         *  @return Insieme IR TypePtr
         */
        virtual core::TypePtr TypeDeclVisit(const clang::TypeDecl* decl, insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided clang function decl visitor. Will be called before clang function decl
         *  is visited by the insieme function decl visitor. If the extension returns an IR expression
         *  the standard visitor is not called.
         *  @param decl clang function decl
         *  @param convFact insieme conversion factory
         *  @return Insieme IR ExpressionPtr
         */
        virtual core::ExpressionPtr FuncDeclVisit(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& convFact, bool symbolic=false);

        /**
         *  User provided clang value decl visitor. Will be called before clang value decl
         *  is visited by the insieme value decl visitor. If the extension returns an IR expression
         *  the standard visitor is not called.
         *  @param decl clang value decl
         *  @param convFact insieme conversion factory
         *  @return Insieme IR ExpressionPtr
         */
        virtual core::ExpressionPtr ValueDeclVisit(const clang::ValueDecl* decl, insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided clang expr visitor. Will be called after clang expression
         *  was visited by the insieme visitor. IR code can be modified after standard
         *  conversion took place.
         *  @param expr clang expression
         *  @param irExpr converted clang expression
         *  @param convFact insieme conversion factory
         *  @return modified IR expression or irExpr if no modification should be done
         */
        virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
                                                       insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided clang type visitor. Will be called after clang type
         *  was visited by the insieme visitor. IR code can be modified after standard
         *  conversion took place.
         *  @param type clang type
         *  @param irType converted clang type
         *  @param convFact insieme conversion factory
         *  @return modified IR type or irType if no modification should be done
         */
        virtual insieme::core::TypePtr PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
                                                 insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided clang stmt visitor. Will be called after clang stmt
         *  was visited by the insieme visitor. IR code can be modified after standard
         *  conversion took place.
         *  @param stmt clang stmt
         *  @param irStmt converted clang stmt
         *  @param convFact insieme conversion factory
         *  @return modified IR stmt or irStmt if no modification should be done
         */
        virtual stmtutils::StmtWrapper PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmt,
                                                 insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided post clang decl visitor. Will be called after clang decl
         *  was visited by the insieme visitor and returns a modified IR type or
         *  expression. This method mustn't be overriden because it is used to
         *  delegate the declaration to the right visitor.
         *  @param decl clang decl
         *  @param ir IR NodePtr
         *  @param convFact insieme conversion factory
         *  @return modified version of IR input
         */
        insieme::core::NodePtr PostVisit(const clang::Decl* decl, insieme::core::NodePtr ir,
                                         insieme::frontend::conversion::Converter& convFact, bool symbolic=false);

        /**
         *  User provided post clang type decl visitor. Will be called after clang decl
         *  was visited by the insieme type decl visitor and returns a modified IR type.
         *  @param decl clang type decl
         *  @param type IR TypePtr
         *  @param convFact insieme conversion factory
         *  @return modified version of IR TypePtr
         */
        virtual core::TypePtr TypeDeclPostVisit(const clang::TypeDecl* decl, insieme::core::TypePtr type,
                                                insieme::frontend::conversion::Converter& convFact);

        /**
         *  User provided post clang function decl visitor. Will be called after clang decl
         *  was visited by the insieme function decl visitor and returns a modified IR expression.
         *  @param decl clang function decl
         *  @param type IR ExpressionPtr
         *  @param convFact insieme conversion factory
         *  @return modified version of IR ExpressionPtr
         */
        virtual core::ExpressionPtr FuncDeclPostVisit(const clang::FunctionDecl* decl, insieme::core::ExpressionPtr expr,
                                                      insieme::frontend::conversion::Converter& convFact, bool symbolic=false);

        /**
         *  User provided post clang value decl visitor. Will be called after clang decl
         *  was visited by the insieme value decl visitor and returns a modified IR expression.
         *  @param decl clang value decl
         *  @param type IR ExpressionPtr
         *  @param convFact insieme conversion factory
         *  @return modified version of IR ExpressionPtr
         */
        virtual core::ExpressionPtr ValueDeclPostVisit(const clang::ValueDecl* decl, insieme::core::ExpressionPtr expr,
                                                       insieme::frontend::conversion::Converter& convFact);


        /*****************POST CLANG STAGE*****************/
        /**
         *  User provided IR visitor. Will be called after clang to IR conversion took
         *  place. Takes the whole insieme program (that contains all translation units)
         *  as an argument and returns a modified or non modified program.
         *  @param prog insieme program
         *  @return modified insieme program. If prog is returned no modification is done
         */
        virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog);

        /**
         *  User provided IR visitor. Will be called after clang to IR conversion took
         *  place. Takes one translation units as an argument and returns a modified
         *  or non modified translation unit.
         *  @param tu insieme translation unit
         *  @return modified insieme translation unit. If tu is returned no modification is done
         */
		virtual insieme::frontend::tu::IRTranslationUnit IRVisit(insieme::frontend::tu::IRTranslationUnit& tu);

		/*****************PRAGMA HANDLING*****************/
        /**
         *  Used to retrieve the list of user defined pragma handlers.
         *  @return User defined pragma handler vector
         */
        const pragmaHandlerVec& getPragmaHandlers() const;
	};

}
}
}
