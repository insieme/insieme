/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

#include <memory>
#include <sstream>
#include <map>
#include <functional>

#include "insieme/core/forward_decls.h"

#include "insieme/frontend/sema.h"
#include "insieme/frontend/pragma/matcher.h"
#include "insieme/frontend/utils/stmt_wrapper.h"
#include "insieme/frontend/clang_forward.h"

#include <clang/Basic/SourceLocation.h>
#include <clang/Lex/Pragma.h>

#include <clang/Parse/Parser.h>

// forward declaration
namespace insieme {

namespace frontend {

namespace stmtutils {
	struct StmtWrapper;
}

namespace conversion {
	class Converter;
} // end convert namespace

namespace pragma {

	// ------------------------------------ Pragma ---------------------------
	/**
	 * Defines a generic pragma which contains the location (start,end), and the
	 * target node
	 */
	class Pragma {
		friend class insieme::frontend::InsiemeSema;

	  public:
		/**
		 * Type representing the target node which could be wither a statement
		 * or a declaration
		 */
		typedef llvm::PointerUnion<clang::Stmt const*, clang::Decl const*> PragmaTarget;

		/**
		 * Creates an empty pragma starting from source location startLoc and ending
		 * ad endLoc.
		 */
		Pragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& type)
		    : mStartLoc(startLoc), mEndLoc(endLoc), mType(type) {}

		/**
		 * Creates a pragma starting from source location startLoc and ending ad endLoc
		 * by passing the content of the map which associates, for each key defined in
		 * the pragma_matcher, the relative parsed list of values.
		 *
		 */
		Pragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& type, const MatchMap& mmap)
		    : mStartLoc(startLoc), mEndLoc(endLoc), mType(type) {}

		const clang::SourceLocation& getStartLocation() const {
			return mStartLoc;
		}
		const clang::SourceLocation& getEndLocation() const {
			return mEndLoc;
		}
		/**
		 * Returns a string which identifies the pragma
		 */
		const std::string& getType() const {
			return mType;
		}

		clang::Stmt const* getStatement() const;
		clang::Decl const* getDecl() const;

		/**
		 * Attach the pragma to a statement. If the pragma is already bound to a
		 * statement or location, a call to this method will produce an error.
		 */
		void setStatement(clang::Stmt const* stmt);

		/**
		 * Attach the pragma to a declaration. If the pragma is already bound to a
		 * statement or location, a call to this method will produce an error.
		 */
		void setDecl(clang::Decl const* decl);

		/**
		 * Returns true if the AST node associated to this pragma is a statement (clang::Stmt)
		 */
		bool isStatement() const {
			return !mTargetNode.isNull() && mTargetNode.is<clang::Stmt const*>();
		}

		/**
		 * Returns true if the AST node associated to this pragma is a declaration (clang::Decl)
		 */
		bool isDecl() const {
			return !mTargetNode.isNull() && mTargetNode.is<clang::Decl const*>();
		}

		/**
		 * Writes the content of the pragma to standard output
		 */
		virtual void dump(std::ostream& out, const clang::SourceManager& sm) const;

		/**
		 * Returns a string representation of the pragma
		 */
		std::string toStr(const clang::SourceManager& sm) const;

		virtual ~Pragma() {}

	  private:
		clang::SourceLocation mStartLoc, mEndLoc;
		std::string mType;
		PragmaTarget mTargetNode;
	};

	typedef std::shared_ptr<Pragma> PragmaPtr;
	typedef std::vector<PragmaPtr> PragmaList;
	typedef std::function<core::NodeList(const insieme::frontend::pragma::MatchObject&, core::NodeList)> pragmaHandlerFunction;

	// ------------------------------------ FrontendExtensionPragma ---------------------------
	class FrontendExtensionPragma : public Pragma {
	  private:
		pragma::MatchMap mMap;
		pragma::MatchObject m;
		const pragmaHandlerFunction f;

	  public:
		FrontendExtensionPragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& type)
		    : Pragma(startLoc, endLoc, type) {
			assert_fail() << "frontend pragma extension cannot be created without a function.";
		}

		FrontendExtensionPragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& type, const MatchMap& mmap)
		    : Pragma(startLoc, endLoc, type) {
			assert_fail() << "frontend pragma extension cannot be created without a function.";
		}

		FrontendExtensionPragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& type, const MatchMap& mmap,
		                        const pragmaHandlerFunction func)
		    : Pragma(startLoc, endLoc, type), mMap(mmap), f(func) {}

		const pragma::MatchObject& getMatchObject(conversion::Converter& fact) {
			// the matchmap is only cloned once; if it was cloned before the clone method will return
			m.cloneFromMatchMap(mMap, fact);
			return m;
		}

		const pragmaHandlerFunction getFunction() {
			return f;
		}
	};

	// ------------------------------------ PragmaStmtMap ---------------------------
	/**
	 * Maps statements and declarations to a Pragma.
	 */
	class PragmaStmtMap {
	  public:
		typedef std::multimap<const clang::Stmt*, const PragmaPtr> StmtMap;
		typedef std::multimap<const clang::Decl*, const PragmaPtr> DeclMap;

		template <class IterT>
		PragmaStmtMap(const IterT& begin, const IterT& end) {
			std::for_each(begin, end, [this](const typename IterT::value_type& pragma) {
				if(pragma->isStatement()) {
					this->stmtMap.insert(std::make_pair(pragma->getStatement(), pragma));
				} else {
					this->declMap.insert(std::make_pair(pragma->getDecl(), pragma));
				}
			});
		}

		const StmtMap& getStatementMap() const {
			return stmtMap;
		}
		const DeclMap& getDeclarationMap() const {
			return declMap;
		}

	  private:
		StmtMap stmtMap;
		DeclMap declMap;
	};

	// -------------------------------- BasicPragmaHandler<T> ---------------------------
	/**
	 * Defines a generic pragma handler which uses the pragma_matcher. Pragmas which are syntactically
	 * correct are then instantiated and associated with the following node (i.e. a Stmt or
	 * Declaration). If an error occurs, the error message is printed out showing the location and the
	 * list of tokens the parser was expecting at that location.
	 */
	template <class T>
	class BasicPragmaHandler : public clang::PragmaHandler {
		pragma::node* pragma_matcher;
		std::string base_name;
		const pragmaHandlerFunction func;

	  public:
		BasicPragmaHandler(clang::IdentifierInfo* name, const node& pragma_matcher, const std::string& base_name = std::string(),
		                   const pragmaHandlerFunction f = nullptr)
		    : PragmaHandler(name->getName().str()), pragma_matcher(pragma_matcher.copy()), base_name(base_name), func(f) {}

		void HandlePragma(clang::Preprocessor& PP, clang::PragmaIntroducerKind kind, clang::Token& FirstToken) {
			// '#' symbol is 1 position before
			clang::SourceLocation startLoc = ParserProxy::get().CurrentToken().getLocation().getLocWithOffset(-1);

			MatchMap mmap;
			ParserStack errStack;

			if(pragma_matcher->MatchPragma(PP, mmap, errStack)) {
				// the pragma type is formed by concatenation of the base_name and identifier, for
				// example the type for the pragma:
				//		#pragma omp barrier
				// will be "omp::barrier", the string is passed to the pragma constructor which store
				// the value
				std::ostringstream pragma_name;
				if(!base_name.empty()) { pragma_name << base_name << "::"; }
				if(!getName().empty()) { pragma_name << getName().str(); }

				clang::SourceLocation endLoc = ParserProxy::get().CurrentToken().getLocation();
				
				// retrieves source location of call site in case of macros
				auto checkAndFixLocation = [&PP](clang::SourceLocation location){
					if(PP.getSourceManager().isMacroArgExpansion(location) || PP.getSourceManager().isMacroBodyExpansion(location)) {
						location = PP.getSourceManager().getExpansionLoc(location);
					}
					return location;
				};

				startLoc = checkAndFixLocation(startLoc);
				endLoc = checkAndFixLocation(endLoc);

				// the pragma has been successfully parsed, now we have to instantiate the correct type
				// which is associated to this pragma (T) and pass the matcher map in order for the
				// pragma to initialize his internal representation. The framework will then take care
				// of associating the pragma to the following node (i.e. a statement or a declaration).
				if(!func) {
					static_cast<InsiemeSema&>(ParserProxy::get().getParser()->getActions()).ActOnPragma<T>(pragma_name.str(), mmap, startLoc, endLoc);
				} else {
					static_cast<InsiemeSema&>(ParserProxy::get().getParser()->getActions())
					    .ActOnFrontendExtensionPragma(pragma::PragmaPtr(new pragma::FrontendExtensionPragma(startLoc, endLoc, pragma_name.str(), mmap, func)));
				}
				return;
			}
			// In case of error, we report it to the console using the clang Diagnostics.
			errorReport(PP, startLoc, errStack);
			PP.DiscardUntilEndOfDirective();
		}

		~BasicPragmaHandler() {
			delete pragma_matcher;
		}
	};

	// ------------------------------------ PragmaHandlerFactory ---------------------------
	struct PragmaHandlerFactory {
		template <class T>
		static clang::PragmaHandler* CreatePragmaHandler(clang::IdentifierInfo* name, node const& re, const std::string& base_name = std::string(),
		                                                 const pragmaHandlerFunction f = nullptr) {
			return new BasicPragmaHandler<T>(name, re, base_name, f);
		}
	};

	// Handle the processing of pragmas in extensions
	core::NodeList handlePragmas(const core::NodeList& nodes, const clang::Stmt* clangNode, conversion::Converter& fact);
	core::NodeList handlePragmas(const core::NodeList& nodes, const clang::Decl* clangDecl, conversion::Converter& fact);

} // end pragma namespace
} // End frontend namespace
} // End insieme namespace
