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

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/runtime/runtime_entities.h"
#include "insieme/backend/runtime/runtime_extension.h"

namespace insieme {
namespace backend {
namespace runtime {

	// ------------------------------------------------------------------------
	//  Within this header file a list of special code fragments used for
	//  creating code to be executed on the Insieme runtime is defined.
	// ------------------------------------------------------------------------

	class ContextHandlingFragment;
	typedef Ptr<ContextHandlingFragment> ContextHandlingFragmentPtr;

	class TypeTable;
	typedef Ptr<TypeTable> TypeTablePtr;

	class ImplementationTable;
	typedef Ptr<ImplementationTable> ImplementationTablePtr;

	class MetaInfoTable;
	typedef Ptr<MetaInfoTable> MetaInfoTablePtr;

	/**
	 * This code fragment is containing code for context handling functions like
	 * insieme_init_context - invoked right after loading a context (shared library
	 * file) or the insieme_cleanup_context which is triggered just before
	 * the file is unloaded.
	 */
	class ContextHandlingFragment : public c_ast::CodeFragment {
		const Converter& converter;

		vector<string> initExpressions;

		vector<string> cleanupExpressions;

		TypeTablePtr typeTable;

		ImplementationTablePtr implTable;

		MetaInfoTablePtr infoTable;

	  public:
		ContextHandlingFragment(const Converter& converter);

		static ContextHandlingFragmentPtr get(const Converter& converter);

		const c_ast::IdentifierPtr getInitFunctionName();

		const c_ast::IdentifierPtr getCleanupFunctionName();

		virtual std::ostream& printTo(std::ostream& out) const;

		/**
		 * This method allows to add additional expressions to the initialization method.
		 * The given expression should be a valid C statement and may contain formating
		 * symbols (like printf). The one parameter to be passed to the formatting will
		 * be the name of the context variable.
		 *
		 * e.g. given the pattern "do_something_with_context(%s);" will be instantiated as
		 * "do_something_with_context(context);" if context is the name of the variable.
		 *
		 * @param expr the initialization expression to be added
		 */
		void addInitExpression(const string& expr) {
			initExpressions.push_back(expr);
		}

		/**
		 * This method allows to add an additional statement to the cleanup method. As for
		 * the initialization method, the given expression may have formatting symbols.
		 *
		 * @param expr the expression to be added to the cleanup method.
		 */
		void addCleanupExpression(const string& expr) {
			cleanupExpressions.push_back(expr);
		}
	};

	class TypeTableStore;

	/**
	 * The type table fragment contains code creating and handling the type table
	 * used by the Insieme runtime to obtain information regarding data item types.
	 */
	class TypeTable : public c_ast::CodeFragment {
		const Converter& converter;

		TypeTableStore* store;

	  public:
		TypeTable(const Converter& converter);

		~TypeTable();

		static TypeTablePtr get(const Converter& converter);

		const c_ast::ExpressionPtr getTable();

		virtual std::ostream& printTo(std::ostream& out) const;

		unsigned registerType(ConversionContext& context, const core::TypePtr& type);

		unsigned size() const;
	};

	struct WorkItemImplCode;

	/**
	 * The implementation table fragment represents code resulting in the creation of the
	 * implementation table. This table consists of a list of work item implementations providing
	 * access points for work-item executions.
	 */
	class ImplementationTable : public c_ast::CodeFragment {
		const Converter& converter;

		utils::map::PointerMap<core::ExpressionPtr, unsigned> index;

		vector<WorkItemImplCode> workItems;

		c_ast::CodeFragmentPtr declaration;

	  public:
		ImplementationTable(const Converter& converter);

		static ImplementationTablePtr get(const Converter& converter);

		c_ast::CodeFragmentPtr getDeclaration();

		const c_ast::ExpressionPtr getTable();

		unsigned registerWorkItemImpl(ConversionContext& context, const core::ExpressionPtr& implementation);

		virtual std::ostream& printTo(std::ostream& out) const;

		unsigned size() const;
	};

	struct MetaInfoTableEntry;

	/**
	 * The code fragment maintaining the meta information to be encoded into the generated
	 * target code to be forwarded to the runtime system.
	 */
	class MetaInfoTable : public c_ast::CodeFragment {
		const Converter& converter;

		vector<MetaInfoTableEntry> infos;

	  public:
		MetaInfoTable(const Converter& converter);

		static MetaInfoTablePtr get(const Converter& converter);

		const c_ast::ExpressionPtr getTable();

		unsigned registerMetaInfoFor(const core::NodePtr& node);

		virtual std::ostream& printTo(std::ostream& out) const;

		unsigned size() const;
	};

} // end namespace runtime
} // end namespace backend
} // end namespace insieme
