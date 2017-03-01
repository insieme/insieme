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
 *
 */
#pragma once

#include <set>

#include "insieme/core/forward_decls.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/operator_converter.h"

namespace insieme {
namespace backend {

	class Converter;
	class ConversionContext;
	struct FunctionInfo;
	struct LambdaInfo;
	struct BindInfo;


	/**
	 * A type definition for a map linking external function names (literals)
	 * with include files those literals are part of.
	 */
	typedef std::map<string, string> FunctionIncludeTable;

	FunctionIncludeTable getBasicFunctionIncludeTable();

	namespace detail {
		class FunctionInfoStore;
	}

	class FunctionManager : private boost::noncopyable {
		const Converter& converter;

		detail::FunctionInfoStore* store;

		OperatorConverterTable operatorTable;

		FunctionIncludeTable includeTable;

	  public:
		FunctionManager(const Converter& converter);

		FunctionManager(const Converter& converter, const OperatorConverterTable& operatorTable, const FunctionIncludeTable& includeTable);

		~FunctionManager();

		const FunctionInfo& getInfo(ConversionContext& context, const core::LiteralPtr& literal);

		const FunctionInfo& getInfo(ConversionContext& context, const core::PureVirtualMemberFunctionPtr& fun);

		const LambdaInfo& getInfo(ConversionContext& context, const core::LambdaExprPtr& lambda);

		const LambdaInfo& getInfo(ConversionContext& context, const core::TagTypePtr& tagType, const core::LambdaExprPtr& lambda);

		const LambdaInfo& getInfo(ConversionContext& context, const core::TagTypePtr& tagType, const core::MemberFunctionPtr& memberFun);

		const BindInfo& getInfo(ConversionContext& context, const core::BindExprPtr& bind);

		const c_ast::NodePtr getCall(ConversionContext& context, const core::CallExprPtr& call);

		const c_ast::ExpressionPtr getValue(ConversionContext& context, const core::ExpressionPtr& fun);

		const c_ast::ExpressionPtr getValue(ConversionContext& context, const core::BindExprPtr& bind);

		const boost::optional<string> getHeaderFor(const string& function) const;

		const boost::optional<string> getHeaderFor(const core::LiteralPtr& function) const;

		const boost::optional<string> getHeaderFor(const core::LambdaExprPtr& function) const;

		bool isBuiltIn(const core::NodePtr& op) const;

		// ------------------------- Management ---------------------

		OperatorConverterTable& getOperatorConverterTable() {
			return operatorTable;
		}

		FunctionIncludeTable& getFunctionIncludeTable() {
			return includeTable;
		}
	};


	struct ElementInfo {
		virtual ~ElementInfo() {}
	};

	struct FunctionInfo : public ElementInfo {
		// to be included:
		// 		- the C-AST function represented
		//		- prototype
		//		- definition
		//		- lambdaWrapper

		c_ast::NodePtr declaration;

		c_ast::FunctionPtr function;

		std::vector<c_ast::TypePtr> instantiationTypes;

		c_ast::CodeFragmentPtr prototype;

		c_ast::IdentifierPtr lambdaWrapperName;

		c_ast::CodeFragmentPtr lambdaWrapper;
	};

	struct LambdaInfo : public FunctionInfo {
		// to be included
		// 		- the definition of the function

		c_ast::CodeFragmentPtr definition;
	};

	struct BindInfo : public ElementInfo {
		// to be included
		//		- the closure type definition
		//		- the mapper function definition
		//		- the constructor

		c_ast::IdentifierPtr closureName;

		c_ast::TypePtr closureType;

		c_ast::CodeFragmentPtr definitions;

		c_ast::IdentifierPtr mapperName;

		c_ast::IdentifierPtr constructorName;
	};

} // end namespace backend
} // end namespace insieme
