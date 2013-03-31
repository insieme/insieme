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

#include <set>

#include "insieme/core/forward_decls.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/operator_converter.h"

namespace insieme {
namespace backend {

	class Converter;
	class ConversionContext;
	class FunctionInfo;
	class LambdaInfo;
	class BindInfo;


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

		const FunctionInfo& getInfo(const core::LiteralPtr& literal);

		const FunctionInfo& getInfo(const core::LiteralPtr& pureVirtualMemberFun, bool isConst);

		const LambdaInfo& getInfo(const core::LambdaExprPtr& lambda);

		const LambdaInfo& getInfo(const core::LambdaExprPtr& memberFun, bool isConst, bool isVirtual);

		const BindInfo& getInfo(const core::BindExprPtr& bind);

		const c_ast::NodePtr getCall(const core::CallExprPtr& call, ConversionContext& context);

		const c_ast::ExpressionPtr getValue(const core::ExpressionPtr& fun, ConversionContext& context);

		const c_ast::ExpressionPtr getValue(const core::BindExprPtr& bind, ConversionContext& context);

		const boost::optional<string> getHeaderFor(const string& function) const;

		const boost::optional<string> getHeaderFor(const core::LiteralPtr& function) const;

		void rename(const core::LambdaExprPtr& lambda, const string& name);

		bool isBuiltIn(const core::ExpressionPtr& op) const;

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

		c_ast::FunctionPtr function;

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
