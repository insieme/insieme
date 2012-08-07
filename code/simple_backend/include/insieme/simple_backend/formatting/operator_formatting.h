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

#include <memory>

#include "insieme/simple_backend/code_management.h"

#include "insieme/utils/map_utils.h"

/**
 * This header file includes a set of utilities to be used to define formatting rules for various operators. The included
 * formatters are used by the simple backend to convert operator invocations like int.add(a,b) into the correct, C-like
 * inline format a + b
 */


// forward declarations
namespace insieme {
	namespace core {

		template<class T> class Pointer;

		class Expression;
		typedef Pointer<const Expression> ExpressionPtr;
		class Literal;
		typedef Pointer<const Literal> LiteralPtr;
		class CallExpr;
		typedef Pointer<const CallExpr> CallExprPtr;

		namespace lang {
			class BasicGenerator;
		}
	}
	namespace simple_backend {
		class StmtConverter;
	}
}


namespace insieme {
namespace simple_backend {
namespace formatting {

	/**
	 * This abstract class represents an interface for formatters. It enables the conversion visitor to format operator
	 * invocations using a variable, specialized format. For instance, this formatter allows to write the operation + in infix notation.
	 */
	class Formatter {
	public:

		/**
		 * Performs the actual code formating. This method is pure abstract and
		 * has to be implemented within sub-classes.
		 *
		 * @param converter the converter and its context using this formatter
		 * @param call the call expression to be handled
		 */
		virtual void format(StmtConverter& converter, const core::CallExprPtr& call) =0;

	};

	/**
	 * Since formatter instances are polymorthic, they need to be handled via pointer or
	 * references. Further, the memory management needs to be considered. Therefore, formatter
	 * should be passed using this pointer type, which is based on a shared pointer.
	 */
	typedef std::shared_ptr<Formatter> FormatterPtr;

	/**
	 * The Lambda Formatter is a concrete generic implementation of the Formatter class. It uses
	 * a lambda expression passed in during the construction to format the actual output.
	 */
	template<typename Lambda>
	class LambdaFormatter : public Formatter {

		/**
		 * The lambda used to perform the formatting.
		 */
		Lambda lambda;

	public:

		/**
		 * Creates a new instance of this type printing the given literal using the
		 * given lambda during the formating.
		 *
		 * @param literal the literal to be handled by this formatter
		 * @param lambda the lambda performing the actual formatting
		 */
		LambdaFormatter(Lambda lambda) : lambda(lambda) {}

		/**
		 * Conducts the actual formatting of the given call expression.
		 *
		 * @param converter the converter and its context using this formatter
		 * @param call the call expression to be handled
		 */
		virtual void format(StmtConverter& converter, const core::CallExprPtr& call) {
			lambda(converter, call);
		}
	};

	/**
	 * A utility function to create LiteralFormatter instances without the need of
	 * specifying generic types. Those types will be inferred automatically.
	 *
	 * @param literal the literal to be handled by the requested formatter
	 * @return a new formatter handling the call expressions using the given lambda
	 */
	template<typename Lambda>
	FormatterPtr make_formatter(Lambda lambda) {
		return std::make_shared<LambdaFormatter<Lambda>>(lambda);
	}

	/**
	 * This type defines a lookup table containing a set of formats indexed by literals representing
	 * the corresponding operations.
	 */
	typedef utils::map::PointerMap<core::ExpressionPtr, FormatterPtr> FormatTable;

	/**
	 * Creates a list of formatters for basic C operators and additional functions being part
	 * of the IR lang-basic.
	 */
	FormatTable getBasicFormatTable(const core::lang::BasicGenerator& basic);


	namespace detail {

		// Two additional utility functions required by the format specification macro.

		/**
		 * A utility function to obtain the n-th argument within the given call expression.
		 *
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the requested argument
		 * @return the requested argument or a NULL pointer in case there is no such argument
		 */
		core::ExpressionPtr getArgument(const core::CallExprPtr& call, unsigned n);

		/**
		 * A utility function visiting the n-th argument of a call expression.
		 *
		 * @param converter the converter to be used for the actual conversion
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the argument to be visited; in case there is no such argument, nothing will be visited
		 */
		void visitArgument(StmtConverter& converter, const core::CallExprPtr& call, unsigned n);

	} // end namespace detail

} // end namespace formatting
} // end namespace simple_backend
} // end namespace insieme
