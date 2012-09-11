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

#include <exception>
#include <set>

#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {

class Expression;
template<typename T> class Pointer;
typedef Pointer<const Expression> ExpressionPtr;

namespace arithmetic {

	class NotAFormulaException;

	/**
	 * A function converting a given expression into an equivalent formula.
	 *
	 * @param expr the expression to be converted
	 * @return an equivalent formula
	 *
	 * @throws a NotAFormulaException if the given expression is not an arithmetic expression
	 */
	Formula toFormula(const ExpressionPtr& expr);

	/**
	 * A function converting a given expression into a constraint.
	 */
	Constraint toConstraint(const ExpressionPtr& expr);
	
	/**
	 * A function converting a given expression into a piecewise
	 */
	Piecewise toPiecewise(const ExpressionPtr& expr);

	/**
	 * A function converting a formula into an equivalent expression.
	 *
	 * @param manager the manager responsible for handling the IR nodes constructed by this method
	 * @param formula the formula to be converted
	 * @return an equivalent IR expression
	 */
	ExpressionPtr toIR(NodeManager& manager, const Formula& formula);

	/**
	 * A function converting a constraint into an equivalent IR expression. The constraint
	 * will be encoded using a DNF format and all literals are of the f(..)<=0 form.
	 *
	 * @param manager the manager responsible for handling the IR nodes constructed by this method
	 * @param constraint the constraint to be converted
	 * @return an equivalent IR expression
	 */
	ExpressionPtr toIR(NodeManager& manager, const Constraint& constraint);

	/**
	 * A function converting a piecewise formula into an equivalent IR expression.
	 *
	 * @param manager the manager responsible for handling the IR nodes constructed by this method
	 * @param piecewise the piecewise formula to be converted
	 * @return an equivalent IR expression
	 */
	ExpressionPtr toIR(NodeManager& manager, const Piecewise& piecewise);

	/**
	 * An exception which will be raised if a expression not representing
	 * a formula should be converted into one.
	 */
	class NotAFormulaException : public std::exception {
		ExpressionPtr expr;
		std::string msg;

	public:
		NotAFormulaException(const ExpressionPtr& expr);
	
		virtual const char* what() const throw();
		ExpressionPtr getCause() const { return expr; }
		virtual ~NotAFormulaException() throw() { }
	};

	/**
	 * An exception which will be raised if a expression not representing
	 * a constraint should be converted into one.
	 */
	class NotAConstraintException : public NotAFormulaException {
	public:
		NotAConstraintException(const ExpressionPtr& expr) : NotAFormulaException(expr) { }
		virtual ~NotAConstraintException() throw() { }
	};

	/**
	 * An exception which will be raised if a expression not representing
	 * a piecewise should be converted into one.
	 */
	class NotAPiecewiseException : public NotAFormulaException {
	public:
		NotAPiecewiseException(const ExpressionPtr& expr) : NotAFormulaException(expr) { }
		virtual ~NotAPiecewiseException() throw() { }
	};

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
