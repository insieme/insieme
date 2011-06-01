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

#include <utility>
#include <vector>

#include "insieme/core/ast_node.h"
#include "insieme/core/expressions.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace arithmetic {

	using std::vector;
	using std::pair;

	/**
	 * A class representing the product of variables. The class is responsible
	 * to aggregate exponents when multiplying equivalent variables several times
	 * and it is defining a order over all products of variables.
	 */
	class Product : public utils::Printable {

	public:

		/**
		 * The type used to represent a single factor. A factor consists of the
		 * represented variable and its exponent. The exponent must never be 0.
		 * However, the exponent may be negative.
		 */
		typedef pair<VariablePtr, int> Factor;

	private:

		/**
		 * The list of factors constituting the represented product. Each factor
		 * is represented by a pair consisting of a variable and its exponent. The
		 * variables within this list always have to be ordered according to their
		 * natural order ( < operator).
		 */
		vector<Factor> factors;

		/**
		 * A private constructor accepting a rvalue reference to a list of factors.
		 * This constructor is internally used and kept private to ensure that the
		 * invariant regarding the ordering of the individual factors within the vector
		 * is maintained at all time.
		 */
		Product(const vector<Factor>&& factors);

	public:

		/**
		 * Creates an empty product representing the value 1.
		 */
		Product() {};

		/**
		 * Creates a product consisting of a single variable and its exponent.
		 *
		 * @param var the variable to be included
		 * @param exponent the exponent to be assigned to the given variable
		 */
		Product(const core::VariablePtr& var, int exponent = 1);

		/**
		 * Obtains a constant reference to the internally maintained factors.
		 *
		 * @return a constant reference to the involved factors
		 */
		const vector<Factor>& getFactors() const {
			return factors;
		}

		/**
		 * Tests whether this product represents the constant 1 - hence, no variables
		 * are involved.
		 *
		 * @return true if this product represents a constant 1, false otherwise
		 */
		bool isOne() const { return factors.empty(); }

		/**
		 * Tests whether this product is linear - hence it contains at most one variable
		 * with an exponent of 1.
		 *
		 * @return true if linear, false otherwise
		 */
		bool isLinear() const;

		/**
		 * Tests whether this product is polynomial - hence, it only contains positive
		 * exponents.
		 *
		 * @return true if it is polynomial, false otherwise
		 */
		bool isPolynomial() const;

		/**
		 * Implements the multiplication operator for two products of variables. The
		 * resulting product will represent the result of arithmetically multiplying
		 * the two products (e.g. xy * xz = x^2yz).
		 *
		 * @param other the value this product should be multiplied with
		 * @return the resulting product
		 */
		Product operator*(const Product& other) const;

		/**
		 * Implements the division operator for two products of variables. The
		 * resulting product will represent the result of arithmetically division of
		 * the two products (e.g. xy / xz = yz^-1).
		 *
		 * @param other the value this product should be divided by
		 * @return the resulting product
		 */
		Product operator/(const Product& other) const;

		/**
		 * Defines a total order on products. The order will be lexicographically
		 * based on the variable names. Factors with higher exponents will be smaller
		 * than factors with smaller exponents
		 *
		 * @param other the product to be compared with
		 * @return true if this product is smaller (not in the numerical sense), false otherwise
		 */
		bool operator<(const Product& other) const;

		/**
		 * Compares this product with another product. Two products are equivalent if
		 * they are formed by the same set of variables, each equipped with the same exponent.
		 *
		 * @param other the product to be compared with
		 * @return true if equivalent, false otherwise
		 */
		bool operator==(const Product& other) const {
			return this==&other || factors == other.factors;
		}

		/**
		 * Compares this product with another product. The result is simply the negation
		 * of the equality operator.
		 *
		 * @param other the product to be compared to
		 * @return true if not equivalent, false otherwise
		 */
		bool operator!=(const Product& other) const {
			return !(*this == other);
		}

		/**
		 * This method is required by the printable interface and allows
		 * instances of this class to be printed to some output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};


	/**
	 * A class representing a normalized arithmetic expression. The formula will
	 * be the sum of terms, each of them formed by a coefficient and a product of
	 * variables. The formula is internally stored in a normalized form to avoid
	 * ambiguities.
	 */
	class Formula : public utils::Printable {

	public:

		/**
		 * The type used to represent a term within this formula. A term consists
		 * of a product of variables and a coefficient. The coefficient must not be 0.
		 */
		typedef pair<Product, int> Term;

	private:

		/**
		 * The list of terms forming this formula. Each term is defined by a pair
		 * of a product of variables and a coefficient. The pairs have to be ordered
		 * according to the product's order within this list. No two pairs may have
		 * the same product as its first component and now coefficient is allowed to
		 * be 0.
		 */
		vector<Term> terms;

		/**
		 * A private constructor allowing the creation of a formula based on a pre-
		 * existing vector (rvalue reference). This way, the afford of creating a
		 * copy can be avoided. The constructor is private to ensure the invariants
		 * defined for the vector of terms is satisfied by the given terms.
		 *
		 * @param terms the terms the resulting formula should consist of
		 * 		   - satisfying all the defined invariants
		 */
		Formula(const vector<Term>&& terms);

	public:

		/**
		 * A default constructor for a formula creating a formula representing zero.
		 */
		Formula() {};

		/**
		 * A constructor supporting the implicit conversion of a integer value into
		 * a formula representing the same value.
		 *
		 * @param value the value to be represented
		 */
		Formula(int value);

		/**
		 * A constructor supporting the creation of a formula consisting of a single
		 * term covering a single variable. The Variables exponent and coefficient can
		 * be stated as well. This constructor also enables the implicit conversion
		 * of variables into formulas.
		 *
		 * @param var the variable to be covered
		 * @param exponent the exponent of the given variable within the resulting formula (must be != 0)
		 * @param coefficient the coefficient of the resulting term within the resulting formula (must be != 0)
		 */
		Formula(const core::VariablePtr& var, int exponent = 1, int coefficient = 1);

		/**
		 * A constructor supporting the creation of a formula consisting of a single term.
		 * It also enables the implicit conversion of a product into a formula.
		 *
		 * @param product the product to form the single term within the resulting formula
		 * @param coefficient the coefficient of this term within the resulting formula (must be != 0)
		 */
		Formula(const Product& product, int coefficient = 1);

		/**
		 * Checks whether this formula represents zero.
		 *
		 * @return true if so, false otherwise
		 */
		bool isZero() const { return terms.empty(); }

		/**
		 * Checks whether this formula represents a constant value.
		 *
		 * @return true if so, false otherwise
		 */
		bool isConstant() const;

		/**
		 * Checks whether this formula is linear, hence every term within this
		 * formula is linear.
		 *
		 * @return true if so, false otherwise.
		 */
		bool isLinear() const;

		/**
		 * Checks whether this formula is polynomial. Hence, all terms within
		 * this formula are polynomial (no negative exponents).
		 *
		 * @return true if so, false otherwise.
		 */
		bool isPolynomial() const;


		/**
		 * Implements the plus operator for formulas. The resulting formula will be
		 * the sum of this formula and the given formula.
		 *
		 * @param other the formula to be added to this formula.
		 * @return the sum of this and the given formula
		 */
		Formula operator+(const Formula& other) const;

		/**
		 * Implements the minus operator for formulas. The resulting formula will be
		 * the difference of this formula and the given formula.
		 *
		 * @param other the formula to be subtracted from this formula.
		 * @return the difference of this and the given formula
		 */
		Formula operator-(const Formula& other) const;

		/**
		 * Implements the multiplication operator for formulas. The resulting formula will be
		 * the product of this formula and the given formula.
		 *
		 * @param other the formula this formula should be multiplied with.
		 * @return the product of this and the given formula
		 */
		Formula operator*(const Formula& other) const;

		/**
		 * Divides all the coefficients of the represented formula by the given divisor.
		 * WARNING: this is a integer division - so the result may not be exact in case
		 * the divisor is not dividing all the coefficients.
		 *
		 * @param divisor the divisor this formula should be divided with
		 * @return the resulting formula containing the reduced coefficients
		 */
		Formula operator/(int divisor) const;

		/**
		 * Divides all the terms of the represented formula by the given divisor.
		 *
		 * @param divisor the product by which all terms of this formula should be divided with
		 * @return the resulting formula containing the reduced terms
		 */
		Formula operator/(const Product& divisor) const;

		/**
		 * Divides all the terms of the represented formula by the given divisor.
		 *
		 * @param divisor the product by which all terms of this formula should be divided with
		 * @return the resulting formula containing the reduced terms
		 */
		Formula operator/(const VariablePtr& divisor) const {
			return *this / Product(divisor);
		}

		/**
		 * Checks whether this formula is equivalent to the given formula.
		 *
		 * @param other the formula to be compared with
		 * @return true if equivalent, false otherwise
		 */
		bool operator==(const Formula& other) const {
			return this==&other || terms == other.terms;
		}

		/**
		 * Checks whether this formula is not equivalent to the given formula.
		 *
		 * @param other the formula to be compared with
		 * @return false if equivalent, true otherwise
		 */
		bool operator!=(const Formula& other) const {
			return !(*this == other);
		}

		/**
		 * Allows this formula to be printed to some output stream via
		 * the Printable interface of the utilities.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

		/**
		 * Provides access to the internally maintained list of terms.
		 *
		 * @return a constant reference to the internally maintained list of terms
		 */
		const vector<Term>& getTerms() const {
			return terms;
		}

	};

	// -----------------------------------------------------------------
	//                   Overloaded Operators
	// -----------------------------------------------------------------

	inline Formula operator+(int a, const VariablePtr& b) {
		return Formula(a) + Formula(b);
	}

	inline Formula operator+(const VariablePtr& a, int b) {
		return Formula(a) + Formula(b);
	}

	inline Formula operator+(int a, const Product& b) {
		return Formula(a) + Formula(b);
	}

	inline Formula operator+(int a, const Formula& b) {
		return Formula(a) + b;
	}

	inline Formula operator+(const Formula& a, int b) {
		return a + Formula(b);
	}

	inline Formula operator+(const VariablePtr& a, const Formula& b) {
		return Formula(a) + b;
	}

	inline Formula operator+(const Formula& a, const VariablePtr& b) {
		return a + Formula(b);
	}

	inline Formula operator+(const VariablePtr& a, const VariablePtr& b) {
		return Formula(a) + Formula(b);
	}

	inline Formula operator+(const Product& a, int b) {
		return Formula(a) + Formula(b);
	}

	inline Formula operator+(const Product& a, const Product& b) {
		return Formula(a) + Formula(b);
	}

	inline Formula operator-(int a, const VariablePtr& b) {
		return Formula(a) - Formula(b);
	}

	inline Formula operator-(const VariablePtr& a, int b) {
		return Formula(a) - Formula(b);
	}

	inline Formula operator-(int a, const Product& b) {
		return Formula(a) - Formula(b);
	}

	inline Formula operator-(const Product& a, int b) {
		return Formula(a) - Formula(b);
	}

	inline Formula operator-(const Product& a, const Product& b) {
		return Formula(a) - Formula(b);
	}

	inline Formula operator*(const Product& a, int b) {
		return Formula(a, b);
	}

	inline Formula operator*(int a, const Product& b) {
		return Formula(b,a);
	}

	inline Formula operator*(const VariablePtr& a, int b) {
		return Formula(a, 1, b);
	}

	inline Formula operator*(int a, const VariablePtr& b) {
		return Formula(b, 1, a);
	}

	inline Product operator*(const VariablePtr& a, const VariablePtr& b) {
		return Product(a) * Product(b);
	}

	inline Product operator*(const VariablePtr& a, const Product& b) {
		return Product(a) * b;
	}

	inline Product operator*(const Product& a, const VariablePtr& b) {
		return a * Product(b);
	}


} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
