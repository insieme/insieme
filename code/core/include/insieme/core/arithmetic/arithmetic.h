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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/constraint.h"

namespace insieme {
namespace core {
namespace arithmetic {

	using std::vector;
	using std::pair;

	/**
	 * A class used to represent rational numbers within the arithmetic infrastructure.
	 * Instances are always stored in their irreducible fraction form.
	 */
	class Rational : public utils::Printable {

		/**
		 * The numerator of the represented rational number.
		 */
		int numerator;

		/**
		 * The denominator of the rational number. To normalize values,
		 * the gcd(numerator,denominator) has to be always one, the denominator
		 * has to be > 0 and in case the numerator is 0, the denominator has to
		 * be 1.
		 */
		unsigned denominator;

	public:

		/**
		 * Creates a new rational number based on a given integer.
		 * This constructor is overloaded since in this case no reduction
		 * is required.
		 */
		Rational(int num = 0) : numerator(num), denominator(1) {}

		/**
		 * Creates a new rational number based on the given numerator / denominator
		 * pair.
		 *
		 * @param num the numerator of the resulting rational number
		 * @param den the denominator of the resulting rational number
		 */
		Rational(int num, unsigned den);

	private:

		/**
		 * A private constructor used internally when constructing rational numbers known
		 * to be normalized. In those cases the reduction step can be skipped.
		 *
		 * @param num the numerator of the resulting rational number
		 * @param den the denominator of the resulting rational number
		 * @param dummy a dummy parameter to distinguish this constructor from others
		 */
		Rational(int num, unsigned den, bool dummy)
			: numerator(num), denominator(den) {
			// ensure proper reduction
			assert(*this == Rational(num, den) && "Input not properly reduced!");
		}

	public:

		/**
		 * Obtains the numerator of this rational.
		 */
		int getNumerator() const {
			return numerator;
		}

		/**
		 * Obtains the denominator of this rational number.
		 */
		unsigned getDenominator() const {
			return denominator;
		}

		/**
		 * Tests whether this rational is representing zero.
		 */
		bool isZero() const {
			return numerator == 0;
		}

		/**
		 * Tests whether this rational is representing one.
		 */
		bool isOne() const {
			return numerator == 1 and denominator == 1;
		}

		/**
		 * Tests whether this rational is representing -1.
		 */
		bool isMinusOne() const {
			return numerator == -1 and denominator == 1;
		}

		/**
		 * Tests whether this rational is representing a negative value.
		 */
		bool isNegative() const {
			return numerator < 0;
		}

		/**
		 * Tests whether this rational is representing a positve value. Zero
		 * is not considered to be positive.
		 */
		bool isPositive() const {
			return numerator > 0;
		}

		/**
		 * Tests whether this rational is representing an integer value.
		 */
		bool isInteger() const {
			return denominator == 1;
		}

		/**
		 * Converts this rational to the closest integer (rounding toward zero).
		 */
		operator int() const {
			return numerator/static_cast<int>(denominator);
		}

		/**
		 * Converts this rational into a float approximating this rational number.
		 */
		operator float() const {
			return static_cast<float>(numerator)/denominator;
		}

		/**
		 * Computes the inverse value of this rational, hence 1/x where x is this value.
		 */
		Rational invert() const {
			// use internal constructor since result is irreducable
			return Rational((numerator>=0)?denominator:-denominator, abs(numerator), false);
		}


		// -- comparison operators --

		/**
		 * Determines whether the given rational number is equivalent to this rational number.
		 */
		bool operator==(const Rational& other) const {
			return numerator == other.numerator && denominator == other.denominator;
		}

		/**
		 * Determines whether the given rational number is not equivalent to this rational number.
		 */
		bool operator!=(const Rational& other) const {
			return !(*this == other);
		}

		/**
		 * Implements the less-than comparison operator for rational numbers.
		 */
		bool operator<(const Rational& other) const {
			return numerator * (int64_t)other.denominator < other.numerator * (int64_t)denominator;
		}

		/**
		 * Implements the less-or-equal-than comparison operator for rational numbers.
		 */
		bool operator<=(const Rational& other) const {
			return *this == other || *this < other;
		}

		/**
		 * Implements the greater-than comparison operator for rational numbers.
		 */
		bool operator>(const Rational& other) const {
			return !(*this <= other);
		}

		/**
		 * Implements the greater-or-equal-than comparison operator for rational numbers.
		 */
		bool operator>=(const Rational& other) const {
			return !(*this < other);
		}


		// -- arithmetic operators --

		/**
		 * Implements the plus operation for rational numbers.
		 */
		Rational operator+(const Rational& other) const;

		/**
		 * Implements the minus operation for rational numbers.
		 */
		Rational operator-(const Rational& other) const;

		/**
		 * Implements the multiplication operation for rational numbers.
		 */
		Rational operator*(const Rational& other) const {
			return Rational(numerator * other.numerator, denominator * other.denominator);
		}

		/**
		 * Implements the division operation for rational numbers.
		 */
		Rational operator/(const Rational& other) const {
			return *this * other.invert();
		}

		/**
		 * Implements the unary - operation for rational numbers.
		 */
		Rational operator-() const {
			return Rational(-numerator, denominator);
		}


		// -- compound assignment operators --

		/**
		 * Implements the plus-assignment operation for rational numbers.
		 */
		Rational& operator+=(const Rational& other) {
			return *this = *this + other;
		}

		/**
		 * Implements the minus-assignment operation for rational numbers.
		 */
		Rational& operator-=(const Rational& other) {
			return *this = *this - other;
		}

		/**
		 * Implements the multiplication-assignment operation for rational numbers.
		 */
		Rational& operator*=(const Rational& other) {
			return *this = *this * other;
		}

		/**
		 * Implements the division-assignment operation for rational numbers.
		 */
		Rational& operator/=(const Rational& other) {
			return *this = *this / other;
		}


		/**
		 * Prints a string-representation of this rational number to the given output stream.
		 */
		std::ostream& printTo(std::ostream& out) const {
			out << numerator;
			if (denominator != 1) out << "/" << denominator;
			return out;
		}

	};

	/**
	 * A class representing an atomic value within formulas. Such an atomic
	 * value might be a single variable, a dereferenced variable, a projected
	 * tuple or any other term considered to be a simple value read.
	 *
	 * Generally, everything which can be read several times during the evaluation
	 * of an arithmetic expression without causing side effects may be considered
	 * to be a value.
	 */
	class Value : public utils::Printable {

		/**
		 * The value to be represented by this class.
		 */
		ExpressionPtr value;

	public:

		/**
		 * Creates a new value representing the given variable. This
		 * constructor also realizes support for implicit variable to
		 * value conversions.
		 */
		Value(const VariablePtr& var) : value(var) {};

		/**
		 * Creates a new value instance based on the given
		 * expression. The value has to be accepted by the
		 * isValue(..) test, otherwise an exception is raised.
		 *
		 * @param value the value to be represented.
		 */
		Value(const ExpressionPtr& value);

		/**
		 * A static test allowing to verify whether a given expression
		 * is a valid encoding of a value. Valid encodings are for instance
		 * variables, dereferences of variables as well as struct and
		 * tuple accesses.
		 *
		 * @param expr the expression to be tested
		 * @return true if it is a valid encoding, false otherwise
		 */
		static bool isValue(const ExpressionPtr& expr);

		/**
		 * An implicit converter of a value into an expression pointer.
		 * The resulting expression will be the expression represented
		 * by this value.
		 */
		operator ExpressionPtr() const {
			return value;
		}

		/**
		 * Compares this value with another value. Two values are equivalent if
		 * they are defined by the same expression.
		 *
		 * @param other the value to be compared with
		 * @return true if equivalent, false otherwise
		 */
		bool operator==(const Value& other) const {
			return this==&other || *value == *other.value;
		}

		/**
		 * Compares this value with another value. The result is simply the negation
		 * of the equality operator.
		 *
		 * @param other the value to be compared to
		 * @return true if not equivalent, false otherwise
		 */
		bool operator!=(const Value& other) const {
			return !(*this == other);
		}

		/**
		 * Defines a total order on values. The order will be based on the structure
		 * of the represented expression.
		 *
		 * @param other the value to be compared with
		 * @return true if this value is smaller (not in the numerical sense), false otherwise
		 */
		bool operator<(const Value& other) const;

		/**
		 * This method is required by the printable interface and allows
		 * instances of this class to be printed to some output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * A class representing the product of variables/values. The class is responsible
	 * to aggregate exponents when multiplying equivalent variables several times
	 * and it is defining a order over all products of variables.
	 */
	class Product : public utils::Printable {

	public:

		/**
		 * The type used to represent a single factor. A factor consists of the
		 * represented value and its exponent. The exponent must never be 0.
		 * However, the exponent may be negative.
		 */
		typedef pair<Value, int> Factor;

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
		 * @param exponent the exponent to be assigned to the given value
		 */
		Product(const VariablePtr& var, int exponent = 1);

		/**
		 * Creates a product consisting of a single value and its exponent.
		 *
		 * @param value the value to be included
		 * @param exponent the exponent to be assigned to the given value
		 */
		Product(const Value& value, int exponent = 1);

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
		 * Returns the degree of this polynomial
		 */
		size_t getDegree() const;

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
		 * Implements the exponent operator between a Product and an integer value. 
		 * This operation multiplies all the exponents of this Product by the given 
		 * exp value.
		 */
		Product operator^(int exp) const;

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
		 * Obtains the exponent of the given variable within this product.
		 *
		 * @param value the value for which's product to be looking for
		 * @return the associated exponent or 0 if the variable is not included.
		 */
		int operator[](const Value& value) const;

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
		typedef pair<Product, Rational> Term;

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
		Formula(const vector<Term>&& terms) : terms(terms) {};

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
		 * A constructor supporting the implicit conversion of a rational value into
		 * a formula representing the same value.
		 *
		 * @param div the value to be represented
		 */
		Formula(const Rational& value);

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
		Formula(const core::VariablePtr& var, int exponent = 1, const Rational& coefficient = 1);

		/**
		 * A constructor supporting the creation of a formula consisting of a single
		 * term covering a single value. The Values exponent and coefficient can
		 * be stated as well. This constructor also enables the implicit conversion
		 * of values into formulas.
		 *
		 * @param value the value to be covered
		 * @param exponent the exponent of the given variable within the resulting formula (must be != 0)
		 * @param coefficient the coefficient of the resulting term within the resulting formula (must be != 0)
		 */
		Formula(const Value& value, int exponent = 1, const Rational& coefficient = 1);

		/**
		 * A constructor supporting the creation of a formula consisting of a single term.
		 * It also enables the implicit conversion of a product into a formula.
		 *
		 * @param product the product to form the single term within the resulting formula
		 * @param coefficient the coefficient of this term within the resulting formula (must be != 0)
		 */
		Formula(const Product& product, const Rational& coefficient = 1);

		/**
		 * Checks whether this formula represents zero.
		 *
		 * @return true if so, false otherwise
		 */
		bool isZero() const { return terms.empty(); }

		/**
		 * Checks whether this formula represents one.
		 */
		bool isOne() const { 
			return terms.size() == static_cast<std::size_t>(1) && 
				terms[0].first.isOne() && terms[0].second.isOne(); 
		}

		/**
		 * Checks whether this formula represents a constant value.
		 *
		 * @return true if so, false otherwise
		 */
		bool isConstant() const;

		/**
		 * Checks whether this formula represents a constant integer value.
		 *
		 * @return true if so, false otherwise
		 */
		bool isInteger() const;

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
		 * Obtains the constant value represented by this formula. If this formula
		 * is not representing a constant value, an assertion is triggered.
		 *
		 * @return the constant value represented by this formula
		 */
		Rational getConstantValue() const;

		/**
		 * Returns the degree of this polynomial
		 */
		size_t getDegree() const;

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
		Formula operator/(const Rational& divisor) const;

		/**
		 * Divides all the terms of the represented formula by the given divisor.
		 *
		 * @param divisor the product by which all terms of this formula should be divided with
		 * @return the resulting formula containing the reduced terms
		 */
		Formula operator/(const Product& divisor) const;

		/**
		 * Divides this formula by the given term.
		 *
		 * @param divisor the term this formula should be divided by
		 * @return the resulting formula representing the resulting formula
		 */
		Formula operator/(const Term& divisor) const;

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
		 * Obtains the coefficient of the given product within this formula.
		 *
		 * @param product the product looking for
		 * @return the coefficient of the given product, 0 if not present
		 */
		Rational operator[](const Product& product) const;

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

	inline Formula operator+(const Rational& a, const Formula& b) {
		return Formula(a) + b;
	}

	inline Formula operator+(const Formula& a, int b) {
		return a + Formula(b);
	}

	inline Formula operator+(const Formula& a, const Rational& b) {
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

	inline Formula operator+(const VariablePtr& a, const Product& b) {
		return Formula(a) + Formula(b);
	}

	inline Formula operator-(int a, const VariablePtr& b) {
		return Formula(a) - Formula(b);
	}

	inline Formula operator-(int a, const Formula& b) {
		return Formula(a) - b;
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
		return Formula(a, Rational(b));
	}

	inline Formula operator*(const Product& a, const Rational& b) {
		return Formula(a, b);
	}

	inline Formula operator*(int a, const Product& b) {
		return Formula(b,Rational(a));
	}

	inline Formula operator*(const Rational& a, const Product& b) {
		return Formula(b,a);
	}

	inline Formula operator*(const VariablePtr& a, int b) {
		return Formula(a, 1, Rational(b));
	}

	inline Formula operator*(const VariablePtr& a, const Rational& b) {
		return Formula(a, 1, b);
	}

	inline Formula operator*(int a, const VariablePtr& b) {
		return Formula(b, 1, Rational(a));
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

	inline Product operator^(const VariablePtr& a, int exp) {
		return Product(Value(a), exp);
	}

	inline Product operator^(const Value& a, int exp) {
		return Product(a, exp);
	}


	// --- Inequality Constraints ---

	// the atoms of the constraints f(x) <= 0
	class Inequality : public utils::Printable {

		/**
		 * The formula to be present on the left hand side of the
		 * represented inequality.
		 */
		Formula formula;

	public:

		/**
		 * Creates a new inequality comparing the given formula with 0. In case
		 * no formula is given, a formula representing zero is used, making the
		 * inequality valid.
		 *
		 * @param formula the formula to compare 0 with
		 */
		Inequality(const Formula& formula = Formula())
			: formula(formula) {}

		/**
		 * Tests whether this inequality has a constant value,
		 * hence whether it is always valid or always unsatisfiable.
		 *
		 * @return true if constant, false otherwise
		 */
		bool isConstant() const {
			return formula.isConstant();
		}

		/**
		 * Tests whether this inequality is valid, hence it is always
		 * true for all variables.
		 *
		 * @return true if valid, false otherwise
		 */
		bool isValid() const {
			return formula.isConstant() && formula.getConstantValue() <= Rational(0);
		}

		/**
		 * Tests whether this inequality is unsatisfiable, hence there is
		 * no variable assignment satisfying the represented inequality.
		 *
		 * @return true if unsatisfiable, false otherwise
		 */
		bool isUnsatisfiable() const {
			return formula.isConstant() && formula.getConstantValue() > Rational(0);
		}


		/**
		 * Compares this inequality with another inequality. Two inequalities are equivalent if
		 * they have the same formula on the left-hand-side.
		 *
		 * @param other the inequality to be compared with
		 * @return true if equivalent, false otherwise
		 */
		bool operator==(const Inequality& other) const {
			return this==&other || formula == other.formula;
		}

		/**
		 * Compares this inequality with another inequality. The result is simply the negation
		 * of the equality operator.
		 *
		 * @param other the inequality to be compared to
		 * @return true if not equivalent, false otherwise
		 */
		bool operator!=(const Inequality& other) const {
			return !(*this == other);
		}

		/**
		 * This method is required by the printable interface and allows
		 * instances of this class to be printed to some output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << formula << " <= 0";
		}
	};


//
//	// the aggregation of inequality is forming constraints
//	class Constraint : public utils::Printable {
//
//		// internally, constraints are stored in a CNF ...
//
//	};
//
//
//	// a piecewise formula is using different formulas for different ranges
//	class PiecewiseFormula : public utils::Printable {
//
//
//	};



	typedef utils::Constraint<Formula> 				Constraint;
	typedef utils::ConstraintCombinerPtr<Formula> 	ConstraintPtr;

	typedef utils::Piecewise<Formula> Piecewise;

	Formula toFormula(const Piecewise& pw);
	bool isFormula(const Piecewise& pw);

	Piecewise::PredicatePtr normalize(const Piecewise::Predicate& other);

	inline Piecewise::PredicatePtr operator<(const Formula& a, const Formula& b) {
		return makeCombiner( Piecewise::Predicate(a - b, Piecewise::PredicateType::LT) );
	}

	inline Piecewise::PredicatePtr operator<=(const Formula& a, const Formula& b) {
		return makeCombiner( Piecewise::Predicate(a - b, Piecewise::PredicateType::LE) );
	}

	inline Piecewise::PredicatePtr operator>(const Formula& a, const Formula& b) {
		return makeCombiner( Piecewise::Predicate(a - b, Piecewise::PredicateType::GT) );
	}

	inline Piecewise::PredicatePtr operator>=(const Formula& a, const Formula& b) {
		return makeCombiner( Piecewise::Predicate(a - b, Piecewise::PredicateType::GE) );
	}

} // end namespace arithmetic
} // end namespace core

namespace utils {
	template <>
	inline int asConstant(const insieme::core::arithmetic::Formula& f) { 
		if (!f.isConstant()) { throw "error"; } //fixme
		if (f.isZero()) { return 0; }
		return f.getConstantValue();
	}
}
} // end namespace insieme
