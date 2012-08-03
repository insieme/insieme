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
#include <set>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/constraint.h"

namespace insieme {
namespace core {
namespace arithmetic {

	using std::vector;
	using std::pair;


	// forward declarations
	class Rational;
	class Value;
	class Formula;

	/**
	 * A type definition for a set of values.
	 */
	typedef std::set<Value> ValueSet;

	/**
	 * A type used to specify value replacements within formulas and other
	 * composed structures.
	 */
	typedef std::map<Value, Formula> ValueReplacementMap;


	/**
	 * A class used to represent rational numbers within the arithmetic infrastructure.
	 * Instances are always stored in their irreducible fraction form.
	 */
	class Rational : public utils::Printable {

		/**
		 * The numerator of the represented rational number.
		 */
		int64_t numerator;

		/**
		 * The denominator of the rational number. To normalize values,
		 * the gcd(numerator,denominator) has to be always one, the denominator
		 * has to be > 0 and in case the numerator is 0, the denominator has to
		 * be 1.
		 */
		uint64_t denominator;

	public:

		/**
		 * Creates a new rational number based on a given integer.
		 * This constructor is overloaded since in this case no reduction
		 * is required.
		 */
		Rational(int64_t num = 0) : numerator(num), denominator(1) {}

		/**
		 * Creates a new rational number based on the given numerator / denominator
		 * pair.
		 *
		 * @param num the numerator of the resulting rational number
		 * @param den the denominator of the resulting rational number
		 */
		Rational(int64_t num, uint64_t den);

	private:

		/**
		 * A private constructor used internally when constructing rational numbers known
		 * to be normalized. In those cases the reduction step can be skipped.
		 *
		 * @param num the numerator of the resulting rational number
		 * @param den the denominator of the resulting rational number
		 * @param dummy a dummy parameter to distinguish this constructor from others
		 */
		Rational(int64_t num, uint64_t den, bool dummy)
			: numerator(num), denominator(den) {
			// ensure proper reduction
			assert(*this == Rational(num, den) && "Input not properly reduced!");
		}

	public:

		/**
		 * Obtains the numerator of this rational.
		 */
		inline int64_t getNumerator() const {
			return numerator;
		}

		/**
		 * Obtains the denominator of this rational number.
		 */
		inline uint64_t getDenominator() const {
			return denominator;
		}

		/**
		 * Tests whether this rational is representing zero.
		 */
		inline bool isZero() const {
			return numerator == 0;
		}

		/**
		 * Tests whether this rational is representing one.
		 */
		inline bool isOne() const {
			return numerator == 1 and denominator == 1;
		}

		/**
		 * Tests whether this rational is representing -1.
		 */
		inline bool isMinusOne() const {
			return numerator == -1 and denominator == 1;
		}

		/**
		 * Tests whether this rational is representing a negative value.
		 */
		inline bool isNegative() const {
			return numerator < 0;
		}

		/**
		 * Tests whether this rational is representing a positve value. Zero
		 * is not considered to be positive.
		 */
		inline bool isPositive() const {
			return numerator > 0;
		}

		/**
		 * Tests whether this rational is representing an integer value.
		 */
		inline bool isInteger() const {
			return denominator == 1;
		}

		/**
		 * Converts this rational to the closest integer (rounding toward zero).
		 */
		inline operator int64_t() const {
			return numerator/static_cast<int64_t>(denominator);
		}

		/**
		 * Converts this rational into a float approximating this rational number.
		 */
		inline operator float() const {
			return static_cast<float>(numerator)/denominator;
		}

		/**
		 * Converts this rational into a doulbe approximating this rational number.
		 */
		inline operator double() const {
			return static_cast<double>(numerator)/denominator;
		}

		/**
		 * Computes the inverse value of this rational, hence 1/x where x is this value.
		 */
		inline Rational invert() const {
			// use internal constructor since result is irreducable
			return Rational((numerator>=0)?denominator:-denominator, abs(numerator), false);
		}


		// -- comparison operators --

		/**
		 * Determines whether the given rational number is equivalent to this rational number.
		 */
		inline bool operator==(const Rational& other) const {
			return numerator == other.numerator && denominator == other.denominator;
		}

		/**
		 * Determines whether the given rational number is not equivalent to this rational number.
		 */
		inline bool operator!=(const Rational& other) const {
			return !(*this == other);
		}

		/**
		 * Implements the less-than comparison operator for rational numbers.
		 */
		inline bool operator<(const Rational& other) const {
			return numerator * (int64_t)other.denominator < other.numerator * (int64_t)denominator;
		}

		/**
		 * Implements the less-or-equal-than comparison operator for rational numbers.
		 */
		inline bool operator<=(const Rational& other) const {
			return *this == other || *this < other;
		}

		/**
		 * Implements the greater-than comparison operator for rational numbers.
		 */
		inline bool operator>(const Rational& other) const {
			return !(*this <= other);
		}

		/**
		 * Implements the greater-or-equal-than comparison operator for rational numbers.
		 */
		inline bool operator>=(const Rational& other) const {
			return !(*this < other);
		}


		// -- arithmetic operators --

		/**
		 * Implements the plus operation for rational numbers.
		 */
		inline const Rational operator+(const Rational& other) const {
			return Rational(*this) += other;
		}

		/**
		 * Implements the minus operation for rational numbers.
		 */
		inline const Rational operator-(const Rational& other) const {
			return Rational(*this) -= other;
		}

		/**
		 * Implements the multiplication operation for rational numbers.
		 */
		inline const Rational operator*(const Rational& other) const {
			return Rational(numerator * other.numerator, denominator * other.denominator);
		}

		/**
		 * Implements the division operation for rational numbers.
		 */
		inline const Rational operator/(const Rational& other) const {
			return *this * other.invert();
		}

		/**
		 * Implements the unary - operation for rational numbers.
		 */
		inline const Rational operator-() const {
			return Rational(-numerator, denominator);
		}


		// -- compound assignment operators --

		/**
		 * Implements the plus-assignment operation for rational numbers.
		 */
		Rational& operator+=(const Rational& other);

		/**
		 * Implements the minus-assignment operation for rational numbers.
		 */
		Rational& operator-=(const Rational& other);

		/**
		 * Implements the multiplication-assignment operation for rational numbers.
		 */
		inline Rational& operator*=(const Rational& other) {
			return *this = *this * other;
		}

		/**
		 * Implements the division-assignment operation for rational numbers.
		 */
		inline Rational& operator/=(const Rational& other) {
			return *this = *this / other;
		}


		/**
		 * Prints a string-representation of this rational number to the given output stream.
		 */
		inline std::ostream& printTo(std::ostream& out) const {
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

		Value(const Value& other) : value(other.value) { }

		Value(Value&& other) : value(std::move(other.value)) { }

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
		inline operator ExpressionPtr() const {
			return value;
		}

		inline Value& operator=(const Value& other) {
			value = other.value;
			return *this;
		}

		inline Value& operator=(Value&& other) {
			value = std::move(other.value);
			return *this;
		}

		/**
		 * Compares this value with another value. Two values are equivalent if
		 * they are defined by the same expression.
		 *
		 * @param other the value to be compared with
		 * @return true if equivalent, false otherwise
		 */
		inline bool operator==(const Value& other) const {
			return this==&other || *value == *other.value;
		}

		/**
		 * Compares this value with another value. The result is simply the negation
		 * of the equality operator.
		 *
		 * @param other the value to be compared to
		 * @return true if not equivalent, false otherwise
		 */
		inline bool operator!=(const Value& other) const {
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
	 * In some cases it might be necessary to allow a certain function symbol to be
	 * included within the expression forming a value. For instance: let v1 be a variable.
	 * The expressions sin(x) or ceil(x/y) should be considered to be values since those are
	 * pure functions being applied on formulas not being coverable by arithmetic
	 * operations.
	 *
	 * To allow function symbols like sin and ceil to occur within values, those have
	 * to be marked as to be value constructors using the this function. The information
	 * is stored inside an annotation of the given expression. Once marked, the tag
	 * can not be removed any more.
	 *
	 * If in doubt whether an expression is a value constructor consider the following rules
	 * 		- arithmetic operations are not
	 * 		- constructors have to be pure (see http://en.wikipedia.org/wiki/Pure_function)
	 * 		- all arguments are formulas
	 * 		- the result is an integer type
	 * 		- an equivalent formula can not be expressed using arithmetic operations / piecewise formulas (e.g. abs(..) is not!)
	 *
	 * @param expr the expression to be considered a value constructor
	 */
	void markAsValueConstructor(const core::ExpressionPtr& expr);

	/**
	 * Tests whether the given expression is a known value constructor. Calls of
	 * the given expression are allowed to occur inside values.
	 *
	 * @see void markAsValueConstructor(const core::ExpressionPtr&)
	 *
	 * @param expr the expression to be tested.
	 * @return true if given expression is a value constructor, false otherwise
	 */
	bool isValueConstructor(const core::ExpressionPtr& expr);


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

		Product(const Product& other) : factors(other.factors) { }

		Product(Product&& other) : factors(std::move(other.factors)) { }

		/**
		 * Obtains a constant reference to the internally maintained factors.
		 *
		 * @return a constant reference to the involved factors
		 */
		inline const vector<Factor>& getFactors() const {
			return factors;
		}

		/**
		 * Appends all values used within this product to the given set
		 * of values.
		 *
		 * @param set the set to be extended
		 */
		void appendValues(ValueSet& set) const;

		/**
		 * Extracts all the values referenced within this product.
		 */
		inline ValueSet extractValues() const {
			ValueSet res; appendValues(res); return res;
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


		inline Product& operator=(const Product& other) {
			factors = other.factors;
			return *this;
		}

		inline Product& operator=(Product&& other) {
			factors = std::move(other.factors);
			return *this;
		}

		/**
		 * Implements the multiplication operator for two products of variables. The
		 * resulting product will represent the result of arithmetically multiplying
		 * the two products (e.g. xy * xz = x^2yz).
		 *
		 * @param other the value this product should be multiplied with
		 * @return the resulting product
		 */
		inline const Product operator*(const Product& other) const {
			return Product(*this) *= other;
		}

		/**
		 * Implements the division operator for two products of variables. The
		 * resulting product will represent the result of arithmetically division of
		 * the two products (e.g. xy / xz = yz^-1).
		 *
		 * @param other the value this product should be divided by
		 * @return the resulting product
		 */
		inline const Product operator/(const Product& other) const {
			return Product(*this) /= other;
		}

		/**
		 * Implements the multiply-assign operation for products.
		 *
		 * @param other the product to be multiplied with
		 * @return a reference to this (modified) instance
		 */
		Product& operator*=(const Product& other);

		/**
		 * Implements the divide-assign operation for products.
		 *
		 * @param other the product to divide this product with
		 * @return a reference to this (modified) instance
		 */
		Product& operator/=(const Product& other);

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
		inline bool operator==(const Product& other) const {
			return this==&other || factors == other.factors;
		}

		/**
		 * Compares this product with another product. The result is simply the negation
		 * of the equality operator.
		 *
		 * @param other the product to be compared to
		 * @return true if not equivalent, false otherwise
		 */
		inline bool operator!=(const Product& other) const {
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
		 * A private constructor allowing the creation of a formula based on a
		 * list of terms. The constructor is private to ensure the invariants
		 * defined for the vector of terms is satisfied by the given terms.
		 *
		 * The value is accepted as a value - so the caller decides whether it is moving
		 * an existing r-value or creating an actual copy.
		 *
		 * @see http://stackoverflow.com/questions/9963798/passing-stdvector-to-constructor-and-move-semantics
		 *
		 * @param terms the terms the resulting formula should consist of
		 * 		   - satisfying all the defined invariants
		 */
		Formula(vector<Term> terms) : terms(std::move(terms)) {};

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
		 * A ordinary copy constructor.
		 */
		Formula(const Formula& other) : terms(other.terms) {}

		/**
		 * A move constructor.
		 */
		Formula(Formula&& other) : terms(std::move(other.terms)) {}

		/**
		 * Checks whether this formula represents zero.
		 *
		 * @return true if so, false otherwise
		 */
		bool isZero() const { return terms.empty(); }

		/**
		 * Checks whether this formula represents one.
		 */
		inline bool isOne() const { 
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
		 * Appends all values used within this formula to the given set of values.
		 */
		void appendValues(ValueSet& set) const;

		/**
		 * Extracts all the values referenced within this formula.
		 */
		ValueSet extractValues() const {
			ValueSet res; appendValues(res); return res;
		}

		/**
		 * Computes a modified version of this formula where all values
		 * are replaced with the substituted specified within the given
		 * replacements.
		 *
		 * @param replacements the replacements to be applied
		 * @return a modified version of this product.
		 */
		Formula replace(const ValueReplacementMap& replacements) const;

		/**
		 * Implements the assignment operator for Formulas
		 * This needs to be declared because we provide an RValue 
		 * constructor for this class 
		 */

		inline Formula& operator=(const Formula& other) {
			terms = other.terms;
			return *this;
		}

		inline Formula& operator=(Formula&& other) {
			terms = std::move(other.terms);
			return *this;
		}

		/**
		 * Implements the plus operator for formulas. The resulting formula will be
		 * the sum of this formula and the given formula.
		 *
		 * @param other the formula to be added to this formula.
		 * @return the sum of this and the given formula
		 */
		inline const Formula operator+(const Formula& other) const {
			return Formula(*this) += other;
		}

		/**
		 * Implements the minus operator for formulas. The resulting formula will be
		 * the difference of this formula and the given formula.
		 *
		 * @param other the formula to be subtracted from this formula.
		 * @return the difference of this and the given formula
		 */
		inline const Formula operator-(const Formula& other) const {
			return Formula(*this) -= other;
		}

		/**
		 * Implements the unary minus operator for formulas.
		 *
		 * @return a formula representing -f if this formula is f
		 */
		const Formula operator-() const;

		/**
		 * Implements the multiplication operator for formulas. The resulting formula will be
		 * the product of this formula and the given formula.
		 *
		 * @param other the formula this formula should be multiplied with.
		 * @return the product of this and the given formula
		 */
		inline const Formula operator*(const Formula& other) const {
			return Formula(*this) *= other;
		}

		/**
		 * Divides all the coefficients of the represented formula by the given divisor.
		 * WARNING: this is a integer division - so the result may not be exact in case
		 * the divisor is not dividing all the coefficients.
		 *
		 * @param divisor the divisor this formula should be divided with
		 * @return the resulting formula containing the reduced coefficients
		 */
		inline const Formula operator/(const Rational& divisor) const {
			return Formula(*this) /= divisor;
		}

		/**
		 * Divides all the terms of the represented formula by the given divisor.
		 *
		 * @param divisor the product by which all terms of this formula should be divided with
		 * @return the resulting formula containing the reduced terms
		 */
		inline const Formula operator/(const Product& divisor) const {
			return Formula(*this) /= divisor;
		}

		/**
		 * Divides this formula by the given term.
		 *
		 * @param divisor the term this formula should be divided by
		 * @return the resulting formula representing the resulting formula
		 */
		inline const Formula operator/(const Term& divisor) const {
			return Formula(*this) /= divisor;
		}

		/**
		 * Divides all the terms of the represented formula by the given divisor.
		 *
		 * @param divisor the product by which all terms of this formula should be divided with
		 * @return the resulting formula containing the reduced terms
		 */
		inline const Formula operator/(const VariablePtr& divisor) const {
			return *this / Product(divisor);
		}

		/**
		 * Implements the plus-assign operator for formulas.
		 *
		 * @param other the formula to be added to this formula
		 * @return a reference to this formula
		 */
		Formula& operator+=(const Formula& other);

		/**
		 * Implements the subtraction-assign operator for formulas.
		 *
		 * @param other the formula to be subtracted from this formula
		 * @return a reference to this formula
		 */
		Formula& operator-=(const Formula& other);

		/**
		 * Implements the multiplication-assign operator for formulas.
		 *
		 * @param other the formula to be multiplied with this formula
		 * @return a reference to this formula
		 */
		Formula& operator*=(const Formula& other);

		/**
		 * Implements the division-assign operator for formulas and rationals.
		 *
		 * @param divisor the structure to divide this formula by
		 * @return a reference to this (modified) formula
		 */
		Formula& operator/=(const Rational& divisor);

		/**
		 * Implements the division-assign operator for formulas and products.
		 *
		 * @param divisor the structure to divide this formula by
		 * @return a reference to this (modified) formula
		 */
		Formula& operator/=(const Product& divisor);

		/**
		 * Implements the division-assign operator for formulas and terms.
		 *
		 * @param divisor the structure to divide this formula by
		 * @return a reference to this (modified) formula
		 */
		Formula& operator/=(const Term& divisor);

		/**
		 * Implements the division-assign operator for formulas and variables.
		 *
		 * @param divisor the structure to divide this formula by
		 * @return a reference to this (modified) formula
		 */
		inline Formula& operator/=(const VariablePtr& divisor) {
			return *this /= Product(divisor);
		}

		/**
		 * Checks whether this formula is equivalent to the given formula.
		 *
		 * @param other the formula to be compared with
		 * @return true if equivalent, false otherwise
		 */
		inline bool operator==(const Formula& other) const {
			return this==&other || terms == other.terms;
		}

		/**
		 * Checks whether this formula is not equivalent to the given formula.
		 */
		inline bool operator!=(const Formula& other) const {
			return !(*this == other);
		}

		/**
		 * Defines a total order on formulas. The order will be lexicographically
		 * based on the terms. Terms with higher coefficients will be smaller
		 * than terms with smaller coefficients.
		 *
		 * NOTE: the operator < is overloaded for constraint handling
		 *
		 * @param other the formula to be compared with
		 * @return true if this formula is smaller (not in the numerical sense), false otherwise
		 */
		bool lessThan(const Formula& other) const;

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
	
	inline Formula operator*(const Rational& a, const VariablePtr& b) {
		return Formula(b, 1, a);
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


	/**
	 * The inequality class is representing constraints in the shape of f <= 0 where
	 * f is an arbitrary formula. Inequalities of this shape are forming the foundation
	 * for constraints being used within piecewise formulas. All other comparison
	 * relations (<, >, >=, == and !=) can be derived from this single inequality constraint.
	 */
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
		Inequality(const Formula& formula = Formula());

		/**
		 * A simple copy constructor.
		 */
		Inequality(const Inequality& other) : formula(other.formula) {}

		/**
		 * A simple move constructor.
		 */
		Inequality(Inequality&& other) : formula(std::move(other.formula)) {}

		/**
		 * Obtains a reference to the formula defining this inequality.
		 */
		inline const Formula& getFormula() const {
			return formula;
		}

		/**
		 * Tests whether this inequality has a constant value,
		 * hence whether it is always valid or always unsatisfiable.
		 *
		 * @return true if constant, false otherwise
		 */
		inline bool isConstant() const {
			return formula.isConstant();
		}

		/**
		 * Tests whether this inequality is valid, hence it is always
		 * true for all variables.
		 *
		 * @return true if valid, false otherwise
		 */
		inline bool isValid() const {
			return formula.isConstant() && formula.getConstantValue() <= Rational(0);
		}

		/**
		 * Tests whether this inequality is unsatisfiable, hence there is
		 * no variable assignment satisfying the represented inequality.
		 *
		 * @return true if unsatisfiable, false otherwise
		 */
		inline bool isUnsatisfiable() const {
			return formula.isConstant() && formula.getConstantValue() > Rational(0);
		}

		/**
		 * Appends all values used within this formula to the given set of values.
		 */
		inline void appendValues(ValueSet& set) const {
			formula.appendValues(set);
		}

		/**
		 * Extracts all the values referenced within this formula.
		 */
		inline ValueSet extractValues() const {
			ValueSet res; appendValues(res); return res;
		}

		/**
		 * Computes a modified version of this inequality where all values
		 * are replaced with the substituted specified within the given
		 * replacements.
		 *
		 * @param replacements the replacements to be applied
		 * @return a modified version of this product.
		 */
		Inequality replace(const ValueReplacementMap& replacements) const {
			return Inequality(formula.replace(replacements));
		}

		inline Inequality& operator=(const Inequality& other) {
			formula = other.formula;
			return *this;
		}

		inline Inequality& operator=(Inequality&& other) {
			formula = std::move(other.formula);
			return *this;
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
		 * Compares this inequality with another inequality. The comparison is required
		 * for sorting literals within constraints.
		 *
		 * @param other the inequality instance to be compared with
		 * @return true if this inequality instance is less, false otherwise
		 */
		bool operator<(const Inequality& other) const {
			return this != &other && formula.lessThan(other.formula);
		}

		/**
		 * This method is required by the printable interface and allows
		 * instances of this class to be printed to some output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << formula << " <= 0";
		}
	};


	namespace detail {

		/**
		 * A helper class used inside the constraint class to represent the boolean structure.
		 * The details of this class are only required within the cpp-file.
		 */
		class BDD;

		// a type def for a shared pointer on the constraint BDD
		typedef std::shared_ptr<BDD> BDDPtr;

		class BDDManager;

		typedef std::shared_ptr<BDDManager> BDDManagerPtr;

		BDDManagerPtr createBDDManager();
	}

	/**
	 * This construct is aggregating inequality constraints within a boolean
	 * formula structure. It allows to form the negation, conjunction and disjunction
	 * of arbitrary nested constraints.
	 *
	 * Internally, the inequalities within a constrain are organized within a BDD
	 * ([reduced and ordered] binary decision diagram) which is a canonical
	 * representation of boolean formulas enabling the simple identification of
	 * valid and unsatisfiable constraints.
	 */
	class Constraint : public utils::Printable {

	public:

		/**
		 * A type to be used for representing literals. The boolean
		 * flag indicates whether the inequality should be negated or not.
		 */
		typedef std::pair<Inequality, bool> Literal;

		/**
		 * The type used to represent conjunctions of literals.
		 */
		typedef vector<Literal> Conjunction;

		/**
		 * The type used to represent a disjunctive normal form. Constraints
		 * can be converted in such a form using the toDNF() method.
		 */
		typedef vector<Conjunction> DNF;


	private:

		/**
		 * A pointer to the internally maintained BDD.
		 */
		detail::BDDPtr bdd;

	public:

		/**
		 * Create a new unsatisfiable constraint (always false).
		 */
		Constraint();

		/**
		 * Creates a new constraint representing the given inequality.
		 *
		 * @param atom the inequality to be represented.
		 */
		Constraint(const Inequality& atom);

	private:

		/**
		 * An internal constructor allowing to create new constraints based
		 * on their internal representation.
		 *
		 * @param bdd the BDD describing the boolean structure of this constraint.
		 */
		Constraint(const detail::BDDPtr& bdd);

	public:

		/**
		 * Obtains a reference to a constant false constraint instance.
		 */
		static inline Constraint getFalse() {
			return Constraint();
		}

		/**
		 * Obtains a reference to a constant false constraint instance.
		 */
		static inline Constraint getTrue() {
			return !getFalse();
		}

		/**
		 * Creates a constraint representing the constant false function
		 * using the given BDD manager.
		 */
		static Constraint getFalse(detail::BDDManagerPtr& manager);

		/**
		 * Creates a constraint representing the constant true function
		 * using the given BDD manager.
		 */
		static Constraint getTrue(detail::BDDManagerPtr& manager);

		/**
		 * Creates a constraint representing the given inequality
		 * using the given BDD manager.
		 */
		static Constraint getConstraint(detail::BDDManagerPtr& manager, const Inequality& inequality);

		/**
		 * Tests whether this constraint is valid, hence it is always
		 * true for all assignments.
		 *
		 * @return true if valid, false otherwise
		 */
		bool isValid() const;

		/**
		 * Tests whether this constraint is unsatisfiable, hence there is
		 * no variable assignment satisfying the represented constraint.
		 *
		 * @return true if unsatisfiable, false otherwise
		 */
		bool isUnsatisfiable() const;

		/**
		 * Determines whether the evaluation of this constraint is independent
		 * of the actual value of any variables.
		 *
		 * @return true if constant, false otherwise
		 */
		bool isConstant() const {
			return isValid() || isUnsatisfiable();
		}

		/**
		 * Appends all values used within this formula to the given set of values.
		 */
		void appendValues(ValueSet& set) const;

		/**
		 * Extracts all the values referenced within this formula.
		 */
		ValueSet extractValues() const {
			ValueSet res; appendValues(res); return res;
		}

		/**
		 * Computes a modified version of this constraint where all values
		 * are replaced with the substituted specified within the given
		 * replacements.
		 *
		 * @param replacements the replacements to be applied
		 * @return a modified version of this constraint.
		 */
		Constraint replace(const ValueReplacementMap& replacements) const;

		/**
		 * Computes a new constraint representing the negation of this constraint.
		 */
		const Constraint operator!() const;

		/**
		 * Computes a new constraint representing the conjunction of this constraint
		 * and the given constraint.
		 */
		const Constraint operator&&(const Constraint& other) const;

		/**
		 * Computes a new constraint representing the disjunction of this constraint
		 * and the given constraint.
		 */
		const Constraint operator||(const Constraint& other) const;

		/**
		 * Compares this constraint with the given constraint.
		 *
		 * @return true if equal, false otherwise
		 */
		bool operator==(const Constraint& other) const;

		/**
		 * Compares this constraint with the given constraint.
		 *
		 * @return true if not equal, false otherwise
		 */
		bool operator!=(const Constraint& other) const {
			return !(*this == other);
		}

		/**
		 * Defines a total order on the constraints to be used inside
		 * sorted containers.
		 */
		bool operator<(const Constraint& other) const;

		/**
		 * Converts this constraint into a DNF form. The method may be used
		 * for converting arithmetic constraints into IR nodes. It is
		 * also used for printing a string representation of constraints.
		 *
		 * @return a DNF version of this constraint.
		 */
		const DNF& toDNF() const;

		/**
		 * This method is required by the printable interface and allows
		 * instances of this class to be printed to some output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};


	inline Constraint operator<=(const Formula& a, const Formula& b) {
		return Constraint(Inequality(a-b));
	}

	inline Constraint operator<(const Formula& a, const Formula& b) {
		return !(b <= a);
	}

	inline Constraint operator>(const Formula& a, const Formula& b) {
		return !(a <= b);
	}

	inline Constraint operator>=(const Formula& a, const Formula& b) {
		return (b <= a);
	}

	inline Constraint eq(const Formula& a, const Formula& b) {
		return (a <= b) && (b <= a);
	}

	inline Constraint ne(const Formula& a, const Formula& b) {
		return !eq(a,b);
	}

	// -- piecewise formula ---

	// a piecewise formula is using different formulas for different ranges

	/**
	 * A piecewise formula is representing
	 */
	class Piecewise : public utils::Printable {

	public:

		typedef pair<Constraint, Formula> Piece;

	private:

		/**
		 * To be canonical (as far as possible) the pieces are ordered
		 * according to the order of the constraints.
		 */
		vector<Piece> pieces;

	public:

//		/**
//		 * Create a new piecewise formula representing a constant value. The
//		 * constant value is covering the entire input domain.
//		 */
//		Piecewise(int value)
//			: pieces(toVector(Piece(Constraint::getTrue(), Formula(value)))) {}

		/**
		 * Create a new piecewise formula based on the given formula. There is
		 * only a single piece covereing the entire input range.
		 */
		Piecewise(const Formula& formula = 0)
			: pieces(toVector(Piece(Constraint::getTrue(), formula))) {}

		/**
		 * Creates a piecewise formula consisting of two pieces. The boundary between
		 * the pieces is defined by the given constrain, the values within the two pieces
		 * by the given values.
		 *
		 * @param constraint the constraint to separate the two pieces
		 * @param thenValue the value if the constraint is satisfied
		 * @param elseValue the value if the constraint is not satisfied
		 */
		Piecewise(const Constraint& constraint, const Formula& thenValue, const Formula& elseValue = 0);

		/**
		 * Creates a piecewise formula consisting of two nested piecewise formulas. Be boundary
		 * between the pieces is defined by the given constraint.
		 *
		 * @param constraint the constraint to separate the two pieces
		 * @param thenValue the value if the constraint is satisfied
		 * @param elseValue the value if the constraint is not satisifed
		 */
		Piecewise(const Constraint& constraint, const Piecewise& thenValue, const Piecewise& elseValue = Formula());

		/**
		 * Creates a new formula based on a single piece. The rest of the range is
		 * set to the constant value 0.
		 *
		 * @param piece the piece to build the piecewise function around.
		 */
		Piecewise(const Piece& piece);

		/**
		 * A copy-constructor creating an actual copy of the piecewise object.
		 */
		Piecewise(const Piecewise& other) : pieces(other.pieces) {}

		/**
		 * A move-constructor for piecewise objects.
		 */
		Piecewise(Piecewise&& other) : pieces(std::move(other.pieces)) {}

//	private:	// TODO: re-enable to make sure piecewise formula invariants are valid

		/**
		 * A private constructor allowing to construct more complex functions.
		 */
		Piecewise(vector<Piece> pieces) : pieces(std::move(pieces)) {}

		/**
		 * Allows to import the utility based piecewise function into the core-arithmetic
		 * infrastructure.
		 */
		Piecewise(const utils::Piecewise<Formula>& other);

	public:

		/**
		 * Obtains a reference to all the pieces forming this function.
		 */
		inline const vector<Piece>& getPieces() const {
			return pieces;
		}

		/**
		 * Tests whether this piecewise formula is representing a formula.
		 */
		inline bool isFormula() const {
			return pieces.size() == 1u && pieces[0].first.isValid();
		}

		/**
		 * Tests whether this piecewise formula is a simple integer constant.
		 */
		inline bool isInteger() const {
			return isFormula() && toFormula().isInteger();
		}

		/**
		 * Converts this piecewise formula into a formula if possible.
		 */
		inline const Formula& toFormula() const {
			assert(isFormula() && "Cannot convert non-formula piecewise function formula!");
			return pieces[0].second;
		}

		/**
		 * Appends all values used within this formula to the given set of values.
		 */
		void appendValues(ValueSet& set) const;

		/**
		 * Extracts all the values referenced within this formula.
		 */
		inline ValueSet extractValues() const {
			ValueSet res; appendValues(res); return res;
		}

		/**
		 * Replaces variables within this piecewise formula by the given substitutions.
		 */
		Piecewise replace(const ValueReplacementMap& replacements) const;

		inline Piecewise& operator=(Piecewise&& other) {
			pieces = std::move(other.pieces);
			return *this;
		}

		inline Piecewise& operator=(const Piecewise& other) {
			pieces = other.pieces;
			return *this;
		}

		/**
		 * Adds support for the unary - operator to the piecewise functions.
		 */
		const Piecewise operator-() const;

		/**
		 * Adds support for the + operator to the piecewise functions.
		 */
		inline const Piecewise operator+(const Piecewise& other) const {
			return Piecewise(*this) += other;
		}

		/**
		 * Adds support for the - operator to the piecewise functions.
		 */
		inline const Piecewise operator-(const Piecewise& other) const {
			return Piecewise(*this) -= other;
		}

		/**
		 * Adds support for the * operator to the piecewise function.
		 */
		inline const Piecewise operator*(const Piecewise& other) const {
			return Piecewise(*this) *= other;
		}

		/**
		 * Adds (limited) support for the / operator to the piecewise function.
		 */
		inline const Piecewise operator/(const Formula::Term& other) const {
			return Piecewise(*this) /= other;
		}

		/**
		 * Adds support for the add-assign operator to the piecewise functions.
		 */
		Piecewise& operator+=(const Piecewise& other);

		/**
		 * Adds support for the sub-assign operator to the piecewise functions.
		 */
		Piecewise& operator-=(const Piecewise& other);

		/**
		 * Adds support for the mul-assign operator to the piecewise functions.
		 */
		Piecewise& operator*=(const Piecewise& other);

		/**
		 * Adds support for the div-assign operator to the piecewise functions.
		 */
		Piecewise& operator/=(const Formula::Term& other);

		/**
		 * Compares this piecewise formula with the given instance.
		 *
		 * @return true if equal, false otherwise
		 */
		inline bool operator==(const Piecewise& other) const {
			return this == &other || pieces == other.pieces;
		}

		/**
		 * Compares this piecewise formula with the given constraint.
		 *
		 * @return true if not equal, false otherwise
		 */
		inline bool operator!=(const Piecewise& other) const {
			return !(*this == other);
		}

		/**
		 * This method is required by the printable interface and allows
		 * instances of this class to be printed to some output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	};


	/**
	 * Computes a piecewise formula representing the minimum of the
	 * two given formulas.
	 *
	 * @param a the first formula to be considered
	 * @param b the second formula to be considered
	 * @return the formula computing the minimum of the two given formulas
	 */
	Piecewise min(const Piecewise& a, const Piecewise& b);

	/**
	 * Computes a piecewise formula representing the maximum of the
	 * two given formulas.
	 *
	 * @param a the first formula to be considered
	 * @param b the second formula to be considered
	 * @return the formula computing the maximum of the two given formulas
	 */
	Piecewise max(const Piecewise& a, const Piecewise& b);

	/**
	 * Computes a piecewise formula representing the absolute value of the
	 * given piecewise formula.
	 *
	 * @param a the formula which's absolute value should be computed
	 * @return a formula representing the absolute value of the given formula.
	 */
	inline Piecewise abs(const Piecewise& a) { return max(a,-a); }

} // end namespace arithmetic
} // end namespace core

namespace utils {
	template <>
	inline int asConstant(const insieme::core::arithmetic::Formula& f) { 
		if (!f.isConstant()) { throw std::logic_error("Formula is not constant"); } //fixme
		if (f.isZero()) { return 0; }
		return static_cast<int64_t>(f.getConstantValue());
	}
}
} // end namespace insieme
