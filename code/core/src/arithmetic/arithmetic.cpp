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

#include "insieme/core/arithmetic/arithmetic.h"

#include <utility>
#include <functional>
#include <algorithm>

// CUDD - the BDD library
#include <cuddObj.hh>
#include <cuddInt.h>

#include "insieme/utils/iterator_utils.h"
#include "insieme/utils/lazy.h"
#include "insieme/utils/constraint.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/lang/basic.h"

namespace insieme {
namespace core {
namespace arithmetic {


	namespace {

		// Compute the Greatest Common Denominator
		uint64_t gcd(uint64_t a, uint64_t b) {
			if (b == 0) { return a; }
			return gcd(b, a%b);
		}

		// Compute the Least Common Multiple
		uint64_t lcm(uint64_t a, uint64_t b) {
			return (a * b) / gcd(a,b);
		}

		void reduce(int64_t& numerator, uint64_t &denominator) {
			uint64_t GCD = gcd(std::abs(numerator), denominator);

			numerator = numerator/static_cast<int64_t>(GCD);
			denominator /= GCD;

			assert(denominator != 0);
		}
	}

	Rational::Rational(int64_t num, uint64_t den) : numerator(num), denominator(den) {
		assert(den != 0 && "Denominator must be > 0");
		if (numerator == 0) {
			denominator = 1;
		} else if (denominator!=1) {
			reduce(numerator, denominator);
		}
	}

	Rational& Rational::operator+=(const Rational& other) {
		uint64_t LCM = lcm( denominator, other.denominator );
		numerator = numerator * (LCM/denominator) + other.numerator * (LCM/other.denominator);
		denominator = LCM;
		return *this;
	}

	Rational& Rational::operator-=(const Rational& other) {
		uint64_t LCM = lcm( std::abs(denominator), other.denominator );
		numerator = numerator * (LCM/denominator) - other.numerator * (LCM/other.denominator);
		denominator = LCM;
		return *this;
	}

	namespace {

		inline vector<Product::Factor> getSingle(const Value& value, int exponent) {
			if (exponent == 0) {
				// do not create an entry if exponent is zero
				return toVector<Product::Factor>();
			}
			return toVector(std::make_pair(value, exponent));
		}

		template<typename ExprTy, typename Combinator, typename Extractor, typename Container>
		Container combine(const Container& a, const Container& b) {
			typedef typename Container::value_type Element;

			// instantiate the combining operator
			Combinator op;
			Extractor ext;

			// assemble the resulting elements
			Container res;

			auto it1 = a.begin();
			auto end1 = a.end();
			auto it2 = b.begin();
			auto end2 = b.end();

			// merge factors in correct order
			while (it1 != end1 && it2 != end2) {
				const Element& a = *it1;
				const Element& b = *it2;
				if (ext(a.first) == ext(b.first)) {
					ExprTy&& exp = op(a.second,b.second);
					if ( exp != ExprTy(0) ) {
						res.push_back(std::make_pair(a.first, exp));
					}
					++it1; ++it2;
				} else if (ext(a.first) < ext(b.first)) {
					res.push_back(a);
					++it1;
				} else {
					// add inverted of second element
					res.push_back(std::make_pair(b.first, op(0,b.second)));
					++it2;
				}
			}

			// copy rest of factors
			auto out = std::back_inserter(res);

			// copy A
			std::copy(it1, end1, out);

			// copy inverse B
			std::transform(it2, end2, out, [](const Element& cur) {
				Combinator op; return std::make_pair(cur.first, op(0, cur.second));
			});

			// done
			return res;
		}

		template<typename ExprTy, typename Extractor, typename Container, typename T>
		inline ExprTy findAssignedValue(const Container& list, const T& value) {
			typedef std::pair<T,ExprTy> Element;
			Extractor ext;

			// quick check
			if (list.empty()) {
				return 0;
			}

			// short cut for single-element lists
			if (list.size() == 1u) {
				if (ext(list[0].first) == ext(value)) {
					return list[0].second;
				}
				return 0;
			}

			// search for product (binary search)
			auto end = list.end();
			auto pos = std::lower_bound(list.begin(), end, std::make_pair(value,ExprTy(0)),
					[](const Element& a, const Element& b) {
						Extractor ext;
						return ext(a.first) < ext(b.first);
					}
			);

			// check whether it has been found
			if (pos != end && ext(pos->first) == ext(value)) {
				return pos->second;
			}

			// not included
			return 0;
		}

		/**
		 * Tests whether node A is less than node B. This
		 * implementation is used to order values within products, hence formulas
		 * and may not be applicable in general.
		 */
		bool lessThan(const NodePtr& a, const NodePtr& b) {
			// check for identity
			if (*a == *b) {
				return false;
			}

			// handle types (compare string representation)
			if (a->getNodeCategory() == NC_Type && b->getNodeCategory() == NC_Type) {
				return toString(*a) < toString(*b);
			}

			// extract node types
			NodeType typeA = a->getNodeType();
			NodeType typeB = b->getNodeType();

			// most important - node type
			if (typeA != typeB) {
				return typeA < typeB;
			}

			// special handling of variables
			if (typeA == core::NT_Variable) {
				const VariablePtr& varA = static_pointer_cast<const Variable>(a);
				const VariablePtr& varB = static_pointer_cast<const Variable>(b);
				return *varA < *varB;
			}

			// special handling for identifiers
			if (typeA == core::NT_StringValue) {
				const StringValuePtr& identA = static_pointer_cast<StringValuePtr>(a);
				const StringValuePtr& identB = static_pointer_cast<StringValuePtr>(b);
				return identA->getValue() < identB->getValue();
			}

			// handle remaining expressions => lexicographically
			const NodeList& listA = a->getChildList();
			const NodeList& listB = b->getChildList();
			return std::lexicographical_compare(listA.begin(), listA.end(), listB.begin(), listB.end(),
					[](const NodePtr& a, const NodePtr& b) { return lessThan(a,b); });
		}

	}


	Value::Value(const ExpressionPtr& value) : value(value) {
		if (!isValue(value)) {
			// TODO: exchange with not a value exception
			throw NotAFormulaException(value);
		}
	}

	namespace {

		bool isValueInternal(const ExpressionPtr& expr, bool topLevel=false) {

			// ---------------------------------------------------
			//  This function is recursively determining whether
			//  a given expression is something considered to
			//  be a value within a formula.
			// ---------------------------------------------------

			const lang::BasicGenerator& basic = expr->getNodeManager().getLangBasic();

			// every value has to be of an integer type
			if (topLevel && !basic.isInt(expr->getType())) {
				return false;
			}

			// all variables are values
			if (expr->getNodeType() == core::NT_Variable) {
				return true;
			}

			// all literals that aren't integers are values
			if (expr->getNodeType() == core::NT_Literal) {
				auto lit = expr.as<LiteralPtr>();

				// check first character to determine whether it is constant
				char first = lit->getStringValue()[0];
				if ('0' <= first && first <= '9') return false;
				if (first == '+' || first == '-') return false;
				return true;
			}

			// all literals are values
			if (!topLevel && expr->getNodeType() == core::NT_Literal) {
				return true;
			}

			// all the rest has to be a call expression
			if (expr->getNodeType() != core::NT_CallExpr) {
				return false;
			}

			const CallExprPtr& call = static_pointer_cast<const CallExpr>(expr);
			const ExpressionPtr& fun = call->getFunctionExpr();
			const ExpressionList& args = call->getArguments();

			// handle references
			if (basic.isRefDeref(fun)) {
				return isValueInternal(args[0]);
			}

			// handle tuples
			if (basic.isTupleRefElem(fun) || basic.isTupleMemberAccess(fun)) {
				return isValueInternal(args[0]);
			}

			// handle composites
			if (basic.isCompositeRefElem(fun) || basic.isCompositeMemberAccess(fun)) {
				return isValueInternal(args[0]);
			}

			try {

				// handle vectors
				if (basic.isVectorSubscript(fun) || basic.isVectorRefElem(fun)) {
					return isValueInternal(args[0]) && toFormula(args[1]).isConstant();
				}

				// handle arrays
				if (basic.isArraySubscript1D(fun) || basic.isArrayRefElem1D(fun)) {
					return isValueInternal(args[0]) && toFormula(args[1]).isConstant();
				}

				// handle other value constructor
				if (isValueConstructor(fun)) {
					// TODO: extend value to store constructor + arguments as formulas or even piecewise
					for_each(args, [](const ExpressionPtr& cur) {
						toFormula(cur);
					});
					return true;
				}

			} catch (const NotAFormulaException& nafe) {
				// subscript was not a constant ..
				return false;
			}

			// everything else is not a value
			return false;
		}
	}


	bool Value::isValue(const ExpressionPtr& expr) {
		return isValueInternal(expr, true);
	}

	bool Value::operator<(const Value& other) const {
		return lessThan(value, other.value);
	}

	std::ostream& Value::printTo(std::ostream& out) const {
		// just use pretty printer to format value
		return out << printer::PrettyPrinter(value, printer::PrettyPrinter::NO_LET_BINDINGS);
	}

	namespace {

		// a marker tag for value constructor expressions
		struct ValueConstructorTag {};

	}

	void markAsValueConstructor(const core::ExpressionPtr& expr) {
		// expression has to be of a function type
		assert(expr->getType()->getNodeType() == NT_FunctionType && "Non-function expression cannot be a value constructor.");

		expr->attachValue(ValueConstructorTag());
	}

	bool isValueConstructor(const core::ExpressionPtr& expr) {
		return expr->hasAttachedValue<ValueConstructorTag>();
	}


	Product::Product(const VariablePtr& var, int exponent)
		: factors(getSingle(var, exponent)) {};

	Product::Product(const Value& value, int exponent)
		: factors(getSingle(value, exponent)) {};

	Product::Product(const vector<Factor>&& factors)
		: factors(factors) {};

	void Product::appendValues(ValueSet& set) const {
		for_each(factors, [&](const Factor& cur) {
			set.insert(cur.first);
		});
	}

	Product& Product::operator*=(const Product& other) {
		factors = combine<int, std::plus<int>, id<Value>>(factors, other.factors);
		return *this;
	}

	Product& Product::operator/=(const Product& other) {
		factors = combine<int, std::minus<int>, id<Value>>(factors, other.factors);
		return *this;
	}

	bool Product::operator<(const Product& other) const {

		// quick shortcut
		if (*this == other) {
			return false;
		}

		// special handling of empty factors => always the last
		if (factors.empty()) {
			return false;
		}
		if (other.factors.empty()) {
			return true;
		}

		// the rest is ordered lexicographically
		return lexicographical_compare(factors, other.factors, [](const Factor& a, const Factor& b) {
			return a.first < b.first || (a.first == b.first && a.second > b.second);
		});
	}

	Product Product::operator^(int exp) const {
		vector<Factor> ret;
	
		for_each(factors, [&] ( const Factor& cur ) {
					ret.push_back( Factor(cur.first, cur.second*exp) );
				});

		return Product( std::move(ret) );
	}

	int Product::operator[](const Value& var) const {
		return findAssignedValue<int, id<Value>>(factors, var);
	}

	std::ostream& Product::printTo(std::ostream& out) const {

		// check whether product is empty
		if (factors.empty()) {
			return out << "1";
		}

		// print individual factors
		return out << join("*", factors, [](std::ostream& out, const Factor& cur) {
			out << cur.first;
			if (cur.second != 1) {
				out << "^" << cur.second;
			}
		});
	}

	bool Product::isValue() const {
		return factors.size() == static_cast<std::size_t>(1) && factors[0].second == 1;
	}

	bool Product::isLinear() const {
		return factors.size() <= static_cast<std::size_t>(1) && all(factors, [](const Factor& cur) {
			return cur.second == 1;
		});
	}

	bool Product::isUnivariate() const {
		return factors.size() == 1u;		// there is exactly one variable
	}

	bool Product::isPolynomial() const {
		return all(factors, [](const Factor& cur) {
			return cur.second > 0;
		});
	}

	size_t Product::getDegree() const {
		// get the degree of each product
		size_t acc = 0;
		for_each(factors, [&] (const Factor& cur) { acc+=cur.second;} );
		return acc;
	}

	


	namespace {
		inline vector<Formula::Term> getSingle(const Product& product, const Rational& coefficient) {
			return toVector(std::make_pair(product, coefficient));
		}
	}

	Formula::Formula(int value) : terms((value==0)?vector<Term>():toVector(Term(Product(), Rational(value)))) {};
	
	Formula::Formula(const Rational& value)
		: terms((value.isZero())?vector<Term>():toVector(Term(Product(), value))) {};

	Formula::Formula(const Product& product, const Rational& coefficient)
		: terms(toVector(std::make_pair(product, coefficient))) {
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};

	Formula::Formula(const core::VariablePtr& var, int exponent, const Rational& coefficient)
		: terms(toVector(std::make_pair(Product(var, exponent), coefficient))) {
		assert(exponent != 0 && "Exponent must be != 0!");
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};

	Formula::Formula(const Value& value, int exponent, const Rational& coefficient)
		: terms(toVector(Term(Product(value, exponent), coefficient))) {
		assert(exponent != 0 && "Exponent must be != 0!");
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};

	void Formula::appendValues(ValueSet& set) const {
		for_each(terms, [&](const Term& cur) {
			cur.first.appendValues(set);
		});
	}

	Formula Formula::replace(const ValueReplacementMap& replacements) const {

		// quick check for empty replacement map
		if (replacements.empty()) {
			return *this;
		}

		// build up resulting formula step by step
		Formula res = 0;
		for_each(terms, [&](const Term& cur) {

			Formula term = 1;
			for_each(cur.first.getFactors(), [&](const Product::Factor& prod) {

				// read current value within factors
				Formula value = prod.first;

				// exchange with replacement if necessary
				auto pos = replacements.find(prod.first);
				if (pos != replacements.end()) {
					value = pos->second;
				}

				// add power
				int exp = prod.second;

				// check for negative exponent
				if (exp < 0) {
					exp = -exp;

					// invert value
					auto& terms = value.getTerms();
					assert(value.getTerms().size() == 1 && "Cannot invert formulas!");
					const Formula::Term& prod = terms[0];

					Product one;
					value = one/prod.first * 1/prod.second;
				}

				// compute power
				Formula pow = value;
				for(int i=1; i<exp; ++i) {
					pow *= value;
				}

				// aggregate result
				term *= pow;
			});

			// sum up terms
			res += term * cur.second;
		});

		return res;
	}

	size_t Formula::getDegree() const {
		// get the degree of each product
		std::set<size_t> degrees;
		for_each(terms, [&](const Term& cur) { degrees.insert(cur.first.getDegree()); });
		if (degrees.empty()) {
			return 0;
		}
		return *degrees.rbegin(); 
	}


	std::ostream& Formula::printTo(std::ostream& out) const {

		// handle empty formula
		if (terms.empty()) { return out << "0"; }

		// print individual factors
		bool firstTerm = true;
		return out << join("", terms, [&](std::ostream& out, const Term& cur)->std::ostream& {

			bool isFirst = firstTerm;
			firstTerm = false;

			// add the + sign if necessary
			if (!isFirst && cur.second.isPositive()) {
				out << "+";
			}

			if (cur.first.isOne()) {
				return out << cur.second;
			}

			if (cur.second.isOne()) {
				return out << cur.first;
			}

			if (cur.second.isMinusOne()) {
				return out << "-" << cur.first;
			}

			return out << cur.second << "*" << cur.first;
		});
	}

	const Formula Formula::operator-() const {
		vector<Term> negTerms;
		for_each(terms, [&](const Term& cur) {
			negTerms.push_back(Term(cur.first, -cur.second));
		});
		return Formula(negTerms);
	}

	Formula& Formula::operator+=(const Formula& other) {
		terms = combine<Rational, std::plus<Rational>, id<Product>>(terms, other.terms);
		return *this;
	}

	Formula& Formula::operator-=(const Formula& other) {
		terms = combine<Rational, std::minus<Rational>, id<Product>>(terms, other.terms);
		return *this;
	}

	Formula& Formula::operator*=(const Formula& other) {

		// compute cross-product of terms
		Formula res;
		for_each(terms, [&](const Term& a) {
			for_each(other.terms, [&](const Term& b){
				const Product& A = a.first;
				const Product& B = b.first;
				const Rational& coeffA = a.second;
				const Rational& coeffB = b.second;
				Rational newCoeff = coeffA * coeffB;
				if (!newCoeff.isZero()) {
					res += (A * B) * newCoeff;
				}
			});
		});
		terms.swap(res.terms);
		return *this;
	}

	Formula& Formula::operator/=(const Rational& divisor) {
		assert(!divisor.isZero() && "Division by 0 detected");
		for_each(terms, [&](Term& cur) {
			cur.second /= divisor;
		});
		return *this;
	}

	Formula& Formula::operator/=(const Product& divisor) {
		for_each(terms, [&](Term& cur) {
			cur.first /= divisor;
		});
		return *this;
	}

	Formula& Formula::operator/=(const Term& divisor) {
		for_each(terms, [&](Term& cur) {
			cur.first /= divisor.first;
			cur.second /= divisor.second;
		});
		return *this;
	}

	bool Formula::lessThan(const Formula& other) const {
		// compare included terms lexicographically
		return this != &other && lexicographical_compare(terms, other.terms, [](const Term& a, const Term& b) {
			return a.first < b.first || (a.first == b.first && a.second > b.second);
		});
	}

	Rational Formula::operator[](const Product& product) const {
		return findAssignedValue<Rational, id<Product>>(terms, product);
	}


	bool Formula::isConstant() const {
		return isZero() || (terms.size() == static_cast<std::size_t>(1) && terms[0].first.isOne());
	}

	bool Formula::isInteger() const {
		return isConstant() && getConstantValue().isInteger();
	}

	bool Formula::isLinear() const {
		return all(terms, [](const Term& cur) {
			return cur.first.isLinear();
		});
	}

	bool Formula::isValue() const {
		return terms.size() == static_cast<std::size_t>(1)
				&& terms[0].second.isOne() && terms[0].first.isValue();
	}

	bool Formula::isUnivariate() const {
		return extractValues().size() == 1u;
	}

	bool Formula::isPolynomial() const {
		return all(terms, [](const Term& cur) {
			return cur.first.isPolynomial();
		});
	}

	Rational Formula::getConstantValue() const {
		assert(isConstant());
		return (terms.empty())?Rational(0):terms[0].second;
	}


	// --- Inequality ------------------------------------------------------------------------------

	Inequality::Inequality(const Formula& f) : formula(f) {
		if (formula.getTerms().empty()) return;

		// reduce coefficients of inequality => no fractions and GCD = 1

		// eliminate fractions
		size_t denominator_lcm = formula.getTerms()[0].second.getDenominator();
		for_each(formula.getTerms(), [&](const Formula::Term& cur) {
			denominator_lcm = lcm(denominator_lcm, cur.second.getDenominator());
		});
		if (denominator_lcm != 1) { formula *= denominator_lcm; }

		// reduce coefficients
		int factor_gcd = formula.getTerms()[0].second.getNumerator();
		for_each(formula.getTerms(), [&](const Formula::Term& cur) {
			factor_gcd = gcd(factor_gcd, std::abs(cur.second.getNumerator()));
		});
		if (factor_gcd != 1) { formula = formula / factor_gcd; }
	}

	// --- Constraints ------------------------------------------------------------------------------

	namespace detail {

		typedef ::BDD CuddBDD;

		class BDDManager {

			Cudd manager;

			/**
			 * A list of equalities maintained by this manager. The position
			 * of each equality is the index of the variable used within the
			 * BDD to represent this equality.
			 */
			vector<Inequality> atomList;


		public:

			int getVarIdFor(const Inequality& inequality) {
				for(std::size_t i =0; i<atomList.size(); ++i) {
					if (atomList[i] == inequality) {
						return i;
					}
				}
				atomList.push_back(inequality);
				return atomList.size() - 1;
			}

			const Inequality* getInequalityWithId(int id) {
				if (0 <= id && id < (int)atomList.size()) {
					return &(atomList[id]);
				}
				return NULL;
			}

			Cudd& getCuddManager() {
				return manager;
			}

			const vector<Inequality>& getAtomList() const {
				return atomList;
			}

			CuddBDD getFalse() {
				return manager.bddZero();
			}

			CuddBDD getTrue() {
				return manager.bddOne();
			}

			CuddBDD getVar(int id) {
				return manager.bddVar(id);
			}

			CuddBDD getVar(const Inequality& var) {
				return manager.bddVar(getVarIdFor(var));
			}

			bool contains(const CuddBDD& bdd) {
				return bdd.manager() == &manager;
			}

		};

		BDDManagerPtr createBDDManager() {
			return std::make_shared<BDDManager>();
		}

		typedef Constraint::DNF DNF;

		class BDD : public utils::Printable {

			/**
			 * The manager maintaining the internally stored BDD.
			 */
			BDDManagerPtr manager;

			/**
			 * This flag is set (and no manager is created) if this BDD is representing true.
			 */
			bool isTrue;

			/**
			 * This flag is set (and no manager is created) if this BDD is representing false.
			 */
			bool isFalse;

			/**
			 * The BDD wrapped by this instance. The pointer might be null if true or false
			 * is represented
			 */
			CuddBDD bdd;

			/**
			 * A lazy-evaluated DNF representation of this BDD.
			 */
			mutable utils::Lazy<DNF> dnf;

		public:

			BDD(bool value)
				: manager(), isTrue(value), isFalse(!value), bdd() {};

			BDD(const BDDManagerPtr& manager, bool value)
				: manager(manager), isTrue(value), isFalse(!value), bdd() {};

			BDD(const BDDManagerPtr& manager, const CuddBDD& bdd)
				: manager(manager), isTrue(bdd == manager->getTrue()), isFalse(bdd == manager->getFalse()), bdd(bdd) {
				assert(manager->contains(bdd) && "Given BDD not managed by given manager!");
			};

			BDDManagerPtr& getBDDManager() {
				return manager;
			}

			// -- some factory methods --

			static BDDPtr getFalseBDD(const BDDManagerPtr& manager) {
				return std::make_shared<BDD>(manager, false);
			}

			static BDDPtr getFalseBDD() {
				return std::make_shared<BDD>(false);
			}

			static BDDPtr getTrueBDD(const BDDManagerPtr& manager) {
				return std::make_shared<BDD>(manager, true);
			}

			static BDDPtr getTrueBDD() {
				return std::make_shared<BDD>(true);
			}

			static BDDPtr getLiteralBDD(const BDDManagerPtr& manager, const Inequality& atom) {
				if (atom.isValid()) {
					return getTrueBDD(manager);
				}
				if (atom.isUnsatisfiable()) {
					return getFalseBDD(manager);
				}
				return std::make_shared<BDD>(manager, manager->getVar(atom));
			}

			static BDDPtr getLiteralBDD(const Inequality& atom) {
				if (atom.isValid()) {
					return getTrueBDD();
				}
				if (atom.isUnsatisfiable()) {
					return getFalseBDD();
				}
				return getLiteralBDD(createBDDManager(), atom);
			}

			bool isValid() const {
				return isTrue;
			}

			bool isUnsatisfiable() const {
				return isFalse;
			}

			// -- support for boolean operations --

			BDD operator!() const {
				// shortcuts
				if (isTrue) return BDD(manager, false);
				if (isFalse) return BDD(manager, true);

				// this is simple - use the same manager + negated BDD
				return BDD(manager, !bdd);
			}

			BDD operator&&(const BDD& other) const {
				// shortcuts
				if (isFalse) return *this;
				if (isTrue) return other;
				if (other.isFalse) return other;
				if (other.isTrue) return *this;

				// move both to the same manager
				CuddBDD otherBDD = getWithinLocalManager(other);
				return BDD(manager, bdd * otherBDD);
			}

			BDD operator||(const BDD& other) const {
				// shortcuts
				if (isFalse) return other;
				if (isTrue) return *this;
				if (other.isFalse) return *this;
				if (other.isTrue) return other;

				// move both to the same manager
				CuddBDD otherBDD = getWithinLocalManager(other);
				return BDD(manager, bdd + otherBDD);
			}

			bool operator==(const BDD& other) const {
				// shortcuts
				if (isTrue) return other.isTrue;
				if (isFalse) return other.isFalse;

				// move both to the same manager
				CuddBDD otherBDD = getWithinLocalManager(other);
				return bdd == otherBDD;
			}

			bool operator!=(const BDD& other) const {
				return !(*this == other);
			}

			bool operator<(const BDD& other) const {
				// order: false < bdd < true
				if (isFalse && !other.isFalse) return true;
				if (isTrue && !other.isTrue) return false;
				if (isFalse && other.isFalse) return false;
				if (isTrue && other.isTrue) return false;

				// compare bdds
				return bdd < getWithinLocalManager(other);
			}

			void toDNFInternal(Constraint::DNF& res, DdNode* node, vector<Constraint::Literal> path, DdNode* zero) const {

				// terminal case - a
				DdNode* N = Cudd_Regular(node);

				// Terminal case: print the path if one has been reached
				if (cuddIsConstant(N)) {
					if (node != zero) {
						// add path to result
						res.push_back(path);
					}
					return;
				}

				// recursively search for paths to 1
				DdNode* Nv = cuddT(N);   // true path
				DdNode* Nnv = cuddE(N);  // false path
				if (Cudd_IsComplement(node)) {
					Nv = Cudd_Not(Nv);
					Nnv = Cudd_Not(Nnv);
				}

				assert(manager->getInequalityWithId(N->index) && "Index should be mapped to inequality!");
				const Inequality& atom = *manager->getInequalityWithId(N->index);

				// false path
				path.push_back(Constraint::Literal(atom, false));
				toDNFInternal(res, Nnv, path, zero);

				// true path
				path.back().second = true;
				toDNFInternal(res, Nv, path, zero);

				// clear path step
				path.pop_back();
			}

			const DNF& toDNF() const {

				// check the lazy evaluated DNF - if it is there, use it
				if (dnf.isEvaluated()) {
					return dnf.getValue();
				}

				// init result DNF
				Constraint::DNF res;

				// collect the literals by iterating through the tree
				vector<Constraint::Literal> path;
				auto zero = bdd.manager()->bddZero().getNode();
				toDNFInternal(res, bdd.getNode(), path, zero);

				// done
				return dnf.setValue(res);
			}

			std::ostream& printTo(std::ostream& out) const {

				// process constant representations
				if (isValid()) return out << "true";
				if (isUnsatisfiable()) return out << "false";

				// the rest is represented using a DNF format
				const DNF& dnf = toDNF();

				return out << join(" or ", dnf, [](std::ostream& out, const Constraint::Conjunction& cur) {
					out << "(" << join(" and ", cur, [](std::ostream& out, const Constraint::Literal& lit) {
						if (lit.second) {
							out << lit.first;
						} else {
							out << "!(" << lit.first << ")";
						}
					}) << ")";
				});
			}

		private:

			CuddBDD getWithinLocalManager(const BDD& bdd) const {

				// check whether it is already within the same manager ...
				if (bdd.manager == manager) {
					return bdd.bdd;
				}

				// migrate to local manager
				CuddBDD res = bdd.bdd.Transfer(manager->getCuddManager());

				// rename variables within result
				// TODO: use only atoms actually in use
				const vector<Inequality>& remote_atoms = bdd.manager->getAtomList();

				Cudd* mgr = &manager->getCuddManager();
				std::size_t numAtoms = remote_atoms.size();

				/**
				 * Within the new manager, the IDs of the variables need to be adapted
				 * to match the local IDs. The method SwapVariables can be used for that.
				 * However, to avoid ID capturing (e.g. by moving var 0 to var 1 and var 1
				 * to var 2 everything will end up to be 2) the src and trg IDs have to be
				 * disjoint.
				 *
				 * To guarantee this, the migration happens in two steps. First, all
				 * the variable IDs are moved by an offset outside the potential target range.
				 * In a second step the moved IDs are moved back into the target range.
				 */

				// prepare some container for the switching
				std::size_t offset = numAtoms + manager->getAtomList().size();
				DdNode* src[numAtoms];
				DdNode* trg[numAtoms];

				// 1) first, move all nodes by an offset of the size of the sum of both managers
				// this step is necessary to avoid index capturing
				{
					for(std::size_t i=0; i<numAtoms; i++) {
						src[i] = manager->getVar(i).getNode();
						trg[i] = manager->getVar(i + offset).getNode();
					}
					BDDvector srcVec(numAtoms, mgr, src);
					BDDvector trgVec(numAtoms, mgr, trg);

					res = res.SwapVariables(srcVec, trgVec);
				}

				// 2) second, move all remote variables back into the proper places
				{
					for(std::size_t i=0; i<numAtoms; i++) {
						int newId = manager->getVarIdFor(remote_atoms[i]);
						src[i] = manager->getVar(i+offset).getNode();
						trg[i] = manager->getVar(newId).getNode();
					}
					BDDvector srcVec(numAtoms, mgr, src);
					BDDvector trgVec(numAtoms, mgr, trg);

					res = res.SwapVariables(srcVec, trgVec);
				}

				// check result
				assert( toString(bdd) == toString(BDD(manager, res)) && "Error during migration!");

				// done
				return res;
			}

		};

	}


	Constraint::Constraint()
		: bdd(detail::BDD::getFalseBDD()) {}

	Constraint::Constraint(const Inequality& atom)
		: bdd(detail::BDD::getLiteralBDD(atom)) {}

	Constraint::Constraint(const detail::BDDPtr& bdd)
		: bdd(bdd) {}

	Constraint Constraint::getFalse(detail::BDDManagerPtr& manager) {
		return Constraint(detail::BDD::getFalseBDD(manager));
	}

	Constraint Constraint::getTrue(detail::BDDManagerPtr& manager) {
		return Constraint(detail::BDD::getTrueBDD(manager));
	}

	Constraint Constraint::getConstraint(detail::BDDManagerPtr& manager, const Inequality& inequality) {
		return Constraint(detail::BDD::getLiteralBDD(manager, inequality));
	}

	bool Constraint::isValid() const {
		return bdd->isValid();
	}

	bool Constraint::isUnsatisfiable() const {
		return bdd->isUnsatisfiable();
	}

	void Constraint::appendValues(ValueSet& set) const {

		// make sure every literal is only processed once
		std::set<const Inequality*, compare_target<const Inequality*>> atoms;

		for_each(toDNF(), [&](const Constraint::Conjunction& conjunct) {
			for_each(conjunct, [&](const Constraint::Literal& lit) {
				// add values to result if literal has been encountered the first time
				if (atoms.insert(&lit.first).second) {
					lit.first.appendValues(set);
				}
			});
		});
	}

	Constraint Constraint::replace(const ValueReplacementMap& replacements) const {

		// quick exit
		if (replacements.empty()) {
			return *this;
		}

		// use common manager for the construction
		detail::BDDManagerPtr manager = bdd->getBDDManager();

		// process DNF form and rebuild result
		Constraint res = Constraint::getFalse(manager);
		for_each(toDNF(), [&](const Constraint::Conjunction& conjunct) {
			if (!res.isValid()) {
				Constraint product = Constraint::getTrue(manager);

				for_each(conjunct, [&](const Constraint::Literal& lit) {
					if (!product.isUnsatisfiable()) {
						Constraint cur = Constraint::getConstraint(manager, lit.first.replace(replacements));
						if (!lit.second) { cur = !cur; }
						product = product && cur;
					}
				});

				res = res || product;
			}
		});

		return res;
	}

	const Constraint Constraint::operator!() const {
		return Constraint(std::make_shared<detail::BDD>(!(*bdd)));
	}

	const Constraint Constraint::operator&&(const Constraint& other) const {
		return Constraint(std::make_shared<detail::BDD>(*bdd && *other.bdd));
	}

	const Constraint Constraint::operator||(const Constraint& other) const {
		return Constraint(std::make_shared<detail::BDD>(*bdd || *other.bdd));
	}

	bool Constraint::operator==(const Constraint& other) const {
		return *bdd == *other.bdd;
	}

	bool Constraint::operator<(const Constraint& other) const {
		return *bdd < *other.bdd;
	}

	const Constraint::DNF& Constraint::toDNF() const {
		return bdd->toDNF();
	}

	std::ostream& Constraint::printTo(std::ostream& out) const {
		return out << *bdd;
	}



	// --- Piecewise ------------------------------------------------------------------------------


	namespace {

		typedef Piecewise::Piece Piece;

		/**
		 * A utility class supporting the proper creation of piece lists.
		 */
		class pieces_builder {

			/**
			 * The actual list of pieces.
			 */
			vector<Piece> pieces;

		public:

			void addPiece(const Constraint& constraint, const Formula& formula) {

				// skip unsatisfiable pieces => not of any value
				if (constraint.isUnsatisfiable()) {
					return;
				}

				// test whether same formula has been computed before
				auto pos = std::find_if(pieces.begin(), pieces.end(), [&](const Piece& cur) {
					return cur.second == formula;
				});
				if (pos != pieces.end()) {
					// append constraint to previous formula
					pos->first = pos->first || constraint;
					return;
				}

				// add new piece
				pieces.push_back(Piece(constraint, formula));
			}

			const vector<Piece>& getPieces() {
				finish();
				return pieces;
			}

			void movePieces(vector<Piece>& target) {
				finish();
				pieces.swap(target);
			}

		private:

			void finish() {
				// sort list before returning
				std::sort(pieces.begin(), pieces.end(), [](const Piece& a, const Piece& b) {
					return a.first < b.first;
				});
			}

		};


		template<typename combinator>
		vector<Piece> combine(const vector<Piece>& as, const vector<Piece>& bs) {

			// create instance of combination operator
			combinator op;

			// create a piece builder
			pieces_builder builder;

			// create all combinations ...
			for_each(as, [&](const Piece& a) {
				for_each(bs, [&](const Piece& b) {
					// just add new piece to the builder
					builder.addPiece(
							a.first && b.first,		// the conjunction of the constraints
							op(a.second, b.second)		// the combination of the values
					);
				});
			});

			// use builder to complete list of pieces
			return builder.getPieces();
		}


		vector<Piece> expandPiece(const Piece& piece, const Formula& elseValue) {
			if (piece.first.isValid()) {
				return toVector(piece);
			}
			if (piece.first.isUnsatisfiable() || piece.second == elseValue) {
				return toVector(Piece(Constraint::getTrue(), elseValue));
			}

			// assign 0 to all non-covered regions
			return toVector(piece, Piece(!piece.first, elseValue));
		}


		struct ConstraintConverter : public utils::ConstraintVisitor<Formula, Constraint> {

			detail::BDDManagerPtr& manager;

			ConstraintConverter(detail::BDDManagerPtr& manager)
				: manager(manager) {}

			Constraint visitRawConstraint(const utils::RawConstraint<Formula>& rcc) {
				const utils::Constraint<Formula>& constraint = rcc.getConstraint();
				const Formula& f = constraint.getFunction();

				// create basic literal using shared manager
				Constraint le = Constraint::getConstraint(manager, Inequality(f));
				Constraint ge = Constraint::getConstraint(manager, Inequality(-f));

				// encode semantic of constraint type
				switch(constraint.getType()) {
				case utils::ConstraintType::EQ: return le && ge; break;
				case utils::ConstraintType::NE: return !(le && ge); break;
				case utils::ConstraintType::LE: return le; break;
				case utils::ConstraintType::LT: return le && !(le && ge); break;
				case utils::ConstraintType::GE: return ge; break;
				case utils::ConstraintType::GT: return ge && !(le && ge); break;
				}
				assert(false && "Unsupported constraint type encountered!");
				return Constraint::getFalse(manager);
			}

			Constraint visitNegConstraint(const utils::NegConstraint<Formula>& ucc) {
				// visit recursive and negate
				return !visit(ucc.getSubConstraint());
			}

			Constraint visitBinConstraint(const utils::BinConstraint<Formula>& bcc) {

				// get both operators recursively
				Constraint lhs = visit(bcc.getLHS());
				Constraint rhs = visit(bcc.getRHS());

				// combine results
				if (bcc.isConjunction()) {
					return lhs && rhs;
				} 
				
				if (bcc.isDisjunction()) {
					return lhs || rhs;
				} 
				assert(false && "Unsupported binary constraint type encountered!");
				return Constraint::getFalse(manager);
			}
		};

		Constraint convert(detail::BDDManagerPtr& manager, const utils::Piecewise<Formula>::PredicatePtr& constraint) {
			ConstraintConverter converter(manager);
			return converter.visit(constraint);
		}

		vector<Piece> extractFrom(const utils::Piecewise<Formula>& other) {

			// handle empty piecewise formula
			if (other.empty()) {
				return toVector(Piece(Constraint::getTrue(), 0));
			}

			// use pieces builder to ensure (most) invariants
			pieces_builder builder;

			detail::BDDManagerPtr manager = detail::createBDDManager();

			Constraint covered = Constraint::getFalse(manager);

			// convert pieces
			for_each(other, [&](const pair<utils::Piecewise<Formula>::PredicatePtr, Formula>& cur) {
				// convert current piece
				Constraint curConstraint = convert(manager, cur.first);
				builder.addPiece(curConstraint, cur.second);

				covered = covered || curConstraint;
			});

			// add final piece, covering all the rest
			builder.addPiece(!covered, Formula(0));

			return builder.getPieces();
		}

	}

	Piecewise::Piecewise(const Constraint& constraint, const Formula& thenValue, const Formula& elseValue)
		: pieces(expandPiece(Piece(constraint, thenValue), elseValue)) {}

	Piecewise::Piecewise(const Piece& piece)
		: pieces(expandPiece(piece, 0)) {}

	Piecewise::Piecewise(const utils::Piecewise<Formula>& other)
		: pieces(extractFrom(other)) {}

	Piecewise::Piecewise(const Constraint& constraint, const Piecewise& thenValue, const Piecewise& elseValue) {
		pieces_builder builder;

		// add then values ...
		for_each(thenValue.pieces, [&](const Piece& cur) {
			builder.addPiece(cur.first && constraint, cur.second);
		});

		// add else values ...
		auto notConstraint = !constraint;
		for_each(elseValue.pieces, [&](const Piece& cur) {
			builder.addPiece(cur.first && notConstraint, cur.second);
		});

		// update pieces
		builder.movePieces(pieces);
	}

	const Piecewise Piecewise::operator-() const {
		vector<Piece> newPieces = pieces;
		for_each(newPieces, [](Piece& cur) {
			cur.second = -cur.second;
		});
		return Piecewise(newPieces);
	}

	Piecewise& Piecewise::operator+=(const Piecewise& other) {
		pieces = combine<std::plus<Formula>>(pieces, other.pieces);
		return *this;
	}

	Piecewise& Piecewise::operator-=(const Piecewise& other) {
		pieces = combine<std::minus<Formula>>(pieces, other.pieces);
		return *this;
	}

	Piecewise& Piecewise::operator*=(const Piecewise& other) {
		pieces = combine<std::multiplies<Formula>>(pieces, other.pieces);
		return *this;
	}

	Piecewise& Piecewise::operator/=(const Formula::Term& divisor) {
		for_each(pieces, [&](Piece& cur) {
			cur.second = cur.second / divisor;
		});
		return *this;
	}

	void Piecewise::appendValues(ValueSet& set) const {
		for_each(pieces, [&](const Piece& cur) {
			cur.first.appendValues(set);
			cur.second.appendValues(set);
		});
	}

	Piecewise Piecewise::replace(const ValueReplacementMap& replacements) const {

		// quick check
		if (replacements.empty()) {
			return *this;
		}

		pieces_builder builder;

		for_each(pieces, [&](const Piece& cur) {
			builder.addPiece(
					cur.first.replace(replacements),
					cur.second.replace(replacements)
			);
		});

		// create resulting piecewise formula
		Piecewise res;
		builder.movePieces(res.pieces);
		return res;
	}

	namespace {

		Constraint getConstraint(const Formula& a, const Formula& b, const LiteralPtr& pred) {
			auto& lang = pred->getNodeManager().getLangBasic();
			if (lang.isSignedIntLt(pred) || lang.isUnsignedIntLt(pred)) {
				return a < b;
			}
			if (lang.isSignedIntLe(pred) || lang.isUnsignedIntLe(pred)) {
				return a <= b;
			}
			if (lang.isSignedIntGt(pred) || lang.isUnsignedIntGt(pred)) {
				return a > b;
			}
			if (lang.isSignedIntGe(pred) || lang.isUnsignedIntGe(pred)) {
				return a >= b;
			}
			if (lang.isSignedIntEq(pred) || lang.isUnsignedIntEq(pred)) {
				return eq(a, b);
			}
			if (lang.isSignedIntNe(pred) || lang.isUnsignedIntNe(pred)) {
				return ne(a, b);
			}
			assert(false && "Unsupported select-predicate encountered!");
			return a < b;		// to avoid a warning regarding no result
		}


		Piecewise buildMinMax(const Piecewise& a, const Piecewise& b, bool min) {

			// build cross product of predicates + constraint values

			// create a piece builder
			pieces_builder builder;

			// create all combinations ...
			for_range(make_product_range(a.getPieces(), b.getPieces()), [&](const pair<Piece,Piece>& cur) {

				// just add new piece to the builder

				auto condition = cur.first.first && cur.second.first;
				auto predTrue = (min) ? cur.first.second < cur.second.second : cur.second.second < cur.first.second;
				builder.addPiece(condition && predTrue, cur.first.second);
				builder.addPiece(condition && !predTrue, cur.second.second);
			});

			// use builder to complete list of pieces
			return builder.getPieces();

		}

	}

	Piecewise min(const Piecewise& a, const Piecewise& b) {
		return buildMinMax(a, b, true);
	}

	Piecewise max(const Piecewise& a, const Piecewise& b) {
		return buildMinMax(a, b, false);
	}

	/**
	 * This method is required by the printable interface and allows
	 * instances of this class to be printed to some output stream.
	 */
	std::ostream& Piecewise::printTo(std::ostream& out) const {
		// Output format:
		// 3*v1+6*v2+6 -> if (2*v1-v2 >= 0); 2 -> if NOT(2*v1-v2 >= 0)
		return out << join("; ", pieces, [](std::ostream& out, const Piecewise::Piece& cur) {
			out << cur.second << " -> if " << cur.first;
		});
	}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
