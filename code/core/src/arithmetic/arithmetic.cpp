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

#include <functional>
#include <algorithm>

#include "insieme/utils/iterator_utils.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace core {
namespace arithmetic {

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

	bool Value::isValue(const ExpressionPtr& expr) {

		// ---------------------------------------------------
		//  This function is recursively determining whether
		//  a given expression is something considered to
		//  be a value within a formula.
		// ---------------------------------------------------

		// all variables are values
		if (expr->getNodeType() == core::NT_Variable) {
			return true;
		}

		// all the rest has to be a call expression
		if (expr->getNodeType() != core::NT_CallExpr) {
			return false;
		}

		const lang::BasicGenerator& basic = expr->getNodeManager().getLangBasic();
		const CallExprPtr& call = static_pointer_cast<const CallExpr>(expr);
		const ExpressionPtr& fun = call->getFunctionExpr();
		const ExpressionList& args = call->getArguments();

		// handle references
		if (basic.isRefDeref(fun)) {
			return isValue(args[0]);
		}

		// handle tuples
		if (basic.isTupleRefElem(fun) || basic.isTupleMemberAccess(fun)) {
			return isValue(args[0]);
		}

		// handle composites
		if (basic.isCompositeRefElem(fun) || basic.isCompositeMemberAccess(fun)) {
			return isValue(args[0]);
		}

		try {

			// handle vectors
			if (basic.isVectorSubscript(fun) || basic.isVectorRefElem(fun)) {
				return isValue(args[0]) && toFormula(args[1]).isConstant();
			}

			// handle arrays
			if (basic.isArraySubscript1D(fun) || basic.isArrayRefElem1D(fun)) {
				return isValue(args[0]) && toFormula(args[1]).isConstant();
			}

		} catch (const NotAFormulaException& nafe) {
			// subscript was not a constant ..
			return false;
		}

		// everything else is not a value
		return false;
	}

	bool Value::operator<(const Value& other) const {
		return lessThan(value, other.value);
	}

	std::ostream& Value::printTo(std::ostream& out) const {
		// just use pretty printer to format value
		return out << printer::PrettyPrinter(value);
	}

	Product::Product(const VariablePtr& var, int exponent)
		: factors(getSingle(var, exponent)) {};

	Product::Product(const Value& value, int exponent)
		: factors(getSingle(value, exponent)) {};

	Product::Product(const vector<Factor>&& factors)
		: factors(factors) {};

	Product Product::operator*(const Product& other) const {
		return Product(combine<int, std::plus<int>, id<Value>>(factors, other.factors));
	}

	Product Product::operator/(const Product& other) const {
		return Product(combine<int, std::minus<int>, id<Value>>(factors, other.factors));
	}

	bool Product::operator<(const Product& other) const {

		// quick shortcut
		if (*this == other) {
			return false;
		}

		// obtain iterators for the variables
		auto it1 = factors.begin();
		auto end1 = factors.end();
		auto it2 = other.factors.begin();
		auto end2 = other.factors.end();

		// check whether any of the lists is empty
		if (it1 == end1) {
			return false;
		}
		if (it2 == end2) {
			return true;
		}

		while (it1 != end1 && it2 != end2) {

			const pair<Value, int>& a = *it1;
			const pair<Value, int>& b = *it2;

			// make distinction based on first element
			if (a.first != b.first) {
				return a.first < b.first;
			}

			// first element is the same => consider exponent
			if (a.second != b.second) {
				return a.second > b.second;
			}

			// check next element
			++it1; ++it2;
		}

		// this is smaller if not done yet
		return it1!=end1;
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

	bool Product::isLinear() const {
		return factors.size() <= static_cast<std::size_t>(1) && all(factors, [](const Factor& cur) {
			return cur.second == 1;
		});
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
	
	// Compute the Greatest Common Denominator 
	unsigned gcd(unsigned a, unsigned b) {
		if (b == 0) { return a; }
		return gcd(b, a%b);
	}

	// Compute the Least Common Multiple
	unsigned lcm(unsigned a, unsigned b) {
		return a * b / gcd(a,b);
	}

	} // end anonymous namespace

	void Div::simplify() {
		unsigned GCD = gcd(abs(numerator), denominator);

		numerator = numerator/static_cast<int>(GCD);
		denominator /= GCD;

		assert(denominator != 0);
	}

	Div::Div(int num, unsigned den) : numerator(num), denominator(den) {
		assert(den != 0 && "Denominator must be > 0");
		simplify();
	}


	Div Div::operator+(const Div& other) const {
		unsigned LCM = lcm( abs(denominator), other.denominator );
		return Div( numerator * (LCM/denominator) + other.numerator * (LCM/other.denominator), LCM );
	}

	Div Div::operator-(const Div& other) const {
		unsigned LCM = lcm( abs(denominator), other.denominator );
		return Div( numerator * (LCM/denominator) - other.numerator * (LCM/other.denominator), LCM );
	}


	bool Div::operator<(const Div& other) const {
		unsigned LCM = lcm( abs(denominator), other.denominator );
		return numerator * (LCM/denominator) < other.numerator * (LCM/other.denominator);
	}

	namespace {

		inline vector<Formula::Term> getSingle(const Product& product, const Div& coefficient) {
			return toVector(std::make_pair(product, coefficient));
		}

	}


	Formula::Formula(const vector<Term>&& terms) : terms(terms) {};

	Formula::Formula(int value) : terms((value==0)?vector<Term>():toVector(std::make_pair(Product(), Div(value)))) {};
	
	Formula::Formula(const Div& div) : terms((div.isZero())?vector<Term>():toVector(std::make_pair(Product(), div))) {};

	Formula::Formula(const Product& product, const Div& coefficient) : terms(toVector(std::make_pair(product, coefficient))) {
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};

	Formula::Formula(const core::VariablePtr& var, int exponent, const Div& coefficient) : terms(toVector(std::make_pair(Product(var, exponent), coefficient))) {
		assert(exponent != 0 && "Exponent must be != 0!");
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};

	Formula::Formula(const Value& value, int exponent, const Div& coefficient) : terms(toVector(std::make_pair(Product(value, exponent), coefficient))) {
		assert(exponent != 0 && "Exponent must be != 0!");
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};


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
		return out << join("", terms, [&](std::ostream& out, const pair<Product, Div>& cur)->std::ostream& {

			bool isFirst = firstTerm;
			firstTerm = false;
			if (cur.first.isOne()) {
				out << format((isFirst?"%d":"%+d"), cur.second.getNum());
				if (!cur.second.isInteger()) {
					out << '/' << cur.second.getDen();
				}

				return out;
			}

			if (cur.second.isOne()) {
				return out << ((isFirst)?"":"+") << cur.first;
			}
			if (cur.second.getNum() == -1 && cur.second.getDen() == 1) {
				return out << "-" << cur.first;
			}

			out <<  format(((isFirst)?"%d":"%+d"), cur.second.getNum());
			if (!cur.second.isInteger())
				out << '/' << cur.second.getDen();
			return out << '*' << cur.first;
		});
	}

	Formula Formula::operator+(const Formula& other) const {
		return Formula(combine<Div, std::plus<Div>, id<Product>>(terms, other.terms));
	}

	Formula Formula::operator-(const Formula& other) const {
		return Formula(combine<Div, std::minus<Div>, id<Product>>(terms, other.terms));
	}

	Formula Formula::operator*(const Formula& other) const {

		// compute cross-product of terms
		Formula res;
		auto range = make_product_range(terms, other.terms);
		for_range(range, [&](const std::pair<Term, Term>& cur){
			const Product& A = cur.first.first;
			const Product& B = cur.second.first;
			const Div& coeffA = cur.first.second;
			const Div& coeffB = cur.second.second;
			Div&& newCoeff = coeffA * coeffB;
			if (!newCoeff.isZero()) {
				res = res + (A * B) * newCoeff;
			}
		});
		return res;
	}

	Formula Formula::operator/(const Div& divisor) const {
		assert(divisor != Div(0) && "Division by 0 detected");
		Formula res = *this;
		for_each(res.terms, [&](Term& cur) {
			cur.second = cur.second / divisor;
		});
		return res;
	}

	Formula Formula::operator/(const Product& divisor) const {
		Formula res = *this;
		for_each(res.terms, [&](Term& cur) {
			cur.first = cur.first / divisor;
		});
		return res;
	}

	Formula Formula::operator/(const Term& divisor) const {
		Formula res = *this;
		for_each(res.terms, [&](Term& cur) {
			cur.first = cur.first / divisor.first;
			cur.second = cur.second / divisor.second;
		});
		return res;
	}

	Div Formula::operator[](const Product& product) const {
		return findAssignedValue<Div, id<Product>>(terms, product);
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

	bool Formula::isPolynomial() const {
		return all(terms, [](const Term& cur) {
			return cur.first.isPolynomial();
		});
	}

	Div Formula::getConstantValue() const {
		assert(isConstant());
		return terms[0].second;
	}


//===== Constraint ================================================================================
Piecewise::PredicatePtr normalize(const Piecewise::Predicate& c) {
	const Piecewise::PredicateType& type = c.getType();

	if ( type == Piecewise::PredicateType::EQ || 
		 type == Piecewise::PredicateType::GE ) 
	{ 
		return makeCombiner(c); 
	}

	if ( type == Piecewise::PredicateType::NE ) {
		// if the contraint is !=, then we convert it into a negation
		return not_( Piecewise::Predicate(c.getFunction(), Piecewise::PredicateType::EQ) );
	}

	Formula newF( c.getFunction() );
	// we have to invert the sign of the coefficients 
	if ( type == Piecewise::PredicateType::LT || 
	     type == Piecewise::PredicateType::LE ) 
	{
		newF = 0 - newF;
	}
	if ( type == Piecewise::PredicateType::LT || 
		 type == Piecewise::PredicateType::GT ) 
	{
		// we have to subtract -1 to the constant part
		newF = newF - 1;
	}
	return newF >= 0;
}


Formula toFormula(const Piecewise& pw) {
	if (pw.empty()) { return Formula(); }
	
	typedef utils::RawConstraintCombiner<Formula> RawPredicate;
	typedef std::shared_ptr<const RawPredicate> RawPredicatePtr;
	
	// The only sitation where a piecewise can be converted into a formula is when a single piece is
	// contained and the predicate is the identity 0 == 0.
	
	const Piecewise::Piece& p = *pw.begin();
	if (RawPredicatePtr pred = std::dynamic_pointer_cast<const RawPredicate>(p.first) ) {
		const Piecewise::Predicate& cons = pred->getConstraint();
		if ( cons.getFunction() == Formula() && cons.getType() == Piecewise::PredicateType::EQ ) {
			return p.second;
		}
	}
	throw NotAFormulaException( NodePtr() );
}

bool isFormula(const Piecewise& pw) {
	try {
		toFormula(pw);
		return true;
	} catch (const NotAFormulaException& e) {}
	return false;
}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
