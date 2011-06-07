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

namespace insieme {
namespace core {
namespace arithmetic {

	namespace {

		inline vector<Product::Factor> getSingle(const core::VariablePtr& var, int exponent) {
			return toVector(std::make_pair(var, exponent));
		}

		template<typename Combinator, typename Extractor, typename Container>
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
					int exp = op(a.second,b.second);
					if (exp != 0) {
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

		template<typename Extractor, typename Container, typename T>
		inline int findAssignedValue(const Container& list, const T& value) {
			typedef std::pair<T,int> Element;
			Extractor ext;

			// quick check
			if (list.empty()) {
				return 0;
			}

			// short cut for single-element lists
			if (list.size() == static_cast<std::size_t>(1)) {
				if (ext(list[0].first) == ext(value)) {
					return list[0].second;
				}
				return 0;
			}

			// search for product (binary search)
			auto end = list.end();
			auto pos = std::lower_bound(list.begin(), end, std::make_pair(value,0),
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

	}

	Product::Product(const core::VariablePtr& var, int exponent)
		: factors(getSingle(var, exponent)) {};

	Product::Product(const vector<Factor>&& factors)
		: factors(factors) {};

	Product Product::operator*(const Product& other) const {
		return Product(combine<std::plus<int>, deref<VariablePtr>>(factors, other.factors));
	}

	Product Product::operator/(const Product& other) const {
		return Product(combine<std::minus<int>, deref<VariablePtr>>(factors, other.factors));
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

			const pair<core::VariablePtr, int>& a = *it1;
			const pair<core::VariablePtr, int>& b = *it2;

			// make distinction based on first element
			if (*a.first != *b.first) {
				return *a.first < *b.first;
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

	int Product::operator[](const VariablePtr& var) const {
		return findAssignedValue<deref<VariablePtr>>(factors, var);
	}

	std::ostream& Product::printTo(std::ostream& out) const {

		// check whether product is empty
		if (factors.empty()) {
			return out << "1";
		}

		// print individual factors
		return out << join("*", factors, [](std::ostream& out, const Factor& cur) {
			out << *cur.first;
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



	namespace {

		inline vector<Formula::Term> getSingle(const Product& product, int coefficient) {
			return toVector(std::make_pair(product, coefficient));
		}

	}


	Formula::Formula(const vector<Term>&& terms) : terms(terms) {};

	Formula::Formula(int value) : terms(toVector(std::make_pair(Product(), value))) {};

	Formula::Formula(const Product& product, int coefficient) : terms(toVector(std::make_pair(product, coefficient))) {
		assert(coefficient != 0 && "Coefficient must be != 0!");
	};

	Formula::Formula(const core::VariablePtr& var, int exponent, int coefficient) : terms(toVector(std::make_pair(Product(var, exponent), coefficient))) {
		assert(exponent != 0 && "Exponent must be != 0!");
		assert(coefficient != 0 && "Coefficient must be != 0!");
	};

	std::ostream& Formula::printTo(std::ostream& out) const {

		// handle empty formula
		if (terms.empty()) {
			return out << "0";
		}

		// print individual factors
		bool firstTerm = true;
		return out << join("", terms, [&](std::ostream& out, const pair<Product, int>& cur)->std::ostream& {

			bool isFirst = firstTerm;
			firstTerm = false;
			if (cur.first.isOne()) {
				return out << format(((isFirst)?"%d":"%+d"), cur.second);
			}

			if (cur.second == 1) {
				return out << ((isFirst)?"":"+") << cur.first;
			}
			if (cur.second == -1) {
				return out << "-" << cur.first;
			}

			return out <<  format(((isFirst)?"%d":"%+d"), cur.second) << "*" << cur.first;
		});
	}

	Formula Formula::operator+(const Formula& other) const {
		return Formula(combine<std::plus<int>, id<Product>>(terms, other.terms));
	}

	Formula Formula::operator-(const Formula& other) const {
		return Formula(combine<std::minus<int>, id<Product>>(terms, other.terms));
	}

	Formula Formula::operator*(const Formula& other) const {

		// compute cross-product of terms
		Formula res;
		auto range = make_product_range(terms, other.terms);
		for_range(range, [&](const std::pair<Term, Term>& cur){
			const Product& A = cur.first.first;
			const Product& B = cur.second.first;
			int coeffA = cur.first.second;
			int coeffB = cur.second.second;
			int newCoeff = coeffA * coeffB;
			if (newCoeff != 0) {
				res = res + (A * B) * newCoeff;
			}
		});
		return res;
	}

	Formula Formula::operator/(int divisor) const {
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


	int Formula::operator[](const Product& product) const {
		return findAssignedValue<id<Product>>(terms, product);
	}


	bool Formula::isConstant() const {
		return isZero() || (terms.size() == static_cast<std::size_t>(1) && terms[0].first.isOne());
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


} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
