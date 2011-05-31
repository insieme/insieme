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

namespace insieme {
namespace core {
namespace arithmetic {

	namespace {

		inline Product::Factors getSingle(const core::VariablePtr& var, int exponent) {
			return toVector(std::make_pair(var, exponent));
		}

		template<typename A, typename B>
		std::size_t hashPairs(const vector<std::pair<A,B>>& factors) {
			std::size_t res = 0;
			for_each(factors, [&](const std::pair<A, B>& cur) {
				appendHash(res, cur.first, cur.second);
			});
			return res;
		}


		template<typename Combinator, typename Extractor, typename Container>
		Container combine(const Container& a, const Container& b) {
			typedef typename Container::value_type Element;

			// instantiate the combining operator
			Combinator op;
			Extractor ext;

			// assemble the resulting factors
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
					res.push_back(b);
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


	}

	Product::Product()
		: utils::HashableImmutableData<Product>(0) {};

	Product::Product(const core::VariablePtr& var, int exponent)
		: utils::HashableImmutableData<Product>(hashPairs(getSingle(var, exponent))), variables(getSingle(var, exponent)) {};

	Product::Product(const Factors&& factors)
		: utils::HashableImmutableData<Product>(hashPairs(factors)), variables(factors) {};

	Product Product::operator*(const Product& other) const {
		return Product(combine<std::plus<int>, deref<VariablePtr>>(variables, other.variables));
	}

	Product Product::operator/(const Product& other) const {
		return Product(combine<std::minus<int>, deref<VariablePtr>>(variables, other.variables));
	}

	bool Product::operator<(const Product& other) const {

		// quick shortcut
		if (*this == other) {
			return false;
		}

		// obtain iterators for the variables
		auto it1 = variables.begin();
		auto end1 = variables.end();
		auto it2 = other.variables.begin();
		auto end2 = other.variables.end();

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

	std::ostream& Product::printTo(std::ostream& out) const {

		// check whether product is empty
		if (variables.empty()) {
			return out << "1";
		}

		// print individual factors
		return out << join("*", variables, [](std::ostream& out, const pair<core::VariablePtr, int>& cur) {
			out << *cur.first;
			if (cur.second != 1) {
				out << "^" << cur.second;
			}
		});
	}


	namespace {

		inline Formula::Terms getSingle(const Product& product, int coefficient) {
			return toVector(std::make_pair(product, coefficient));
		}

	}


	Formula::Formula()
		: utils::HashableImmutableData<Formula>(0) {};

	Formula::Formula(const Terms&& terms)
		: utils::HashableImmutableData<Formula>(hashPairs(terms)), terms(terms) {};

	Formula::Formula(int value)
		: utils::HashableImmutableData<Formula>(hashPairs(getSingle(Product(), value))), terms(toVector(std::make_pair(Product(), value))) {};

	Formula::Formula(const Product& product, int coefficient)
		: utils::HashableImmutableData<Formula>(hashPairs(getSingle(product, coefficient))), terms(toVector(std::make_pair(product, coefficient))) {};

	Formula::Formula(const core::VariablePtr& var, int exponent, int coefficient)
		: utils::HashableImmutableData<Formula>(hashPairs(getSingle(Product(var, exponent), coefficient))), terms(toVector(std::make_pair(Product(var, exponent), coefficient))) {};

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
			if (cur.first.isOne() && cur.second == 1) {
				return out << ((isFirst)?"1":"+1");
			}

			if (cur.first.isOne()) {
				return out << format(((isFirst)?"%d":"%+d"), cur.second);
			}

			if (cur.second == 1) {
				return out << ((isFirst)?"":"+") << cur.first;
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

		// TODO: continue here
		// form the cross-product of the terms within the formulas
		// => implement a cross product iterator! in the utils
		// collect products!

		return other;
	}

	Formula operator+(int a, const Product& b) {
		return Formula(a) + Formula(b);
	}

	Formula operator+(const Product& a, int b) {
		return Formula(a) + Formula(b);
	}

	Formula operator+(const Product& a, const Product& b) {
		return Formula(a) + Formula(b);
	}
	Formula operator-(const Product& a, const Product& b) {
		return Formula(a) - Formula(b);
	}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
