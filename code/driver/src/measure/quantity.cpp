/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/driver/measure/quantity.h"

namespace insieme {
namespace driver {
namespace measure {

	namespace {

		short sign(int64_t value) {
			if(value < 0) { return -1; }
			if(value > 0) { return 1; }
			return 0;
		}

		int64_t abs(int64_t value) {
			return value * sign(value);
		}
	}

	Prefix Prefix::operator*(const Prefix& other) const {
		if(sign(coefficient) == sign(other.coefficient)) {
			// same sign
			return Prefix(sign(coefficient) * coefficient * other.coefficient);
		}

		// different sign
		if(abs(coefficient) > abs(other.coefficient)) { return Prefix(coefficient / abs(other.coefficient)); }

		return Prefix(-1 * (abs(other.coefficient) / coefficient));
	}

	Prefix Prefix::operator/(const Prefix& other) const {
		return *this * other.inverse();
	}

	Prefix Prefix::operator^(int exponent) const {
		short s = sign(coefficient);
		int64_t a = abs(coefficient);
		int64_t res = 1;
		for(int i = 0; i < exponent; i++) {
			res *= a;
		}
		return s * res;
	}


	std::ostream& Prefix::printTo(std::ostream& out) const {
		// deal with the unit prefix
		if(coefficient == 1) { return out; }

		// print symbol of known prefixes
		#define PREFIX(SYMBOL, COEFF)                                                                                                                          \
			if(coefficient == COEFF) return out << #SYMBOL

		// positive exponent
		//		PREFIX(Y,1000l*1000l*1000l*1000l*1000l*1000l*1000l*1000l);
		//		PREFIX(Z,1000l*1000l*1000l*1000l*1000l*1000l*1000l);
		PREFIX(E, 1000l * 1000l * 1000l * 1000l * 1000l * 1000l);
		PREFIX(P, 1000l * 1000l * 1000l * 1000l * 1000l);
		PREFIX(T, 1000l * 1000l * 1000l * 1000l);
		PREFIX(G, 1000l * 1000l * 1000l);
		PREFIX(M, 1000l * 1000l);
		PREFIX(k, 1000l);
		PREFIX(h, 100l);
		PREFIX(da, 10l);

		// negative exponent
		//		PREFIX(y,-1000l*1000l*1000l*1000l*1000l*1000l*1000l*1000l);
		//		PREFIX(z,-1000l*1000l*1000l*1000l*1000l*1000l*1000l);
		PREFIX(a, -1000l * 1000l * 1000l * 1000l * 1000l * 1000l);
		PREFIX(f, -1000l * 1000l * 1000l * 1000l * 1000l);
		PREFIX(p, -1000l * 1000l * 1000l * 1000l);
		PREFIX(n, -1000l * 1000l * 1000l);
		PREFIX(u, -1000l * 1000l);
		PREFIX(m, -1000l);
		PREFIX(c, -100l);
		PREFIX(d, -10l);

		// binary multiples
		//		PREFIX(Yi,1024l*1024l*1024l*1024l*1024l*1024l*1024l*1024l);
		//		PREFIX(Zi,1024l*1024l*1024l*1024l*1024l*1024l*1024l);
		PREFIX(Ei, 1024l * 1024l * 1024l * 1024l * 1024l * 1024l);
		PREFIX(Pi, 1024l * 1024l * 1024l * 1024l * 1024l);
		PREFIX(Ti, 1024l * 1024l * 1024l * 1024l);
		PREFIX(Gi, 1024l * 1024l * 1024l);
		PREFIX(Mi, 1024l * 1024l);
		PREFIX(Ki, 1024l);

		#undef PREFIX

		return out << coefficient;
	}

	namespace {

		string buildMsg(const Unit& a, const Unit& b) {
			std::stringstream out;
			out << "Unable to convert " << a << " into " << b;
			return out.str();
		}
	}

	UnitException::UnitException(const Unit& a, const Unit& b) : invalid_argument(buildMsg(a, b)){};

	namespace {

		Prefix getScale(known_unit unit) {
			// create single special case
			switch(unit) {
			case percent: return Prefix(-100);
			default: return Prefix(1);
			}
			assert_fail() << "Should not be reachable!";
			return Prefix();
		}

		Unit::Terms getTerms(known_unit unit) {
			switch(unit) {
			case m: return toVector(Unit::Term("m", 1));
			case s: return toVector(Unit::Term("s", 1));
			case kg: return toVector(Unit::Term("kg", 1));
			case byte: return toVector(Unit::Term("byte", 1));
			case celsius: return toVector(Unit::Term("celsius", 1));
			case percent: return Unit::Terms(); // empty!
			}
			assert_fail() << "Unsupported known unit!";
			return Unit::Terms();
		}
	}

	Unit::Unit(known_unit unit) : scale(getScale(unit)), terms(getTerms(unit)) {}

	namespace {

		template <typename connector_op>
		Unit::Terms merge(const Unit::Terms& a, const Unit::Terms& b) {
			connector_op op;

			Unit::Terms res;

			auto it1 = a.begin();
			auto end1 = a.end();
			auto it2 = b.begin();
			auto end2 = b.end();

			// merge factors in correct order
			while(it1 != end1 && it2 != end2) {
				const Unit::Term& a = *it1;
				const Unit::Term& b = *it2;
				if(a.first == b.first) {
					int newExp = op(a.second, b.second);
					if(newExp != 0) { res.push_back(Unit::Term(a.first, newExp)); }
					++it1;
					++it2;
				} else if(a.first < b.first) {
					res.push_back(a);
					++it1;
				} else {
					// add inverted of second element
					res.push_back(Unit::Term(b.first, op(0, b.second)));
					++it2;
				}
			}

			// copy rest of factors
			auto out = std::back_inserter(res);

			// copy A
			std::copy(it1, end1, out);

			// copy inverse B
			std::transform(it2, end2, out, [](const Unit::Term& cur) -> Unit::Term {
				connector_op op;
				return Unit::Term(cur.first, op(0, cur.second));
			});

			return res;
		}
	}


	Unit Unit::operator*(const Unit& other) const {
		return Unit(scale * other.scale, merge<std::plus<int>>(terms, other.terms));
	}

	Unit Unit::operator/(const Unit& other) const {
		return Unit(scale / other.scale, merge<std::minus<int>>(terms, other.terms));
	}

	Unit Unit::operator^(int exp) const {
		// multiply current exponents with the given exponent
		Unit res = *this;
		res.scale = res.scale ^ exp;
		for_each(res.terms, [&](Unit::Term& cur) { cur.second *= exp; });
		return res;
	}

	bool Unit::operator<(const Unit& other) const {
		// quick check whether it is equal
		if(this == &other) { return false; }

		// compare components first
		if(lexicographical_compare(terms, other.terms)) { return true; }

		// if components are different, the result is clear
		if(terms != other.terms) { return false; }

		// if components are identical => compare prefixes
		return scale < other.scale;
	}


	std::ostream& Unit::printTo(std::ostream& out) const {
		// deal with well known special cases
		if(*this == percent) { return out << "%"; }
		if(isUnit()) { return out << "unit"; }

		if(!scale.isUnit()) {
			out << scale;
			if(terms.size() != 1u) { out << " "; }
		}

		return out << join("*", terms, [](std::ostream& out, const Unit::Term& cur) {
			       out << cur.first;
			       if(cur.second != 1) { out << '^' << cur.second; }
			   });
	}

	UnitPtr Unit::getCommonBase(const UnitPtr& a, const UnitPtr& b) {
		// same type => obvious case
		if(a == b || *a == *b) { return a; }

		// otherwise the components have to be identical
		if(a->terms == b->terms) {
			// return unit with smaller prefix
			return (a->scale < b->scale) ? a : b;
		}

		// no common basis => quit with an exception
		throw UnitException(*a, *b);
	}


	// --- quantity ---

	Quantity::Quantity() : value(0), valid(false), unit() {}

	Quantity Quantity::to(const Unit& unit) const {
		return Quantity(scaleTo(value, *this->unit, unit), unit);
	}

	Quantity& Quantity::operator+=(const Quantity& other) {
		// make sure invalid values are not used within computations
		if(!isValid()) { return *this; }
		if(!other.isValid()) {
			*this = other;
			return *this;
		}

		UnitPtr resUnit = Unit::getCommonBase(unit, other.unit);
		value = scaleTo(value, *unit, *resUnit) + scaleTo(other.value, *other.unit, *resUnit);
		unit = resUnit;
		return *this;
	}

	Quantity& Quantity::operator-=(const Quantity& other) {
		// make sure invalid values are not used within computations
		if(!isValid()) { return *this; }
		if(!other.isValid()) {
			*this = other;
			return *this;
		}

		UnitPtr resUnit = Unit::getCommonBase(unit, other.unit);
		value = scaleTo(value, *unit, *resUnit) - scaleTo(other.value, *other.unit, *resUnit);
		unit = resUnit;
		return *this;
	}

	Quantity& Quantity::operator*=(const Quantity& other) {
		// make sure invalid values are not used within computations
		if(!isValid()) { return *this; }
		if(!other.isValid()) {
			*this = other;
			return *this;
		}

		value = value * other.value;
		unit = (unit->isUnit()) ? other.unit : std::make_shared<Unit>(*unit * *other.unit);
		return *this;
	}

	Quantity& Quantity::operator/=(const Quantity& other) {
		// make sure invalid values are not used within computations
		if(!isValid()) { return *this; }
		if(!other.isValid()) {
			*this = other;
			return *this;
		}

		value = value / other.value;
		unit = (unit->isUnit()) ? other.unit : std::make_shared<Unit>(*unit / *other.unit);
		return *this;
	}

	bool Quantity::operator==(const Quantity& other) const {
		return valid == other.valid && scaleTo(value, *unit, *other.unit) == other.value;
	}

	bool Quantity::operator<(const Quantity& other) const {
		return scaleTo(value, *unit, *other.unit) < other.value;
	}

	std::ostream& Quantity::printTo(std::ostream& out) const {
		if(!valid) { return out << "invalid"; }
		out << format("%.3f", value);
		if(!unit->isUnit()) { out << *unit; }
		return out;
	}


} // end namespace measure
} // end namespace driver
} // end namespace insieme
