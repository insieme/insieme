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
 *
 */
#pragma once

#include <map>
#include <vector>
#include <memory>

#include <boost/operators.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace driver {
namespace measure {

	/**
	 * This header file is defining a set of utilities for modeling quantities resulting
	 * from measurements. Each quantity has a unit assigned (also unit-less quantities like
	 * counters - for which the generic 'unit' unit is used.)
	 *
	 * Within this header, the following classes are defined
	 * 		- Prefix   .. a prefix for a unit, used to model milli, micor or mebi unit prefixes
	 * 		- Unit     .. the class used to represent composed units in a canonical form
	 * 		- Quantity .. a measurement value linked to its unit - potentially invalid
	 */


	using std::pair;


	// --------------------------------------------------------------------------------------------
	//											Prefix
	// --------------------------------------------------------------------------------------------

	/**
	 * An enumeration of known prefixes to be used for easy specification.
	 */
	enum known_prefix {
		kilo = 1000,
		mega = kilo * 1000,
		giga = mega * 1000,
		milli = -1000,
		micro = milli * 1000,
		nano = micro * 1000,
		kibi = 1024,
		mebi = kibi * 1024,
		gibi = mebi * 1024,
	};

	/**
	 * The class used to represent a prefix for a unit.
	 */
	class Prefix : public utils::Printable, public boost::less_than_comparable<Prefix, Prefix> {
		/**
		 * This coefficient is representing the factor this prefix
		 * is imposing on the unit. E.g. kilo = 1000, mebi = 1024.
		 *
		 * To model prefixes defining fractions (e.g. milli) negative
		 * values are considered to be divisors (milli = -1000).
		 *
		 * This value must not be -1 or 0.
		 */
		int64_t coefficient;

	  public:
		/**
		 * Creates a new prefix based on the given coefficient.
		 */
		Prefix(int64_t coefficient = 1) : coefficient(coefficient) {
			assert_ne(coefficient, 0) << "Coefficient invariant violated!";
			assert_ne(coefficient, -1) << "Coefficient invariant violated!";
		}

		/**
		 * Test whether this prefix is the unit prefix (coefficient == 1).
		 */
		bool isUnit() const {
			return coefficient == 1;
		}

		/**
		 * Inverts this prefix (milli <=> kilo).
		 */
		Prefix inverse() const {
			return Prefix((coefficient == 1) ? 1 : -coefficient);
		}

		/**
		 * Computes the product of the given prefixes.
		 */
		Prefix operator*(const Prefix& prefix) const;

		/**
		 * Computes the quotient of the this and the given prefix.
		 */
		Prefix operator/(const Prefix& prefix) const;

		/**
		 * Raises this coefficient by the given power.
		 */
		Prefix operator^(int exponent) const;

		/**
		 * Compares this prefix with the given one for equality.
		 */
		bool operator==(const Prefix& prefix) const {
			return coefficient == prefix.coefficient;
		}

		/**
		 * Compares this prefix with the given one for inequality.
		 */
		bool operator!=(const Prefix& prefix) const {
			return coefficient != prefix.coefficient;
		}

		/**
		 * Compares this prefix with the given one according to its scaling factor.
		 * This order imposes a total order on all prefixes.
		 */
		bool operator<(const Prefix& prefix) const {
			return coefficient < prefix.coefficient;
		}

		/**
		 * The corresponding equivalent to the < operator.s
		 */
		bool operator>(const Prefix& prefix) const {
			return coefficient > prefix.coefficient;
		}

		/**
		 * A generic function scaling the given value according
		 * to this prefix. E.g. kilo.scale(1.5) = 1500
		 */
		template <typename T>
		T scale(const T& value) const {
			if(coefficient > 0) { return value * coefficient; }
			return value / -coefficient;
		}

		/**
		 * Implemented to allow instances to be printed to streams.
		 * Well known prefixes will be printed according to their SI names.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};


	// forward declaration of units
	class Unit;
	typedef std::shared_ptr<Unit> UnitPtr;


	/**
	 * An exception which will be raised in case a unit-related error occurred
	 * while conducting operations.
	 */
	class UnitException : public std::invalid_argument {
	  public:
		/**
		 * Creates an exception indicating a conversion error between the given units.
		 */
		UnitException(const Unit& a, const Unit& b);
		virtual ~UnitException() throw() {}
	};


	// --------------------------------------------------------------------------------------------
	//											Unit
	// --------------------------------------------------------------------------------------------

	/**
	 * An enumeration of known prefixes to be used for easy specification.
	 */
	enum known_unit {
		m,       // < meter
		s,       // < second
		kg,      // < kg
		percent, // < percent are defined as prefix(-100) * unit()
		byte,    // < byte
		celsius, // < degrees celsius
	};

	/**
	 * The class used to represent units in a canonical form.
	 */
	class Unit : public utils::Printable {
	  public:
		/**
		 * The type used to represent a component of a unit (e.g. m within m/s).
		 * The first component is the name of the unit, the second its exponent.
		 * The exponent must not be 0.
		 */
		typedef pair<string, int> Term;

		/**
		 * A list of terms forming the main part of a unit (only the prefix is missing).
		 * The terms have to be ordered according to their first component. No two
		 * Terms representing the same basic unit must be contained.
		 */
		typedef vector<Term> Terms;

	  private:
		/**
		 * The prefix of the represented unit.
		 */
		Prefix scale;

		/**
		 * The terms forming the actual unit.
		 */
		Terms terms;

		/**
		 * An internal constructor for units to be used internally when creating
		 * new unit instances within operators.
		 */
		Unit(Prefix prefix, const Terms& components) : scale(prefix), terms(components) {}

	  public:
		/**
		 * The default constructor for this type creating the 'unit' unit. This
		 * unit should be used for unit-less quantities like counters.
		 */
		Unit() : scale(1){};

		/**
		 * A implicit converter from the list of known units to an actual unit instance.
		 */
		Unit(known_unit unit);

		/**
		 * Creates a new unit based on the given name.
		 */
		Unit(const string& unit) : scale(1), terms(toVector(Term(unit, 1))) {}

		/**
		 * A test verifying that this instance of a unit is the 'unit' unit.
		 */
		bool isUnit() const {
			return scale.isUnit() && terms.empty();
		}

		/**
		 * Obtains a reference to the prefix of this unit.
		 */
		const Prefix& getPrefix() const {
			return scale;
		}

		/**
		 * Obtains a reference to the unit terms of this unit.
		 */
		const Terms& getUnitTerm() const {
			return terms;
		}

		/**
		 * Creates the product of two units - e.g. m * m => m^2
		 */
		Unit operator*(const Unit& other) const;

		/**
		 * The inverse of the multiplication operation.
		 */
		Unit operator/(const Unit& other) const;

		/**
		 * Computes the exponent of a unit.
		 */
		Unit operator^(int exp) const;

		/**
		 * Scales this unit by the given prefix (it is not replacing the
		 * prefix). Since terms like m * milli are less attractive than
		 * milli * m the multiplication is also overloaded outside of this
		 * class definition.
		 */
		Unit operator*(const Prefix& prefix) const {
			return Unit(prefix * scale, terms);
		}

		/**
		 * Compares this unit with the given one for equality.
		 */
		bool operator==(const Unit& other) const {
			return this == &other || (scale == other.scale && terms == other.terms);
		}

		/**
		 * Compares this unit with the given one for inequality.
		 */
		bool operator!=(const Unit& other) const {
			return !(*this == other);
		}

		/**
		 * Defines a total order on units.
		 */
		bool operator<(const Unit& other) const;

		/**
		 * Enables unit instances to be printed in a human-readable way to the
		 * given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;


		//  -- additional utility functions --

		/**
		 * A utility function computing the common base type of the two given types.
		 * Typically, a base type is selected by comparing the unit terms. If they
		 * are not equal, than no common base can be obtained (indicated by a
		 * UnitException). Otherwise the unit with the smaller prefix is used.
		 *
		 * @param a the first unit to be considered
		 * @param b the second unit to be considered
		 * @return the common base (the smaller of those two)
		 * @throw UnitException if a and b do not have a common base.
		 */
		static UnitPtr getCommonBase(const UnitPtr& a, const UnitPtr& b);
	};

	/**
	 * A conveniences function allowing to combine known prefixes and known units using the
	 * multiplication operation.
	 */
	inline Unit operator*(known_prefix prefix, known_unit unit) {
		return Unit(unit) * Prefix(prefix);
	}

	/**
	 * A conveniences function allowing to combine known prefixes and units using the
	 * multiplication operation.
	 */
	inline Unit operator*(known_prefix prefix, const Unit& unit) {
		return unit * Prefix(prefix);
	}

	/**
	 * A conveniences function allowing to combine prefixes and known units using the
	 * multiplication operation.
	 */
	inline Unit operator*(const Prefix& prefix, known_unit unit) {
		return Unit(unit) * prefix;
	}

	/**
	 * A conveniences function allowing to combine prefixes and units using the
	 * multiplication operation.
	 */
	inline Unit operator*(const Prefix& prefix, const Unit& unit) {
		return unit * prefix;
	}

	/**
	 * A conveniences function allowing to combine prefixes and units using the
	 * multiplication operation.
	 */
	inline Unit operator*(known_unit a, known_unit b) {
		return Unit(a) * Unit(b);
	}
	inline Unit operator*(known_unit a, const Unit& b) {
		return Unit(a) * b;
	}
	inline Unit operator*(const Unit& a, known_unit b) {
		return a * Unit(b);
	}

	inline Unit operator^(known_unit a, int exp) { return Unit(a) ^ exp; }

	inline bool
	operator==(known_unit a, known_unit b) {
		return Unit(a) == Unit(b);
	}
	inline bool operator==(known_unit a, const Unit& b) {
		return Unit(a) == b;
	}
	inline bool operator==(const Unit& a, known_unit b) {
		return a == Unit(b);
	}

	inline bool operator!=(known_unit a, known_unit b) {
		return Unit(a) != Unit(b);
	}
	inline bool operator!=(known_unit a, const Unit& b) {
		return Unit(a) != b;
	}
	inline bool operator!=(const Unit& a, known_unit b) {
		return a != Unit(b);
	}

	/**
	 * A factory method for unit pointers if required.
	 */
	inline UnitPtr makeUnitPtr(const Unit& unit) {
		return std::make_shared<Unit>(unit);
	}

	/**
	 * A generic utility function scaling a given generic, numerical value from one
	 * unit to another if possible. Otherwise a UnitException will be thrown.
	 *
	 * @param value the value to be scaled
	 * @param from the unit of the given value
	 * @param to the unit to be scaled to
	 * @return the scaled value
	 */
	template <typename T>
	T scaleTo(const T& value, const Unit& from, const Unit& to) {
		// shortcut
		if(from == to) { return value; }

		// check common unit term
		if(from.getUnitTerm() != to.getUnitTerm()) { throw UnitException(from, to); }

		const Prefix& pF = from.getPrefix();
		const Prefix& pT = to.getPrefix();
		return pT.inverse().scale(pF.scale(value));
	}


	// --------------------------------------------------------------------------------------------
	//											Quantity
	// --------------------------------------------------------------------------------------------

	/**
	 * A class modeling a quantity - hence the result of a measurement. Quantities are values
	 * linked to units. Also, quantities might be invalid.
	 */
	class Quantity : public utils::Printable,
	                 public boost::equality_comparable<Quantity>,
	                 public boost::less_than_comparable<Quantity, Quantity>,
	                 public boost::addable<Quantity>,
	                 public boost::subtractable<Quantity>,
	                 public boost::multipliable<Quantity>,
	                 public boost::dividable<Quantity> {
		/**
		 * The value to be represented.
		 */
		double value;

		/**
		 * A flag indicating whether the quantity was obtained via a valid measurement.
		 */
		bool valid;

		/**
		 * A pointer to the unit associated to this quantity. A shared pointer is used
		 * to avoid frequent copies when doing computations on values of the same type.
		 */
		UnitPtr unit;

		/**
		 * Create a new, invalid quantity instance based on the given unit.
		 * The constructor is private to avoid accidental conversions. To
		 * create quantities marked being invalid the static factory method
		 * invalid(..) should be used.
		 */
		Quantity(const UnitPtr& unit) : value(0), valid(false), unit(unit) {}


	  public:
		/**
		 * A default constructor creating an invalid value. The unit type
		 * will not be initialized.
		 */
		Quantity();

		/**
		 * Creates a new quantity based on the given parameters.
		 *
		 * @param value the value to be represented
		 * @param unit the associated unit
		 */
		Quantity(double value, Unit unit = Unit()) : value(value), valid(true), unit(std::make_shared<Unit>(unit)) {}

		/**
		 * Creates a new quantity based on the given parameters.
		 *
		 * @param value the value to be represented
		 * @param unit the associated unit
		 */
		Quantity(double value, const UnitPtr& unit) : value(value), valid(true), unit(unit) {}

		/**
		 * A static factory function creating an invalid value for the given unit type.
		 */
		static Quantity invalid(Unit u = Unit()) {
			return Quantity(std::make_shared<Unit>(u));
		}

		/**
		 * A static factory function creating an invalid value for the given unit type.
		 */
		static Quantity invalid(const Unit& u = Unit()) {
			return Quantity(std::make_shared<Unit>(u));
		}

		/**
		 * A static factory function creating an invalid value for the given unit type.
		 */
		static Quantity invalid(const UnitPtr& unit) {
			return Quantity(unit);
		}

		/**
		 * Converts this quantity to the given unit.
		 */
		Quantity to(const Unit& unit) const;

		/**
		 * Checks whether this quantity is valid.
		 */
		bool isValid() const {
			return valid;
		}

		/**
		 * Obtains the contained value.
		 */
		double getValue() const {
			return value;
		}

		/**
		 * Obtains a reference to the unit attached to this quantity.
		 */
		const Unit& getUnit() const {
			return *unit;
		}

		/**
		 * Tests whether the represented value is an integer value.
		 */
		bool isInteger() const {
			return toInteger() == value;
		}

		/**
		 * Converts this value to an integer. The result will only be lossless
		 * if the represented value is an integer.
		 */
		uint64_t toInteger() const {
			return (uint64_t)value;
		}


		// support for arithmetic operations (rest is imported via inheritence)

		Quantity& operator+=(const Quantity& other);
		Quantity& operator-=(const Quantity& other);
		Quantity& operator*=(const Quantity& other);
		Quantity& operator/=(const Quantity& other);

		bool operator==(const Quantity& other) const;
		bool operator<(const Quantity& other) const;

		/**
		 * An implicit conversion to double values.
		 */
		operator double() const {
			return value;
		};

		/**
		 * Allows quantities to be printed to a stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * An overloaded operator converting a pair of value / unit to a quantity.
	 */
	template <typename T>
	Quantity operator*(const T& value, const Unit& unit) {
		return Quantity(value, unit);
	}

	/**
	 * An overloaded operator converting a pair of value / unit to a quantity.
	 */
	template <typename T>
	Quantity operator*(const T& value, const UnitPtr& unit) {
		return Quantity(value, unit);
	}

	/**
	 * An overloaded operator converting a pair of value / unit to a quantity.
	 */
	template <typename T>
	Quantity operator*(const T& value, known_unit unit) {
		return Quantity(value, Unit(unit));
	}

} // end namespace measure
} // end namespace driver
} // end namespace insieme

namespace std {

	/**
	 * Support a pretty print for known units.
	 */
	inline std::ostream& operator<<(std::ostream& out, insieme::driver::measure::known_unit unit) {
		return out << insieme::driver::measure::Unit(unit);
	}
}
