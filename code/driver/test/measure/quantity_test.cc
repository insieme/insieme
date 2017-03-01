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
#include <gtest/gtest.h>

#include <cstdlib>

#include "insieme/driver/measure/quantity.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace measure {

	TEST(Measuring, Prefix) {
		// check prefixes
		Prefix one;
		EXPECT_EQ("", toString(one));

		// some derived prefixes
		EXPECT_EQ("k", toString(one * 1000));
		EXPECT_EQ("m", toString(one / 1000));

		// test inverse
		EXPECT_EQ(one * 1000, (one / 1000).inverse());


		auto kilo = one * 1000;
		auto mega = kilo * 1000;
		auto milli = mega.inverse();

		EXPECT_EQ(mega, one * 1000 * 1000);
		EXPECT_EQ(mega, kilo * kilo);

		EXPECT_EQ(milli, one / 1000 / 1000);

		EXPECT_NE(mega, one * 1024 * 1024);


		EXPECT_EQ(1000, kilo.scale(1));
		EXPECT_EQ(1000000.0, mega.scale(1.0));
	}

	TEST(Measuring, Units) {
		// play a little using units
		EXPECT_EQ("m", toString(m));
		EXPECT_EQ("kg^2", toString(kg ^ 2));
		EXPECT_EQ("kg*m", toString(kg * m));

		// some combined value
		EXPECT_EQ("kg*m*s^-2", toString((kg * m) / (s ^ 2)));

		// no-unit
		EXPECT_EQ("unit", toString(Unit()));
		EXPECT_TRUE(Unit().isUnit());

		EXPECT_FALSE(Unit(m).isUnit());

		// try prefixes
		EXPECT_EQ("km", toString(kilo * m));
		EXPECT_EQ("ns", toString(nano * s));

		auto ns = nano * s;
		auto ms = milli * s;

		// compute result of multiplying ns and milli seconds
		EXPECT_EQ("ps^2", toString(ns * ms));

		// test special cases
		EXPECT_EQ("%", toString(percent));
	}


	TEST(Measuring, Quantities) {
		// play a little using units
		auto a = 1 * m;   // should be 1 meter
		auto b = 1.5 * m; // another distance

		EXPECT_EQ("1.000m", toString(a));
		EXPECT_EQ("1.500m", toString(b));
		EXPECT_EQ("1.500m^2", toString(a * b));

		auto d = 150 * m;
		auto t = 20 * s;
		EXPECT_EQ("150.000m", toString(d));
		EXPECT_EQ("20.000s", toString(t));
		EXPECT_EQ("7.500m*s^-1", toString(d / t));


		// do some scaling
		auto mm = milli * m;
		EXPECT_EQ("1000.000mm", toString(a.to(mm)));


		// adding up quantities with different prefixes
		auto ns = nano * s;
		auto us = micro * s;

		auto t1 = 10 * us;
		auto t2 = 1500 * ns;

		EXPECT_EQ(15000 * ns, 15 * us);

		EXPECT_EQ("10.000us", toString(t1));
		EXPECT_EQ("1500.000ns", toString(t2));

		EXPECT_EQ(11500 * ns, t1 + t2);
		EXPECT_EQ(8500 * ns, t1 - t2);

		EXPECT_THROW(toString(Quantity(11500) == (t1 + t2)), UnitException);

		EXPECT_LT(t2, t1);
		EXPECT_LE(t2, t1);
		EXPECT_GT(t1, t2);
		EXPECT_GE(t1, t2);

		EXPECT_TRUE(t2 < t1);
		EXPECT_TRUE(t1 > t2);
		EXPECT_TRUE(t2 <= t1);
		EXPECT_TRUE(t1 >= t2);
		EXPECT_TRUE(t1 == t1);
		EXPECT_TRUE(t1 != t2);

		// some stuff that should not be possible
		EXPECT_THROW(a.to(s), UnitException);
	}

	TEST(Measuring, QuantityValue) {
		auto a = 150 * m;
		EXPECT_TRUE(a.isInteger());
		EXPECT_EQ(m, a.getUnit());
		EXPECT_EQ(150.0, a.getValue());
		EXPECT_EQ(150u, a.toInteger());

		auto km = kilo * m;
		auto b = a.to(km);
		EXPECT_FALSE(b.isInteger());
		EXPECT_EQ(km, b.getUnit());
		EXPECT_EQ(0.15, b.getValue());
	}

	TEST(Measuring, ArithmeticOps) {
		auto a = 150 * m;
		auto b = 200 * m;
		auto c = 20 * (milli * s);

		EXPECT_EQ("350.000m", toString(a + b));
		EXPECT_EQ("-50.000m", toString(a - b));
		EXPECT_EQ("30000.000m^2", toString(a * b));
		EXPECT_EQ("0.750", toString(a / b));

		EXPECT_EQ("3000.000m m*s", toString(a * c));
		EXPECT_EQ("7.500k m*s^-1", toString(a / c));

		auto x = 36 * (nano * s);
		auto y = 72 * (nano * s);
		EXPECT_EQ("0.500", toString(x / y));
	}

} // end namespace measure
} // end namespace driver
} // end namespace insieme
