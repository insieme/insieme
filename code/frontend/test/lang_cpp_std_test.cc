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

#include <gtest/gtest.h>

#include "insieme/core/lang/cpp_std.h"

#include "insieme/frontend/utils/test_utils.h"

namespace insieme {
namespace frontend {

	using namespace core;
	using namespace core::lang;

	TEST(LangCppStd, String) {
		NodeManager mgr;
		// in the end, a quite simple test ..
		EXPECT_PRED1(isStdString,utils::parseType(mgr,"std::string"));
	}

	TEST(LangCppStd, Array) {
		NodeManager mgr;
		auto& basic = mgr.getLangBasic();

		// some simple cases
		EXPECT_PRED1(isStdArray,utils::parseType(mgr,"std::array<int,3>"));
		EXPECT_PRED1(isStdArray,utils::parseType(mgr,"std::array<double,2>"));
		EXPECT_PRED1(isStdArray,utils::parseType(mgr,"std::array<A,8>", "struct A {};"));

		// check the access to the element type
		auto type = utils::parseType(mgr,"std::array<int,123223>");
		EXPECT_EQ(basic.getInt4(),getStdArrayElementType(type));

		type = utils::parseType(mgr,"std::array<bool,2>");
		EXPECT_EQ(basic.getBool(),getStdArrayElementType(type));

		type = utils::parseType(mgr,"std::array<double,7>");
		EXPECT_EQ(basic.getReal8(),getStdArrayElementType(type));
	}

	TEST(LangCppStd, Vector) {
		NodeManager mgr;
		auto& basic = mgr.getLangBasic();

		// some simple cases
		EXPECT_PRED1(isStdVector,utils::parseType(mgr,"std::vector<int>"));
		EXPECT_PRED1(isStdVector,utils::parseType(mgr,"std::vector<double>"));
		EXPECT_PRED1(isStdVector,utils::parseType(mgr,"std::vector<A>", "struct A {};"));

		// check the access to the element type
		auto type = utils::parseType(mgr,"std::vector<int>");
		EXPECT_EQ(basic.getInt4(),getStdVectorElementType(type));

		type = utils::parseType(mgr,"std::vector<bool>");
		EXPECT_EQ(basic.getBool(),getStdVectorElementType(type));

		type = utils::parseType(mgr,"std::vector<double>");
		EXPECT_EQ(basic.getReal8(),getStdVectorElementType(type));
	}


	TEST(LangCppStd, Pair) {
		NodeManager mgr;
		auto& basic = mgr.getLangBasic();

		// some simple cases
		EXPECT_PRED1(isStdPair,utils::parseType(mgr,"std::pair<int,double>"));
		EXPECT_PRED1(isStdPair,utils::parseType(mgr,"std::pair<bool,char>"));
		EXPECT_PRED1(isStdPair,utils::parseType(mgr,"std::pair<A,int>", "struct A {};"));

		// check the access to the element type
		auto type = utils::parseType(mgr,"std::pair<int,double>");
		EXPECT_EQ(basic.getInt4(),getStdPairFirstType(type));
		EXPECT_EQ(basic.getReal8(),getStdPairSecondType(type));

		type = utils::parseType(mgr,"std::pair<bool,char>");
		EXPECT_EQ(basic.getBool(),getStdPairFirstType(type));
		EXPECT_EQ(basic.getChar(),getStdPairSecondType(type));

	}


} // frontend namespace
} // insieme namespace
