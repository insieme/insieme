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

#include "insieme/core/lang/enum.h"
#include "insieme/core/test/test_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {

	TEST(Enum, Simple) {
		NodeManager nm;
		IRBuilder builder(nm);
		GenericTypePtr enumName = GenericType::get(nm, "Color");
		GenericTypePtr enumBlue = GenericType::get(nm, "Blue");
		GenericTypePtr enumRed = GenericType::get(nm, "Red");
		GenericTypePtr enumGreen = GenericType::get(nm, "Green");
		/****ENUM ENTRY*****/
		GenericTypePtr eeBlue = lang::EnumEntry::create(enumBlue, builder.intLit(3));
		GenericTypePtr eeRed = lang::EnumEntry::create(enumRed, builder.intLit(5));
		GenericTypePtr eeGreen = lang::EnumEntry::create(enumGreen, builder.add(builder.intLit(4), builder.intLit(3)));
		EXPECT_EQ("enum_entry<Blue,3>", toString(*eeBlue));
		EXPECT_EQ("enum_entry<Red,5>", toString(*eeRed));
		EXPECT_EQ("enum_entry<Green,7>", toString(*eeGreen));
		/****ENUM DEFINITION*****/
		GenericTypePtr finalEnum = lang::EnumDefinition::create(enumName, builder.getLangBasic().getInt4(), { eeBlue, eeGreen, eeRed });
		EXPECT_EQ("enum_def<Color,int<4>,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>", toString(*finalEnum));
		/****ENUM TAG TYPE*****/
		auto enumTy = core::lang::buildEnumType(finalEnum);
		EXPECT_TRUE(core::lang::isEnum(enumTy));
		/****ENUM TAG TYPE SEMA CHECKS*****/
		auto correct = builder.parseType("(enum_def<Color,enum_entry<Blue,3>>, int<8>)");
		auto c = core::checks::check(correct);
		EXPECT_FALSE(c.getErrors().size());
		//missing name sema fail
		auto incorrectName = builder.parseType("(enum_def<enum_entry<Blue,3>>, int<8>)");
		c = core::checks::check(incorrectName);
		EXPECT_TRUE(c.getErrors().size() == 1);
		EXPECT_TRUE(c.getErrors()[0].getMessage().find("Enum definition contains invalid name") != std::string::npos);
		//enum entry sema fail
		auto incorrectEntry = builder.parseType("(enum_def<Name, enum_entry<Blue>>, int<8>)");
		c = core::checks::check(incorrectEntry);
		EXPECT_TRUE(c.getErrors().size() == 1);
		EXPECT_TRUE(c.getErrors()[0].getMessage().find("Invalid enum entry discovered") != std::string::npos) << c;
	}

	TEST(Enum, EnumInit) {
		NodeManager nm;
		IRBuilder builder(nm);
		GenericTypePtr enumName = GenericType::get(nm, "Color");
		GenericTypePtr enumBlue = GenericType::get(nm, "Blue");
		GenericTypePtr enumRed = GenericType::get(nm, "Red");
		GenericTypePtr enumGreen = GenericType::get(nm, "Green");
		/****ENUM ENTRY*****/
		GenericTypePtr eeBlue = lang::EnumEntry::create(enumBlue, builder.intLit(3));
		GenericTypePtr eeRed = lang::EnumEntry::create(enumRed, builder.intLit(5));
		GenericTypePtr eeGreen = lang::EnumEntry::create(enumGreen, builder.add(builder.intLit(4), builder.intLit(3)));
		/****ENUM DEFINITION + TYPE*****/
		GenericTypePtr finalEnum = lang::EnumDefinition::create(enumName, builder.getLangBasic().getInt4(), { eeBlue, eeGreen, eeRed });
		auto enumTy = core::lang::buildEnumType(finalEnum);
		/****ENUM DEFINITION*****/
		auto x = lang::buildEnumValue(finalEnum, builder.intLit(5));
		auto y = lang::buildEnumValue(finalEnum, builder.intLit(3));
		EXPECT_EQ("tuple(type<enum_def<Color,int<4>,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>>,5)", toString(*x));
		EXPECT_EQ("tuple(type<enum_def<Color,int<4>,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>>,3)", toString(*y));
		EXPECT_EQ("(type<enum_def<Color,int<4>,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>>,int<4>)", toString(*(x->getType())));
		EXPECT_EQ("(type<enum_def<Color,int<4>,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>>,int<4>)", toString(*(y->getType())));
		auto c1 = core::checks::check(x);
		EXPECT_TRUE(c1.getErrors().empty());
		auto c2 = core::checks::check(y);
		EXPECT_TRUE(c2.getErrors().empty());
	}

	TEST(Enum, enumChecks) {
		NodeManager nm;
		IRBuilder b(nm);
		auto& basic = b.getLangBasic();

		auto inttype = basic.getInt4();
		auto genType = b.genericType("A");
		auto deeptype = b.genericType("B", TypeList(inttype, inttype));

		// check isEnumDefinition
		EXPECT_FALSE(EnumDefinition::isEnumDefinition(inttype));
		EXPECT_FALSE(EnumDefinition::isEnumDefinition(genType));
		EXPECT_FALSE(EnumDefinition::isEnumDefinition(b.genericType("enum_def", TypeList(inttype, inttype))));
		EXPECT_FALSE(EnumDefinition::isEnumDefinition(b.genericType("enum_def", TypeList(deeptype, inttype))));
		EXPECT_FALSE(EnumDefinition::isEnumDefinition(b.genericType("enum_def", TypeList(inttype, deeptype))));
		auto list = TypeList(inttype, deeptype);
		list.push_back(inttype);
		EXPECT_FALSE(EnumDefinition::isEnumDefinition(b.genericType("A", list)));
	}

	TEST(Enum, isEnumEntry) {
		NodeManager nm;
		IRBuilder b(nm);
		auto& basic = b.getLangBasic();

		auto inttype = basic.getInt4();
		auto strtype = basic.getString();
		auto numtype = b.numericType(1);
		auto gentype = b.genericType("A");

		auto list1 = TypeList();
		list1.push_back(inttype);
		list1.push_back(gentype);

		auto list2 = TypeList();
		list2.push_back(gentype);
		list2.push_back(gentype);

		auto list3 = TypeList();
		list3.push_back(gentype);
		list3.push_back(numtype);

		EXPECT_FALSE(EnumEntry::isEnumEntry(inttype));
		EXPECT_FALSE(EnumEntry::isEnumEntry(gentype));
		EXPECT_FALSE(EnumEntry::isEnumEntry(b.genericType("A", list1)));
		EXPECT_FALSE(EnumEntry::isEnumEntry(b.genericType("enum_entry", list1)));
		EXPECT_FALSE(EnumEntry::isEnumEntry(b.genericType("enum_entry", list2)));
		EXPECT_TRUE( EnumEntry::isEnumEntry(b.genericType("enum_entry", list3)));
	}

	TEST(Enum, EnumDerivedOps) {
		NodeManager nm;
		IRBuilder builder(nm);
		auto& ext = nm.getLangExtension<EnumExtension>();
		GenericTypePtr enumName = GenericType::get(nm, "Color");
		GenericTypePtr enumBlue = GenericType::get(nm, "Blue");
		GenericTypePtr enumRed = GenericType::get(nm, "Red");
		GenericTypePtr enumGreen = GenericType::get(nm, "Green");
		GenericTypePtr eeBlue = lang::EnumEntry::create(enumBlue, builder.intLit(3));
		GenericTypePtr eeRed = lang::EnumEntry::create(enumRed, builder.intLit(5));
		GenericTypePtr eeGreen = lang::EnumEntry::create(enumGreen, builder.add(builder.intLit(4), builder.intLit(3)));

		GenericTypePtr finalEnum = lang::EnumDefinition::create(enumName, builder.getLangBasic().getInt4(), { eeBlue, eeGreen, eeRed });

		auto x = lang::buildEnumValue(finalEnum, builder.intLit(5));
		auto y = lang::buildEnumValue(finalEnum, builder.intLit(3));

		auto equality = builder.callExpr(builder.getLangBasic().getBool(), ext.getEnumEquals(), x, y);
		auto enum_to_int = builder.callExpr(builder.getLangBasic().getInt4(), ext.getEnumToInt(), x);
		auto int_to_enum = builder.callExpr(x->getType(), ext.getEnumFromInt(), builder.getTypeLiteral(x->getType()), builder.intLit(7));

		auto c = core::checks::check(equality);
		EXPECT_TRUE(c.getErrors().empty());

		c = core::checks::check(enum_to_int);
		EXPECT_TRUE(c.getErrors().empty());

		c = core::checks::check(int_to_enum);
		EXPECT_TRUE(c.getErrors().empty());
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
