/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
		GenericTypePtr eeBlue = lang::getEnumElement(enumBlue, builder.intLit(3));
		GenericTypePtr eeRed = lang::getEnumElement(enumRed, builder.intLit(5));
		GenericTypePtr eeGreen = lang::getEnumElement(enumGreen, builder.add(builder.intLit(4), builder.intLit(3)));
		EXPECT_EQ("enum_entry<Blue,3>", toString(*eeBlue));
		EXPECT_EQ("enum_entry<Red,5>", toString(*eeRed));
		EXPECT_EQ("enum_entry<Green,7>", toString(*eeGreen));
		/****ENUM DEFINITION*****/
		GenericTypePtr finalEnum = core::lang::getEnumDef(enumName, { eeBlue, eeGreen, eeRed });
		EXPECT_EQ("enum_def<Color,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>", toString(*finalEnum));
		/****ENUM TAG TYPE*****/
		auto enumTy = core::lang::getEnumType(builder.getLangBasic().getInt4(), finalEnum);
		EXPECT_TRUE(core::lang::isEnumType(enumTy));
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
		GenericTypePtr eeBlue = lang::getEnumElement(enumBlue, builder.intLit(3));
		GenericTypePtr eeRed = lang::getEnumElement(enumRed, builder.intLit(5));
		GenericTypePtr eeGreen = lang::getEnumElement(enumGreen, builder.add(builder.intLit(4), builder.intLit(3)));
		/****ENUM DEFINITION + TYPE*****/
		GenericTypePtr finalEnum = core::lang::getEnumDef(enumName, { eeBlue, eeGreen, eeRed });
		auto enumTy = core::lang::getEnumType(builder.getLangBasic().getInt4(), finalEnum);
		/****ENUM DEFINITION*****/
		auto x = lang::getEnumInit(builder.intLit(5), enumTy);
		auto y = lang::getEnumInit(builder.intLit(3), enumTy);
		EXPECT_EQ("tuple(type<enum_def<Color,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>>,5)", toString(*x));
		EXPECT_EQ("tuple(type<enum_def<Color,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>>,3)", toString(*y));
		EXPECT_EQ("(type<enum_def<Color,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>>,int<4>)", toString(*(x->getType())));
		EXPECT_EQ("(type<enum_def<Color,enum_entry<Blue,3>,enum_entry<Green,7>,enum_entry<Red,5>>>,int<4>)", toString(*(y->getType())));
		auto c1 = core::checks::check(x);
		EXPECT_TRUE(c1.getErrors().empty());
		auto c2 = core::checks::check(y);
		EXPECT_TRUE(c2.getErrors().empty());
	}

	TEST(Enum, EnumDerivedOps) {
		NodeManager nm;
		IRBuilder builder(nm);
		auto& ext = nm.getLangExtension<EnumExtension>();
		GenericTypePtr enumName = GenericType::get(nm, "Color");
		GenericTypePtr enumBlue = GenericType::get(nm, "Blue");
		GenericTypePtr enumRed = GenericType::get(nm, "Red");
		GenericTypePtr enumGreen = GenericType::get(nm, "Green");		
		GenericTypePtr eeBlue = lang::getEnumElement(enumBlue, builder.intLit(3));
		GenericTypePtr eeRed = lang::getEnumElement(enumRed, builder.intLit(5));
		GenericTypePtr eeGreen = lang::getEnumElement(enumGreen, builder.add(builder.intLit(4), builder.intLit(3)));

		GenericTypePtr finalEnum = core::lang::getEnumDef(enumName, { eeBlue, eeGreen, eeRed });
		auto enumTy = core::lang::getEnumType(builder.getLangBasic().getInt4(), finalEnum);

		auto x = lang::getEnumInit(builder.intLit(5), enumTy);
		auto y = lang::getEnumInit(builder.intLit(3), enumTy);

		auto equality = builder.callExpr(builder.getLangBasic().getBool(), ext.getEnumEquals(), x, y);
		auto enum_to_int = builder.callExpr(builder.getLangBasic().getInt4(), ext.getEnumToInt(), x);
		auto int_to_enum = builder.callExpr(x->getType(), ext.getIntToEnum(), builder.getTypeLiteral(x->getType()), builder.intLit(7));

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
