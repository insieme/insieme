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

#include "insieme/utils/name_mangling.h"

using namespace insieme::utils;

TEST(NameMangling, Basic) {
	EXPECT_EQ("IMP_bla", mangle("bla"));
	EXPECT_EQ("bla", demangle("bla"));
	EXPECT_EQ("bla", demangle("IMP_bla"));
	EXPECT_EQ("bla", demangle("IMP_bla_IMLOC_110_28"));
	EXPECT_EQ("IMP_kls_IMLOC__slash_bla_slash_xy_slash_z_dot_cpp_5_299", mangle("kls", "/bla/xy/z.cpp", 5, 299));
	EXPECT_EQ("kls", demangle("IMP_kls_IMLOC__slash_bla_slash_xy_slash_z_dot_cpp_5_299"));
}

TEST(NameMangling, Special) {
	EXPECT_EQ("IMP_bla_colon_klu_plus_r_wave__slash_", mangle("bla:klu+r~/"));
	EXPECT_EQ("bla:klu+r~/", demangle("IMP_bla_colon_klu_plus_r_wave__slash_"));
}

TEST(NameMangling, Empty) {
	EXPECT_EQ("IMP_EMPTY_IMLOC_foo_dot_cpp_42_7", mangle("", "foo.cpp", 42, 7));
	EXPECT_EQ("IMP_EMPTY_IMLOC_foo_dot_cpp_42_7", mangle("foo.cpp", 42, 7));
	EXPECT_EQ("", demangle("IMP_EMPTY_IMLOC_foo_dot_cpp_42_7"));
	EXPECT_EQ("IMP__not_really_mangle_empty__IMLOC_foo_dot_cpp_42_7", mangle("EMPTY", "foo.cpp", 42, 7));
	EXPECT_EQ("EMPTY", demangle("IMP__not_really_mangle_empty__IMLOC_foo_dot_cpp_42_7"));
}

TEST(NameMangling, OperatorManglingPrecedence) {
	EXPECT_EQ("IMP__operator_plus_",          mangle("operator+"));
	EXPECT_EQ("IMP__operator_minus_",         mangle("operator-"));
	EXPECT_EQ("IMP__operator_mult_",          mangle("operator*"));
	EXPECT_EQ("IMP__operator_div_",           mangle("operator/"));
	EXPECT_EQ("IMP__operator_mod_",           mangle("operator%"));
	EXPECT_EQ("IMP__operator_xor_",           mangle("operator^"));
	EXPECT_EQ("IMP__operator_and_",           mangle("operator&"));
	EXPECT_EQ("IMP__operator_or_",            mangle("operator|"));
	EXPECT_EQ("IMP__operator_complement_",    mangle("operator~"));
	EXPECT_EQ("IMP__operator_assign_",        mangle("operator="));
	EXPECT_EQ("IMP__operator_lt_",            mangle("operator<"));
	EXPECT_EQ("IMP__operator_gt_",            mangle("operator>"));
	EXPECT_EQ("IMP__operator_plus_assign_",   mangle("operator+="));
	EXPECT_EQ("IMP__operator_minus_assign_",  mangle("operator-="));
	EXPECT_EQ("IMP__operator_mult_assign_",   mangle("operator*="));
	EXPECT_EQ("IMP__operator_div_assign_",    mangle("operator/="));
	EXPECT_EQ("IMP__operator_mod_assign_",    mangle("operator%="));
	EXPECT_EQ("IMP__operator_xor_assign_",    mangle("operator^="));
	EXPECT_EQ("IMP__operator_and_assign_",    mangle("operator&="));
	EXPECT_EQ("IMP__operator_or_assign_",     mangle("operator|="));
	EXPECT_EQ("IMP__operator_lshift_",        mangle("operator<<"));
	EXPECT_EQ("IMP__operator_rshift_",        mangle("operator>>"));
	EXPECT_EQ("IMP__operator_rshift_assign_", mangle("operator>>="));
	EXPECT_EQ("IMP__operator_lshift_assign_", mangle("operator<<="));
	EXPECT_EQ("IMP__operator_eq_",            mangle("operator=="));
	EXPECT_EQ("IMP__operator_neq_",           mangle("operator!="));
	EXPECT_EQ("IMP__operator_le_",            mangle("operator<="));
	EXPECT_EQ("IMP__operator_ge_",            mangle("operator>="));
	EXPECT_EQ("IMP__operator_land_",          mangle("operator&&"));
	EXPECT_EQ("IMP__operator_lor_",           mangle("operator||"));
	EXPECT_EQ("IMP__operator_inc_",           mangle("operator++"));
	EXPECT_EQ("IMP__operator_dec_",           mangle("operator--"));
	EXPECT_EQ("IMP__operator_comma_",         mangle("operator,"));
	EXPECT_EQ("IMP__operator_memberpointer_", mangle("operator->*"));
	EXPECT_EQ("IMP__operator_member_",        mangle("operator->"));
	EXPECT_EQ("IMP__operator_call_",          mangle("operator()"));
	EXPECT_EQ("IMP__operator_subscript_",     mangle("operator[]"));
	EXPECT_EQ("IMP__conversion_operator_",    mangle("operator "));
	EXPECT_EQ("IMP__conversion_operator_int", mangle("operator int"));
}

TEST(NameMangling, Readable) {
	EXPECT_EQ("Bla", getReadableName("Bla"));

	EXPECT_EQ("Hat", getReadableName("IMP_Hat"));

	EXPECT_EQ("ns_S::ctor", getReadableName("IMP_ns_colon__colon_S::ctor"));

	auto t1 = "IMP___anon_tagtype__slash_home_slash_zangerl_slash_insieme_dev_slash_allscale_slash_test_slash_basic_slash_basic_dot_cpp_9_6_IMLOC__slash_home_slash_zangerl_slash_insieme_dev_slash_allscale_slash_test_slash_basic_slash_basic_dot_cpp_9_6";
	EXPECT_EQ("anon_basic_cpp_9_6", getReadableName(t1));

	auto t2 = "IMP__not_really_mangle_empty__IMLOC_foo_dot_cpp_42_7";
	EXPECT_EQ("anon_foo_cpp_42_7", getReadableName(t2));
}
