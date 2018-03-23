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
	EXPECT_EQ("foo_cpp_42_7", getReadableName(t2));

	auto t3 = "IMP_allscale_colon__colon_api_colon__colon_core_colon__colon_detail_colon__colon_callable_lt_0_comma__space_allscale_colon__colon_api_colon__colon_core_colon__colon_fun_def_lt_int_comma__space_int_comma__space__lparen_lambda_space_at_space__dot__dot__slash_allscale_slash_test_slash_basic_slash_basic_dot_cpp_colon_8_colon_6_rparen__comma__space_std_colon__colon_tuple_lt__lparen_lambda_space_at_space__dot__dot__slash_allscale_slash_test_slash_basic_slash_basic_dot_cpp_colon_9_colon_6_rparen__gt__comma__space_std_colon__colon_tuple_lt__lparen_lambda_space_at_space__dot__dot__slash_allscale_slash_test_slash_basic_slash_basic_dot_cpp_colon_10_colon_6_rparen__gt__space__gt__space__gt__colon__colon_SequentialCallable_IMLOC__slash_home_slash_zangerl_slash_insieme_dev_slash_allscale_slash_api_slash_code_slash_include_slash_allscale_slash_api_slash_core_slash_prec_dot_h_287_4";
	EXPECT_EQ("allscale_api_core_detail_callable_0_allscale_api_core_fun_def_int_int__lambda_at__allscale_test_basic_basic_cpp_8_6__std_tuple_lambda_at__allscale_test_basic_basic_cpp_9_6__std_tuple_lambda_at__allscale_test_basic_basic_cpp_10_6____SequentialCallable__home_zangerl_insieme_dev_allscale_api_code_include_allscale_api_core_prec_h_287_4", getReadableName(t3));
}
