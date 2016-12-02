/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <boost/lexical_cast.hpp>

#include "insieme/core/encoder/lists.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace encoder {

	TEST(Lists, languageExtension) {
		NodeManager manager;
		const lang::ListExtension& ext = manager.getLangExtension<lang::ListExtension>();

		EXPECT_EQ("AP((('a,list<'a>)->list<'a>))", toString(ext.getListCons()->getType()));
		EXPECT_EQ("AP(((type<'a>)->list<'a>))", toString(ext.getListEmpty()->getType()));
	}


	TEST(Lists, listConversion) {
		NodeManager manager;

		// create a list

		vector<int> list = toVector(1, 2, 3);
		core::ExpressionPtr irList = toIR(manager, list);
		vector<int> back = toValue<vector<int>>(irList);

		EXPECT_EQ("[1,2,3]", toString(list));
		EXPECT_EQ("list_cons(1, list_cons(2, list_cons(3, list_empty(type<int<4>>))))", toString(*irList));

		EXPECT_TRUE(isEncodingOf<vector<int>>(irList));
		EXPECT_EQ(list, back);

		EXPECT_EQ("[]", toString(check(irList, checks::getFullCheck())));


		// test another type
		EXPECT_EQ("list_cons(3.75, list_cons(1.47, list_empty(type<real<8>>)))", toString(*toIR(manager, toVector<double>(3.75, 1.47))));
	}


	struct Info : public encodable {

		int x;

		Info(int x = 0) : x(x) {}

		bool operator==(const Info& i) const {
			return x == i.x;
		}

		static bool isEncoding(const ExpressionPtr& expr) {
			return encoder::isEncodingOf<int>(expr);
		}

		static TypePtr getEncodedType(NodeManager& mgr) {
			return encoder::getTypeFor<int>(mgr);
		}

		ExpressionPtr toIR(NodeManager& mgr) const {
			return encoder::toIR(mgr,x);
		}

		static Info fromIR(const ExpressionPtr& expr) {
			Info res;
			res.x = encoder::toValue<int>(expr);
			return res;
		}

	};


	TEST(Encodable, SimpleEncodableList) {

		NodeManager mgr;

		std::vector<Info> is;
		is.push_back(Info(12));
		is.push_back(Info(14));


		EXPECT_EQ("list<int<4>>",toString(*getTypeFor<std::vector<Info>>(mgr)));
		EXPECT_EQ("list_cons(12, list_cons(14, list_empty(type<int<4>>)))",toString(*toIR(mgr,is)));

		EXPECT_EQ(is,toValue<std::vector<Info>>(toIR(mgr,is)));

	}



} // end namespace lists
} // end namespace core
} // end namespace insieme
