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

#include <gtest/gtest.h>


#include "insieme/analysis/cba/framework/entities/data_path.h"

namespace insieme {
namespace analysis {
namespace cba {

	namespace {

		DataPath seq(int x) {
			if (x == 0) return DataPath();
			return seq(x-1) << SingleIndex(x);
		}

	}


	TEST(CBA, DataPath) {

		typedef NominalIndex<string> NominalIndex;

		// just some simple data path handling
		DataPath dp;
		EXPECT_EQ("#", toString(dp));

		dp <<= NominalIndex("a");
		EXPECT_EQ("#.a", toString(dp));

		dp <<= SingleIndex(4);
		EXPECT_EQ("#.a.4", toString(dp));

		// check pop function
		EXPECT_EQ("#.a", toString(dp.pop()));
		EXPECT_EQ("#", toString(dp.pop(2)));


		// check equals and hash functions
		DataPath root;
		DataPath a = root << NominalIndex("T");
		DataPath b = root << NominalIndex("T");
		DataPath c = root << NominalIndex("S");

		EXPECT_EQ(root, root);
		EXPECT_NE(root, a);
		EXPECT_NE(root, b);

		EXPECT_NE(&a,&b);
		EXPECT_EQ(a,b);
		EXPECT_EQ(a.hash(), b.hash());

		EXPECT_NE(a,c);
		EXPECT_NE(a.hash(), c.hash());

		// stress reference counting when treating DataPaths as values
		DataPath x;
		{
			DataPath y = seq(5);
			EXPECT_EQ("#.1.2.3.4.5", toString(y));

			x = y.pop(3);
		}

		EXPECT_EQ("#.1.2", toString(x));
	}

	TEST(CBA, DataPathComparison) {

		// simply check whether data paths can be sorted lexicographically
		typedef NominalIndex<string> NominalIndex;

		DataPath root;

		DataPath a = root << SingleIndex(1) << SingleIndex(3);
		DataPath b = root << SingleIndex(2) << SingleIndex(3);
		DataPath c = root << SingleIndex(1) << SingleIndex(4);
		DataPath d = a << SingleIndex(1);

		EXPECT_LT(root, a);
		EXPECT_LT(root, b);
		EXPECT_LT(root, c);
		EXPECT_LT(root, d);

		EXPECT_LT(a, b);
		EXPECT_LT(a, c);
		EXPECT_LT(a, d);

		EXPECT_LT(c, b);

		EXPECT_LT(d, c);
		EXPECT_LT(d, b);

	}

	TEST(CBA, DataPathConcatenation) {

		// simply check whether data paths can be sorted lexicographically
		typedef NominalIndex<string> NominalIndex;

		DataPath a;
		DataPath b = DataPath() << NominalIndex("a");
		DataPath c = DataPath() << NominalIndex("b");
		DataPath d = DataPath() << NominalIndex("c") << NominalIndex("d");

		EXPECT_EQ("#", 			toString(a));
		EXPECT_EQ("#.a", 		toString(a << b));
		EXPECT_EQ("#.a.b", 		toString(a << b << c));
		EXPECT_EQ("#.a.b.c.d", 	toString(a << b << c << d));
		EXPECT_EQ("#.a.b", 		toString(b << c));
		EXPECT_EQ("#.a.c.d", 	toString(b << d));
		EXPECT_EQ("#.c.d.c.d", 	toString(d << d));

		EXPECT_EQ(a<<b<<c<<d, a<<b<<c<<d);
	}

	TEST(CBA, DataPathVisit) {

		typedef NominalIndex<string> NominalIndex;

		DataPath a = DataPath() << NominalIndex("a") << NominalIndex("b") << NominalIndex("c");

		std::stringstream out;
		a.visit([&](const detail::DataPathElement& cur) {
			out << cur << " ";
		});
		EXPECT_EQ("#.a #.a.b #.a.b.c ", out.str());

	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
