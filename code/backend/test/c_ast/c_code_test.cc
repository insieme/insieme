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

#include "insieme/backend/c_ast/c_code.h"


namespace insieme {
namespace backend {
namespace c_ast {

	namespace {

		// a dummy fragment just representing text
		class TextFragment : public c_ast::CodeFragment {
			string text;

		  public:
			TextFragment(const string& text) : text(text){};
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << text;
			}
		};

		CodeFragmentPtr getTextFragment(const SharedCodeFragmentManager& manager, const string& text) {
			return manager->create<TextFragment>(text);
		}
	}


	TEST(C_AST, FragmentDependencyResolution) {
		SharedCodeFragmentManager fragmentManager = CodeFragmentManager::createShared();

		// create a simple code fragment
		CodeFragmentPtr code = getTextFragment(fragmentManager, "A");

		EXPECT_PRED2(containsSubString, toString(CCode(fragmentManager, core::NodePtr(), code)), "A");

		// add something with dependencies

		CodeFragmentPtr codeA = getTextFragment(fragmentManager, "A");
		CodeFragmentPtr codeB = getTextFragment(fragmentManager, "B");
		CodeFragmentPtr codeC = getTextFragment(fragmentManager, "C");
		CodeFragmentPtr codeD = getTextFragment(fragmentManager, "D");

		codeB->addDependency(codeA);
		codeC->addDependency(codeB);
		codeD->addDependency(codeC);

		EXPECT_PRED2(containsSubString, toString(CCode(fragmentManager, core::NodePtr(), codeD)), "ABCD");

		// add additional edge (should not change anything)
		codeD->addDependency(codeA);
		EXPECT_PRED2(containsSubString, toString(CCode(fragmentManager, core::NodePtr(), codeD)), "ABCD");
	}


	TEST(C_AST, DependencyCycleDedectionTest) {
		SharedCodeFragmentManager fragmentManager = CodeFragmentManager::createShared();

		// create a simple code fragment
		CodeFragmentPtr code = getTextFragment(fragmentManager, "A");

		EXPECT_PRED2(containsSubString, toString(CCode(fragmentManager, core::NodePtr(), code)), "A");

		// add something with dependencies

		CodeFragmentPtr codeA = getTextFragment(fragmentManager, "A");
		CodeFragmentPtr codeB = getTextFragment(fragmentManager, "B");
		CodeFragmentPtr codeC = getTextFragment(fragmentManager, "C");
		CodeFragmentPtr codeD = getTextFragment(fragmentManager, "D");
		CodeFragmentPtr codeX = getTextFragment(fragmentManager, "X");

		codeB->addDependency(codeA);
		codeC->addDependency(codeB);
		codeD->addDependency(codeC);

		EXPECT_FALSE(codeA->isDependingOn(codeB));
		EXPECT_FALSE(codeA->isDependingOn(codeC));

		EXPECT_TRUE(codeB->isDependingOn(codeA));

		EXPECT_TRUE(codeC->isDependingOn(codeA));
		EXPECT_TRUE(codeC->isDependingOn(codeB));

		EXPECT_TRUE(codeD->isDependingOn(codeA));
		EXPECT_TRUE(codeD->isDependingOn(codeB));
		EXPECT_TRUE(codeD->isDependingOn(codeC));

		EXPECT_TRUE(codeA->isDependingOn(codeA));
		EXPECT_TRUE(codeB->isDependingOn(codeB));
		EXPECT_TRUE(codeC->isDependingOn(codeC));
		EXPECT_TRUE(codeD->isDependingOn(codeD));

		// something not linear
		EXPECT_FALSE(codeD->isDependingOn(codeX));
		codeC->addDependency(codeX);
		EXPECT_TRUE(codeD->isDependingOn(codeX));
	}

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme
