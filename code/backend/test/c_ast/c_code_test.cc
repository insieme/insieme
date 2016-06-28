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
