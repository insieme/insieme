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

#include "insieme/transform/dump/text_dump.h"

#include "insieme/transform/primitives.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/rulebased/transformations.h"

#include "insieme/transform/filter/standard_filter.h"

#include <sstream>

using std::shared_ptr;

namespace insieme {
namespace transform {
namespace dump {

	using namespace std;


	TEST(TextDump, SimpleStoreLoad) {
		// create a (simple) transformation
		TransformationPtr transform = makePipeline(makeNoOp());

		// save transformation within stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// add some comment
		buffer << "# some comment \n";

		// dump IR using a text format
		dumpTransformation(buffer, transform);

		// restore the transformation
		const Catalog& catalog = getStandardCatalog();

		TransformationPtr restored = loadTransformation(buffer, catalog);

		EXPECT_EQ(*transform, *restored);
	}

	TEST(TextDump, EmptyListStoreLoad) {
		vector<TransformationPtr> transformations;

		// save transformation within stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a text format
		dumpTransformations(buffer, transformations);

		// restore the transformation
		const Catalog& catalog = getStandardCatalog();

		vector<TransformationPtr> restored = loadTransformations(buffer, catalog);

		EXPECT_TRUE(restored.empty());
	}


} // end namespace dump
} // end namespace transform
} // end namespace insieme
