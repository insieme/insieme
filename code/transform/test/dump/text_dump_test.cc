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

#include "insieme/transform/dump/text_dump.h"

#include "insieme/transform/primitives.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/rulebased/transformations.h"
#include "insieme/transform/polyhedral/transformations.h"

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

TEST(TextDump, ComplexStoreLoad) {

	// create a (simple) transformation
	TransformationPtr transform = makePipeline(
			polyhedral::makeLoopTiling({15,20}),
			makeForAll(
					filter::outermostSCoPs(),
					makeTry( polyhedral::makeLoopInterchange(1,3))
			)
	);

	// save transformation within stream
	stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

	// dump IR using a text format
	dumpTransformation(buffer, transform);

	// restore the transformation
	const Catalog& catalog = getStandardCatalog();

	TransformationPtr restored = loadTransformation(buffer, catalog);

	EXPECT_EQ(*transform, *restored);

}


TEST(TextDump, ListStoreLoad) {

	vector<TransformationPtr> transformations = toVector(
		makePipeline(
				polyhedral::makeLoopTiling({15,20}),
				makeForAll(
						filter::outermostSCoPs(),
						makeTry( polyhedral::makeLoopInterchange(1,3))
				)
		),
		makeNoOp(),
		makePipeline(
				makeForAll(
						filter::innermostLoops(2),
						makeTry( polyhedral::makeLoopInterchange(1,3))
				),
				polyhedral::makeLoopTiling({15,20})
		)
	);

	// save transformation within stream
	stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

	// dump IR using a text format
	dumpTransformations(buffer, transformations);

//	std::cout << "Encoded transformations: \n" << buffer.str() << "\n";

	// restore the transformation
	const Catalog& catalog = getStandardCatalog();

	vector<TransformationPtr> restored = loadTransformations(buffer, catalog);

	EXPECT_EQ(transformations.size(), restored.size());
	for(unsigned i = 0; i<transformations.size(); i++) {
		EXPECT_EQ(*transformations[i], *restored[i]);
	}
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

