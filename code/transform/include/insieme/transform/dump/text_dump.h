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

#pragma once

#include "insieme/transform/transformation.h"
#include "insieme/transform/catalog.h"

namespace insieme {
namespace transform {
namespace dump {

	/**
	 * Writes a text-based encoding of the given transformation to the given output stream.
	 *
	 * @param out the stream to be writing to
	 * @param transform the transformation to be written
	 */
	void dumpTransformation(std::ostream& out, const TransformationPtr& transform);

	void dumpTransformations(std::ostream& out, const vector<TransformationPtr>& transformations);

	/**
	 * Restores a transformation from the given input stream. The given catalog will
	 * be used to resolve the names used within the text based encoding. In case the
	 * stream contains an invalid encoding, an InvalidEncodingException will be thrown.
	 *
	 * @param in the stream to be reading from
	 * @param catalog the catalog used for creating the resulting transformations
	 * @return the resolved transformation
	 */
	TransformationPtr loadTransformation(std::istream& in, const Catalog& catalog);

	vector<TransformationPtr> loadTransformations(std::istream& in, const Catalog& catalog);

} // end namespace dump
} // end namespace transform
} // end namespace insieme
