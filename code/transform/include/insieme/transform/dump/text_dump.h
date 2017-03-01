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
