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

#pragma once

#include <string>


#include "insieme/frontend/clang_forward.h"
#include "insieme/core/annotations/source_location.h"

namespace insieme {
namespace frontend {
namespace utils {

	std::string FileName(clang::SourceLocation const& l, clang::SourceManager const& sm);

	std::string FileId(clang::SourceLocation const& l, clang::SourceManager const& sm);

	unsigned Line(clang::SourceLocation const& l, clang::SourceManager const& sm);

	std::pair<unsigned, unsigned> Line(clang::SourceRange const& r, clang::SourceManager const& sm);

	unsigned Column(clang::SourceLocation const& l, clang::SourceManager const& sm);

	std::pair<unsigned, unsigned> Column(clang::SourceRange const& r, clang::SourceManager const& sm);

	std::string location(clang::SourceLocation const& l, clang::SourceManager const& sm);

	clang::SourceLocation getExpansionLoc(const clang::SourceManager& sm, clang::SourceLocation loc);

	// Convert clang source locations into a core::annotations::Location object to be attached to a node
	core::annotations::Location convertClangSrcLoc(core::NodeManager& man, const clang::SourceManager& sm, clang::SourceLocation start,
	                                               clang::SourceLocation end);

	// Attach source location based on clang coordinates to IR node
	const core::NodePtr& attachLocationFromClang(const core::NodePtr& node, const clang::SourceManager& sm, clang::SourceLocation start,
	                                             clang::SourceLocation end);

} // End utils namespace
} // End frontend namespace
} // End insieme namespace
