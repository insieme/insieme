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

#include <boost/optional.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

#include "insieme/frontend/clang.h"
#include "insieme/core/forward_decls.h"

namespace clang {
	class Decl;
}

namespace insieme {
namespace frontend {
namespace utils {

namespace fs = boost::filesystem;
namespace ba = boost::algorithm;

/**
 * class which helps finding the more suitable header for a declaration, not allways top
 * level since we might have a system header included deep in a includes chain.
 * the most apropiate header has to be computed
 */
class HeaderTagger {
	vector<fs::path> stdLibDirs;
	vector<fs::path> interceptedHeaderDirs;
	vector<fs::path> userIncludeDirs;
	const clang::SourceManager& sm;

	mutable std::map<clang::FileID, std::pair<std::string, bool>> isStdCache;
	mutable std::map<clang::FileID, std::pair<std::string, bool>> isInterceptedCache;
	mutable std::map<clang::FileID, std::pair<std::string, bool>> isUserCache;

	/**
	 * A utility function cutting down std-lib header files.
	 */
	boost::optional<fs::path> toStdLibHeader(const fs::path& path) const;

	bool isStdLibHeader(const clang::SourceLocation& loc) const;

	bool isStdLibHeader(const fs::path& path) const;

	bool isInterceptedLibHeader(const clang::SourceLocation& loc) const;

	bool isInterceptedLibHeader(const fs::path& path) const;

	boost::optional<fs::path> toInterceptedLibHeader(const fs::path& path) const;

	bool isUserLibHeader(const clang::SourceLocation& loc) const;

	bool isUserLibHeader(const fs::path& path) const;

	boost::optional<fs::path> toUserLibHeader(const fs::path& path) const;

	bool isHeaderFile(const string& name) const;

	string getTopLevelInclude(const clang::SourceLocation& loc) const;

	bool isIntrinsicHeader(const string& name) const;

	bool isInjectedHeader(const clang::PresumedLoc& ploc) const;

	boost::optional<fs::path> toIntrinsicHeader(const fs::path& path) const;


  public:
	HeaderTagger(const vector<fs::path>& stdLibDirs, const vector<fs::path>& interceptedHeaderDirs, const vector<fs::path>& userIncludeDirs,
	             const clang::SourceManager& srcMgr);

	/**
	 * Check whether this declaration is in a system header,
	 * @param decl the declaration we are asking for
	 * @return is a system header
	 */
	bool isDefinedInSystemHeader(const clang::Decl* decl) const;

	/**
	 * Attaches a header annotation to the given node which is supposed to be
	 * the result of converting the given declaration.
	 *
	 * @param node the node to be annotated
	 * @param decl the declaration this node has been derived from
	 */
	void addHeaderForDecl(const core::NodePtr& node, const clang::Decl* decl, bool attachUserDefined = false) const;
};

} // end namespace utils
} // end namespace frontend
} // end namespace utils
