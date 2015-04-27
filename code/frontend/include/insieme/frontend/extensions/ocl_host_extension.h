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

#include <boost/filesystem/path.hpp>

#include "insieme/core/pattern/pattern.h"
#include "insieme/frontend/extensions/frontend_extension.h"

#include "insieme/utils/config.h"

namespace insieme {
namespace frontend {
namespace extensions {

// extension for OpenCl host files

class OclHostExtension : public FrontendExtension {
    bool flagActivated;
	std::vector<boost::filesystem::path> oclJobIncludeDirs;
public:
	OclHostExtension() : flagActivated(false) {
		//ocl host extension needs additional include path
		this->FrontendExtension::includeDirs.push_back(CLANG_SRC_DIR);
		this->FrontendExtension::includeDirs.push_back(CLANG_SRC_DIR "inputs");
		macros["INSIEME"] = "";
	};
    virtual FrontendExtension::flagHandler registerFlag(boost::program_options::options_description& options);

    virtual core::ProgramPtr IRVisit(core::ProgramPtr& prog);
};

// extension for icl host files

class IclHostExtension : public FrontendExtension {
    bool flagActivated;
	std::vector<boost::filesystem::path> iclJobIncludeDirs;
	core::pattern::TreePattern iclRunKernel;
public:
	IclHostExtension() : flagActivated(false) {
		//icl host extension needs additional include path
		this->FrontendExtension::includeDirs.push_back(CLANG_SRC_DIR "../../../test/ocl/common/");  // lib_icl
		this->FrontendExtension::includeDirs.push_back(CLANG_SRC_DIR);
		this->FrontendExtension::includeDirs.push_back(CLANG_SRC_DIR "inputs");
		macros["INSIEME"] = "";
	};
    virtual FrontendExtension::flagHandler registerFlag(boost::program_options::options_description& options);

    virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
                                                   insieme::frontend::conversion::Converter& convFact);

    virtual core::ProgramPtr IRVisit(core::ProgramPtr& prog);
};

} //namespace extension
} //namespace frontend
} //namespace insieme
