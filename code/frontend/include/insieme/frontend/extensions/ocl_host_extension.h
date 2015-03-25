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

namespace insieme {
namespace frontend {
namespace extensions {

// extension for OpenCl host files

class OclHostExtension : public FrontendExtension {
	const std::vector<boost::filesystem::path>& includeDirs;
public:
	OclHostExtension(const std::vector<boost::filesystem::path>& includeDirs);
private:
    virtual core::ProgramPtr IRVisit(core::ProgramPtr& prog);
};

// extension for icl host files

class IclHostExtension : public FrontendExtension {
	const std::vector<boost::filesystem::path>& includeDirs;
	core::pattern::TreePattern iclRunKernel;

public:
	IclHostExtension(const std::vector<boost::filesystem::path>& includeDirs);
private:
    virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
                                                   insieme::frontend::conversion::Converter& convFact);


    virtual core::ProgramPtr IRVisit(core::ProgramPtr& prog);
};

} //namespace extension
} //namespace frontend
} //namespace insieme
