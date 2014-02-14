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

#include "insieme/frontend/extensions/ocl_host_extension.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/ocl/ocl_host_replace_buffers.h"
#include "insieme/frontend/ocl/ocl_type_fixer.h"
#include "insieme/frontend/ocl/ocl_host_utils.h"
#include "insieme/frontend/ocl/ocl_host_handler.h"

namespace fe = insieme::frontend;

using namespace insieme::frontend;

namespace insieme {
namespace frontend {
namespace extensions {

using namespace insieme::core;
using namespace insieme::frontend::ocl;

OclHostPlugin::OclHostPlugin() {

}

core::ProgramPtr OclHostPlugin::IRVisit(insieme::core::ProgramPtr& prog) {
	ocl::BufferReplacer br(prog);
	core::NodePtr root = br.getTransformedProgram();

	ocl::OclSimpleFunHandler osfh;
	root = osfh.mapElement(0, root);

	ocl::TypeFixer otf;
	root = otf.mapElement(0, root);

	core::IRBuilder builder(prog->getNodeManager());
	core::ExpressionList list;
	list.push_back(root.as<core::ExpressionPtr>());

//std::cout << printer::PrettyPrinter(root) << std::endl;

//	prog = builder.program(list);

	return prog;
}

} //namespace plugin
} //namespace frontend
} //namespace extensions
