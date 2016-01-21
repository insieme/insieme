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

#include "insieme/backend/opencl/opencl_code_fragments.h"

#include <algorithm>

#include "insieme/utils/logging.h"

#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/annotations/meta_info/meta_infos.h"

namespace insieme {
namespace backend {
namespace opencl {

	#define KERNEL_TABLE_NAME "g_insieme_kernel_table"

	KernelTable::KernelTable(const Converter& converter) :
		converter(converter),
	    declaration(c_ast::CCodeFragment::createNew(
			converter.getFragmentManager(),
			converter.getCNodeManager()->create<c_ast::OpaqueCode>("extern irt_ocl_kernel_code " KERNEL_TABLE_NAME "[];"))) {
		addDependency(declaration);
	}

	KernelTablePtr KernelTable::get(const Converter& converter) {
		static string ENTRY_NAME = "KernelTable";

		// look up the entry within the fragment manager
		auto manager = converter.getFragmentManager();
		auto res = manager->getFragment(ENTRY_NAME);
		if(!res) {
			// create new instance
			KernelTablePtr table = manager->create<KernelTable>(boost::ref(converter));
			manager->bindFragment(ENTRY_NAME, table);
			res = table;
		}
		return static_pointer_cast<KernelTable>(res);
	}

	c_ast::CodeFragmentPtr KernelTable::getDeclaration() {
		return declaration;
	}

	const c_ast::ExpressionPtr KernelTable::getTable() {
		return c_ast::ref(converter.getCNodeManager()->create<c_ast::Literal>(KERNEL_TABLE_NAME));
	}

	unsigned KernelTable::size() const {
		return impls.size();
	}

	void KernelTable::registerKernel(const core::ExpressionPtr& id, const core::ExpressionPtr& impl) {
		#if 0
		// check whether implementation has already been resolved
		auto pos = impls.find(id);
		if(pos != impls.end()) return;
		// now it is safe to insert it into our registration map
		impls.insert(std::make_pair(id, impl));
		#endif
	}

	std::ostream& KernelTable::printTo(std::ostream& out) const {
		out << "// --- the kernel table --- \n"
		       "irt_ocl_kernel_code " KERNEL_TABLE_NAME "[] = {\n";
		for_each(impls, [&](const std::pair<core::LiteralPtr, core::LiteralPtr>& arg) {
			out << "    { " << arg.first->getStringValue() << ", ";
			
			std::string source = arg.second->getStringValue();
			std::string target;
			// inform the stl allocator that we need at least as much space as the inital one
			target.reserve(source.size());
			for (std::string::iterator it = source.begin(); it != source.end(); ++it) {
				if (*it == '\n') target.append("\"\n\"");
				else			 target.push_back(*it);
			}
			out << target << " },\n";
		});
		// the empty element to know the bounds of the table
		out << "{0, 0}\n";
		return out << "};\n\n";
	}

} // end namespace opencl
} // end namespace backend
} // end namespace insieme
