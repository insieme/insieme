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

#include <algorithm>

#include "insieme/annotations/meta_info/meta_infos.h"

#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"
#include "insieme/backend/opencl/opencl_entities.h"
#include "insieme/backend/opencl/opencl_code_fragments.h"
#include "insieme/backend/runtime/runtime_code_fragments.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace opencl {

	#define KERNEL_TABLE_NAME "g_insieme_opencl_kernel_table"
	utils::SimpleIDGenerator<unsigned> KernelTable::idGenerator;

	KernelTable::KernelTable(const Converter& converter) :
		converter(converter),
	    declaration(c_ast::CCodeFragment::createNew(
			converter.getFragmentManager(),
			converter.getCNodeManager()->create<c_ast::OpaqueCode>("extern irt_opencl_kernel_implementation *" KERNEL_TABLE_NAME "[];"))) {
		addDependency(declaration);
		runtime::ContextHandlingFragmentPtr ctx = runtime::ContextHandlingFragment::get(converter);
		// add the dataReq. table as well
		ctx->addInitExpression("\tirt_opencl_init_context(%s, " KERNEL_TABLE_NAME ");\n");
		ctx->addCleanupExpression("\tirt_opencl_cleanup_context(%s);\n");
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

	unsigned KernelTable::getNextUnique() {
		// substract one as we want to start with 0
		return idGenerator.getNext() - 1;
	}

	c_ast::CodeFragmentPtr KernelTable::getDeclaration() const {
		return declaration;
	}

	const c_ast::ExpressionPtr KernelTable::getTable() const {
		return c_ast::ref(converter.getCNodeManager()->create<c_ast::Literal>(KERNEL_TABLE_NAME));
	}

	unsigned KernelTable::size() const {
		return impls.size();
	}

	unsigned KernelTable::registerKernel(const core::ExpressionPtr& id, const core::ExpressionPtr& source, const core::ExpressionPtr& routine) {
		// grab the kernelId for this source
		unsigned int kernelId = core::encoder::toValue<unsigned int>(id);
		// check if it is already present, if it is .. oops the codegeneration has a bug!
		assert_true(impls.find(kernelId) == impls.end()) << "kernel with id " << kernelId << " has already been registered!";

		auto entry = std::make_shared<Entry>();
		entry->id = kernelId;
		entry->source = analysis::getArgument(source, 0).as<core::LiteralPtr>()->getStringValue();
		entry->routine = analysis::getArgument(routine, 0).as<core::LiteralPtr>()->getStringValue();
		// now it is safe to insert it into our registration map
		impls.insert(std::make_pair(kernelId, entry));
		return kernelId;
	}

	std::ostream& KernelTable::printTo(std::ostream& out) const {
		out << "// --- opencl kernel implementations ---\n";
		// first of all, we generate a struct for each single kernel
		for (const auto& arg : impls) {
			auto entry = arg.second;
			// first member must be '0' as it points to irt_private
			out << "irt_opencl_kernel_implementation g_insieme_opencl_kernel_" << entry->id << "_implementation = {0,\n";
			// second arg is the printed kernel in an appropriate format
			for (auto it = entry->source.begin(); it != entry->source.end(); ++it) {
				if (*it == '\n') out << "\\n\"\n\"";
				else			 out << *it;
			}
			out << ",\n" << entry->routine << "};\n\n";
		};

		out << "// --- opencl kernel table ---\n";
		// now generate the actual kernel table which stores a pointer to the impls
		out << "irt_opencl_kernel_implementation *" KERNEL_TABLE_NAME "[] = {\n";
		for (const auto& arg : impls) {
			out << "&g_insieme_opencl_kernel_" << arg.first << "_implementation,\n";
		}
		// end of table marker
		return out << "0};\n\n";
	}
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
