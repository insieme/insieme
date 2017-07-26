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
