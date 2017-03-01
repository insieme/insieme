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
#include "insieme/driver/utils/driver_utils.h"

#include <algorithm>
#include <string>
#include <iomanip>
#include <vector>

#include <boost/algorithm/string/replace.hpp>

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/opencl/opencl_backend.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_statistic.h"
#include "insieme/core/printer/error_printer.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/compiler/compiler.h"


namespace insieme {
namespace driver {
namespace utils {

	namespace iu = insieme::utils;

	namespace {
		// Minimum size of the context string reported by the error checker (context will be extended when smaller)
		#define MIN_CONTEXT 40

		#define TEXT_WIDTH 120
	}

	//***************************************************************************************
	// 				 Output formatting helpers
	//***************************************************************************************
	void openBoxTitle(const std::string title) {
		LOG(INFO) <<
			// Opening ascii row
			"\n//" << std::setfill('*') << std::setw(TEXT_WIDTH) << std::right << "//" <<
			// Section title left aligned
			"\n//" << std::setfill(' ') << std::setw(TEXT_WIDTH - 2) << std::left << " " + title + " " << std::right << "//" <<
			// Closing ascii row
			"\n//" << std::setfill('*') << std::setw(TEXT_WIDTH) << "//";
	}

	void closeBox() {
		LOG(INFO) << "\n//" << std::setfill('=') << std::setw(TEXT_WIDTH) << "";
	}

	//***************************************************************************************
	// 				 STATS: show statistics about the IR
	//***************************************************************************************
	void showStatistics(const core::ProgramPtr& program) {
		openBoxTitle("IR Statistics");
		iu::measureTimeFor<INFO>("ir.statistics ", [&]() { LOG(INFO) << "\n" << core::IRStatistic::evaluate(program); });
		closeBox();
	}

	//****************************************************************************************
	//                BENCHMARK CORE: Perform some performance benchmarks
	//****************************************************************************************
	void benchmarkCore(const core::NodePtr& program) {
		core::NodeManager& mgr = program->getNodeManager();

		openBoxTitle("Core Benchmarking");

		int count = 0;
		// Benchmark pointer-based visitor
		iu::measureTimeFor<INFO>("Benchmark.IterateAll.Pointer ",
			[&]() { core::visitDepthFirst(program, core::makeLambdaVisitor([&](const core::NodePtr& cur) { count++; }, true)); });
		LOG(INFO) << "Number of nodes: " << count;

		// Benchmark address based visitor
		iu::Timer visitAddrTime("");
		count = 0;
		iu::measureTimeFor<INFO>("Benchmark.IterateAll.Address ", [&]() {
			core::visitDepthFirst(core::ProgramAddress(program), core::makeLambdaVisitor([&](const core::NodeAddress& cur) { count++; }, true));
		});
		LOG(INFO) << "Number of nodes: " << count;

		// Benchmark empty-substitution operation
		count = 0;
		iu::measureTimeFor<INFO>("Benchmark.IterateAll.Address ", [&]() {
			core::SimpleNodeMapping* h;
			auto mapper = core::makeLambdaMapper([&](unsigned, const core::NodePtr& cur) -> core::NodePtr {
				count++;
				return cur->substitute(mgr, *h);
			});
			h = &mapper;
			mapper.map(0, program);
		});
		LOG(INFO) << "Number of modifications: " << count;

		// Benchmark empty-substitution operation (non-types only)
		count = 0;
		iu::measureTimeFor<INFO>("Benchmark.NodeSubstitution.Non-Types ", [&]() {
			core::SimpleNodeMapping* h2;
			auto mapper2 = core::makeLambdaMapper([&](unsigned, const core::NodePtr& cur) -> core::NodePtr {
				if(cur->getNodeCategory() == core::NC_Type) { return cur; }
				count++;
				return cur->substitute(mgr, *h2);
			});
			h2 = &mapper2;
			mapper2.map(0, program);
		});
		LOG(INFO) << "Number of modifications: " << count;
		closeBox();
	}

	//***************************************************************************************
	// 					SEMA: Performs semantic checks on the IR
	//***************************************************************************************
	int checkSema(const core::NodePtr& program, core::checks::MessageList& list, core::checks::CheckPtr checks) {
		int retval = 0;

		using namespace insieme::core::printer;

		openBoxTitle("IR Semantic Checks");

		iu::measureTimeFor<INFO>("Semantic Checks ", [&]() { list = core::checks::check(program, checks); });

		auto errors = list.getAll();

		std::sort(errors.begin(), errors.end());
		for_each(errors, [&](const core::checks::Message& cur) {
			LOG(ERROR) << cur;
			core::NodeAddress address = cur.getOrigin();
			std::stringstream ss;
			unsigned contextSize = 1;
			do {
				ss.str("");
				ss.clear();
				core::NodePtr&& context = address.getParentNode(std::min((unsigned)contextSize, address.getDepth() - contextSize));
				ss << PrettyPrinter(context, PrettyPrinter::OPTIONS_SINGLE_LINE, 1 + 2 * contextSize);

			} while(ss.str().length() < MIN_CONTEXT && contextSize++ < 5);
			//		LOG(ERROR) << "\t Source-Node-Type: " << address->getNodeType();
			LOG(ERROR) << "\t Source: " << PrettyPrinter(address, PrettyPrinter::OPTIONS_SINGLE_LINE);
			LOG(ERROR) << "\t Context: " << ss.str() << std::endl;

			// find enclosing function
			auto fun = address;
			while(!fun.isRoot() && fun->getNodeType() != core::NT_LambdaExpr) {
				fun = fun.getParentAddress();
			}
			if(fun->getNodeType() == core::NT_LambdaExpr) {
				LOG(ERROR) << "\t Context:\n" << PrettyPrinter(fun, PrettyPrinter::PRINT_DEREFS | PrettyPrinter::JUST_LOCAL_CONTEXT | PrettyPrinter::PRINT_CASTS)
					<< std::endl;
			}

			//		LOG(INFO) << "\t All: " << PrettyPrinter(address.getRootNode());
		});

		// In the case of semantic errors, quit
		if(!list.getErrors().empty()) {
			dumpErrors(list, std::cerr);

			std::cerr << "---- Semantic errors encountered!! ----\n";
			retval = 1;
		}

		closeBox();
		return retval;
	}


	//***************************************************************************************
	//									Backend selection
	//***************************************************************************************
	insieme::backend::BackendPtr getBackend(const std::string& backendString, const std::string& dumpOclKernel) {
		// prepare for setting up backend
		if(backendString == "runtime" || backendString == "run") {
			return backend::runtime::RuntimeBackend::getDefault();

		} else if(backendString == "sequential" || backendString == "seq") {
			return backend::sequential::SequentialBackend::getDefault();

		} else if(backendString == "opencl" || backendString == "ocl") {
			auto config = std::make_shared<backend::BackendConfig>();
			config->dumpOclKernel = dumpOclKernel;
			return backend::opencl::OpenCLBackend::getDefault(config);
		}

		LOG(ERROR) << "Error: Unsupported backend: " << backendString << " - supported: sequential | runtime | ocl\n";
		return {};
	}

	//***************************************************************************************
	//									Compiler selection
	//***************************************************************************************
	insieme::utils::compiler::Compiler getCompiler(const std::string& backendString, const bool isCpp) {
		insieme::utils::compiler::Compiler compiler = isCpp ?
				insieme::utils::compiler::Compiler::getDefaultCppCompiler() : insieme::utils::compiler::Compiler::getDefaultC99Compiler();

		if(backendString == "runtime" || backendString == "run") {
			compiler = insieme::utils::compiler::Compiler::getRuntimeCompiler(compiler);
		}
		return compiler;
	}

} // end namespace utils
} // end namespace driver
} // end namespace insieme
