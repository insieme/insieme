/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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


namespace insieme {
namespace driver {
namespace utils {

	namespace iu = insieme::utils;
	namespace be = insieme::backend;

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
	int checkSema(const core::NodePtr& program, core::checks::MessageList& list) {
		int retval = 0;

		using namespace insieme::core::printer;

		openBoxTitle("IR Semantic Checks");

		iu::measureTimeFor<INFO>("Semantic Checks ", [&]() { list = core::checks::check(program); });

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
	insieme::backend::BackendPtr getBackend(const core::ProgramPtr& program, const cmd::Options& options) {
		if(options.backendHint == cmd::BackendEnum::Runtime) { return be::runtime::RuntimeBackend::getDefault(); }
		if(options.backendHint == cmd::BackendEnum::OpenCL) {
				auto config = std::make_shared<be::BackendConfig>();
				config->dumpOclKernel = options.settings.dumpOclKernel.string();
				return be::opencl::OpenCLBackend::getDefault(config);
		}
		return be::sequential::SequentialBackend::getDefault();
	}

} // end namespace utils
} // end namespace driver
} // end namespace insieme
