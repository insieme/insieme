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

#include "insieme/core/analysis/region/mpi_selector.h"

#include <set>

#include "insieme/core/annotations/source_location.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/pattern/ir_generator.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace analysis {
namespace region {

	namespace pt = insieme::core::pattern;

	namespace {
		
		std::string getNameForFunCall(core::NodePtr funPtr) {
			std::string retVal("");
			if(auto exprPtr = funPtr.isa<core::CallExprPtr>()) { funPtr = exprPtr->getFunctionExpr(); }
			if(auto litPtr = funPtr.isa<core::LiteralPtr>()) { retVal = litPtr->getStringValue(); }
			return utils::demangle(retVal);
		}

	}

	pt::TreePattern MPISelector::getMPITransport(pt::TreePattern mpiRequest) const {
		return pt::var("transport", pt::irp::callExpr(pt::lambda([](const core::NodeAddress& nodeAddr) {
					                                      return getNameForFunCall(nodeAddr).find("MPI_I") == 0
					                                             && getNameForFunCall(nodeAddr).find("MPI_Init") == std::string::npos;
					                                  }),
				                                      pt::ListPattern(*pt::any << pt::irp::ptrFromRef(mpiRequest))));
	}

	pt::TreePattern MPISelector::getMPITest(pt::TreePattern mpiRequest) const {
		return pt::irp::callExpr(pt::lambda([](const core::NodeAddress& nodeAddr) { return getNameForFunCall(nodeAddr).find("MPI_Test") == 0; }),
			pt::ListPattern(pt::irp::ptrFromRef(mpiRequest) << pt::any << pt::any));
	}

	pt::TreePattern MPISelector::getMPIWait(pt::TreePattern mpiRequest) const {
		return pt::irp::callExpr(pt::lambda([](const core::NodeAddress& nodeAddr) { return getNameForFunCall(nodeAddr).find("MPI_Wait") == 0; }),
			pt::ListPattern(pt::irp::ptrFromRef(mpiRequest) << pt::any));
	}

	pt::TreePattern MPISelector::getMPIWhileTest(pt::TreePattern mpiRequest) const {
		return pt::irp::whileStmt(pt::any, pt::irp::compoundStmt(*pt::any << getMPITest(mpiRequest) << *pt::any));
	}

	pt::ListPattern MPISelector::getMPIAsyncPattern(bool matchRequestObjects) const {
		// ensures that all MPI calls refer to the same MPI_Request object
		pt::TreePattern mpiRequest;
		if(matchRequestObjects) {
			mpiRequest = pt::irp::variable(pt::var("request"), pt::var("x"));
		} else {
			mpiRequest = pt::any;
		}

		auto mpiTransport = getMPITransport(mpiRequest);
		auto mpiTest = getMPITest(mpiRequest);

		// matches MPI_Wait with a given request object
		auto mpiWait = getMPIWait(mpiRequest);

		// look for a while loop containing an MPI_test
		pt::TreePattern whileTestPattern = getMPIWhileTest(mpiRequest);

		// matches a full asynchronous communication construct including the blocking wait mechanism
		pt::ListPattern fullMPI = mpiTransport << *pt::any << pt::var("block", (mpiWait | whileTestPattern));

		return fullMPI;
	}

	namespace {

		pt::TreePattern matchAddr(const core::NodeAddress& addr) {
			return pt::lambda([&](const core::NodeAddress& cur) { return cur == addr; });
		}

	}

	RegionList MPISelector::getRegions(const core::NodeAddress& code) const {
		RegionList res;

		const pt::TreePattern searchPattern = pt::irp::compoundStmt(*pt::any << getMPIAsyncPattern(matchRequestObjs) << *pt::any);

		std::set<Region> regionSet;

		auto getValue = [](const pt::AddressMatch& match, const std::string& label) { return match.getVarBinding(label).getValue(); };

		pt::irp::matchAllPairs(searchPattern, code, [&](const core::NodeAddress& addr, const pt::AddressMatch match) {
			core::StatementAddress startAddress = addr.concat(getValue(match, "transport")).as<core::StatementAddress>();
			core::StatementAddress endAddress = addr.concat(getValue(match, "block")).as<core::StatementAddress>();
			LOG(DEBUG) << "matchAll: " << startAddress << " -> " << endAddress;

			regionSet.insert(Region(startAddress, endAddress));

			// try to iteratively match additional patterns in compound statements (cannot be individually
			// matched by non-recursive patterns, and recursive ones are slower for large codes)
			bool continueMatching = true;
			// break after n consecutive regions in a compound statement, in case we are looping due to shared nodes
			unsigned breakIndex = 100;
			while(continueMatching && breakIndex-- > 0) {
				LOG(DEBUG) << "trying to match, continueMatching=" << continueMatching << ", breakIndex=" << breakIndex;
				continueMatching = false;
				// match the same pattern prefixed with the end address of the last match
				const auto continuedSearchPattern = pt::irp::compoundStmt(*pt::any << matchAddr(endAddress) << *pt::any << getMPIAsyncPattern(matchRequestObjs) << *pt::any);
				const auto matchOpt = continuedSearchPattern.matchAddress(addr);
				if(matchOpt) {
					startAddress = getValue(matchOpt.get(), "transport").as<core::StatementAddress>();
					endAddress = getValue(matchOpt.get(), "block").as<core::StatementAddress>();
					LOG(DEBUG) << "additional match: " << startAddress << " -> " << endAddress;

					regionSet.insert(std::make_pair(startAddress, endAddress));

					continueMatching = true;
				}
			}

			if(breakIndex <= 0) { LOG(INFO) << "Broke pattern matching loop after too many iterations, there are likely uncaptured MPI constructs"; }

		});

		LOG(DEBUG) << "regionSet:\n" << regionSet << "\n" << "regionSet size: " << regionSet.size();

		for(const auto& e : regionSet) {
			res.push_back(e);
		}

		return res;
	}

} // end namespace region
} // end namespace analysis
} // end namespace core
} // end namespace insieme
