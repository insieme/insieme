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

#include "insieme/analysis/cba/framework/cba.h"

#include <boost/algorithm/string/replace.hpp>

#include "insieme/analysis/cba/utils/cba_utils.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/utils/expect.h"

namespace insieme {
namespace analysis {
namespace cba {


	using std::set;

	using namespace core;

	CBA::CBA(const StatementAddress& root)
		: root(root),
		  solver([&](const set<ValueID>& sets) {
				Constraints res;
				for (auto set : sets) {
					this->addConstraintsFor(set, res);
//					std::cout << "Resolving set " << set << "...\n";
//					Constraints tmp;
//					this->addConstraintsFor(set, tmp);
//					std::cout << "Constraints: " << tmp << "\n";
//					if(!all(tmp, [&](const ConstraintPtr& cur) { return set == cur->getOutputs()[0]; })) { std::cout << "WARNING\n"; }
//					res.add(tmp);
				}
				return res;
		  }),
		  setCounter(0), idCounter(0), callSiteMgr(root),
		  callStringFilter(*this)
	{
		expect_true(core::checks::check(root).empty()) << core::checks::check(root);
	};

	void CBA::addConstraintsFor(const ValueID& value, Constraints& res) {

		// obtain constraint generator
		auto pos = value2generator.find(value);
		assert_true(pos != value2generator.end()) << "Unknown value id encountered: " << value << "\n";

		// let generator create constraints
		pos->second->addConstraints(*this, value, res);
	}

	namespace {

		string escape(string str) {
			boost::replace_all(str, "\"", "\\\"");
			return str;
		}

	}


	void CBA::plot(std::ostream& out) const {
		const Constraints& constraints = solver.getConstraints();
		const Assignment& ass = solver.getAssignment();


		// get solutions as strings
		auto solution = ass.toStringMap();

		out << "digraph G {\n\t";

		// solution resolution utility
		auto getSolution = [&](const ValueID& value)->const string& {
			static const string& none = "";
			auto pos = solution.find(value);
			return (pos != solution.end()) ? pos->second : none;
		};

		// print names of all sets
		for(const auto& cur : value2generator) {
			ValueID id = cur.first;
			// use constraint generator to format value
			out << id << " [label=\"";

			// print it to a intermediate buffer to filter out quotes
			std::stringstream res;
			cur.second->printValueInfo(res, *this, id);
			out << escape(res.str());

			out << " = " << escape(getSolution(id)) << "\""
						<< ((solver.isResolved(id)) ? " shape=box" : "") << "];\n\t";
		}

		// print constraints
		for(auto cur : constraints) {
			out << "\n\t";
			cur->writeDotEdge(out, ass);
		}

		out << "\n}\n";
	}

	void CBA::plotRoots(std::ostream& out) const {
		const Constraints& constraints = solver.getConstraints();
		const Assignment& ass = solver.getAssignment();

		// get solutions as strings
		auto solution = ass.toStringMap();

		out << "digraph G {\n\t";

		// solution resolution utility
		auto getSolution = [&](const ValueID& value)->const string& {
			static const string& none = "";
			auto pos = solution.find(value);
			return (pos != solution.end()) ? pos->second : none;
		};

		// print names of all sets
		for(const auto& cur : value2generator) {

			// skip non-resolved values (not important)
			ValueID id = cur.first;
			if (!solver.isResolved(id)) continue;

			// only print root nodes
			bool isRoot = all(constraints, [&](const ConstraintPtr& cstr)->bool {
				return !::contains(cstr->getOutputs(), cur.first);
			});

			// skip everything that isn't a root node
			if (!isRoot) continue;

			// use constraint generator to format value
			out << id << " [label=\"";
			cur.second->printValueInfo(out, *this, id);
			out << " = " << getSolution(id) << "\""
						<< ((solver.isResolved(id)) ? " shape=box" : "") << "];\n\t";
		}

		out << "\n}\n";
	}

	void CBA::plotStats(std::ostream& out) const {

		// some basic stuff
		out << "-------- CBA Statistics -----------\n";
		out << format(" %-25s %7d\n", "#Values", getNumSets());
		out << format(" %-25s %7d\n", "#Constraints", getNumConstraints());
		out << format(" %-25s %7d\n", "#Labels", labels.size());
		out << "-----------------------------------\n";

		// statistics on the value types
		std::map<AnalysisType,int> counts;
		for(const auto& cur : value2analysis) {
			counts[cur.second]++;
		}

		// create a sorted version of the counts
		std::vector<std::pair<string,int>> sorted_counts;
		for(const auto& cur : counts) {
			sorted_counts.push_back(std::make_pair(getAnalysisName(cur.first), cur.second));
		}

		// sort counts
		std::sort(sorted_counts.begin(), sorted_counts.end(),
				[](const std::pair<string,int>& a, const std::pair<string,int>& b)->bool {
					return a.second > b.second;
		});

		// print detailed counts
		int sum = 0;
		for(const auto& cur : sorted_counts) {
			out << format(" %-25s %7d\n", cur.first, cur.second);
			sum += cur.second;
		}
		out << "-----------------------------------\n";
		out << format(" %-25s %7d\n", "Total:", sum);
		out << "-----------------------------------\n";
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
