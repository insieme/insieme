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

#pragma once

#include "insieme/analysis/cba/analysis/execution_nets.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * This class implements an analysis creating an execution net (petri-net based model of the
	 * parallel execution of an application) as the result of an analysis.
	 */


	// ----------------- execution state graph ---------------

	template<typename Context> class ExecutionStateConstraintGenerator;

	struct execution_state_analysis {
		template<typename C> struct lattice   { typedef utils::constraint::Lattice<utils::petri_net::StateGraph<Place<typename C::context_type>,Transition<typename C::context_type>>> type; };
		template<typename C> struct generator { typedef ExecutionStateConstraintGenerator<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType> type; };
	};

	extern const execution_state_analysis ExecutionStateAnalysis;

	namespace {

		template<typename Context>
		ConstraintPtr createExecutionStateConstraint(CBA& cba);

	}

	template<typename Context>
	class ExecutionStateConstraintGenerator : public ConstraintGenerator {

	public:

		ExecutionStateConstraintGenerator(CBA& cba) {}

		virtual void addConstraints(CBA& cba, const sc::ValueID& value, Constraints& constraints) {

			// just create the one constraint assembling the graph - the magic happens in there
			constraints.add(createExecutionStateConstraint<Context>(cba));

		}

		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {
			out << "ExecutionStateGraph = " << value;
		}

	};

	namespace {

		template<typename Context>
		class ExecutionStateConstraint : public utils::constraint::Constraint {

			typedef utils::petri_net::StateGraph<Place<Context>,Transition<Context>> StateGraph;

			// some type definitions
			typedef TypedValueID<typename execution_net_analysis::lattice<analysis_config<Context>>::type> 		ExecutionNetValueID;
			typedef TypedValueID<typename execution_state_analysis::lattice<analysis_config<Context>>::type> 	ExecutionStateValueID;

			CBA& cba;

			ExecutionNetValueID in;

			ExecutionStateValueID out;

		public:

			ExecutionStateConstraint(CBA& cba) :
				Constraint(
						toVector<ValueID>(cba.getSet<Context>(ExecutionNetAnalysis)),
						toVector<ValueID>(cba.getSet<Context>(ExecutionStateAnalysis)), false, false
				),
				cba(cba),
				in(cba.getSet<Context>(ExecutionNetAnalysis)),
				out(cba.getSet<Context>(ExecutionStateAnalysis)) { }

			virtual UpdateResult update(Assignment& ass) const {

				// get value to be updated
				auto& is = ass[out];

				// obtain should-value
				auto should = buildGraph(ass);

				// check whether something has changed
				if (is == should) return Unchanged;

				// update it and indicate complete change
				is = should;
				return Altered;
			}

			virtual bool check(const Assignment& ass) const {

				// check whether value is what it should be
				return ass[out] == buildGraph(ass);
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				// print merged in thread dependencies
				return out << in << " -> " << this->out << "[label=\"generates\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "ExecutionNetBuilder";
			}

		private:

			StateGraph buildGraph(const Assignment& ass) const {
				typedef utils::petri_net::Marking<Place<Context>, Transition<Context>> Marking;

				const auto& net = ass[in];

				// create marking
				Marking marking(net);

				// create initial marking
				for(const auto& cur : net.getInitialPlaces()) {
					marking.setMarking(cur);
				}

				// compute state graph
				return utils::petri_net::extractStateGraph(marking);
			}

		};

		template<typename Context>
		ConstraintPtr createExecutionStateConstraint(CBA& cba) {
			return std::make_shared<ExecutionStateConstraint<Context>>(cba);
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
