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

#include "insieme/analysis/cba/framework/analysis_type.h"
#include "insieme/analysis/cba/framework/entities/execution_net.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/analysis/sync_points.h"
#include "insieme/analysis/cba/analysis/thread_regions.h"
#include "insieme/analysis/cba/analysis/thread_bodies.h"
#include "insieme/analysis/cba/analysis/channels.h"

#include "insieme/analysis/cba/utils/cba_utils.h"
#include "insieme/analysis/cba/utils/constraint_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * This class implements an analysis creating an execution net (petri-net based model of the
	 * parallel execution of an application) as the result of an analysis.
	 */

	// ----------------- execution net ---------------

	template<typename Context> class ExecutionNetConstraintGenerator;

	struct execution_net_analysis {
		template<typename C> struct lattice   { typedef utils::constraint::Lattice<ExecutionNet<typename C::context_type>> type; };
		template<typename C> struct generator { typedef ExecutionNetConstraintGenerator<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType> type; };
	};

	extern const execution_net_analysis ExecutionNetAnalysis;

	namespace {

		template<typename Context>
		ConstraintPtr createExecutionNetConstraint(CBA& cba);

	}

	template<typename Context>
	class ExecutionNetConstraintGenerator : public ConstraintGenerator {

	public:

		ExecutionNetConstraintGenerator(CBA& cba) {}

		virtual void addConstraints(CBA& cba, const sc::ValueID& value, Constraints& constraints) {

			// just create the one constraint assembling the network - the magic happens in there
			constraints.add(createExecutionNetConstraint<Context>(cba));

		}

		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {
			out << "ExecutionNet = " << value;
		}

	};

	namespace {

		template<typename Context>
		class ExecutionNetConstraint : public utils::constraint::Constraint {

			// some type definitions
			typedef TypedValueID<typename execution_net_analysis::lattice<analysis_config<Context>>::type> 		ExecutionNetValueID;
			typedef TypedValueID<typename sync_points_analysis::lattice<analysis_config<Context>>::type> 		SyncPointValueID;
			typedef TypedValueID<typename thread_regions_analysis::lattice<analysis_config<Context>>::type> 	ThreadRegionValueID;
			typedef TypedValueID<typename thread_body_analysis::lattice<analysis_config<Context>>::type> 		ThreadBodyValueID;
			typedef TypedValueID<typename channel_analysis_data::lattice<analysis_config<Context>>::type> 		ChannelValueID;

			CBA& cba;

			ExecutionNetValueID net;

			SyncPointValueID syncPoints;

			ThreadRegionValueID regions;

			mutable std::map<ProgramPoint<Context>, ThreadBodyValueID> threadBodies;

			mutable std::map<ProgramPoint<Context>, ChannelValueID> channelValues;

			mutable std::vector<ValueID> inputs;

		public:

			ExecutionNetConstraint(CBA& cba) :
				Constraint(
						toVector<ValueID>(cba.getSet<Context>(SyncPoints), cba.getSet<Context>(ThreadRegions)),
						toVector<ValueID>(cba.getSet<Context>(ExecutionNetAnalysis)), true, true
				),
				cba(cba),
				net(cba.getSet<Context>(ExecutionNetAnalysis)),
				syncPoints(cba.getSet<Context>(SyncPoints)),
				regions(cba.getSet<Context>(ThreadRegions)) {
				inputs.push_back(syncPoints);
				inputs.push_back(regions);
			}

			virtual UpdateResult update(Assignment& ass) const {

				// get value to be updated
				ExecutionNet<Context>& is = ass[net];

				ExecutionNet<Context> should = buildNet(ass);

				// check whether something has changed
				if (is == should) return Unchanged;

				// update it and indicate complete change
				is = should;
				return Altered;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// dependencies are 'depending' on sync points
				const set<ProgramPoint<Context>>& all_sync_points = ass[syncPoints];

				bool changed = false;
				for (const auto& point : all_sync_points) {

					// sort out the different types of sync points
					if (point.isThreadStart() || point.isThreadEnd()) {
						// nothing to do
					} else if (point.isSpawn() || point.isMerge()) {

						// check whether the thread body is already known
						if (threadBodies.find(point) != threadBodies.end()) continue;

						// add dependency to thread bodies
						auto bodySet = cba.getSet(ThreadBodies, point.getStatement(), point.getContext());
						threadBodies[point] = bodySet;
						inputs.push_back(bodySet);
						changed = true;

					} else if (point.isSend() || point.isRecv()) {

						// check whether the channel is already known
						if (channelValues.find(point) != channelValues.end()) continue;

						// add dependency to channel
						auto channelSet = cba.getSet(Ch, point.getStatement().template as<CallExprAddress>()[0], point.getContext());
						channelValues[point] = channelSet;
						inputs.push_back(channelSet);
						changed = true;

					} else {
						assert_not_implemented() << " No support implemented for: " << point;
					}

				}

				// indicated whether some dependencies have changed
				return changed;
			}

			virtual const std::vector<ValueID>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

			virtual bool check(const Assignment& ass) const {

				// check whether dependencies are fixed
				if (updateDynamicDependencies(ass)) return false;

				// check whether value is what it should be
				return ass[net] == buildNet(ass);
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : inputs) {
					out << cur << " -> " << net << "[label=\"uses\"]\n";
				}

				return out;
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "ExecutionNetBuilder";
			}

		private:

			ExecutionNet<Context> buildNet(const Assignment& ass) const {
				ExecutionNet<Context> res;

				const set<ThreadRegion<Context>>& regions = ass[this->regions];

				// get set of sync points and index them
				std::map<ProgramPoint<Context>, std::set<ThreadRegion<Context>>> pred;
				std::map<ProgramPoint<Context>, std::set<ThreadRegion<Context>>> succ;
				for(const auto& cur : regions) {
					// built up index
					pred[cur.getEnd()].insert(cur);
					succ[cur.getBegin()].insert(cur);
				}

				// index thread-start and end regions
				std::map<ThreadBody<Context>, std::vector<ThreadRegion<Context>>> startRegions;
				std::map<ThreadBody<Context>, std::vector<ThreadRegion<Context>>> endRegions;
				for(const auto& cur : regions) {
					if (cur.getBegin().isThreadStart()) {
						auto point = cur.getBegin();
						auto body = ThreadBody<Context>(point.getStatement(), point.getContext());
						startRegions[body].push_back(cur);
					}
					if (cur.getEnd().isThreadEnd()) {
						auto point = cur.getEnd();
						auto body = ThreadBody<Context>(point.getStatement(), point.getContext());
						endRegions[body].push_back(cur);
					}
				}


				// ------- create places -----------

				// register dummy place (will be utilized in case construction information is missing)
				Place<Context> dummy;		// NOTE: this is the place maps will return if the key is invalid!
				res.addPlace(dummy);

				// create list of region-places
				std::map<ThreadRegion<Context>, Place<Context>> r2p;
				for(const auto& cur : regions) {
					r2p[cur] = res.createRegion(cur);

					// mark as initial if it is a initial place
					auto start = cur.getBegin();
					if (start.isThreadStart() && start.getContext() == Context()) {
						res.markInitial(r2p[cur]);
					}
				}

				// create body-start places
				std::map<ThreadBody<Context>, Place<Context>> startPlace;
				for(const auto& cur : startRegions) {
					if (cur.second.size() == 1u) {
						startPlace[cur.first] = r2p[cur.second[0]];
					} else {
						startPlace[cur.first] = res.createAuxiliary();
						for(const auto& region : cur.second) {
							res.link(startPlace[cur.first], r2p[region]);
						}
					}
				}

				// create body-end places
				std::map<ThreadBody<Context>, Place<Context>> endPlace;
				for(const auto& cur : endRegions) {
					if (cur.second.size() == 1u) {
						endPlace[cur.first] = r2p[cur.second[0]];
					} else {
						endPlace[cur.first] = res.createAuxiliary();
						for(const auto& region : cur.second) {
							res.link(r2p[region], endPlace[cur.first]);
						}
					}
				}

				// collect list of channels
				std::map<Channel<Context>, Place<Context>> c2p;
				for(const auto& cur : channelValues) {
					const std::set<Channel<Context>>& chls = ass[cur.second];
					for(const auto& chl : chls) {
						if (c2p.find(chl) == c2p.end()) {
							c2p[chl] = res.createChannel(chl);
							// TODO: fix channel capacity
							// TODO: external channels may be non-deterministically filled / drained
						}
					}
				}


				// ------- create transitions -----------

				auto buildInOut = [&](const ProgramPoint<Context>& point)->Transition<Context> {

					// get list of pre- and post-states
					vector<Place<Context>> ins;
					for(const auto& cur : pred[point]) {
						ins.push_back(r2p[cur]);
					}

					vector<Place<Context>> outs;
					for(const auto& cur : succ[point]) {
						outs.push_back(r2p[cur]);
					}

					// link in-states to one in-node
					Place<Context> in;
					if (ins.size() == 1u) {
						in = ins[0];
					} else {
						// add transition from in to auxiliary node
						in = res.createAuxiliary();
						for(const auto& cur : ins) {
							res.link(cur, in);
						}
					}


					// link out-node to out-states
					Place<Context> out;
					if (outs.size() == 1u) {
						out = outs[0];
					} else {
						// make out an auxiliary node and link it to out states
						out = res.createAuxiliary();
						for(const auto& cur : outs) {
							res.link(out, cur);
						}
					}

					// create spawn-transition
					Transition<Context> trans = res.createTransition(point);
					res.addPrePlace(in, trans);
					res.addPostPlace(trans, out);

					return trans;
				};

				// transitions are created by sync points

				const std::set<ProgramPoint<Context>>& points = ass[syncPoints];
				for(const auto& p : points) {

					if (p.isThreadStart() || p.isThreadEnd()) {
						// can be ignores => handled by spawn/merge
					} else if (p.isSpawn()) {

						// create operation within thread
						auto spawn = buildInOut(p);

						// link spawn-transition with
						const std::set<ThreadBody<Context>>& bodies = ass[threadBodies[p]];

						Place<Context> body;
						if (bodies.size() == 1u) {
							// direct body
							body = startPlace[*bodies.begin()];
						} else {
							body = res.createAuxiliary();
							for(const auto& cur : bodies) {
								res.link(body, startPlace[cur]);
							}
						}

						// link spawn-transition with body
						res.addPostPlace(spawn, body);

					} else if (p.isMerge()) {

						// create operation within thread
						auto merge = buildInOut(p);

						// link merge-transition with bodies
						const std::set<ThreadBody<Context>>& bodies = ass[threadBodies[p]];

						Place<Context> body;
						if (bodies.size() == 1u) {
							// direct body
							body = endPlace[*bodies.begin()];
						} else {
							body = res.createAuxiliary();
							for(const auto& cur : bodies) {
								res.link(endPlace[cur], body);
							}
						}

						// link spawn-transition with body
						res.addPrePlace(body, merge);


					} else if (p.isSend()) {

						// create operation within thread
						auto send = buildInOut(p);

						// get list of targeted channels
						const std::set<Channel<Context>>& chls = ass[channelValues[p]];

						Place<Context> chl;
						if (chls.size() == 1u) {
							// direct send
							chl = c2p[*chls.begin()];
						} else {
							// create an auxiliary channel
							chl = res.createAuxiliary();
							for(const auto& cur : chls) {
								res.link(chl, c2p[cur]);
							}
						}

						// link send to channel
						res.addPostPlace(send, chl);

					} else if (p.isRecv()) {

						// create operation within thread
						auto recv = buildInOut(p);

						// get list of targeted channels
						const std::set<Channel<Context>>& chls = ass[channelValues[p]];

						Place<Context> chl;
						if (chls.size() == 1u) {
							// direct send
							chl = c2p[*chls.begin()];
						} else {
							// create an auxiliary channel
							chl = res.createAuxiliary();
							for(const auto& cur : chls) {
								res.link(c2p[cur], chl);
							}
						}

						// link send to channel
						res.addPrePlace(chl, recv);

					} else {
						assert_not_implemented() << "Unsupported sync-point encountered: " << p << "\n";
					}
				}

				// check whether somebody is referencing the dummy place
				if (res.getNumPreTransitions(dummy) != 0 || res.getNumPostTransitions(dummy) != 0) {
					// dummy is referenced => invalid value
					return ExecutionNet<Context>();		// return default value
				}

				// value is valid (and hence more likely to be stabel)
				// => remove dummy node
				res.removePlace(dummy);

				// done
				return res;
			}

		};

		template<typename Context>
		ConstraintPtr createExecutionNetConstraint(CBA& cba) {
			return std::make_shared<ExecutionNetConstraint<Context>>(cba);
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
