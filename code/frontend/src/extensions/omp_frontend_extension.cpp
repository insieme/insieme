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
#include "insieme/frontend/extensions/omp_frontend_extension.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/tu/ir_translation_unit.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/parallel.h"

#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/pragma/matcher.h"

#include "insieme/utils/config.h"

using namespace insieme::frontend::pragma::tok;
using namespace insieme::frontend::pragma;
using namespace insieme::frontend;

namespace insieme {
namespace frontend {
namespace extensions {

	namespace {

		/*********** OMP PRAGMA SYNTAX ************/
		// if(scalar-expression)
		auto if_expr = kwd("if") >> l_paren >> tok::expr["if"] >> r_paren;

		// default(shared | none)
		auto def = Tok<clang::tok::kw_default>() >> l_paren >> (kwd("shared") | kwd("none"))["default"] >> r_paren;

		// identifier *(, identifier)
		auto var_list = var["v"] >> *(~comma >> var["v"]);

		// private(list)
		auto private_clause = kwd("private") >> l_paren >> var_list["private"] >> r_paren;

		// firstprivate(list)
		auto firstprivate_clause = kwd("firstprivate") >> l_paren >> var_list["firstprivate"] >> r_paren;

		// lastprivate(list)
		auto lastprivate_clause = kwd("lastprivate") >> l_paren >> var_list["lastprivate"] >> r_paren;

		// local(list)
		auto local_clause = kwd("local") >> l_paren >> var_list["local"] >> r_paren;

		// firstlocal(list)
		auto firstlocal_clause = kwd("firstlocal") >> l_paren >> var_list["firstlocal"] >> r_paren;

		// lastlocal(list)
		auto lastlocal_clause = kwd("lastlocal") >> l_paren >> var_list["lastlocal"] >> r_paren;

		// shared(list)
		auto shared_clause = kwd("shared") >> l_paren >> var_list["shared"] >> r_paren;

		// copyin(list)
		auto copyin_clause = kwd("copyin") >> l_paren >> var_list["copyin"] >> r_paren;

		// num_threads(list)
		auto num_threads_clause = kwd("num_threads") >> l_paren >> expr["num_threads"] >> r_paren;

		// + or - or * or & or | or ^ or && or ||
		auto op = tok::plus | tok::minus | tok::star | tok::amp | tok::pipe | tok::caret | tok::ampamp | tok::pipepipe;

		// reduction(operator: list)
		auto reduction_clause = kwd("reduction") >> l_paren >> op["reduction_op"] >> colon >> var_list["reduction"] >> r_paren;

		auto param_quality_range = tok::expr["quality_range"] >> colon >> tok::expr["quality_range"] >> colon >> tok::expr["quality_range"];
		// range(l_bound:u_bound:step)
		auto param_range = kwd("range") >> l_paren >> tok::expr["range"] >> colon >> tok::expr["range"] >> colon >> tok::expr["range"]
		                   >> !(tok::semi >> param_quality_range) >> r_paren;
		// enum(A: s)
		auto param_enum =
		    Tok<clang::tok::kw_enum>() >> l_paren >> var["enum_list"] >> colon >> tok::expr["enum_size"] >> !(tok::semi >> param_quality_range) >> r_paren;

		// param(var, [range(l:u:s) | enum(A:s)])
		auto param_clause = kwd("param") >> l_paren >> var["param_var"] >> !(comma >> (param_range | param_enum)) >> r_paren;

		//  auto | ( i | i1,i2,i3,...,in ) | i ... j
		auto target_group = kwd("auto") | (tok::expr["target_group"] >> !(tok::ellipsis >> tok::expr["target_group_upper"]));
		auto target_core = kwd("auto") | (tok::expr["target_core"] >> !(tok::ellipsis >> tok::expr["target_core_upper"]));

		// target(target-type[:group-id[:core-id]])
		auto target_clause = kwd("target") >> l_paren >> (kwd("general") | kwd("accelerator"))["target_type"] >> !(colon >> target_group)
		                     >> !(colon >> (target_core)) >> r_paren;

		// f * (T | E | P | Q)
		auto objective_weight = tok::numeric_constant["obj_weights_factors"] >> tok::star
		                        >> (kwd("T")["obj_weights_params"] | kwd("E")["obj_weights_params"] | kwd("P")["obj_weights_params"]
		                            | kwd("Q")["obj_weights_params"]);

		// f1 * T + f2 * E + f3 * P + f4 * Q
		auto objective_weights = objective_weight >> tok::plus >> objective_weight >> tok::plus >> objective_weight >> tok::plus >> objective_weight;

		//  < or <= or == or >= or >
		auto obj_constraints_op = tok::less | tok::lessequal | tok::equalequal | tok::greaterequal | tok::greater;

		// constraint = ( T | P | E ) op expr
		auto objective_constraint =
		    (kwd("T")["obj_constraints_params"] | kwd("E")["obj_constraints_params"] | kwd("P")["obj_constraints_params"] | kwd("Q")["obj_constraints_params"])
		    >> obj_constraints_op["obj_constraints_ops"] >> tok::expr["obj_constraints_exprs"];

		// constraints = constraint, constraints | 0
		auto objective_constraints = objective_constraint >> *(tok::semi >> objective_constraint);

		// objective((weights, constraints)
		auto objective_clause = kwd("objective") >> l_paren >> !objective_weights >> !(tok::colon >> objective_constraints) >> r_paren;

		auto approximate_clause = kwd("approximate") >> l_paren >> expr["approx_target_expr"] >> colon >> expr["approx_replacement_expr"] >> r_paren;

		auto region_clause = (  // param(var, [range(l,u,s) | enum(A,s)])
		    param_clause |      // local(list)
		    local_clause |      // firstlocal(list)
		    firstlocal_clause | // lastlocal(list)
		    lastlocal_clause |  // target(target-type[:group-id[:core-id]])
		    target_clause |     // objective((weights, constraints)
		    objective_clause |  // param(var, [range(l,u,s) | enum(A,s)])
		    param_clause);

		auto region_clause_list = !(region_clause >> *(!comma >> region_clause));

		auto parallel_clause = (  // if(scalar-expression)
		    if_expr |             // num_threads(integer-expression)
		    num_threads_clause |  // default(shared | none)
		    def |                 // private(list)
		    private_clause |      // firstprivate(list)
		    firstprivate_clause | // shared(list)
		    shared_clause |       // copyin(list)
		    copyin_clause |       // reduction(operator: list)
		    reduction_clause |    // local(list)
		    local_clause |        // firstlocal(list)
		    firstlocal_clause |   // lastlocal(list)
		    lastlocal_clause |    // target(target-type[:group-id[:core-id]])
		    target_clause |       // objective((weights, constraints)
		    objective_clause |    // param(var, [range(l,u,s) | enum(A,s)])
		    param_clause);

		auto kind = Tok<clang::tok::kw_static>() | kwd("dynamic") | kwd("guided") | kwd("auto") | kwd("runtime");

		auto for_clause = (private_clause | firstprivate_clause | lastprivate_clause | reduction_clause
		                   // schedule( (static | dynamic | guided | atuo | runtime) (, chunk_size) )
		                   | (kwd("schedule") >> l_paren >> kind["schedule"] >> !(comma >> expr["chunk_size"]) >> r_paren)
		                   // collapse( expr )
		                   | (kwd("collapse") >> l_paren >> expr["collapse"] >> r_paren)
		                   // ordered
		                   | kwd("ordered")
		                   // nowait
		                   | kwd("nowait"));

		auto for_clause_list = !(for_clause >> *(!comma >> for_clause));

		auto sections_clause = (  // private(list)
		    private_clause |      // firstprivate(list)
		    firstprivate_clause | // lastprivate(list)
		    lastprivate_clause |  // reduction(operator: list)
		    reduction_clause |    // nowait
		    kwd("nowait"));

		auto sections_clause_list = !(sections_clause >> *(!comma >> sections_clause));

		// [clause[ [, ]clause] ...] new-line
		auto parallel_for_clause_list = (parallel_clause | for_clause | sections_clause) >> *(!comma >> (parallel_clause | for_clause | sections_clause));

		auto parallel_clause_list = !((Tok<clang::tok::kw_for>("for") >> !parallel_for_clause_list) | (kwd("sections") >> !parallel_for_clause_list)
		                              | (parallel_clause >> *(!comma >> parallel_clause)));

		auto single_clause = (                                                    // private(list)
		    private_clause |                                                      // firstprivate(list)
		    firstprivate_clause |                                                 // copyprivate(list)
		    kwd("copyprivate") >> l_paren >> var_list["copyprivate"] >> r_paren | // nowait
		    kwd("nowait"));

		auto single_clause_list = !(single_clause >> *(!comma >> single_clause));

		auto task_clause = (                                            // if(scalar-expression)
		    if_expr |                                                   // untied
		    kwd("untied") |                                             // default(shared | none)
		    def |                                                       // private(list)
		    private_clause |                                            // firstprivate(list)
		    firstprivate_clause |                                       // shared(list)
		    kwd("shared") >> l_paren >> var_list["shared"] >> r_paren | // local(list)
		    local_clause |                                              // firstlocal(list)
		    firstlocal_clause |                                         // lastlocal(list)
		    lastlocal_clause |                                          // target(target-type[:group-id[:core-id]])
		    target_clause |                                             // objective((weights, constraints)
		    objective_clause |                                          // param(var, [range(l,u,s) | enum(A,s)])
		    param_clause |                                              // approximate(target:replacement)
		    approximate_clause);

		auto task_clause_list = !(task_clause >> *(!comma >> task_clause));

		// threadprivate(list)
		auto threadprivate_clause = l_paren >> var_list["thread_private"] >> r_paren;

		/************************************/

		/**
		 *  Checks given match object for all identifiers
		 *  that are contained in the expression or variable list of
		 *  the match object.
		 */
		omp::VarListPtr handleIdentifierList(const MatchObject& m, const std::string& key) {
			std::vector<core::ExpressionPtr> ret;
			for(core::ExpressionPtr p : m.getExprs(key)) {
				ret.push_back(p);
			}
			for(core::VariablePtr p : m.getVars(key)) {
				ret.push_back(p.as<core::ExpressionPtr>());
			}
			return std::make_shared<std::vector<core::ExpressionPtr>>(ret);
		}

		/**
		 *  Checks given match object for single expression clauses
		 *  that are contained in the expression or variable list of
		 *  the match object. (e.g. num_threads, if, ...)
		 */
		core::ExpressionPtr handleSingleExpression(const MatchObject& m, const std::string& key) {
			return m.getSingleExpr(key);
		}

		/**
		 *  Checks given match object for default clauses
		 *  that are contained in the string list of the match object
		 */
		omp::DefaultPtr handleDefaultClause(const MatchObject& m) {
			auto def = m.getString("default");
			if(def.empty()) { return omp::DefaultPtr(); }

			omp::Default::Kind k = omp::Default::SHARED;
			if(def == "shared") {
				k = omp::Default::SHARED;
			} else if(def == "none") {
				k = omp::Default::NONE;
			} else {
				assert_fail() << "Unsupported default kind";
			}

			return std::make_shared<omp::Default>(k);
		}

		/**
		 *  Checks given match object for schedule clauses
		 *  that are contained in the string list of the match object
		 */
		omp::SchedulePtr handleScheduleClause(const MatchObject& m) {
			auto kindStr = m.getString("schedule");
			if(kindStr.empty()) { return omp::SchedulePtr(); }

			// we have a schedule clause
			omp::Schedule::Kind k = omp::Schedule::STATIC;
			if(kindStr == "static") {
				k = omp::Schedule::STATIC;
			} else if(kindStr == "dynamic") {
				k = omp::Schedule::DYNAMIC;
			} else if(kindStr == "guided") {
				k = omp::Schedule::GUIDED;
			} else if(kindStr == "auto") {
				k = omp::Schedule::AUTO;
			} else if(kindStr == "runtime") {
				k = omp::Schedule::RUNTIME;
			} else {
				assert_fail() << "Unsupported scheduling kind";
			}

			// check for chunk_size expression
			core::ExpressionPtr chunkSize = handleSingleExpression(m, "chunk_size");
			return std::make_shared<omp::Schedule>(k, chunkSize);
		}

		/**
		 *  Checks given match object for reduction clauses
		 */
		omp::ReductionPtr handleReductionClause(const MatchObject& mmap) {
			auto vars = mmap.getVars("reduction");
			auto exprs = mmap.getExprs("reduction");
			if(vars.empty() && exprs.empty()) { return omp::ReductionPtr(); }

			// we have a reduction
			// check the operator
			const std::string opIt = mmap.getString("reduction_op");

			assert_false(opIt.empty()) << "Reduction clause doesn't contain an operator";

			omp::Reduction::Operator op = omp::Reduction::PLUS;
			if(opIt == "+") {
				op = omp::Reduction::PLUS;
			} else if(opIt == "-") {
				op = omp::Reduction::MINUS;
			} else if(opIt == "*") {
				op = omp::Reduction::MUL;
			} else if(opIt == "&") {
				op = omp::Reduction::AND;
			} else if(opIt == "|") {
				op = omp::Reduction::OR;
			} else if(opIt == "^") {
				op = omp::Reduction::XOR;
			} else if(opIt == "&&") {
				op = omp::Reduction::LAND;
			} else if(opIt == "||") {
				op = omp::Reduction::LOR;
			} else {
				assert_fail() << "Reduction operator not supported.";
			}

			return std::make_shared<omp::Reduction>(op, handleIdentifierList(mmap, "reduction"));
		}
		
		/**
		 * Type traits used to determine the Marker type used to
		 * attach annotations to the current IR node.
		 * We only need the marker stmt.
		 */
		template <class NodeTy>
		struct marker_type_trait;

		template <>
		struct marker_type_trait<core::Statement> {
			// in the case the node is a statement a MarkerStmt has to be used
			typedef core::MarkerStmt marker_type;
		};

		typedef typename marker_type_trait<core::Statement>::marker_type MarkerTy;

		/**
		 *  This method turns a standard node into a marked node.
		 *  The list of annotations is appended to the annotation
		 *  list of the passed node.
		 */
		template <class NodeTy = core::Statement>
		core::Pointer<const NodeTy> getMarkedNode(core::StatementPtr& stmt, frontend::omp::BaseAnnotation::AnnotationList& anns) {
			if(anns.empty()) { return stmt; }
			// get old annotation list and append our annotations
			if(stmt->hasAnnotation(omp::BaseAnnotation::KEY)) {
				auto annotations = stmt->getAnnotation(omp::BaseAnnotation::KEY)->getAnnotationList();
				for(auto cur : anns) {
					annotations.push_back(cur);
				}
				stmt->addAnnotation(std::make_shared<frontend::omp::BaseAnnotation>(annotations));
				return stmt;
			}
			// if it doesn't have a annotation list, we create a new one
			auto marker = MarkerTy::get(stmt->getNodeManager(), stmt);
			marker->addAnnotation(std::make_shared<frontend::omp::BaseAnnotation>(anns));
			return marker;
		}
	}

	OmpFrontendExtension::OmpFrontendExtension() : flagActivated(false) {
		// Add the required header and macro definitions
		kidnappedHeaders.push_back(utils::getInsiemeSourceRootDir() + "frontend/include/insieme/frontend/omp/input/");
		macros.insert(std::make_pair("_OPENMP", ""));
		
		// Add a handler for pragma omp parallel:
		// #pragma omp parallel [clause[ [, ]clause] ...] new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(insieme::frontend::extensions::PragmaHandler(
		    "omp", "parallel", parallel_clause_list >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    core::ExpressionPtr ifClause = handleSingleExpression(object, "if");
			    // check for num_threads clause
			    core::ExpressionPtr numThreadsClause = handleSingleExpression(object, "num_threads");
			    // check for default clause
			    omp::DefaultPtr defaultClause = handleDefaultClause(object);
			    // check for private clause
			    omp::VarListPtr privateClause = handleIdentifierList(object, "private");
			    // check for firstprivate clause
			    omp::VarListPtr firstPrivateClause = handleIdentifierList(object, "firstprivate");
			    // check for shared clause
			    omp::VarListPtr sharedClause = handleIdentifierList(object, "shared");
			    // check for copyin clause
			    omp::VarListPtr copyinClause = handleIdentifierList(object, "copyin");
			    // check for reduction clause
			    omp::ReductionPtr reductionClause = handleReductionClause(object);

			    // check for 'for'
			    if(object.stringValueExists("for")) {
				    // this is a parallel for
				    // check for last private clause
				    omp::VarListPtr lastPrivateClause = handleIdentifierList(object, "lastprivate");
				    // check for schedule clause
				    omp::SchedulePtr scheduleClause = handleScheduleClause(object);
				    // check for collapse clause
				    core::ExpressionPtr collapseClause = handleSingleExpression(object, "collapse");
				    // check for nowait keyword
				    bool noWait = object.stringValueExists("nowait");
				    // check for ordered keyword
				    bool ordered = object.stringValueExists("ordered");

				    frontend::omp::BaseAnnotation::AnnotationList anns;
				    anns.push_back(std::shared_ptr<omp::ParallelFor>(
				        new omp::ParallelFor(ifClause, numThreadsClause, defaultClause, privateClause, firstPrivateClause, sharedClause, copyinClause,
				                             reductionClause, lastPrivateClause, scheduleClause, collapseClause, noWait, ordered)));

				    // get next for stmt from node list and annotate it
				    for(auto& node : nodes) {
					    core::StatementPtr&& stmt = node.as<core::StatementPtr>();
					    // if it is already a marker stmt check the sub stmt
					    // else check if it is a for and annotate it.
					    if(stmt.isa<core::MarkerStmtPtr>()) {
						    if(stmt.as<core::MarkerStmtPtr>()->getSubStatement().isa<core::ForStmtPtr>()) {
							    node = getMarkedNode(stmt, anns);
							    return nodes;
						    }
					    } else if(stmt.isa<core::ForStmtPtr>()) {
						    node = getMarkedNode(stmt, anns);
						    return nodes;
					    }
				    }
			    }

			    // check for 'sections'
			    if(object.stringValueExists("sections")) {
				    // this is a parallel sections
				    omp::VarListPtr lastPrivateClause = handleIdentifierList(object, "lastprivate");
				    // check for nowait keyword
				    bool noWait = object.stringValueExists("nowait");
				    frontend::omp::BaseAnnotation::AnnotationList anns;
				    anns.push_back(std::make_shared<omp::ParallelSections>(ifClause, numThreadsClause, defaultClause, privateClause, firstPrivateClause,
				                                                           sharedClause, copyinClause, reductionClause, lastPrivateClause, noWait));
				    for(auto& e : nodes) {
					    core::StatementPtr&& stmt = e.as<core::StatementPtr>();
					    e = getMarkedNode(stmt, anns);
				    }
				    return nodes;
			    }

			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Parallel>(ifClause, numThreadsClause, defaultClause, privateClause, firstPrivateClause, sharedClause,
			                                                   copyinClause, reductionClause));
			    for(auto& e : nodes) {
				    core::StatementPtr&& stmt = e.as<core::StatementPtr>();
				    e = getMarkedNode(stmt, anns);
			    }

			    return nodes;
			})));

		// Add a handler for pragma omp for
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "for", for_clause_list >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    // check for private clause
			    omp::VarListPtr privateClause = handleIdentifierList(object, "private");
			    // check for firstprivate clause
			    omp::VarListPtr firstPrivateClause = handleIdentifierList(object, "firstprivate");
			    // check for lastprivate clause
			    omp::VarListPtr lastPrivateClause = handleIdentifierList(object, "lastprivate");
			    // check for reduction clause
			    omp::ReductionPtr reductionClause = handleReductionClause(object);
			    // check for schedule clause
			    omp::SchedulePtr scheduleClause = handleScheduleClause(object);
			    // check for collapse clause
			    core::ExpressionPtr collapseClause = handleSingleExpression(object, "collapse");
			    // check for nowait keyword
			    bool noWait = object.stringValueExists("nowait");
			    // check for ordered keyword
			    bool ordered = object.stringValueExists("ordered");

			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::For>(privateClause, firstPrivateClause, lastPrivateClause, reductionClause, scheduleClause, 
				                                          collapseClause, noWait, ordered));

			    // apply omp for annotation to outermost loop
			    for(auto& node : nodes) {
				    core::StatementPtr stmt = node.as<core::StatementPtr>();

					core::StatementAddress foundWhile;
					visitDepthFirstOnceInterruptible(core::NodeAddress(stmt), [&](const core::WhileStmtAddress& whileAddr) {
						foundWhile = whileAddr;
						return true;
					});
					if(foundWhile != core::NodeAddress()) {
						auto whileNode = foundWhile.getAddressedNode();
						node = core::transform::replaceAddress(node->getNodeManager(), foundWhile, getMarkedNode(whileNode, anns)).getRootNode();
						return nodes;
					}
				}
				assert_fail() << "Could not attach OMP for annotation";
			    return nodes;
			})));

		// Add a handler for #pragma omp sections [clause[[,] clause] ...] new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(insieme::frontend::extensions::PragmaHandler(
		    "omp", "sections", sections_clause_list >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    omp::VarListPtr privateClause = handleIdentifierList(object, "private");
			    // check for firstprivate clause
			    omp::VarListPtr firstPrivateClause = handleIdentifierList(object, "firstprivate");
			    // check for lastprivate clause
			    omp::VarListPtr lastPrivateClause = handleIdentifierList(object, "lastprivate");
			    // check for reduction clause
			    omp::ReductionPtr reductionClause = handleReductionClause(object);
			    // check for nowait keyword
			    bool noWait = object.stringValueExists("nowait");

			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Sections>(privateClause, firstPrivateClause, lastPrivateClause, reductionClause, noWait));
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp sections [clause[[,] clause] ...] new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "section", tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    assert_eq(1, nodes.size()) << "OpenMP section pragma requires a single statement";
			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Section>());
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp single
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "single", single_clause_list >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    // check for private clause
			    omp::VarListPtr privateClause = handleIdentifierList(object, "private");
			    // check for firstprivate clause
			    omp::VarListPtr firstPrivateClause = handleIdentifierList(object, "firstprivate");
			    // check for copyprivate clause
			    omp::VarListPtr copyPrivateClause = handleIdentifierList(object, "copyprivate");
			    // check for nowait keyword
			    bool noWait = object.stringValueExists("nowait");

			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Single>(privateClause, firstPrivateClause, copyPrivateClause, noWait));
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp task [clause[[,] clause] ...] new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "task", task_clause_list >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    // check for if clause
			    core::ExpressionPtr ifClause = handleSingleExpression(object, "if");
			    // check for nowait keyword
			    bool untied = object.stringValueExists("untied");
			    // check for default clause
			    omp::DefaultPtr defaultClause = handleDefaultClause(object);
			    // check for private clause
			    omp::VarListPtr privateClause = handleIdentifierList(object, "private");
			    // check for firstprivate clause
			    omp::VarListPtr firstPrivateClause = handleIdentifierList(object, "firstprivate");
			    // check for shared clause
			    omp::VarListPtr sharedClause = handleIdentifierList(object, "shared");

			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Task>(ifClause, untied, defaultClause, privateClause, firstPrivateClause, sharedClause));

			    for(auto& node : nodes) {
				    core::StatementPtr&& stmt = node.as<core::StatementPtr>();
				    node = getMarkedNode(stmt, anns);
			    }
			    return nodes;
			})));

		// Add a handler for #pragma omp master new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "master", tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    assert_eq(1, nodes.size()) << "OpenMP master pragma requires a single statement";
			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Master>());
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp critical [(name)] new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(insieme::frontend::extensions::PragmaHandler(
		    "omp", "critical", !(l_paren >> identifier["critical"] >> r_paren) >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    // checking region name (if existing)
			    auto fit = object.getString("critical");
			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    if(!fit.empty()) {
				    anns.push_back(std::make_shared<omp::Critical>(fit));
			    } else {
				    anns.push_back(std::make_shared<omp::Critical>(std::string()));
			    }
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp barrier new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "barrier", tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    // does not need to be a single statement, can just be attached to first in list
			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Barrier>());
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp taskwait new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "taskwait", tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    // does not need to be a single statement, can just be attached to first in list
			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::TaskWait>());
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp atomic new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "atomic", tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    assert_eq(1, nodes.size()) << "OpenMP atomic pragma requires a single statement";
			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Atomic>());
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp flush [(list)] new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(insieme::frontend::extensions::PragmaHandler(
		    "omp", "flush", !(l_paren >> var_list["flush"] >> r_paren) >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    // check for flush identifier list
			    omp::VarListPtr flushList = handleIdentifierList(object, "flush");
			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Flush>(flushList));
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp ordered new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    insieme::frontend::extensions::PragmaHandler("omp", "ordered", tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // attach annotation
			    assert_eq(1, nodes.size()) << "OpenMP ordered pragma requires a single statement";
			    frontend::omp::BaseAnnotation::AnnotationList anns;
			    anns.push_back(std::make_shared<omp::Ordered>());
			    core::StatementPtr&& stmt = nodes[0].as<core::StatementPtr>();
			    nodes[0] = getMarkedNode(stmt, anns);
			    return nodes;
			})));

		// Add a handler for #pragma omp threadprivate(list) new-line
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(insieme::frontend::extensions::PragmaHandler(
		    "omp", "threadprivate", threadprivate_clause >> tok::eod, [&](const MatchObject& object, core::NodeList nodes) {
			    // store the name of the variables
			    omp::VarListPtr tp = handleIdentifierList(object, "thread_private");
			    for(unsigned i = 0; i < tp->size(); i++) {
				    thread_privates.push_back(tp->at(i));
			    }
			    return nodes;
			})));
	}


	/**
	 *  Insieme frontend extension IR visitor. This needs to be done to find all thread_private variables.
	 *  All thread_private variables are annotated with the threadprivate annotation and finally
	 *  the omp sema is called.
	 */
	core::tu::IRTranslationUnit OmpFrontendExtension::IRVisit(core::tu::IRTranslationUnit& tu) {
		for(auto& pair : tu.getGlobals()) {
			if(std::find(thread_privates.begin(), thread_privates.end(), pair.first) != thread_privates.end()) {
				core::LiteralPtr lit = pair.first;
				lit->addAnnotation(std::make_shared<omp::BaseAnnotation>(omp::BaseAnnotation::AnnotationList({std::make_shared<omp::ThreadPrivate>()})));
				std::pair<core::LiteralPtr, core::ExpressionPtr> replacement(lit, pair.second);
				tu.replaceGlobal(pair, replacement);
			}
		}

		for(auto& pair : tu.getFunctions()) {
			core::LambdaExprPtr func = pair.second;
			auto filter = [&func](const core::NodePtr& node) -> bool {
				if(core::LambdaExprPtr call = node.isa<core::LambdaExprPtr>()) {
					if(call == func) {
						return true;
					} else {
						return false;
					}
				}
				return true;
			};
			// wrap all returns with a cpp ref conversion
			auto fixer = [&](const core::NodePtr& node) -> core::NodePtr {
				// find all uses of the variables
				if(std::find(thread_privates.begin(), thread_privates.end(), node) != thread_privates.end()) {
					frontend::omp::BaseAnnotation::AnnotationList anns;
					anns.push_back(std::make_shared<omp::ThreadPrivate>());
					core::StatementPtr stm = node.as<core::StatementPtr>();
					// return getMarkedNode(stm, anns);
					node->addAnnotation(std::make_shared<omp::BaseAnnotation>(omp::BaseAnnotation::AnnotationList({std::make_shared<omp::ThreadPrivate>()})));
				}
				return node;
			};
			auto TPfixer = core::transform::makeCachedLambdaMapper(fixer, filter);
			func = TPfixer.map(func);
			tu.replaceFunction(pair.first.as<core::LiteralPtr>(), func);
		}

		// apply open mp sema
		tu = omp::applySema(tu, tu.getNodeManager());

		return tu;
	}

	FrontendExtension::FlagHandler OmpFrontendExtension::registerFlag(boost::program_options::options_description& options) {
		// register omp flag
		options.add_options()("fopenmp", boost::program_options::value<bool>(&flagActivated)->implicit_value(true), "OpenMP support");
		// create lambda
		auto lambda = [&](const ConversionJob& job) { return flagActivated; };
		return lambda;
	}

	core::ProgramPtr OmpFrontendExtension::IRVisit(core::ProgramPtr& prog) {
		std::map<core::NodeAddress, core::NodePtr> replacements;
		auto& mgr = prog->getNodeManager();
		auto builder = core::IRBuilder(mgr);
		auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
		auto& parExt = mgr.getLangExtension<core::lang::ParallelExtension>();

		// Collect and replace all zero inits of global locks with noOps.
		core::visitDepthFirstOnce(core::NodeAddress(prog), [&](const core::CallExprAddress& cA){
			core::CallExprPtr call = cA.getAddressedNode();
			if(core::analysis::isCallOf(call, refExt.getRefAssign())) {
				//check if rhs is a literal (global)
				//check if lhs is a zero(type_lit(lock))
				if(call->getArgument(0).isa<core::LiteralPtr>() &&
					(call->getArgument(1) == builder.getZero(parExt.getLock()))) {
						replacements.insert({ cA, builder.getNoOp() });
				}
			}
		});
		// replace all occurrences
		if(!replacements.empty()) prog = core::transform::replaceAll(mgr, replacements).as<core::ProgramPtr>();
		return prog;
	}


} // end namespace extensions
} // end namespace frontend
} // end namespace insieme
