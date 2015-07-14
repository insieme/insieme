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

#include "insieme/analysis/features/code_feature_catalog.h"

#include "insieme/core/pattern/ir_generator.h"
#include "insieme/core/pattern/ir_pattern.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/logging.h"

namespace itpi = insieme::core::pattern::irp;

namespace insieme {
namespace analysis {
namespace features {

using insieme::core::pattern::any;

	namespace {

		void addScalarFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {

			// create lists of considered types
			std::map<string, vector<core::TypePtr>> types;

			types["any"] = vector<core::TypePtr>();

			types["char"] = toVector(basic.getChar());

			types["int1"] = toVector(basic.getInt1());
			types["int2"] = toVector(basic.getInt2());
			types["int4"] = toVector(basic.getInt4());
			types["int8"] = toVector(basic.getInt8());
			types["int*"] = core::convertList<core::Type>(basic.getSignedIntGroup());

			types["uint1"] = toVector(basic.getUInt1());
			types["uint2"] = toVector(basic.getUInt2());
			types["uint4"] = toVector(basic.getUInt4());
			types["uint8"] = toVector(basic.getUInt8());
			types["uint*"] = core::convertList<core::Type>(basic.getUnsignedIntGroup());

			types["integer"] = core::convertList<core::Type>(basic.getIntGroup());

			types["real4"] = toVector(basic.getFloat());
			types["real8"] = toVector(basic.getDouble());
			types["real*"] = core::convertList<core::Type>(basic.getRealGroup());

			// create lists of considered operations
			std::map<string, vector<core::ExpressionPtr>> ops;

			ops["arithmetic"] = core::convertList<core::Expression>(basic.getArithOpGroup());
			ops["comparison"] = core::convertList<core::Expression>(basic.getCompOpGroup());
			ops["bitwise"] = core::convertList<core::Expression>(basic.getBitwiseOpGroup());

			ops["all"] = vector<core::ExpressionPtr>();
			addAll(ops["all"], ops["arithmetic"]);
			addAll(ops["all"], ops["comparison"]);
			addAll(ops["all"], ops["bitwise"]);

			// modes
			std::map<string, FeatureAggregationMode> modes;
			modes["static"] = FA_Static;
			modes["weighted"] = FA_Weighted;
			modes["real"] = FA_Real;
			modes["polyhedral"] = FA_Polyhedral;


			// create the actual features
			for_each(types, [&](const std::pair<string, vector<core::TypePtr>>& cur_type) {
				for_each(ops, [&](const std::pair<string, vector<core::ExpressionPtr>>& cur_ops){
					for_each(modes, [&](const std::pair<string, FeatureAggregationMode>& cur_mode) {

						string name = format("SCF_NUM_%s_%s_OPs_%s", cur_type.first.c_str(),
								cur_ops.first.c_str(), cur_mode.first.c_str());

						string desc = format("Counts the number of %s operations producing values of type %s - aggregation mode: %s",
								cur_ops.first.c_str(), cur_type.first.c_str(), cur_mode.first.c_str());

						catalog.addFeature(createSimpleCodeFeature(name, desc, SimpleCodeFeatureSpec(cur_type.second, cur_ops.second, cur_mode.second)));
					});
				});
			});
		}

		void addVectorFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {

			// create list of considered types
			std::map<string, vector<core::TypePtr>> types;

			types["any"] = vector<core::TypePtr>();

			types["char"] = toVector(basic.getChar());

			types["int1"] = toVector(basic.getInt1());
			types["int2"] = toVector(basic.getInt2());
			types["int4"] = toVector(basic.getInt4());
			types["int8"] = toVector(basic.getInt8());
			types["int*"] = core::convertList<core::Type>(basic.getSignedIntGroup());

			types["uint1"] = toVector(basic.getUInt1());
			types["uint2"] = toVector(basic.getUInt2());
			types["uint4"] = toVector(basic.getUInt4());
			types["uint8"] = toVector(basic.getUInt8());
			types["uint*"] = core::convertList<core::Type>(basic.getUnsignedIntGroup());

			types["integer"] = core::convertList<core::Type>(basic.getIntGroup());

			types["real4"] = toVector(basic.getFloat());
			types["real8"] = toVector(basic.getDouble());
			types["real*"] = core::convertList<core::Type>(basic.getRealGroup());


			// create a list of considered operation classes
			std::map<string, vector<core::ExpressionPtr>> ops;

			ops["arithmetic"] = core::convertList<core::Expression>(basic.getArithOpGroup());
			ops["comparison"] = core::convertList<core::Expression>(basic.getCompOpGroup());
			ops["bitwise"] = core::convertList<core::Expression>(basic.getBitwiseOpGroup());

			ops["all"] = vector<core::ExpressionPtr>();
			addAll(ops["all"], ops["arithmetic"]);
			addAll(ops["all"], ops["comparison"]);
			addAll(ops["all"], ops["bitwise"]);

			// modes
			std::map<string, FeatureAggregationMode> modes;
			modes["static"] = FA_Static;
			modes["weighted"] = FA_Weighted;
			modes["real"] = FA_Real;
			modes["polyhedral"] = FA_Polyhedral;

			// create the actual features
			for_each(types, [&](const std::pair<string, vector<core::TypePtr>>& cur_type) {
				for_each(ops, [&](const std::pair<string, vector<core::ExpressionPtr>>& cur_ops){
					for_each(modes, [&](const std::pair<string, FeatureAggregationMode>& cur_mode) {

						string name = format("SCF_NUM_%s_%s_VEC_OPs_%s", cur_type.first.c_str(),
								cur_ops.first.c_str(), cur_mode.first.c_str());

						string desc = format("Counts the number of vectorized %s operations operating on %s - aggregation mode: %s",
								cur_ops.first.c_str(), cur_type.first.c_str(), cur_mode.first.c_str());

						catalog.addFeature(createSimpleCodeFeature(name, desc, createVectorOpSpec(cur_type.second, cur_ops.second, false, cur_mode.second)));
					});
				});
			});

		}

		void addMemoryAccessFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {

			// is adding features for:
			// 	- number of read operations
			//	- number of write operations
			//	- number of scalar read operations
			//  - number of scalar write operations
			//	- number of vector read operations
			//	- number of vector write operations
			//	- number of array read operations
			//	- number of array write operations

			// still to do: Volume
			//	- volume of read operations
			//	- volume of write operations

			std::map<string, MemoryAccessTarget> target;
			target["any"] = MemoryAccessTarget::ANY;
			target["scalar"] = MemoryAccessTarget::SCALAR;
			target["vector"] = MemoryAccessTarget::VECTOR;
			target["array"] = MemoryAccessTarget::ARRAY;

			std::map<string, MemoryAccessMode> access;
			access["read"] = MemoryAccessMode::READ;
			access["write"] = MemoryAccessMode::WRITE;
			access["read/write"] = MemoryAccessMode::READ_WRITE;

			// modes
			std::map<string, FeatureAggregationMode> modes;
			modes["static"] = FA_Static;
			modes["weighted"] = FA_Weighted;
			modes["real"] = FA_Real;
			modes["polyhedral"] = FA_Polyhedral;

			// create the actual features
			for_each(target, [&](const std::pair<string, MemoryAccessTarget>& cur_target){
				for_each(access, [&](const std::pair<string, MemoryAccessMode>& cur_mode) {
					for_each(modes, [&](const std::pair<string, FeatureAggregationMode>& cur_aggreagation) {

						string name = format("SCF_IO_NUM_%s_%s_OPs_%s", cur_target.first.c_str(),
								cur_mode.first.c_str(), cur_aggreagation.first.c_str());

						string desc = format("Counts the number of %s memory accesses to %s elements - aggregation mode: %s",
								cur_mode.first.c_str(), cur_target.first.c_str(), cur_aggreagation.first.c_str());

						catalog.addFeature(createSimpleCodeFeature(name, desc,
								createMemoryAccessSpec(cur_mode.second, cur_target.second, cur_aggreagation.second))
						);
					});
				});
			});


		}

		// add features related to parallelism (e.g. number of barrieres)
		void addParallelFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {
			// create lists of considered operations
			std::map<string, core::ExpressionPtr> ops;
			ops["barrier"] = basic.getBarrier();

			// not sure if all makes sense in this case...
//			ops["all"] = vector<core::ExpressionPtr>();
//			addAll(ops["all"], ops["barrier"]);

			// modes
			std::map<string, FeatureAggregationMode> modes;
			modes["static"] = FA_Static;
			modes["weighted"] = FA_Weighted;
			modes["real"] = FA_Real;
			modes["polyhedral"] = FA_Polyhedral;


			// create the actual features
			for_each(ops, [&](const std::pair<string, core::ExpressionPtr>& cur_ops){
				for_each(modes, [&](const std::pair<string, FeatureAggregationMode>& cur_mode) {

					string name = format("SCF_NUM_%s_Calls_%s",
							cur_ops.first.c_str(), cur_mode.first.c_str());

					string desc = format("Counts the number of %s - aggregation mode: %s",
							cur_ops.first.c_str(), cur_mode.first.c_str());

					catalog.addFeature(createSimpleCodeFeature(name, desc, SimpleCodeFeatureSpec(basic.getUnit(), cur_ops.second, cur_mode.second)));
				});
			});
		}


		// add features that count occurrences of certain patterns
		void addPatternFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {
			// create lists of considered operations
			std::map<string, core::pattern::TreePattern> patterns;
			patterns["function"] = itpi::callExpr(any, *any);
			// TODO: @Klaus: I had to change this pattern due to added casts - hope this is still OK ...
			patterns["globalMemoryAccess"] = itpi::callExpr( itpi::literal("_ocl_unwrap_global"), *any);
			patterns["localMemoryAccess"] = itpi::callExpr( itpi::literal("_ocl_unwrap_local"), *any);

			// not sure if all makes sense in this case...
//			ops["all"] = vector<core::ExpressionPtr>();
//			addAll(ops["all"], ops["barrier"]);

			// modes
			std::map<string, FeatureAggregationMode> modes;
			modes["static"] = FA_Static;
			modes["weighted"] = FA_Weighted;
			modes["real"] = FA_Real;
			modes["polyhedral"] = FA_Polyhedral;


			// create the actual features
			for_each(patterns, [&](const std::pair<string, core::pattern::TreePattern>& cur_pattern){
				for_each(modes, [&](const std::pair<string, FeatureAggregationMode>& cur_mode) {

					string name = format("SCF_NUM_%s_calls_%s",
							cur_pattern.first.c_str(), cur_mode.first.c_str());

					string desc = format("Counts the number of %s - aggregation mode: %s",
							cur_pattern.first.c_str(), cur_mode.first.c_str());

					catalog.addFeature(createPatternCodeFeature(name, desc, PatternCodeFeatureSpec(cur_pattern.second, cur_mode.second)));
				});
			});
		}

		// add features that count on how many nodes a passed lambda evaluates to true
		void addLambdaFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {

			std::map<string, std::function<simple_feature_value_type(const core::NodePtr)> > lambdas;
			lambdas["variables"] = [=](core::NodePtr node) { if(node->getNodeType() == core::NT_Variable) return true; return false; };
			lambdas["pattern"] = [=](core::NodePtr node) {
				core::pattern::TreePattern pattern = itpi::callExpr(any, *any);
				insieme::core::pattern::MatchOpt&& match = pattern.matchPointer(node);
				return !!match;
			};
			lambdas["externalFunction"] = [&](core::NodePtr node) {
				if(const core::CallExprPtr call = dynamic_pointer_cast<const core::CallExpr>(node)) {
					if(const core::LiteralPtr literal = dynamic_pointer_cast<const core::Literal>(call->getFunctionExpr())) {
						if(!core::lang::isBuiltIn(literal)) {
							//ignore helper functons introduced by the OpenCL backend
							if(literal.toString().substr(0,5).compare(string("_ocl_")) != 0)
								return 1.0;
						}
					}
				}
				return 0.0;
			};
			lambdas["builtinFunction"] = [&](core::NodePtr node) {
				if(const core::CallExprPtr call = dynamic_pointer_cast<const core::CallExpr>(node)) {
					if (core::lang::isBuiltIn(call->getFunctionExpr())) {
						return 1;
					}
				}
				return 0;
			};
			lambdas["branches"] = [&](core::NodePtr node) {
				if(node->getNodeType() == core::NT_IfStmt)
					return 1;
				if(node->getNodeType() == core::NT_SwitchStmt)
					return 1;
				if(core::analysis::isCallOf(node, node->getNodeManager().getLangBasic().getIfThenElse()))
					return 1;

				return 0;
			};
			lambdas["loops"] = [&](core::NodePtr node) {
				if(node->getNodeType() == core::NT_ForStmt)
					return 1;
				if(node->getNodeType() == core::NT_WhileStmt)
					return 1;
				return 0;
			};
			lambdas["breaks"] = [&](core::NodePtr node) {
				if(node->getNodeType() == core::NT_ReturnStmt)
					return 1;
				if(node->getNodeType() == core::NT_ContinueStmt)
					return 1;
				if(node->getNodeType() == core::NT_BreakStmt)
					return 1;
				return 0;
			};

			lambdas["calls"] = [&](core::NodePtr node) {
				if(node->getNodeType() == core::NT_CallExpr)
					return 1;
				return 0;
			};

			// not sure if all makes sense in this case...
//			ops["all"] = vector<core::ExpressionPtr>();
//			addAll(ops["all"], ops["barrier"]);

			// modes
			std::map<string, FeatureAggregationMode> modes;
			modes["static"] = FA_Static;
			modes["weighted"] = FA_Weighted;
			modes["real"] = FA_Real;
			modes["polyhedral"] = FA_Polyhedral;


			// create the actual features
			for_each(lambdas, [&](const std::pair<string, std::function<simple_feature_value_type(const core::NodePtr&)> > & cur_lambda){
				for_each(modes, [&](const std::pair<string, FeatureAggregationMode>& cur_mode) {

					string name = format("SCF_NUM_%s_lambda_%s",
							cur_lambda.first.c_str(), cur_mode.first.c_str());

					string desc = format("Counts the number of %s - aggregation mode: %s",
							cur_lambda.first.c_str(), cur_mode.first.c_str());

					catalog.addFeature(createLambdaCodeFeature(name, desc, LambdaCodeFeatureSpec(cur_lambda.second, cur_mode.second)));
				});
			});
		}


		void addBinaryComposedFeature(const char* name, const char* component0, const char* component1, const char* mode,
				FeatureCatalog& catalog, std::map<string, std::vector<FeaturePtr> >& composedFeatures) {
			std::vector<FeaturePtr> features;

			features.push_back(catalog.getFeature(format(component0, mode)));
			assert_true(features.back()) << "Feature 1 of 2 is invalid";
			features.push_back(catalog.getFeature(format(component1, mode)));
			assert_true(features.back()) << "Feature 2 of 2 is invalid";

			composedFeatures[format(name, mode)] = features;
		}

		void addTernaryComposedFeature(const char* name, const char* component0, const char* component1, const char* component2, const char* mode,
				FeatureCatalog& catalog, std::map<string, std::vector<FeaturePtr> >& composedFeatures) {
			std::vector<FeaturePtr> features;

			features.push_back(catalog.getFeature(format(component0, mode)));
			assert_true(features.back()) && "Feature 1 of 3 is invalid";
			features.push_back(catalog.getFeature(format(component1, mode)));
			assert_true(features.back()) && "Feature 2 of 3 is invalid";
			features.push_back(catalog.getFeature(format(component2, mode)));
			assert_true(features.back()) && "Feature 3 of 3 is invalid";

			composedFeatures[format(name, mode)] = features;
		}


		void addQuaternaryComposedFeature(const char* name, const char* component0, const char* component1, const char* component2, const char* component3,
				const char* mode, FeatureCatalog& catalog, std::map<string, std::vector<FeaturePtr> >& composedFeatures) {
			std::vector<FeaturePtr> features;

			features.push_back(catalog.getFeature(format(component0, mode)));
			assert_true(features.back()) << "Feature 1 of 4 is invalid";
			features.push_back(catalog.getFeature(format(component1, mode)));
			assert_true(features.back()) << "Feature 2 of 4 is invalid";
			features.push_back(catalog.getFeature(format(component2, mode)));
			assert_true(features.back()) << "Feature 3 of 4 is invalid";
			features.push_back(catalog.getFeature(format(component3, mode)));
			assert_true(features.back()) << "Feature 4 of 4 is invalid";

			composedFeatures[format(name, mode)] = features;
		}

		// add features that are composed of other features by a composing function
		void addComposedFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {

			// modes
			std::map<string, FeatureAggregationMode> modes;
			modes["static"] = FA_Static;
			modes["weighted"] = FA_Weighted;
			modes["real"] = FA_Real;
			modes["polyhedral"] = FA_Polyhedral;

			std::map<string, std::vector<FeaturePtr> > composedFeatures;
			for_each(modes, [&](const std::pair<string, FeatureAggregationMode>& cur_mode) {

				addBinaryComposedFeature("any_read/write_OPs_Calls_%s", "SCF_IO_NUM_any_read/write_OPs_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("real*_all_VEC_OPs_Calls_%s", "SCF_NUM_real*_all_VEC_OPs_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("externalFunction_Calls_%s", "SCF_NUM_externalFunction_lambda_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("integer_all_OPs_Calls_%s", "SCF_NUM_integer_all_OPs_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("integer_all_VEC_OPs_Calls_%s", "SCF_NUM_integer_all_VEC_OPs_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("real*_all_OPs_Calls_%s", "SCF_NUM_real*_all_OPs_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("loops_Calls_%s", "SCF_NUM_loops_lambda_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("branches_Calls_%s", "SCF_NUM_branches_lambda_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("barrier_Calls_%s", "SCF_NUM_barrier_Calls_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addTernaryComposedFeature("scalarOPs-vectorOPs_Calls_%s", "SCF_NUM_any_all_OPs_%s", "SCF_NUM_any_all_VEC_OPs_%s", "SCF_NUM_calls_lambda_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);


				addBinaryComposedFeature("scalarOPs-memoryAccess_%s", "SCF_NUM_any_all_OPs_%s", "SCF_IO_NUM_any_read/write_OPs_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("vectorOPs-memoryAccess_%s", "SCF_NUM_any_all_VEC_OPs_%s", "SCF_IO_NUM_any_read/write_OPs_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("scalarOPs-vectorOPs_%s", "SCF_NUM_any_all_OPs_%s", "SCF_NUM_any_all_VEC_OPs_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addTernaryComposedFeature("any_any_OPs_%s", "SCF_NUM_any_all_OPs_%s", "SCF_NUM_any_all_VEC_OPs_%s",
						"SCF_NUM_externalFunction_lambda_%s",	cur_mode.first.c_str(), catalog, composedFeatures);

				addQuaternaryComposedFeature("instructions_%s", "SCF_NUM_any_all_OPs_%s", "SCF_NUM_any_all_VEC_OPs_%s",
						"SCF_NUM_externalFunction_lambda_%s", "SCF_IO_NUM_any_read/write_OPs_%s", cur_mode.first.c_str(), catalog, composedFeatures);

				addBinaryComposedFeature("localMemoryAccesses-allMemoryAccesses_%s", "SCF_NUM_localMemoryAccess_calls_%s", "SCF_IO_NUM_any_read/write_OPs_%s",
						cur_mode.first.c_str(), catalog, composedFeatures);

				addTernaryComposedFeature("allOPs-memoryAccesses_%s", "SCF_NUM_any_all_OPs_%s", "SCF_NUM_any_all_VEC_OPs_%s",
						"SCF_IO_NUM_any_read/write_OPs_%s",	cur_mode.first.c_str(), catalog, composedFeatures);
			});

			std::map<string, ComposedFeature::composingFctTy > binaryComposingFunctions, ternaryComposingFunctions, quaternaryComposingFunctions;
			binaryComposingFunctions["sum"] = GEN_COMPOSING_FCT(
					return (component(0) + component(1));
			);
			binaryComposingFunctions["difference"] = GEN_COMPOSING_FCT(
					return (component(0) - component(1));
			);
			binaryComposingFunctions["product"] = GEN_COMPOSING_FCT(
					return (component(0) * component(1));
			);
			binaryComposingFunctions["ratio"] = GEN_COMPOSING_FCT(
					return (component(0) / component(1));
			);

			ternaryComposingFunctions["2:1ratio"] = GEN_COMPOSING_FCT(
					return ((component(0) + component(1)) / component(2));
			);
			ternaryComposingFunctions["1:2ratio"] = GEN_COMPOSING_FCT(
					return (component(0) / (component(1) + component(2)));
			);
			ternaryComposingFunctions["sum"] = GEN_COMPOSING_FCT(
					return (component(0) + component(1) + component(2));
			);
			ternaryComposingFunctions["product"] = GEN_COMPOSING_FCT(
					return (component(0) * component(1) * component(2));
			);

			quaternaryComposingFunctions["sum"] = GEN_COMPOSING_FCT(
					return (component(0) + component(1) + component(2) + component(3));
			);
			quaternaryComposingFunctions["product"] = GEN_COMPOSING_FCT(
					return (component(0) * component(1) * component(2) * component(3));
			);

			// create the actual features
			for_each(composedFeatures, [&](const std::pair<string, std::vector<FeaturePtr> > & cur_features){
				if(cur_features.second.size() == 2) {
					for_each(binaryComposingFunctions, [&](const std::pair<string, ComposedFeature::composingFctTy >& cur_fct) {

						string name = format("SCF_COMP_%s_%s",
								cur_features.first.c_str(), cur_fct.first.c_str());

						string desc = format("Counts the %s of %s",
								cur_fct.first.c_str(), cur_features.first.c_str());

						catalog.addFeature(createComposedFeature(name, desc, cur_fct.second, cur_features.second));
					});
				} else if(cur_features.second.size() == 3) {
					for_each(ternaryComposingFunctions, [&](const std::pair<string, ComposedFeature::composingFctTy >& cur_fct) {

						string name = format("SCF_COMP_%s_%s",
								cur_features.first.c_str(), cur_fct.first.c_str());

						string desc = format("Counts the %s of %s",
								cur_fct.first.c_str(), cur_features.first.c_str());

						catalog.addFeature(createComposedFeature(name, desc, cur_fct.second, cur_features.second));
					});
				} else if(cur_features.second.size() == 4) {
					for_each(quaternaryComposingFunctions, [&](const std::pair<string, ComposedFeature::composingFctTy >& cur_fct) {

						string name = format("SCF_COMP_%s_%s",
								cur_features.first.c_str(), cur_fct.first.c_str());

						string desc = format("Counts the %s of %s",
								cur_fct.first.c_str(), cur_features.first.c_str());

						catalog.addFeature(createComposedFeature(name, desc, cur_fct.second, cur_features.second));
					});
				} else {
					LOG(ERROR) << "Invalid number of features: " <<  cur_features.second.size() << std::endl;
					assert_le(cur_features.second.size(), 4) << "Too many components passed";
					assert_ge(cur_features.second.size(), 2) << "Too few components passed";
				}
			});
		}

		FeatureCatalog initCatalog() {
			// the node manager managing nodes inside the catalog
			static core::NodeManager manager;
			auto& basic = manager.getLangBasic();

			FeatureCatalog catalog;

			addScalarFeatures(basic, catalog);
			addVectorFeatures(basic, catalog);
			addMemoryAccessFeatures(basic, catalog);
			addParallelFeatures(basic, catalog);
			addPatternFeatures(basic, catalog);
			addLambdaFeatures(basic, catalog);
			addComposedFeatures(basic, catalog);

			return catalog;
		}

	}

	const FeatureCatalog& getFullCodeFeatureCatalog() {
		const static FeatureCatalog catalog = initCatalog();
		return catalog;
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme

// ./genDB oclKernel -f SCF_NUM_globalMemoryAccess_calls_static -f SCF_NUM_externalFunction_lambda_static -f SCF_NUM_allMemoryAccesses-localMemoryAccesses_real_ratio -c
