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

#include "insieme/analysis/features/feature.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {

// forward declarations
namespace transform {
namespace pattern {
	class TreePattern;
	typedef std::shared_ptr<TreePattern> TreePatternPtr;
}
}

namespace analysis {
namespace features {

	// the type used for representing simple code feature values.
	typedef double simple_feature_value_type;

	enum FeatureAggregationMode {
		FA_Static,			/* < Features are statically extracted, not considering any repetitions. */
		FA_Weighted,		/* < Features are extracted by weighting code inside loops / recursions / branches. */
		FA_Real,			/* < Features are extracted as within the weighted variant, yet considering actual loop boundaries. */
		FA_Polyhedral		/* < Features are extracted and weighted according to the cardinality within the PM. */
	};


	// just for experimenting
	simple_feature_value_type countOps(const core::NodePtr& root, const core::LiteralPtr& op, FeatureAggregationMode mode = FA_Weighted);

	class CodeFeatureSpec {
	protected:
		typedef std::function<simple_feature_value_type(core::NodePtr)> extractor_function;

		extractor_function extractor;
		FeatureAggregationMode mode;

	public:
		CodeFeatureSpec(const extractor_function& extractor, FeatureAggregationMode mode) : extractor(extractor), mode(mode) {}
		CodeFeatureSpec(FeatureAggregationMode mode) : mode(mode) {}

		simple_feature_value_type extract(const core::NodePtr& node) const {
			return extractor(node);
		}

		FeatureAggregationMode getMode() const {
			return mode;
		}

	};

	// a generic implementation extracting all kind of simple features
	simple_feature_value_type evalFeature(const core::NodePtr& root, const CodeFeatureSpec& feature);

	// -- utilities for simple code features --

	class SimpleCodeFeatureSpec : public CodeFeatureSpec {
		typedef std::function<simple_feature_value_type(core::NodePtr)> extractor_function;

//		extractor_function extractor;


	public:

		SimpleCodeFeatureSpec(const core::ExpressionPtr& op, FeatureAggregationMode mode = FA_Weighted);

		SimpleCodeFeatureSpec(const vector<core::ExpressionPtr>& ops, FeatureAggregationMode mode = FA_Weighted);

		SimpleCodeFeatureSpec(const core::TypePtr& type, const core::ExpressionPtr& op, FeatureAggregationMode mode = FA_Weighted);

		SimpleCodeFeatureSpec(const vector<core::TypePtr>& types, const vector<core::ExpressionPtr>& ops, FeatureAggregationMode mode = FA_Weighted);

		SimpleCodeFeatureSpec(const extractor_function& extractor, FeatureAggregationMode mode = FA_Weighted) : CodeFeatureSpec(extractor, mode) {}

	};

	/**
	 * The bridge between simple code features and the general feature framework. The
	 * SimpleCodeFeature class is extending the abstract Feature base class and allows
	 * simple features to be used within any feature-related context.
	 */
	class SimpleCodeFeature : public Feature {

		const SimpleCodeFeatureSpec spec;

	public:

		SimpleCodeFeature(const string& name, const string& desc, const SimpleCodeFeatureSpec& spec)
			: Feature(true, name, desc, atom<simple_feature_value_type>(desc)), spec(spec) {}

		const SimpleCodeFeatureSpec& getSpec() const {
			return spec;
		}

	protected:

		virtual Value evaluateFor(const core::NodePtr& code) const {
			return evalFeature(code, spec);
		}
	};

	typedef std::shared_ptr<SimpleCodeFeature> SimpleCodeFeaturePtr;


	SimpleCodeFeaturePtr createSimpleCodeFeature(const string& name, const string& desc, const SimpleCodeFeatureSpec& spec);


	SimpleCodeFeatureSpec createVectorOpSpec(const core::ExpressionPtr& elementOp, FeatureAggregationMode mode = FA_Weighted);
	SimpleCodeFeatureSpec createVectorOpSpec(const vector<core::ExpressionPtr>& elementOps, FeatureAggregationMode mode = FA_Weighted);

	SimpleCodeFeatureSpec createVectorOpSpec(const core::ExpressionPtr& elementOp, bool considerOpWidth, FeatureAggregationMode mode = FA_Weighted);
	SimpleCodeFeatureSpec createVectorOpSpec(const vector<core::ExpressionPtr>& elementOps, bool considerOpWidth, FeatureAggregationMode mode = FA_Weighted);

	SimpleCodeFeatureSpec createVectorOpSpec(const vector<core::TypePtr>& elementTypes, const vector<core::ExpressionPtr>& elementOps, bool considerOpWidth, FeatureAggregationMode mode = FA_Weighted);

	SimpleCodeFeatureSpec createNumForLoopSpec(FeatureAggregationMode mode =FA_Static);

	// Simple Code Features covering read/write operations
	enum MemoryAccessMode {
		READ, WRITE, READ_WRITE
	};

	enum MemoryAccessTarget {
		ANY, SCALAR, VECTOR, ARRAY
	};

	SimpleCodeFeatureSpec createMemoryAccessSpec(MemoryAccessMode mode, MemoryAccessTarget target, FeatureAggregationMode aggregation);

	// -- combined feature evaluation --

	struct FeatureValues : public vector<simple_feature_value_type> {

		FeatureValues() {}
		FeatureValues(unsigned size) : vector<simple_feature_value_type>(size) {}

		FeatureValues operator+(const FeatureValues& other) const;
		FeatureValues operator*(double factor) const;

		FeatureValues& operator+=(const FeatureValues& other);
		FeatureValues& operator*=(double factor);
	};

	FeatureValues evalFeatures(const core::NodePtr& root, const vector<SimpleCodeFeatureSpec>& features);
	FeatureValues evalFeatures(const core::NodePtr& root, const vector<const SimpleCodeFeatureSpec*>& features);

	// -- a generic feature counting individual operators --

	struct OperatorStatistic : public utils::map::PointerMap<core::LiteralPtr, unsigned> {

		OperatorStatistic operator+(const OperatorStatistic& other) const;
		OperatorStatistic operator*(double factor) const;

		OperatorStatistic& operator+=(const OperatorStatistic& other);
		OperatorStatistic& operator*=(double factor);

	};

	OperatorStatistic getOpStats(const core::NodePtr& root, FeatureAggregationMode mode = FA_Weighted);


	// -- utilities for pattern code features --

	/*
	 * Counts how often the given pattern occurs in the code.
	 */
	//FIXME currently only looks for callExpr to match for performance reasons
	class PatternCodeFeatureSpec : public CodeFeatureSpec {

		typedef std::function<simple_feature_value_type(core::NodePtr)> extractor_function;

	public:

		PatternCodeFeatureSpec(const transform::pattern::TreePatternPtr& pattern, FeatureAggregationMode mode = FA_Weighted);

		PatternCodeFeatureSpec(const extractor_function& extractor, FeatureAggregationMode mode = FA_Weighted) : CodeFeatureSpec(extractor, mode) {}
	};

	/**
	 * The bridge between simple pattern features and the general feature framework. The
	 * PatternCodeFeature class is extending the abstract Feature base class and allows
	 * features based on patterns to be used within any feature-related context.
	 */
	class PatternCodeFeature : public Feature {

		const PatternCodeFeatureSpec spec;

	public:

		PatternCodeFeature(const string& name, const string& desc, const PatternCodeFeatureSpec& spec)
			: Feature(true, name, desc, atom<simple_feature_value_type>(desc)), spec(spec) {}

		const PatternCodeFeatureSpec& getSpec() const {
			return spec;
		}

	protected:

		virtual Value evaluateFor(const core::NodePtr& code) const {
			return evalFeature(code, spec);
		}
	};

	typedef std::shared_ptr<PatternCodeFeature> PatternCodeFeaturePtr;

	PatternCodeFeaturePtr createPatternCodeFeature(const string& name, const string& desc, const PatternCodeFeatureSpec& spec);

	// -- utilities for lambda code features --

	/*
	 * Counts on how many nodes the given lambda evaluates too true.
	 */
	class LambdaCodeFeatureSpec : public CodeFeatureSpec {

		typedef std::function<simple_feature_value_type(core::NodePtr)> extractor_function;

	public:

		LambdaCodeFeatureSpec(const extractor_function& extractor, FeatureAggregationMode mode = FA_Weighted) : CodeFeatureSpec(extractor, mode) {}

	};

	/**
	 * The bridge between simple lambda based features and the general feature framework. The
	 * LambdaCodeFeature class is extending the abstract Feature base class and allows
	 * features based on lambdas to be used within any feature-related context.
	 */
	class LambdaCodeFeature : public Feature {

		const LambdaCodeFeatureSpec spec;

	public:

		LambdaCodeFeature(const string& name, const string& desc, const std::function<simple_feature_value_type(core::NodePtr)>& lambda,
				FeatureAggregationMode mode = FA_Weighted)
			: Feature(true, name, desc, atom<simple_feature_value_type>(desc)), spec(lambda, mode) {}

		LambdaCodeFeature(const string& name, const string& desc, const LambdaCodeFeatureSpec& spec)
			: Feature(true, name, desc, atom<simple_feature_value_type>(desc)), spec(spec) {}

		const LambdaCodeFeatureSpec& getSpec() const {
			return spec;
		}

	protected:

		virtual Value evaluateFor(const core::NodePtr& code) const {
			return evalFeature(code, spec);
		}
	};

	typedef std::shared_ptr<LambdaCodeFeature> LambdaCodeFeaturePtr;

	LambdaCodeFeaturePtr createLambdaCodeFeature(const string& name, const string& desc, const LambdaCodeFeatureSpec& spec);


	// -- utilities for composed features --
	class ComposedFeatureSpec {

	};

	class ComposedFeature : public Feature {
	public:
		typedef std::function<Value(core::NodePtr, const std::vector<FeaturePtr>& components, std::function<Value(size_t) > component)> composingFctTy;
	private:
		ComposedFeatureSpec spec;

		std::function<Value(core::NodePtr, const std::vector<FeaturePtr>& components, std::function<Value(size_t) > component)> composingFct;

		std::vector<FeaturePtr> components;

	public:

		ComposedFeature(const string& name, const string& desc, const composingFctTy composingFct,
				FeaturePtr component0, FeaturePtr component1);
		ComposedFeature(const string& name, const string& desc, const composingFctTy composingFct,
				 FeaturePtr component0, FeaturePtr component1, FeaturePtr component2);
		ComposedFeature(const string& name, const string& desc, const composingFctTy composingFct,
				 FeaturePtr component0, FeaturePtr component1, FeaturePtr component2, FeaturePtr component3);
		ComposedFeature(const string& name, const string& desc, const composingFctTy composingFct,
				const std::vector<FeaturePtr>& components)
			: Feature(false, name, desc, atom<simple_feature_value_type>(desc)), composingFct(composingFct), components(components) {
			assert(components.size() > 0 && "Composed features cannot have no composing features");
		}

	protected:

		virtual Value evaluateFor(const core::NodePtr& code) const {
			const auto componentAccess = [&](size_t idx) -> Value { return components.at(idx)->extractFrom(code); } ;

			return Value();//composingFct(code, components, componentAccess);
		};

	};

	// helper to access composed feature components
//	#define cf_component(idx) this->components.at(idx)->extractFrom(code)

 	#define GEN_COMPOSING_FCT(BODY) [&](const core::NodePtr code, const std::vector<FeaturePtr>& components, std::function<Value(size_t) > component) \
		->Value { BODY }

	typedef std::shared_ptr<ComposedFeature> ComposedFeaturePtr;

	ComposedFeaturePtr createComposedFeature(const string& name, const string& desc, const ComposedFeature::composingFctTy composingFct,
			const std::vector<FeaturePtr>& components);

} // end namespace features
} // end namespace analysis
} // end namespace insieme
