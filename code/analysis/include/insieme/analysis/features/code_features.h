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

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace features {

	enum SimpleFeature {

		FT_NUM_INT_ARITHMETIC_OPs,
		FT_NUM_INT_COMPARISON_OPs,
		FT_NUM_INT_BITWISE_OPs,

		FT_NUM_UINT_ARITHMETIC_OPs,
		FT_NUM_UINT_COMPARISON_OPs,
		FT_NUM_UINT_BITWISE_OPs,

		FT_NUM_FLOAT_ARITHMETIC_OPs,
		FT_NUM_FLOAT_COMPARISON_OPs,

		FT_NUM_DOUBLE_ARITHMETIC_OPs,
		FT_NUM_DOUBLE_COMPARISON_OPs,

//		FT_NUM_INT_VEC_ARITHMETIC_OPs,
//		FT_NUM_INT_VEC_COMPARISON_OPs,
//		FT_NUM_INT_VEC_BITWISE_OPs,
//
//		FT_NUM_UINT_VEC_ARITHMETIC_OPs,
//		FT_NUM_UINT_VEC_COMPARISON_OPs,
//		FT_NUM_UINT_VEC_BITWISE_OPs,
//
//		FT_NUM_FLOAT_VEC_ARITHMETIC_OPs,
//		FT_NUM_FLOAT_VEC_COMPARISON_OPs,
//
//		FT_NUM_DOUBLE_VEC_ARITHMETIC_OPs,
//		FT_NUM_DOUBLE_VEC_COMPARISON_OPs,
//
//		FT_NUM_FLOAT_OPs,
//		FT_NUM_FLOAT_VEC_OPs,
//		FT_NUM_BITWISE_OPs,
//		FT_NUM_BRANCHEs,
//		FT_NUM_UNCONDITIONAL_BRANCHEs,
//		FT_NUM_SWITCH_STMTs,
//		FT_NUM_READ_OPs,
//		FT_NUM_WRITE_OPs,
//		FT_NUM_SCALAR_READ_OPs,
//		FT_NUM_SCALAR_WRITE_OPs,
//		FT_NUM_ARRAY_READ_OPs,
//		FT_NUM_ARRAY_WRITE_OPs,
//		FT_NUM_POINTER_CHASING_OPs,
//		FT_NUM_CALLs,
//		FT_NUM_FUNCTION_CALLs,
//		FT_NUM_FUNCTION_POINTER_CALLs,
//		FT_NUM_CLOSURE_CALLs,
//		FT_NUM_EXTERNAL_FUNCTION_CALLs,
//		FT_NUM_PARALLELs,
//		FT_NUM_PFORs,
//		FT_NUM_BARRIERs

	};


	enum FeatureAggregationMode {
		FA_Static,			/* < Features are statically extracted, not considering any repetitions. */
		FA_Weighted,		/* < Features are extracted by weighting code inside loops / recursions / branches. */
		FA_Real,			/* < Features are extracted as within the weighted variant, yet considering actual loop boundaries. */
		FA_Polyhedral		/* < Features are extracted and weighted according to the cardinality within the PM. */
	};



	struct SimpleCodeFeatureSpec : public core::IRVisitor<unsigned> {
		const FeatureAggregationMode mode;
		SimpleCodeFeatureSpec(FeatureAggregationMode mode) : IRVisitor<unsigned>(false), mode(mode) {}
		virtual ~SimpleCodeFeatureSpec() {};
	};

	typedef std::shared_ptr<SimpleCodeFeatureSpec> SimpleCodeFeatureSpecPtr;

	SimpleCodeFeatureSpecPtr countCalls(const core::ExpressionPtr& op, FeatureAggregationMode mode = FA_Weighted);
	SimpleCodeFeatureSpecPtr countCalls(const core::TypePtr& type, const core::ExpressionPtr& op, FeatureAggregationMode mode = FA_Weighted);

	typedef std::function<bool(const core::TypePtr&)> TypeFilter;
	typedef std::function<bool(const core::ExpressionPtr&)> OperationFilter;

	SimpleCodeFeatureSpecPtr countCalls(const TypeFilter& typeFilter, const OperationFilter& opFilter, FeatureAggregationMode mode = FA_Weighted);




	// just for experimenting
	unsigned countOps(const core::NodePtr& root, const core::LiteralPtr& op, FeatureAggregationMode mode = FA_Weighted);


	// -- some basic features --

	// a generic implementation extracting all kind of simple features
	unsigned evalFeature(const core::NodePtr& root, SimpleFeature feature, FeatureAggregationMode mode = FA_Weighted);


	struct FeatureValues : public vector<unsigned> {

		FeatureValues operator+(const FeatureValues& other) const;
		FeatureValues operator*(double factor) const;

		FeatureValues& operator+=(const FeatureValues& other);
		FeatureValues& operator*=(double factor);
	};


	FeatureValues evalFeatures(const core::NodePtr& root, const vector<SimpleFeature>& features, FeatureAggregationMode mode = FA_Weighted);

	// -- a generic feature counting individual operators --

	struct OperatorStatistic : public utils::map::PointerMap<core::LiteralPtr, unsigned> {

		OperatorStatistic operator+(const OperatorStatistic& other) const;
		OperatorStatistic operator*(double factor) const;

		OperatorStatistic& operator+=(const OperatorStatistic& other);
		OperatorStatistic& operator*=(double factor);

	};

	OperatorStatistic getOpStats(const core::NodePtr& root, FeatureAggregationMode mode = FA_Weighted);

//	int countIntOps(const core::NodePtr& root, FeatureAggregationMode mode = FA_Weighted);
//	int countIntOps(const core::NodePtr& root, FeatureAggregationMode mode = FA_Weighted);

} // end namespace features
} // end namespace analysis
} // end namespace insieme
