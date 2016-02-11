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

#pragma once

#include "insieme/core/forward_decls.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/backend/opencl/opencl_extension.h"

namespace insieme {
namespace backend {
namespace opencl {
	using namespace insieme::core;

	class DataRange;
	typedef std::shared_ptr<DataRange> DataRangePtr;
	typedef std::vector<DataRangePtr> DataRangeList;
	
	class DataRange {
		ExpressionPtr size;
		ExpressionPtr start;
		ExpressionPtr end;
	public:
		DataRange(const ExpressionPtr& size, const ExpressionPtr& start, const ExpressionPtr& end) :
			size(size), start(start), end(end)
		{ }
		const ExpressionPtr& getSize() const { return size; }
		const ExpressionPtr& getStart() const { return start; }
		const ExpressionPtr& getEnd() const { return end; }
		
		bool operator==(const DataRange& other) const {
			return size == other.size && start == other.start && end == other.end;
		}
		
		static DataRangePtr get(NodeManager& manager, const ExpressionPtr& size, const ExpressionPtr& start, const ExpressionPtr& end) {
			return std::make_shared<DataRange>(size, start, end);
		}
		
		static DataRangePtr decode(const ExpressionPtr& expr) {
			return std::make_shared<DataRange>(encoder::toValue<DataRange>(expr));
		}

		static ExpressionPtr encode(NodeManager& manager, const DataRangePtr& value) {
			return encoder::toIR<DataRange>(manager, *value);
		}
	};
	
	class DataRequirement;
	typedef std::shared_ptr<DataRequirement> DataRequirementPtr;
	typedef std::vector<DataRequirementPtr> DataRequirementList;
	
	class DataRequirement {
	public:
		enum AccessMode { RO = 0, WO = 1, RW = 2 };

		AccessMode getAccessMode() const { return accessMode; }
		void setAccessMode(AccessMode mode) { accessMode = mode; }
		const TypePtr& getType() const { return typePtr; }
		void setType(const TypePtr& type) { typePtr = type; }
		const ExpressionPtr& getNumRanges() const { return numRanges; }
		void setNumRanges(const ExpressionPtr& expr) { numRanges = expr; }
		const LambdaExprPtr& getRangeExpr() const { return rangeExpr; }
		void setRangeExpr(const LambdaExprPtr& expr) { rangeExpr = expr; }
		
		bool operator==(const DataRequirement& other) const {
			return accessMode == other.accessMode && typePtr == other.typePtr &&
				   numRanges == other.numRanges && rangeExpr == other.rangeExpr;
		}
		
		static DataRequirementPtr decode(const ExpressionPtr& expr) {
			return std::make_shared<DataRequirement>(encoder::toValue<DataRequirement>(expr));
		}
		
		static ExpressionPtr encode(NodeManager& manager, const DataRequirementPtr& value) {
			return encoder::toIR<DataRequirement>(manager, *value);
		}
	private:
		AccessMode accessMode;
		TypePtr typePtr;
		ExpressionPtr numRanges;
		LambdaExprPtr rangeExpr;
	};
	
	class NDRange;
	typedef std::shared_ptr<NDRange> NDRangePtr;
	
	class NDRange {
	public:
		const ExpressionPtr& getWorkDim() const { return workDim; }
		void setWorkDim(const ExpressionPtr& workDim) { this->workDim = workDim; }
		const ExpressionList& getGlobalWorkSizes() const { return globalWorkSize; }
		void addGlobalWorkSize(const ExpressionPtr& sz) { globalWorkSize.push_back(sz); }
		const ExpressionList& getLocalWorkSizes() const { return localWorkSize; }
		void addLocalWorkSize(const ExpressionPtr& sz) { localWorkSize.push_back(sz); }
		
		bool operator==(const NDRange& other) const {
			return workDim == other.workDim && globalWorkSize == other.globalWorkSize &&
				   localWorkSize == other.localWorkSize;
		}
		
		static NDRangePtr decode(const ExpressionPtr& expr) {
			return std::make_shared<NDRange>(encoder::toValue<NDRange>(expr));
		}
		
		static ExpressionPtr encode(NodeManager& manager, const NDRangePtr& value) {
			return encoder::toIR<NDRange>(manager, *value);
		}
	private:
		ExpressionPtr workDim;
		ExpressionList globalWorkSize;
		ExpressionList localWorkSize;
	};
} // end namespace opencl
} // end namespace backend

namespace core {
namespace encoder {
	namespace opencl = insieme::backend::opencl;

	template <>
	struct type_factory<opencl::DataRange> {
		TypePtr operator()(NodeManager& manager) const {
			return manager.getLangExtension<opencl::OpenCLExtension>().getDataRange();
		}
	};

	template <>
	struct value_to_ir_converter<opencl::DataRange> {
		ExpressionPtr operator()(NodeManager& manager, const opencl::DataRange& value) const {
			IRBuilder builder(manager);
			auto& oclExt = manager.getLangExtension<opencl::OpenCLExtension>();
			return builder.callExpr(oclExt.getDataRange(), oclExt.getMakeDataRange(),
									toVector(value.getSize(), value.getStart(), value.getEnd()));
		}
	};

	template <>
	struct ir_to_value_converter<opencl::DataRange> {
		opencl::DataRange operator()(const core::ExpressionPtr& expr) const {
			// paranoia check
			if (!isEncodingOf<opencl::DataRange>(expr)) throw InvalidExpression(expr);
			// construct the new object from the expr's arguments			
			return opencl::DataRange(analysis::getArgument(expr, 0), analysis::getArgument(expr, 1),
									 analysis::getArgument(expr, 2));
		}
	};

	template <>
	struct is_encoding_of<opencl::DataRange> {
		bool operator()(const core::ExpressionPtr& expr) const {
			auto& oclExt = expr->getNodeManager().getLangExtension<opencl::OpenCLExtension>();
			return analysis::isCallOf(expr, oclExt.getMakeDataRange());
		}
	};
	
	template <>
	struct type_factory<opencl::DataRequirement> {
		TypePtr operator()(NodeManager& manager) const {
			return manager.getLangExtension<opencl::OpenCLExtension>().getDataRequirement();
		}
	};

	template <>
	struct value_to_ir_converter<opencl::DataRequirement> {
		ExpressionPtr operator()(NodeManager& manager, const opencl::DataRequirement& value) const {
			IRBuilder builder(manager);
			auto& oclExt = manager.getLangExtension<opencl::OpenCLExtension>();
			
			LiteralPtr accessMode;
			switch(value.getAccessMode()) {
			case opencl::DataRequirement::AccessMode::RO:	accessMode = builder.uintLit(0); break;
			case opencl::DataRequirement::AccessMode::WO:	accessMode = builder.uintLit(1); break;
			case opencl::DataRequirement::AccessMode::RW:	accessMode = builder.uintLit(2); break;
			}
			// use the default IRBuilder to generate the callExpr
			return builder.callExpr(oclExt.getDataRequirement(), oclExt.getMakeDataRequirement(),
									builder.getTypeLiteral(value.getType()), value.getNumRanges(),
									value.getRangeExpr(), accessMode);
		}
	};

	template <>
	struct ir_to_value_converter<opencl::DataRequirement> {
		opencl::DataRequirement operator()(const core::ExpressionPtr& expr) const {
			// paranoia check
			if (!isEncodingOf<opencl::DataRequirement>(expr)) throw InvalidExpression(expr);
			// prepare an empty requirement such that we can fill it up
			opencl::DataRequirement requirement;

			// extract the enclosed type
			requirement.setType(analysis::getArgument(expr, 0)->getType().as<GenericTypePtr>()->getTypeParameter(0));
			// extract the number of ranges
			requirement.setNumRanges(analysis::getArgument(expr, 1));
			// as well as the range expression itself
			requirement.setRangeExpr(analysis::getArgument(expr, 2).as<LambdaExprPtr>());
			
			unsigned int accessMode = toValue<unsigned int>(analysis::getArgument(expr, 3));
			if 		(accessMode == 0) requirement.setAccessMode(opencl::DataRequirement::AccessMode::RO);
			else if (accessMode == 1) requirement.setAccessMode(opencl::DataRequirement::AccessMode::WO);
			else					  requirement.setAccessMode(opencl::DataRequirement::AccessMode::RW);
			// DataRequirement is done now and can be returend to user-code
			return requirement;
		}
	};

	template <>
	struct is_encoding_of<opencl::DataRequirement> {
		bool operator()(const core::ExpressionPtr& expr) const {
			auto& oclExt = expr->getNodeManager().getLangExtension<opencl::OpenCLExtension>();
			return analysis::isCallOf(expr, oclExt.getMakeDataRequirement());
		}
	};
	
	template <>
	struct type_factory<opencl::NDRange> {
		TypePtr operator()(NodeManager& manager) const {
			return manager.getLangExtension<opencl::OpenCLExtension>().getNDRange();
		}
	};
	
	template <>
	struct value_to_ir_converter<opencl::NDRange> {
		ExpressionPtr operator()(NodeManager& manager, const opencl::NDRange& value) const {
			IRBuilder builder(manager);
			auto& oclExt = manager.getLangExtension<opencl::OpenCLExtension>();
			// use the default IRBuilder to generate the callExpr
			return builder.callExpr(oclExt.getNDRange(), oclExt.getMakeNDRange(),
									value.getWorkDim(),
									toIR<ExpressionList, DirectExprListConverter>(manager, value.getGlobalWorkSizes()),
									toIR<ExpressionList, DirectExprListConverter>(manager, value.getLocalWorkSizes()));
		}
	};

	template <>
	struct ir_to_value_converter<opencl::NDRange> {
		opencl::NDRange operator()(const core::ExpressionPtr& expr) const {
			// paranoia check
			if (!isEncodingOf<opencl::NDRange>(expr)) throw InvalidExpression(expr);
			// prepare an empty requirement such that we can fill it up
			opencl::NDRange ndrange;

			// extract the enclosed workDim
			ndrange.setWorkDim(analysis::getArgument(expr, 0));

			ExpressionList globalWorkSizes = toValue<ExpressionList, DirectExprListConverter>(analysis::getArgument(expr, 1));
			for (const auto& sz: globalWorkSizes) ndrange.addGlobalWorkSize(sz);
			
			ExpressionList localWorkSizes = toValue<ExpressionList, DirectExprListConverter>(analysis::getArgument(expr, 2));
			for (const auto& sz: localWorkSizes) ndrange.addLocalWorkSize(sz);
			// ndrange is ready to return it to the user who requested the conversion
			return ndrange;
		}
	};
	
	template <>
	struct is_encoding_of<opencl::NDRange> {
		bool operator()(const core::ExpressionPtr& expr) const {
			auto& oclExt = expr->getNodeManager().getLangExtension<opencl::OpenCLExtension>();
			return analysis::isCallOf(expr, oclExt.getMakeNDRange());
		}
	};
} // end namespace core
} // end namespace encoder
} // end namespace insieme
