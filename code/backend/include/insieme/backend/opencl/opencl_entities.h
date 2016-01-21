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
		ExpressionPtr start;
		ExpressionPtr end;
	public:
		DataRange(const ExpressionPtr& start, const ExpressionPtr& end) :
			start(start), end(end)
		{ }
		const ExpressionPtr& getStart() const { return start; }
		const ExpressionPtr& getEnd() const { return end; }
		
		bool operator==(const DataRange& other) const {
			return start == other.start && end == other.end;
		}
		
		static DataRangePtr get(NodeManager& manager, unsigned int start, unsigned int end) { 
			IRBuilder builder(manager);
			return std::make_shared<DataRange>(builder.uintLit(start), builder.uintLit(end));
		}
		
		static DataRangePtr decode(const ExpressionPtr& expr) {
			assert_false(encoder::isEncodingOf<DataRange>(expr)) << "Not an encoding of a matching value!";
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
		const std::vector<unsigned int>& getDims() const { return dim; }
		void addDim(unsigned int range) { dim.push_back(range); }
		const DataRangeList& getRanges() const { return ranges; }
		void addRange(const DataRangePtr& range) { ranges.push_back(range); }
		
		bool operator==(const DataRequirement& other) const {
			return accessMode == other.accessMode && typePtr == other.typePtr &&
				   dim == other.dim && ranges == other.ranges;
		}
		
		static DataRequirementPtr decode(const ExpressionPtr& expr) {
			assert_false(encoder::isEncodingOf<DataRequirement>(expr)) << "Not an encoding of a matching value!";
			return std::make_shared<DataRequirement>(encoder::toValue<DataRequirement>(expr));
		}
		
		static ExpressionPtr encode(NodeManager& manager, const DataRequirementPtr& value) {
			return encoder::toIR<DataRequirement>(manager, *value);
		}
	private:
		AccessMode accessMode;
		TypePtr typePtr;
		std::vector<unsigned int> dim;
		DataRangeList ranges;
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
									toVector(value.getStart(), value.getEnd()));
		}
	};

	template <>
	struct ir_to_value_converter<opencl::DataRange> {
		opencl::DataRange operator()(const core::ExpressionPtr& expr) const {
			// paranoia check
			if (!isEncodingOf<opencl::DataRange>(expr)) throw InvalidExpression(expr);
			// construct the new object from the expr's arguments			
			return opencl::DataRange(analysis::getArgument(expr, 0), analysis::getArgument(expr, 1));
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
			
			ExpressionList dims;
			for (unsigned int dim: value.getDims()) dims.push_back(builder.uintLit(dim));
			
			ExpressionList ranges;
			for (const auto& range: value.getRanges()) ranges.push_back(opencl::DataRange::encode(manager, range));
			
			LiteralPtr accessMode;
			switch(value.getAccessMode()) {
			case opencl::DataRequirement::AccessMode::RO:	accessMode = builder.uintLit(0); break;
			case opencl::DataRequirement::AccessMode::WO:	accessMode = builder.uintLit(1); break;
			case opencl::DataRequirement::AccessMode::RW:	accessMode = builder.uintLit(2); break;
			}
			// use the default IRBuilder to generate the callExpr
			return builder.callExpr(oclExt.getDataRequirement(), oclExt.getMakeDataRequirement(),
									toIR<ExpressionList, DirectExprListConverter>(manager, dims),
									toIR<ExpressionList, DirectExprListConverter>(manager, ranges),
									accessMode);
		}
	};

	template <>
	struct ir_to_value_converter<opencl::DataRequirement> {
		opencl::DataRequirement operator()(const core::ExpressionPtr& expr) const {
			// paranoia check
			if (!isEncodingOf<opencl::DataRequirement>(expr)) throw InvalidExpression(expr);
			// prepare an empty requirement such that we can fill it up
			opencl::DataRequirement requirement;

			ExpressionList dims = toValue<ExpressionList, DirectExprListConverter>(analysis::getArgument(expr, 0));
			for (const auto& dim: dims) requirement.addDim(toValue<unsigned int>(dim));
			
			ExpressionList ranges = toValue<ExpressionList, DirectExprListConverter>(analysis::getArgument(expr, 1));
			for (const auto& range: ranges) requirement.addRange(opencl::DataRange::decode(range));
			
			unsigned int accessMode = toValue<unsigned int>(analysis::getArgument(expr, 2));
			if 		(accessMode == 0) requirement.setAccessMode(opencl::DataRequirement::AccessMode::RO);
			else if (accessMode == 1) requirement.setAccessMode(opencl::DataRequirement::AccessMode::WO);
			else					  requirement.setAccessMode(opencl::DataRequirement::AccessMode::RW);
			// DataRequirement is done now (however 'Type' is omitted for now)
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
} // end namespace core
} // end namespace encoder
} // end namespace insieme
