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
	
	/**
	 * Represents a range bound to a specific dimension, where @size donates the full length
	 * of the target and (@start, @size) specify a single subrange.
	 */
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
		
		bool operator ==(const DataRange& other) const {
			return *size == *other.size && *start == *other.start && *end == *other.end;
		}

		bool operator !=(const DataRange& other) const {
			return !(*this == other);
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
	
	/**
	 * Models the data requirements of one data blob. The blob has an associated access mode
	 * along with a dedicated data range for each dimension.
	 */
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
		
		bool operator ==(const DataRequirement& other) const {
			return accessMode == other.accessMode && *typePtr == *other.typePtr &&
				   *numRanges == *other.numRanges && *rangeExpr == *other.rangeExpr;
		}

		bool operator !=(const DataRequirement& other) const {
			return !(*this == other);
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
	
	/**
	 * IR-Equivalent of the well-known OpenCL NDRange concept
	 */
	class NDRange {
	public:
		const ExpressionPtr& getWorkDim() const { return workDim; }
		void setWorkDim(const ExpressionPtr& workDim) { this->workDim = workDim; }
		const ExpressionList& getGlobalOffsets() const { return globalOffsets; }
		void setGlobalOffsets(const ExpressionList& lst) { globalOffsets = lst; }
		const ExpressionList& getGlobalWorkSizes() const { return globalWorkSize; }
		void setGlobalWorkSize(const ExpressionList& lst) { globalWorkSize = lst; }
		const ExpressionList& getLocalWorkSizes() const { return localWorkSize; }
		void setLocalWorkSize(const ExpressionList& lst) { localWorkSize = lst; }

		bool operator ==(const NDRange& other) const {
			return *workDim == *other.workDim && globalOffsets == other.globalOffsets &&
				   globalWorkSize == other.globalWorkSize && localWorkSize == other.localWorkSize;
		}

		bool operator !=(const NDRange& other) const {
			return !(*this == other);
		}

		static NDRangePtr decode(const ExpressionPtr& expr) {
			return std::make_shared<NDRange>(encoder::toValue<NDRange>(expr));
		}

		static ExpressionPtr encode(NodeManager& manager, const NDRangePtr& value) {
			return encoder::toIR<NDRange>(manager, *value);
		}
	private:
		ExpressionPtr workDim;
		ExpressionList globalOffsets;
		ExpressionList globalWorkSize;
		ExpressionList localWorkSize;
	};

	class Optional;
	typedef std::shared_ptr<Optional> OptionalPtr;
	typedef std::vector<OptionalPtr> OptionalList;

	/**
	 * An Optional argument is a triple (sizeof(type_lit<'a>), value, modifier) where
	 * 1. 'a       is the type of the passed in value
	 * 2. value    is the actual expression which is expressed by this optional
	 * 3. modifier influences runtime interpretation
	 */
	class Optional {
	public:
		enum Modifier {
		  HOST_PRIMITIVE = 0, ///< value is of primitive type and shall be forwared to the kernel
		  KRNL_BUFFER    = 1  ///< value is the size of the __local buffer which shall be allocated
		};

		Modifier getModifier() const { return modifier; }
		void setModifier(Modifier modifier) { this->modifier = modifier; }
		const ExpressionPtr& getValue() const { return value; }
		void setValue(const ExpressionPtr& value) { this->value = value; }
		const ExpressionPtr& getSize() const { return size; }
		void setSize(const ExpressionPtr& size) {
			assert_true(core::analysis::isCallOf(size, size->getNodeManager().getLangBasic().getSizeof()))
				<< "size of an optional must be modeled as sizeof(type_lit<'a>)";
			this->size = size;
		}

		bool operator ==(const Optional& other) const {
			return *size == *other.size && *value == *other.value && modifier == other.modifier;
		}

		bool operator !=(const Optional& other) const {
			return !(*this == other);
		}

		static OptionalPtr decode(const ExpressionPtr& expr) {
			return std::make_shared<Optional>(encoder::toValue<Optional>(expr));
		}

		static ExpressionPtr encode(NodeManager& manager, const OptionalPtr& value) {
			return encoder::toIR<Optional>(manager, *value);
		}
	private:
		ExpressionPtr size;
		ExpressionPtr value;
		Modifier modifier;
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
			return builder.callExpr(oclExt.getNDRange(), oclExt.getMakeNDRange(), value.getWorkDim(),
									toIR<ExpressionList, DirectExprListConverter>(manager, value.getGlobalOffsets()),
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
			// extract the associated expression lists
			ndrange.setGlobalOffsets(toValue<ExpressionList, DirectExprListConverter>(analysis::getArgument(expr, 1)));
			ndrange.setGlobalWorkSize(toValue<ExpressionList, DirectExprListConverter>(analysis::getArgument(expr, 2)));
			ndrange.setLocalWorkSize(toValue<ExpressionList, DirectExprListConverter>(analysis::getArgument(expr, 3)));
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

	template <>
	struct type_factory<opencl::Optional> {
		TypePtr operator()(NodeManager& manager) const {
			return manager.getLangExtension<opencl::OpenCLExtension>().getOptional();
		}
	};

	template <>
	struct value_to_ir_converter<opencl::Optional> {
		ExpressionPtr operator()(NodeManager& manager, const opencl::Optional& value) const {
			IRBuilder builder(manager);
			auto& oclExt = manager.getLangExtension<opencl::OpenCLExtension>();

			LiteralPtr modifier;
			switch(value.getModifier()) {
			case opencl::Optional::Modifier::HOST_PRIMITIVE:	modifier = builder.uintLit(0); break;
			case opencl::Optional::Modifier::KRNL_BUFFER:		modifier = builder.uintLit(1); break;
			}
			// use the default IRBuilder to generate the callExpr
			return builder.callExpr(oclExt.getOptional(), oclExt.getMakeOptional(),
									value.getSize(), value.getValue(), modifier);
		}
	};

	template <>
	struct ir_to_value_converter<opencl::Optional> {
		opencl::Optional operator()(const core::ExpressionPtr& expr) const {
			// paranoia check
			if (!isEncodingOf<opencl::Optional>(expr)) throw InvalidExpression(expr);
			// prepare an empty requirement such that we can fill it up
			opencl::Optional optional;

			// extract the enclosed size
			optional.setSize(analysis::getArgument(expr, 0));
			// extract the enclosed value itself
			optional.setValue(analysis::getArgument(expr, 1));
			// extract the modifier converted to an uintLit
			unsigned int modifier = toValue<unsigned int>(analysis::getArgument(expr, 2));
			if 		(modifier == 0) optional.setModifier(opencl::Optional::Modifier::HOST_PRIMITIVE);
			else if (modifier == 1) optional.setModifier(opencl::Optional::Modifier::KRNL_BUFFER);
			else					{ assert_fail() << "literal " << modifier << " cannot be mapped to a valid modifier"; }
			// DataRequirement is done now and can be returend to user-code
			return optional;
		}
	};

	template <>
	struct is_encoding_of<opencl::Optional> {
		bool operator()(const core::ExpressionPtr& expr) const {
			auto& oclExt = expr->getNodeManager().getLangExtension<opencl::OpenCLExtension>();
			return analysis::isCallOf(expr, oclExt.getMakeOptional());
		}
	};
} // end namespace core
} // end namespace encoder
} // end namespace insieme
