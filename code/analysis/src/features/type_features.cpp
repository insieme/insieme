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

#include "insieme/analysis/features/type_features.h"

#include <algorithm>
#include <functional>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/basic.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace analysis {
namespace features {

	namespace {

		struct SizeAnnotation {
			unsigned size;
			SizeAnnotation(unsigned size) : size(size) {};
			bool operator==(const SizeAnnotation& other) const {
				return size == other.size;
			}
		};

		struct SizeOfEstimator : public core::IRVisitor<unsigned> {

			unsigned unknownContainerSize;

			SizeOfEstimator(unsigned unknownContainerSize) :
				core::IRVisitor<unsigned>(true),
				unknownContainerSize(unknownContainerSize) {}

			unsigned visit(const core::TypePtr& type) {
				// first check whether size has been attached
				if (type->hasAttachedValue<SizeAnnotation>()) {
					return type->getAttachedValue<SizeAnnotation>().size;
				}

				// compute size and return size
				unsigned size = core::IRVisitor<unsigned>::visit(type);
				type->attachValue(SizeAnnotation(size));
				return size;
			}


			unsigned visitGenericType(const core::GenericTypePtr& type) {
				auto& basic = type->getNodeManager().getLangBasic();

				// 1 byte types
				if (basic.isInt1(type) || basic.isUInt1(type) || basic.isBool(type) || basic.isChar(type)) {
					return 1;
				}

				// 2 byte types
				if (basic.isInt2(type) || basic.isUInt2(type)) {
					return 2;
				}

				// 4 byte types
				if (basic.isInt4(type) || basic.isUInt4(type) || basic.isFloat(type)) {
					return 4;
				}

				// 8 byte types
				if (basic.isInt8(type) || basic.isUInt8(type) || basic.isDouble(type)) {
					return 8;
				}

				// fall back if unknown ...
				return visitType(type);
			}

			unsigned visitRecType(const core::RecTypePtr& type) {
				return visit(type->getTypeDefinition());
			}

		private:
	
			// Extract the type from a NamedType
			struct type {
				core::TypePtr operator()(const core::NamedTypePtr& ty) const { return ty->getType(); }
			};

			// Generic visitor for all types containing more than 1 element 
			template <typename SubType, typename Extractor, typename Aggregator>
			unsigned visitElements(const core::TypePtr& parentType, 
								   const core::NodeRange<SubType>& elements,
								   const Extractor& extractor, 
								   const Aggregator& aggregate) 
			{
				bool isUndefined = false;
				unsigned res = 0;

				for_each(elements, [&](const SubType& cur) {
					unsigned size = 0;
					try { 
						size = this->visit(extractor(cur));
					} catch(const UndefinedSize&& ex) {
						isUndefined = true;
						size = ex.getEstimatedSize();
					} 
					res = aggregate(res, size);
				});

				// If this type contained at least 1 undefined type then rethrow the exception
				if (isUndefined) { throw UndefinedSize(parentType, res); }
				return res;
			}
			
		public:
			unsigned visitUnionType(const core::UnionTypePtr& type) {
				// sum up size of element types
				typedef const unsigned& (* FuncPtr)(const unsigned&, const unsigned&);
				FuncPtr aggr = std::max<unsigned>;
				return visitElements(type, type->getEntries(), SizeOfEstimator::type(), aggr);
			}

			unsigned visitStructType(const core::StructTypePtr& type) {
				// sum up size of element types
				return visitElements(type, type->getEntries(), SizeOfEstimator::type(), std::plus<unsigned>());
			}

			unsigned visitTupleType(const core::TupleTypePtr& type) {
				// sum up size of element types
				return visitElements(type, type->getElementTypes(), id<core::TypePtr>(), std::plus<unsigned>());
			}

			unsigned visitArrayType(const core::ArrayTypePtr& type) {
				// extract dimension
				unsigned dim = 1;
				core::IntTypeParamPtr sizeParam = type->getDimension();
				if (sizeParam->getNodeType() == core::NT_ConcreteIntTypeParam) {
					dim = static_pointer_cast<core::ConcreteIntTypeParamPtr>(sizeParam)->getValue();
				} 
				// statically assume a size of containerSizeUpperBound elements along each dimension
				size_t estimation = std::pow(unknownContainerSize, dim) * visit(type->getElementType());
				throw UndefinedSize(type, estimation);
			}

			unsigned visitVectorType(const core::VectorTypePtr& type) {
				// extract size
				unsigned elemSize =  visit(type->getElementType());
				core::IntTypeParamPtr sizeParam = type->getSize();
				if (sizeParam->getNodeType() == core::NT_ConcreteIntTypeParam) {
					size_t size = static_pointer_cast<core::ConcreteIntTypeParamPtr>(sizeParam)->getValue();
					return size * elemSize;
				} 
				throw UndefinedSize(type, unknownContainerSize * elemSize);
			}

			unsigned visitRefType(const core::RefTypePtr& type) {
				// assuming a 64-bit system, a reference (=pointer) has 8 bytes
				return 64/8;
			}

			unsigned visitType(const core::TypePtr& type) {
				LOG(FATAL) << "Unsupported type encountered: " << *type;
				assert(false && "Unsupported type encountered!");
				return 0;
			}

			unsigned visitNode(const core::NodePtr& node) {
				assert(false && "This visitor only supports types!");
				return 0;
			}

		};
	}

	unsigned getSizeInBytes(const core::TypePtr& type, unsigned unknownContainerSize) {
		// just use a size-of estimator for the job
		static SizeOfEstimator estimator(unknownContainerSize);
		return estimator.visit(type);
	}

	unsigned getEstimatedSizeInBytes(const core::TypePtr& type, unsigned unknownContainerSize) {
		try {
			return getSizeInBytes(type, unknownContainerSize);
		} catch (const UndefinedSize&& ex) {
			return ex.getEstimatedSize();
		}
		return 0;
	}



	unsigned getMemberOffsetInBytes(const core::StructTypePtr& type, const core::StringValuePtr& member) {

		unsigned offset = 0;
		for(auto it = type.begin(); it!= type.end(); ++it ) {
			const core::NamedTypePtr& cur = *it;
			if (*cur->getName() == *member) {
				return offset;
			}
			offset += getEstimatedSizeInBytes(cur->getType());
		}

		assert(false && "Cannot determine offset of non-included member!");
		return offset;
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme
