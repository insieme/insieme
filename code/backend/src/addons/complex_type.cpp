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
 */

#include "insieme/backend/addons/complex_type.h"


#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/complex.h"


namespace insieme {
namespace backend {
namespace addons {

	namespace {

		bool isComplexType(const core::TypePtr& type) {
			const core::StructPtr& structType = core::analysis::isStruct(type);
			if(!structType) { return false; }
			if(!(structType->getFields().size() == 2)) { return false; }
			const core::FieldPtr t1 = structType->getFields()[0];
			const core::FieldPtr t2 = structType->getFields()[1];
			if((t1->getName()->getValue().find("_real") == std::string::npos) || (t2->getName()->getValue().find("_img") == std::string::npos)) {
				return false;
			}
			return true;
		}


		const TypeInfo* ComplexTypeHandler(ConversionContext& context, const core::TypePtr& type) {
			if(!isComplexType(type)) { return NULL; }

			// build up TypeInfo for complex type
			TypeManager& typeManager = context.getConverter().getTypeManager();

			const core::StructPtr& structType = core::analysis::isStruct(type);
			const core::FieldPtr t1 = structType->getFields()[0];
			const TypeInfo& baseInfo = typeManager.getTypeInfo(context, t1->getType());

			// copy base information
			TypeInfo* refInfo = new TypeInfo(baseInfo);

			// alter r / l / external C type
			refInfo->lValueType = c_ast::complexType(refInfo->lValueType);

			refInfo->rValueType = c_ast::complexType(refInfo->rValueType);

			refInfo->externalType = c_ast::complexType(refInfo->externalType);

			// add the header dummy
			c_ast::CodeFragmentPtr decl = c_ast::IncludeFragment::createNew(context.getConverter().getFragmentManager(), "complex.h");
			refInfo->declaration = decl;
			refInfo->definition = decl;

			return refInfo;
		}


		OperatorConverterTable getComplexTypeOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<core::lang::ComplexExtension>();
			const auto& gen = manager.getLangBasic();

			#include "insieme/backend/operator_converter_begin.inc"

			// ------------------ complex specific operators ---------------
			res[ext.getRefComplexReal()] = OP_CONVERTER { return c_ast::ref(c_ast::complexReal(c_ast::deref(CONVERT_ARG(0)))); };

			res[ext.getRefComplexImg()] = OP_CONVERTER { return c_ast::ref(c_ast::complexImag(c_ast::deref(CONVERT_ARG(0)))); };

			res[ext.getComplexReal()] = OP_CONVERTER { return c_ast::complexReal(CONVERT_ARG(0)); };

			res[ext.getComplexImg()] = OP_CONVERTER { return c_ast::complexImag(CONVERT_ARG(0)); };

			// -------------------- cast operators -------------------------

			res[ext.getConstantToComplex()] = OP_CONVERTER { return CONVERT_ARG(0); };

			res[ext.getComplexToBool()] = OP_CONVERTER { return CONVERT_ARG(0); };

			res[ext.getComplexToComplex()] = OP_CONVERTER { return CONVERT_ARG(0); };

			// -------------------- generic operators ----------------------
			res[gen.getGenAdd()] = OP_CONVERTER {
				if(isComplexType(ARG(0)->getType()) || isComplexType(ARG(1)->getType())) { return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); }
				return NULL;
			};

			res[gen.getGenSub()] = OP_CONVERTER {
				if(isComplexType(ARG(0)->getType()) || isComplexType(ARG(1)->getType())) { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); }
				return NULL;
			};

			res[gen.getGenMul()] = OP_CONVERTER {
				if(isComplexType(ARG(0)->getType()) || isComplexType(ARG(1)->getType())) { return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); }
				return NULL;
			};

			res[gen.getGenDiv()] = OP_CONVERTER {
				if(isComplexType(ARG(0)->getType()) || isComplexType(ARG(1)->getType())) { return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); }
				return NULL;
			};


			#include "insieme/backend/operator_converter_end.inc"

			return res;
		}
	}

	void ComplexType::installOn(Converter& converter) const {
		// registers type handler
		converter.getTypeManager().addTypeHandler(ComplexTypeHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getComplexTypeOperatorTable(converter.getNodeManager()));
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme
