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
#include "insieme/backend/addons/enum_type.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/operator_converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/annotations/c/include.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/enum.h"

namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* EnumTypeHandler(ConversionContext& context, const core::TypePtr& type) {

			if(!core::lang::isEnum(type)) { return nullptr; }

			const Converter& converter = context.getConverter();

			// check if the enum is defined in a system header
			const TypeInfo* t = NULL;
			std::function<void(std::string&, const core::TypePtr&)> ff = [](std::string& s, const core::TypePtr& type){ s = "enum" + s; };
			t = type_info_utils::headerAnnotatedTypeHandler(converter, type, ff);
			if(t) { return t; }

			const core::TupleTypePtr tt = type.as<core::TupleTypePtr>();

			// get the enum definition
			core::lang::EnumDefinition enumTy(core::lang::getEnumTypeDefinition(tt));

			// get the enum name and convert it
			const c_ast::SharedCNodeManager& cnodemgr = converter.getCNodeManager();
			c_ast::IdentifierPtr enumName =
				cnodemgr->create<c_ast::Identifier>(utils::demangleToIdentifier(enumTy.getEnumName().as<core::GenericTypePtr>()->getName()->getValue(), true));
			// get the enum int type and convert it if applicable
			c_ast::TypePtr enumIntType = nullptr;
			if(!type->getNodeManager().getLangBasic().isInt4(enumTy.getIntType())) {
				auto intTypeInfo = converter.getTypeManager().getTypeInfo(context, enumTy.getIntType());
				enumIntType = intTypeInfo.lValueType;
			}
			// get the enum values and convert them
			std::vector<std::pair<c_ast::IdentifierPtr, c_ast::LiteralPtr>> values;
			for(auto e : enumTy.getElements()) {
				core::lang::EnumEntry ee(e);
				c_ast::IdentifierPtr eeName = cnodemgr->create<c_ast::Identifier>(utils::demangleToIdentifier(ee.getName(), true));
				c_ast::LiteralPtr eeVal = cnodemgr->create<c_ast::Literal>(std::to_string(ee.getEnumEntryValue()));
				values.push_back({eeName, eeVal});
			}
			// build up the enum code fragments
			auto cEnumType = cnodemgr->create<c_ast::EnumType>(enumName, values, enumIntType);
			auto cEnumVarTy = cnodemgr->create<c_ast::NamedType>(enumName);

			TypeInfo* retTypeInfo = new TypeInfo();
			// create definition of the enum type
			c_ast::CodeFragmentPtr definition = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), cEnumType);

			retTypeInfo->declaration = definition;
			retTypeInfo->definition = definition;
			retTypeInfo->lValueType = cEnumVarTy;
			retTypeInfo->rValueType = cEnumVarTy;
			retTypeInfo->externalType = cEnumVarTy;

			// build up and return resulting type information
			return retTypeInfo;
		}


		OperatorConverterTable getEnumTypeOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<core::lang::EnumExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			// ------------------ enum specific operators ---------------
			res[ext.getEnumToInt()] = OP_CONVERTER { return CONVERT_ARG(0); };

			res[ext.getEnumFromInt()] = OP_CONVERTER { return CONVERT_ARG(1); };

			#include "insieme/backend/operator_converter_end.inc"

			return res;
		}

	}

	namespace {
		c_ast::NodePtr enumExprConverter(ConversionContext& context, const core::NodePtr& node) {
			if(node.getNodeCategory() != core::NC_Expression) return nullptr;
			if(!core::lang::isEnum(node)) return nullptr;
			if(!node.isa<core::TupleExprPtr>()) return nullptr;
			auto expr = node.as<core::ExpressionPtr>();
			auto entry = core::lang::EnumEntry(core::lang::getEnumEntry(expr));
			auto name = utils::demangleToIdentifier(entry.getName(), true);
			return c_ast::lit(context.getConverter().getStmtConverter().convertType(context, expr->getType()), name);
		}
	}

	void EnumType::installOn(Converter& converter) const {
		// registers type handler
		converter.getTypeManager().addTypeHandler(EnumTypeHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getEnumTypeOperatorTable(converter.getNodeManager()));

		// register expression handler (to handle enum tuples)
		converter.getStmtConverter().addStmtHandler(enumExprConverter);
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme
