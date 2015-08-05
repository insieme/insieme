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

#include <sstream>

#include "insieme/backend/addons/enum_type.h"


#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/backend/name_manager.h"


namespace insieme {
namespace backend {
namespace addons {

namespace {

const TypeInfo* EnumTypeHandler(const Converter& converter, const core::TypePtr& type) {
	static const TypeInfo* NOT_HANDLED = NULL;
	// check whether it is a enum type
	if(!(type->getNodeManager().getLangExtension<core::lang::EnumExtension>().isEnumType(type))) {
		return NOT_HANDLED;	// not handled by this handler
	}
	
	c_ast::CNodeManager& manager = *converter.getCNodeManager();
	auto fragmentManager = converter.getFragmentManager();
	
	// create constructor (C-style)
	core::GenericTypePtr gt = static_pointer_cast<const core::GenericType>(type);
	auto enumParams  = gt->getTypeParameter();
	
	string name = enumParams[0].as<core::GenericTypePtr>()->getFamilyName();
	
	if(name.empty()) {
		name = converter.getNameManager().getName(type);
	}
	
	// get enum constants as string
	std::stringstream enumConstants;
	for(unsigned i=1; i < enumParams.size(); ++i) {
		auto constantParams =enumParams[i].as<core::GenericTypePtr>()->getTypeParameter();
		enumConstants << constantParams[0].as<core::GenericTypePtr>()->getFamilyName() << " = " <<
		              constantParams[1].as<core::GenericTypePtr>()->getFamilyName();
		if(i<enumParams.size()-1) {
			enumConstants << ", ";
		}
	}
	
	//create declaration
	c_ast::NodePtr ctr = manager.create<c_ast::EnumType>(name, enumConstants.str());
	
	// add constructor (C-style)
	TypeInfo* info = new TypeInfo();
	
	//identifier pointer of enum
	c_ast::IdentifierPtr idptrname = manager.create(name);
	c_ast::TypePtr typeType = manager.create<c_ast::NamedType>(idptrname);
	info->lValueType = typeType;
	info->rValueType = typeType;
	info->externalType = typeType;
	
	// externalizer & internalizer
	info->externalize = &type_info_utils::NoOp;
	info->internalize = &type_info_utils::NoOp;
	
	//set declaration and definition
	c_ast::NodePtr typed;
	string enumtypedef = "enum " + name;
	c_ast::IdentifierPtr idptrenumname = manager.create(enumtypedef);
	c_ast::TypePtr typeTypeDef = manager.create<c_ast::NamedType>(idptrenumname);
	typed = manager.create<c_ast::TypeDefinition>(typeTypeDef, idptrname);
	info->declaration = c_ast::CCodeFragment::createNew(fragmentManager, toVector(ctr, typed));
	info->definition = info->declaration;
	
	return info;
}


OperatorConverterTable getEnumTypeOperatorTable(core::NodeManager& manager) {
	OperatorConverterTable res;
	const auto& ext = manager.getLangExtension<core::lang::EnumExtension>();
	
#include "insieme/backend/operator_converter_begin.inc"
	
	res[ext.getEnumElementAsInt()]  = OP_CONVERTER({ return CONVERT_ARG(0); });
	res[ext.getEnumElementAsUInt()]  = OP_CONVERTER({ return CONVERT_ARG(0); });
	res[ext.getIntAsEnum()]  = OP_CONVERTER({ return  c_ast::cast(CONVERT_TYPE(call->getType()), CONVERT_ARG(0)); });
	res[ext.getUIntAsEnum()]  = OP_CONVERTER({ return c_ast::cast(CONVERT_TYPE(call->getType()), CONVERT_ARG(0)); });
	
#include "insieme/backend/operator_converter_end.inc"
	return res;
}

c_ast::NodePtr EnumLiteralHandler(ConversionContext& context, const core::NodePtr& ptr) {

	if(!ptr.isa<core::LiteralPtr>()) {
		// not handled by this handler
		return NULL;
	}
	
	auto literal = ptr.as<core::LiteralPtr>();
	auto type = literal->getType();
	
	if(!(type->getNodeManager().getLangExtension<core::lang::EnumExtension>().isEnumType(type))) {
		// not handled by this handler
		return NULL;
	}
	
	
	if(type->getNodeManager().getLangExtension<core::lang::EnumExtension>().isEnumType(type)) {
		auto toLiteral = [&](const string& value) {
			return context.getConverter().getCNodeManager()->create<c_ast::Literal>(value);
		};
		c_ast::ExpressionPtr res = toLiteral(ptr.as<core::LiteralPtr>()->getStringValue());
		
		auto typeInfo = context.getConverter().getTypeManager().getTypeInfo(literal->getType());
		context.addDependency(typeInfo.definition);
		
		return res;
	}
	// not handled by this handler
	return NULL;
}
}

void EnumTypes::installOn(Converter& converter) const {

	// registers type handler
	converter.getTypeManager().addTypeHandler(EnumTypeHandler);
	
	// register additional operators
	converter.getFunctionManager().getOperatorConverterTable().insertAll(getEnumTypeOperatorTable(converter.getNodeManager()));
	
	// register additional StatementConverter
	converter.getStmtConverter().addStmtHandler(EnumLiteralHandler);
	
}

} // end namespace addons
} // end namespace backend
} // end namespace insieme
