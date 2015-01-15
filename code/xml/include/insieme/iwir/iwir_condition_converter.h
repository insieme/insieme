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

#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_builder.h"

#include "insieme/iwir/iwir_ast.h"

#include "insieme/iwir/iwir_extension.h"

namespace iwir {
namespace condition_ast {

using namespace iwir::ast;
using namespace insieme;
using namespace std;

namespace {
	typedef map<pair<Task*, Port*>, core::VariablePtr> VarMap;
}

/*
 * take a ConditionExpr (a condition_ast) and convert it into an IR_expression - replaces the
 * Port* with the IR_variables used 
 */
struct condition_ast_to_inspire : boost::static_visitor<core::ExpressionPtr> {
	Task* parentTask;
	const VarMap& varMap;
	const core::IRBuilder& irBuilder;

	condition_ast_to_inspire (Task* parentTask, const VarMap& varMap, const core::IRBuilder& irBuilder): parentTask(parentTask), varMap(varMap), irBuilder(irBuilder) {}

	core::VariablePtr lookup(Port* port) const {
		core::VariablePtr var = nullptr;
		auto p = varMap.find({parentTask, port});
		if(p != varMap.end()) {
			var = p->second;
		} 
		//VLOG(2) << var << "(" << var->getType() << ")" << " " << parentTask << " " << port;
		return var;
	};

	//apply implict casting rules of the condition expr
	std::tuple<core::ExpressionPtr, core::ExpressionPtr> implicit_cast(core::ExpressionPtr lhs, core::ExpressionPtr rhs) const {
		const core::lang::BasicGenerator& gen = irBuilder.getLangBasic();
		core::TypePtr lTy = lhs->getType();
		core::TypePtr rTy = rhs->getType();
		if(*lTy == *rTy) {
			return std::make_tuple(lhs, rhs);
		}

		//string > integer > double > boolean
		int cast = 0;

		if(gen.isString(lTy)) {cast = 1;}
		if(gen.isInt(lTy)) {cast = 2;}
		if(gen.isDouble(lTy)) {cast = 3;}
		if(gen.isBool(lTy)) {cast = 4;}

		if(gen.isString(rTy)) {cast += 10;}
		if(gen.isInt(rTy)) {cast += 20;}
		if(gen.isDouble(rTy)) {cast += 30;}
		if(gen.isBool(rTy)) {cast += 40;}

		switch(cast) {
			case 0: assert(false); break;
				//lhs = int -- rhs = string
			case 12: {
							//rhs = stringToInt(rhs);
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToInt();
					rhs = irBuilder.callExpr(op,irBuilder.getIntParamLiteral(4),rhs);
					break;
						}
				//lhs = double -- rhs = string
			case 13: {//rhs = stringToDouble(rhs);
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToDouble();
					rhs = irBuilder.callExpr(op,irBuilder.getIntParamLiteral(8),rhs);
					break;
						}
				//lhs = bool -- rhs = string
			case 14: {//rhs = stringToBool(rhs);
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToBool();
					rhs = irBuilder.callExpr(op,rhs);
					break;
						}

				//lhs = string-- rhs = int 
			case 21: //lhs = stringToInt(lhs);
						break;
				//lhs = double -- rhs = int 
			case 23: //rhs = intToDouble(rhs); 
						rhs = irBuilder.callExpr(gen.getSignedToReal(),irBuilder.getIntParamLiteral(8),rhs);
						break;
				//lhs = bool -- rhs = int 
			case 24: // rhs = intToBool(rhs); 
						rhs = irBuilder.callExpr(gen.getSignedToBool(),irBuilder.getIntParamLiteral(8),rhs);
						break;

				//lhs = string -- rhs = double 
			case 31: {//lhs = stringToDouble(lhs);
						auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToDouble();
						lhs = irBuilder.callExpr(op,irBuilder.getIntParamLiteral(8),lhs);
						break;
						}
				//lhs = int -- rhs = double 
			case 32: //lhs = intToDouble(lhs); 
						lhs = irBuilder.callExpr(gen.getSignedToReal(),irBuilder.getIntParamLiteral(8),lhs);
						break;
				//lhs = bool -- rhs = double 
			case 34: //rhs = doubleToBool(rhs);
						rhs = irBuilder.callExpr(gen.getRealToBool(),irBuilder.getIntParamLiteral(8),rhs);
						break;
				
				//lhs = string -- rhs = bool 
			case 41: {//lhs = stringToBool(lhs);
						auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToBool();
						lhs = irBuilder.callExpr(op,lhs);
						break;
						}
				//lhs = int -- rhs = bool 
			case 42:  //lhs = intToBool(lhs);
						lhs = irBuilder.callExpr(gen.getSignedToBool(), lhs);
						break;
				//lhs = double -- rhs = bool 
			case 43: //lhs = doubleToBool(lhs);
						lhs = irBuilder.callExpr(gen.getRealToBool(), lhs);
						break;
		}

		return std::make_tuple(lhs, rhs);
	}

	core::ExpressionPtr cast_to_bool(core::ExpressionPtr oper) const {
		const core::lang::BasicGenerator& gen = irBuilder.getLangBasic();
		core::ExpressionPtr op = nullptr;
		core::TypePtr operTy = oper.getType();

		if(gen.isBool(operTy)) { 
			return oper;
		}

		if(gen.isString(operTy)) { op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToBool(); }
		if(gen.isInt(operTy)) { op = gen.getSignedToBool(); }
		if(gen.isDouble(operTy)) { op = gen.getRealToBool(); }
		assert(op);
		return irBuilder.callExpr(op, oper);
	}


	core::ExpressionPtr operator()(int& v) const { return irBuilder.intLit(v); }
	core::ExpressionPtr operator()(double& v) const { return irBuilder.doubleLit(v); }
	core::ExpressionPtr operator()(bool& v) const { return irBuilder.boolLit(v); }
	core::ExpressionPtr operator()(std::string& v) const {  return irBuilder.stringLit("\"" + v + "\""); }
	core::ExpressionPtr operator()(condition_ast::port& v) const { 
		core::VariablePtr portVar = lookup(v.p);
		return irBuilder.tryDeref(portVar);
	}

	core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_and>& b) const { 
		core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
		core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
		assert(lhs);
		assert(rhs);

		lhs = cast_to_bool(lhs);
		rhs = cast_to_bool(rhs);

		return irBuilder.logicAnd(lhs, rhs);
	}

	core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_or >& b) const { 
		
		core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
		core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
		assert(lhs);
		assert(rhs);

		lhs = cast_to_bool(lhs);
		rhs = cast_to_bool(rhs);

		return irBuilder.logicOr(lhs, rhs);
	}

	core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_eq >& b) const { 
		core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
		core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
		assert(lhs);
		assert(rhs);

		if(*lhs->getType() != *rhs->getType()) {
			std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
		}
		
		if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
			auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringEq(); 
			return irBuilder.callExpr(op, lhs, rhs);
		} else {
			return irBuilder.eq(lhs, rhs);
		}
	}

	core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_neq>& b) const { 
		core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
		core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
		assert(lhs);
		assert(rhs);

		if(*lhs->getType() != *rhs->getType()) {
			std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
		}

		if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
			auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringNe(); 
			return irBuilder.callExpr(op, lhs, rhs);
		} else {
			return irBuilder.ne(lhs, rhs);
		}
	}
	
	core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_gt >& b) const { 
		core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
		core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
		assert(lhs);
		assert(rhs);

		if(*lhs->getType() != *rhs->getType()) {
			std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
		}

		if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
			auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringGt(); 
			return irBuilder.callExpr(op, lhs, rhs);
		} else { 
			return irBuilder.gt(lhs, rhs);
		}
	}
	core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_gte>& b) const { 
		core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
		core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
		assert(lhs);
		assert(rhs);

		if(*lhs->getType() != *rhs->getType()) {
			std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
		}

		if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
			auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringGe(); 
			return irBuilder.callExpr(op, lhs, rhs);
		} else { 
			return irBuilder.ge(lhs, rhs);
		}
	}
	core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_lt >& b) const { 
		core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
		core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
		assert(lhs);
		assert(rhs);

		if(*lhs->getType() != *rhs->getType()) {
			std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
		}
		
		if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
			auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringLt(); 
			return irBuilder.callExpr(op, lhs, rhs);
		} else { 
			return irBuilder.lt(lhs, rhs);
		}
	}
	core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_lte>& b) const { 
		core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
		core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
		assert(lhs);
		assert(rhs);

		if(*lhs->getType() != *rhs->getType()) {
			std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
		}
		
		if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
			auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringLe(); 
			return irBuilder.callExpr(op, lhs, rhs);
		} else { 
			return irBuilder.le(lhs, rhs);
		}
	}

	core::ExpressionPtr operator()(condition_ast::unop<condition_ast::op_not>& u) const {
		core::ExpressionPtr condExpr = nullptr;
		core::ExpressionPtr oper = boost::apply_visitor(*this, u.oper1); 
		assert(oper);

		oper = cast_to_bool(oper);
		condExpr = irBuilder.logicNeg(oper);
		
		return condExpr;
	}
};

core::ExpressionPtr convert_condition_ast_to_inspire(Condition* node, const VarMap& varMap, const core::IRBuilder& irBuilder) {
	return boost::apply_visitor(condition_ast_to_inspire(node->parentTask,varMap,irBuilder), node->condition);
};

} // condition_ast end
} // iwir end
