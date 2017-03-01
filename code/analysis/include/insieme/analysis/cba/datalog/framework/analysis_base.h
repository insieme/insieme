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

#include "insieme/analysis/cba/datalog/framework/forward_decls.h"

#include "insieme/analysis/cba/common/failure.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/lang/lang.h"
#include "insieme/core/lang/basic.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {
namespace framework {

	#define make_node_list(LOOP_OBJECT) {                                              \
	            int counter = 0;                                                       \
	            for(const auto& cur : LOOP_OBJECT) {                                   \
	                    inserter.insert("NodeList", id, counter++, this->visit(cur));  \
	            }                                                                      \
	            inserter.insert("NodeListLength", id, counter);                        \
	            };


	template<template <class Target> class Ptr, class Inserter>
	class FactExtractor : public core::IRVisitor<int,Ptr> {

		using super = core::IRVisitor<int,Ptr>;

		using NodeIndexer = std::function<void(Ptr<const core::Node>,int)>;

		NodeIndexer indexer;

		Inserter inserter;

		int counter = 0;

		std::map<core::NodePtr,int> uniqueIndex;

		std::map<core::TypePtr,int> typeCache;

	public:

		FactExtractor(const NodeIndexer& indexer)
		        : core::IRVisitor<int,Ptr>(true)
		        , indexer(indexer)
		        , inserter() {}

		Inserter &getInserter() {
			return inserter;
		}

		int getUniqueID(const Ptr<const core::Node>& node) {
			core::NodePtr entry(&*node);
			auto pos = uniqueIndex.find(entry);
			if (pos != uniqueIndex.end()) return pos->second;
			int newIndex = uniqueIndex.size();
			uniqueIndex[entry] = newIndex;

			// also add element to named constructs list
			if (entry.isa<core::ExpressionPtr>() && core::lang::isBuiltIn(entry)) {
				inserter.insert("NamedConstruct", core::lang::getConstructName(entry), newIndex);
			}

			// done
			return newIndex;
		}

		int getFreshID(const Ptr<const core::Node>& node) {
			int res = counter++;
			indexer(core::NodeAddress(node),res);
			inserter.insert("NodeIdentity", res, getUniqueID(node));
			return res;
		}

		int visit(const Ptr<const core::Node>& node) override {

			// check type cache
			auto type = node.template isa<Ptr<const core::Type>>();
			if (!type) return super::visit(node);

			// look up cache
			auto pos = typeCache.find(type);
			if (pos != typeCache.end()) return pos->second;

			// resolve and cache
			return typeCache[type] = super::visit(node);
		}


		// -- Type Nodes --

		int visitGenericType(const Ptr<const core::GenericType>& type) override {
			int id = getFreshID(type);
			const string& name = type->getName()->getValue();
			int parents = this->visit(type->getParents());
			int params = this->visit(type->getTypeParameter()); // TODO check this again
			inserter.insert("GenericType", id, name, parents, params);
			return id;
		}

		int visitTupleType(const Ptr<const core::TupleType>& tuple) override {
			int id = getFreshID(tuple);
			inserter.insert("TupleType", id);
			make_node_list(tuple);
			return id;
		}

		int visitFunctionType(const Ptr<const core::FunctionType>& fun) override {
			int id = getFreshID(fun);
			int parameter_types = this->visit(fun->getParameterTypes());
			int return_type = this->visit(fun->getReturnType());
			uint kind = fun->getFunctionKind()->getValue();
			int instantiation_types = this->visit(fun->getInstantiationTypes());
			inserter.insert("FunctionType", id, parameter_types, return_type, kind, instantiation_types);
			return id;
		}

		int visitTypeVariable(const Ptr<const core::TypeVariable>& var) override {
			int id = getFreshID(var);
			inserter.insert("TypeVariable", id, var->getVarName()->getValue());
			return id;
		}

		int visitVariadicTypeVariable(const Ptr<const core::VariadicTypeVariable>& var) override {
			int id = getFreshID(var);
			const string& var_name = var->getVarName()->getValue();
			inserter.insert("VariadicTypeVariable", id, var_name);
			return id;
		}

		int visitGenericTypeVariable(const Ptr<const core::GenericTypeVariable>& var) override {
			int id = getFreshID(var);
			const string& var_name = var->getVarName()->getValue();
			int type_parameter = this->visit(var->getTypeParameter());
			inserter.insert("GenericTypeVariable", id, var_name, type_parameter);
			return id;
		}

		int visitVariadicGenericTypeVariable(const Ptr<const core::VariadicGenericTypeVariable>& var) override {
			int id = getFreshID(var);
			const string& var_name = var->getVarName()->getValue();
			int type_parameter = this->visit(var->getTypeParameter());
			inserter.insert("VariadicGenericTypeVariable", id, var_name, type_parameter);
			return id;
		}

		int visitNumericType(const Ptr<const core::NumericType>& var) override {
			int id = getFreshID(var);
			int node = this->visit(var->getValue());
			inserter.insert("NumericType", id, node);
			return id;
		}

		int visitTagType(const Ptr<const core::TagType>& var) override {
			int id = getFreshID(var);
			int tag = this->visit(var->getTag());
			int defintion = this->visit(var->getDefinition());
			inserter.insert("TagType", id, tag, defintion);
			return id;
		}

		int visitTagTypeReference(const Ptr<const core::TagTypeReference>& var) override {
			int id = getFreshID(var);
			const string& name = var->getName()->getValue();
			inserter.insert("TagTypeReference", id, name);
			return id;
		}

		int visitStruct(const Ptr<const core::Struct>& var) override {
			int id = getFreshID(var);
			const string& name = var->getName()->getValue();
			int fields = this->visit(var->getFields());
			int constructors = this->visit(var->getConstructors());
			int destructor_opt = this->visit(var->getOptionalDestructor());
			bool destructor_virtual = var->getDestructorVirtual()->getValue();
			int member_functions = this->visit(var->getMemberFunctions());
			int pure_virtual_member_functions = this->visit(var->getPureVirtualMemberFunctions());
			int parents = this->visit(var->getParents());

			inserter.insert("Struct", id, name, fields, constructors, destructor_opt, destructor_virtual,
			                member_functions, pure_virtual_member_functions, parents);

			return id;
		}

		int visitProgram(const Ptr<const core::Program>& var) override {
			int id = getFreshID(var);
			inserter.insert("Program", id);
			make_node_list(var);
			return id;
		}

		int visitUnion(const Ptr<const core::Union>& var) override {
			int id = getFreshID(var);
			const string& name = var->getName()->getValue();
			int fields = this->visit(var->getFields());
			int constructors = this->visit(var->getConstructors());
			int destructor_opt = this->visit(var->getOptionalDestructor());
			bool destructor_virtual = var->getDestructorVirtual()->getValue();
			int member_functions = this->visit(var->getMemberFunctions());
			int pure_virtual_member_functions = this->visit(var->getPureVirtualMemberFunctions());

			inserter.insert("Union", id, name, fields, constructors, destructor_opt, destructor_virtual,
			                member_functions, pure_virtual_member_functions);

			return id;
		}

		// -- Expression Nodes --

		template <typename T>
		bool stringRepresentsNumber(const std::string &str) {
			try {
				boost::lexical_cast<T>(str);
			} catch (boost::bad_lexical_cast &) {
				return false;
			}
			return true;
		}

		template <typename T>
		T convertStringToNumber(const std::string &str) {
			return boost::lexical_cast<T>(str);
		}


		int visitLiteral(const Ptr<const core::Literal>& var) override {
			int id = getFreshID(var);
			int type = this->visit(var->getType());
			const string& string_value = var->getStringValue();
			inserter.insert("Literal", id, type, string_value);

			// Also fill in an integer literal if possible
			const auto &opt_number_value = var->template getValueAs<int32_t>();
			if (opt_number_value) {
				inserter.insert("IntegerLiteral", id, opt_number_value.get());
			}

			return id;
		}

		int visitVariable(const Ptr<const core::Variable>& var) override {
			int id = getFreshID(var);
			int type = this->visit(var->getType());
			unsigned var_id = var->getId();
			inserter.insert("Variable", id, type, var_id);
			return id;
		}

		int visitCallExpr(const Ptr<const core::CallExpr>& var) override {
			int id = getFreshID(var);
			int function_expr = this->visit(var->getFunctionExpr());
			inserter.insert("CallExpr", id, function_expr);
			make_node_list(var);
			return id;
		}

		int visitBindExpr(const Ptr<const core::BindExpr>& var) override {
			int id = getFreshID(var);
			int parameters = this->visit(var->getParameters());
			int call = this->visit(var->getCall());
			inserter.insert("BindExpr", id, parameters, call);
			return id;
		}

		int visitCastExpr(const Ptr<const core::CastExpr>& var) override {
			int id = getFreshID(var);
			int sub_expression = this->visit(var->getSubExpression());
			inserter.insert("CastExpr", id, sub_expression);
			return id;
		}

		int visitInitExpr(const Ptr<const core::InitExpr>& var) override {
			int id = getFreshID(var);
			int memory_expr = this->visit(var->getMemoryExpr());
			int init_exprs = this->visit(var->getInitExprs());
			inserter.insert("InitExpr", id, memory_expr, init_exprs);
			return id;
		}

		int visitJobExpr(const Ptr<const core::JobExpr>& var) override {
			int id = getFreshID(var);
			int thread_num_range = this->visit(var->getThreadNumRange());
			int body = this->visit(var->getBody());
			inserter.insert("JobExpr", id, thread_num_range, body);
			return id;
		}

		int visitLambdaExpr(const Ptr<const core::LambdaExpr>& var) override {
			int id = getFreshID(var);
			int type = this->visit(var->getType());
			int reference = this->visit(var->getReference());
			int definition = this->visit(var->getDefinition());
			inserter.insert("LambdaExpr", id, type, reference, definition);
			return id;
		}

		int visitLambdaReference(const Ptr<const core::LambdaReference>& var) override {
			int id = getFreshID(var);
			int type = this->visit(var->getType());
			const string& name = var->getName()->getValue();
			inserter.insert("LambdaReference", id, type, name);
			return id;
		}

		int visitMarkerExpr(const Ptr<const core::MarkerExpr>& var) override {
			// Special case: Markers are just piped through and have no ir.dl entry
			return this->visit(var->getSubExpression());
		}

		int visitTupleExpr(const Ptr<const core::TupleExpr>& var) override {
			int id = getFreshID(var);
			int expressions = this->visit(var->getExpressions());
			inserter.insert("TupleExpr", id, expressions);
			return id;
		}

		// -- Statement Nodes --

		int visitCompoundStmt(const Ptr<const core::CompoundStmt>& var) override {
			int id = getFreshID(var);
			inserter.insert("CompoundStmt", id);
			make_node_list(var);
			return id;
		}

		int visitDeclarationStmt(const Ptr<const core::DeclarationStmt>& var) override {
			int id = getFreshID(var);
			int declaration = this->visit(var->getDeclaration());
			int variable = this->visit(var->getVariable());
			inserter.insert("DeclarationStmt", id, declaration, variable);
			return id;
		}

		int visitIfStmt(const Ptr<const core::IfStmt>& var) override {
			int id = getFreshID(var);
			int condition = this->visit(var->getCondition());
			int then_body = this->visit(var->getThenBody());
			int else_body = this->visit(var->getElseBody());
			inserter.insert("IfStmt", id, condition, then_body, else_body);
			return id;
		}

		int visitBreakStmt(const Ptr<const core::BreakStmt>& var) override {
			int id = getFreshID(var);
			inserter.insert("BreakStmt", id);
			return id;
		}

		int visitContinueStmt(const Ptr<const core::ContinueStmt>& var) override {
			int id = getFreshID(var);
			inserter.insert("ContinueStmt", id);
			return id;
		}

		int visitForStmt(const Ptr<const core::ForStmt>& var) override {
			int id = getFreshID(var);
			int declaration = this->visit(var->getDeclaration());
			int end = this->visit(var->getEnd());
			int step = this->visit(var->getStep());
			int body = this->visit(var->getBody());
			inserter.insert("ForStmt", id, declaration, end, step, body);
			return id;
		}

		int visitGotoStmt(const Ptr<const core::GotoStmt>& var) override {
			assert_not_implemented() << "Goto statement not implemented!";
			return -1;
		}

		int visitLabelStmt(const Ptr<const core::LabelStmt>& var) override {
			assert_not_implemented() << "Label statement not implemented!";
			return -1;
		}

		int visitMarkerStmt(const Ptr<const core::MarkerStmt>& var) override {
			// Special case: Markers are just piped through and have no ir.dl entry
			return this->visit(var->getSubStatement());
		}

		int visitReturnStmt(const Ptr<const core::ReturnStmt>& var) override {
			int id = getFreshID(var);
			int return_decl = this->visit(var->getReturnDeclaration());
			inserter.insert("ReturnStmt", id, return_decl);
			return id;
		}

		int visitSwitchStmt(const Ptr<const core::SwitchStmt>& var) override {
			int id = getFreshID(var);
			int switch_expr = this->visit(var->getSwitchExpr());
			int cases = this->visit(var->getCases());
			int default_case = this->visit(var->getDefaultCase());
			inserter.insert("SwitchStmt", id, switch_expr, cases, default_case);
			return id;
		}

		int visitThrowStmt(const Ptr<const core::ThrowStmt>& var) override {
			int id = getFreshID(var);
			int throw_expr = this->visit(var->getThrowExpr());
			inserter.insert("ThrowStmt", id, throw_expr);
			return id;
		}

		int visitTryCatchStmt(const Ptr<const core::TryCatchStmt>& var) override {
			int id = getFreshID(var);
			int body = this->visit(var->getBody());
			inserter.insert("TryCatchStmt", id, body);
			make_node_list(var);
			return id;
		}

		int visitWhileStmt(const Ptr<const core::WhileStmt>& var) override {
			int id = getFreshID(var);
			int condition = this->visit(var->getCondition());
			int body = this->visit(var->getBody());
			inserter.insert("WhileStmt", id, condition, body);
			return id;
		}

		// -- Support Nodes --

		int visitTypes(const Ptr<const core::Types>& types) override {
			int id = getFreshID(types);
			inserter.insert("Types", id);
			make_node_list(types);
			return id;
		}

		int visitParents(const Ptr<const core::Parents>& parents) override {
			int id = getFreshID(parents);
			inserter.insert("Parents", id);
			make_node_list(parents);
			return id;
		}

		int visitCatchClause(const Ptr<const core::CatchClause>& var) override {
			int id = getFreshID(var);
			int variable = this->visit(var->getVariable());
			int body = this->visit(var->getBody());
			inserter.insert("CatchClause", id, variable, body);
			return id;
		}

		int visitDeclaration(const Ptr<const core::Declaration>& decl) override {
			int id = getFreshID(decl);
			int type = this->visit(decl->getType());
			int initialization = this->visit(decl->getInitialization());
			inserter.insert("Declaration", id, type, initialization);
			return id;
		}

		int visitExpressions(const Ptr<const core::Expressions>& expressions) override {
			int id = getFreshID(expressions);
			inserter.insert("Expressions", id);
			make_node_list(expressions);
			return id;
		}

		int visitField(const Ptr<const core::Field>& field) override {
			int id = getFreshID(field);
			const string& name = field->getName()->getValue();
			int type = this->visit(field->getType());
			inserter.insert("Field", id, name, type);
			return id;
		}

		int visitFields(const Ptr<const core::Fields>& fields) override {
			int id = getFreshID(fields);
			inserter.insert("Fields", id);
			make_node_list(fields);
			return id;
		}

		int visitLambda(const Ptr<const core::Lambda>& lambda) override {
			int id = getFreshID(lambda);
			int type = this->visit(lambda->getType());
			int parameters = this->visit(lambda->getParameters());
			int body = this->visit(lambda->getBody());
			inserter.insert("Lambda", id, type, parameters, body);
			return id;
		}

		int visitLambdaBinding(const Ptr<const core::LambdaBinding>& binding) override {
			int id = getFreshID(binding);
			int reference = this->visit(binding->getReference());
			int lambda = this->visit(binding->getLambda());
			inserter.insert("LambdaBinding", id, reference, lambda);
			return id;
		}

		int visitLambdaDefinition(const Ptr<const core::LambdaDefinition>& def) override {
			int id = getFreshID(def);
			inserter.insert("LambdaDefinition", id);
			make_node_list(def);
			return id;
		}

		int visitMemberFunction(const Ptr<const core::MemberFunction>& fun) override {
			int id = getFreshID(fun);
			const string& name = fun->getName()->getValue();
			bool virtual_flag = fun->getVirtualFlag()->getValue();
			int implementation = this->visit(fun->getImplementation());
			inserter.insert("MemberFunction", id, name, virtual_flag, implementation);
			return id;
		}

		int visitMemberFunctions(const Ptr<const core::MemberFunctions>& member_funcs) override {
			int id = getFreshID(member_funcs);
			inserter.insert("MemberFunctions", id);
			make_node_list(member_funcs);
			return id;
		}

		int visitParameters(const Ptr<const core::Parameters>& params) override {
			int id = getFreshID(params);
			inserter.insert("Parameters", id);
			make_node_list(params);
			return id;
		}

		int visitParent(const Ptr<const core::Parent>& var) override {
			int id = getFreshID(var);
			bool vvirtual = var->getVirtual()->getValue();
			unsigned access_specifier_kind = var->getAccessSpecifierKind()->getValue();
			int type = this->visit(var->getType());
			inserter.insert("Parent", id, vvirtual, access_specifier_kind, type);
			return id;
		}

		int visitPureVirtualMemberFunction(const Ptr<const core::PureVirtualMemberFunction>& pvmf) override {
			int id = getFreshID(pvmf);
			const string& name = pvmf->getName()->getValue();
			int type = this->visit(pvmf->getType());
			inserter.insert("PureVirtualMemberFunction", id, name, type);
			return id;
		}

		int visitPureVirtualMemberFunctions(const Ptr<const core::PureVirtualMemberFunctions>& pvm_funcs) override {
			int id = getFreshID(pvm_funcs);
			inserter.insert("PureVirtualMemberFunctions", id);
			make_node_list(pvm_funcs);
			return id;
		}

		int visitSwitchCase(const Ptr<const core::SwitchCase>& var) override {
			int id = getFreshID(var);
			int guard = this->visit(var->getGuard());
			int body = this->visit(var->getBody());
			inserter.insert("SwitchCase", id, guard, body);
			return id;
		}

		int visitSwitchCases(const Ptr<const core::SwitchCases>& var) override {
			int id = getFreshID(var);
			inserter.insert("SwitchCases", id);
			make_node_list(var);
			return id;
		}

		int visitTagTypeDefinition(const Ptr<const core::TagTypeDefinition>& var) override {
			int id = getFreshID(var);
			inserter.insert("TagTypeDefinition", id);
			make_node_list(var);
			return id;
		}

		int visitTagTypeBinding(const Ptr<const core::TagTypeBinding>& var) override {
			int id = getFreshID(var);
			int tag = this->visit(var->getTag());
			int record = this->visit(var->getRecord());
			inserter.insert("TagTypeBinding", id, tag, record);
			return id;
		}


		// -- Unimplemented Nodes: Fail --

		int visitNode(const Ptr<const core::Node>& cur) override {
			assert_not_implemented() << "Unsupported node type: " << cur->getNodeType() << "\n";
			return 0;
		}

	};

	#undef make_node_list

} // end namespace framework
} // end namespace datalog
} //'end namespace cba
} // end namespace analysis
} // end namespace insieme
