/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/datalog/framework/analysis_base.h"

#include <souffle/SouffleInterface.h>

#include "insieme/analysis/common/failure.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/lang/lang.h"
#include "insieme/core/lang/basic.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace analysis {
namespace datalog {
namespace framework {

	namespace {

		using std::string;

		#define make_node_list(LOOP_OBJECT) {                                              \
		            int counter = 0;                                                       \
		            for(const auto& cur : LOOP_OBJECT) {                                   \
		                    insert("NodeList", id, counter++, this->visit(cur));           \
		            }                                                                      \
		            insert("NodeListLength", id, counter);                                 \
		    };


		template<template <class Target> class Ptr = core::Pointer>
		class FactExtractor : public core::IRVisitor<int,Ptr> {

			using super = core::IRVisitor<int,Ptr>;

			using NodeIndexer = std::function<void(Ptr<const core::Node>,int)>;

			NodeIndexer indexer;

			int counter = 0;

			souffle::Program& analysis;

			std::map<core::NodePtr,int> uniqueIndex;

			std::map<core::TypePtr,int> typeCache;

		public:

			FactExtractor(souffle::Program& analysis,const NodeIndexer& indexer)
				: core::IRVisitor<int,Ptr>(true), indexer(indexer), analysis(analysis) {}

			int extractFacts(const Ptr<const core::Node>& rootNode) {
				return this->visit(rootNode);
			}

			int getUniqueID(const Ptr<const core::Node>& node) {
				core::NodePtr entry(&*node);
				auto pos = uniqueIndex.find(entry);
				if (pos != uniqueIndex.end()) return pos->second;
				int newIndex = uniqueIndex.size();
				uniqueIndex[entry] = newIndex;

				// also add element to named constructs list
				if (entry.isa<core::ExpressionPtr>() && core::lang::isBuiltIn(entry)) {
					insert("NamedConstruct", core::lang::getConstructName(entry), newIndex);
				}

				// done
				return newIndex;
			}

			int getFreshID(const Ptr<const core::Node>& node) {
				int res = counter++;
				indexer(core::NodeAddress(node),res);
				insert("NodeIdentity", res, getUniqueID(node));
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
				insert("GenericType", id, name, parents, params);
				return id;
			}

			int visitTupleType(const Ptr<const core::TupleType>& tuple) override {
				int id = getFreshID(tuple);
				insert("TupleType", id);
				make_node_list(tuple);
				return id;
			}

			int visitFunctionType(const Ptr<const core::FunctionType>& fun) override {
				int id = getFreshID(fun);
				int parameter_types = this->visit(fun->getParameterTypes());
				int return_type = this->visit(fun->getReturnType());
				uint kind = fun->getFunctionKind()->getValue();
				int instantiation_types = this->visit(fun->getInstantiationTypes());
				insert("FunctionType", id, parameter_types, return_type, kind, instantiation_types);
				return id;
			}

			int visitTypeVariable(const Ptr<const core::TypeVariable>& var) override {
				int id = getFreshID(var);
				insert("TypeVariable", id, var->getVarName()->getValue());
				return id;
			}

			int visitVariadicTypeVariable(const Ptr<const core::VariadicTypeVariable>& var) override {
				int id = getFreshID(var);
				const string& var_name = var->getVarName()->getValue();
				insert("VariadicTypeVariable", id, var_name);
				return id;
			}

			int visitGenericTypeVariable(const Ptr<const core::GenericTypeVariable>& var) override {
				int id = getFreshID(var);
				const string& var_name = var->getVarName()->getValue();
				int type_parameter = this->visit(var->getTypeParameter());
				insert("GenericTypeVariable", id, var_name, type_parameter);
				return id;
			}

			int visitVariadicGenericTypeVariable(const Ptr<const core::VariadicGenericTypeVariable>& var) override {
				int id = getFreshID(var);
				const string& var_name = var->getVarName()->getValue();
				int type_parameter = this->visit(var->getTypeParameter());
				insert("VariadicGenericTypeVariable", id, var_name, type_parameter);
				return id;
			}

			int visitNumericType(const Ptr<const core::NumericType>& var) override {
				int id = getFreshID(var);
				int node = this->visit(var->getValue());
				insert("NumericType", id, node);
				return id;
			}

			int visitTagType(const Ptr<const core::TagType>& var) override {
				int id = getFreshID(var);
				int tag = this->visit(var->getTag());
				int defintion = this->visit(var->getDefinition());
				insert("TagType", id, tag, defintion);
				return id;
			}

			int visitTagTypeReference(const Ptr<const core::TagTypeReference>& var) override {
				int id = getFreshID(var);
				const string& name = var->getName()->getValue();
				insert("TagTypeReference", id, name);
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

				insert("Struct", id, name, fields, constructors, destructor_opt, destructor_virtual,
				       member_functions, pure_virtual_member_functions, parents);

				return id;
			}

			int visitProgram(const Ptr<const core::Program>& var) override {
				int id = getFreshID(var);
				insert("Program", id);
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

				insert("Union", id, name, fields, constructors, destructor_opt, destructor_virtual,
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
				insert("Literal", id, type, string_value);

				// Also fill in an integer literal if possible
				const auto &opt_number_value = var->template getValueAs<int32_t>();
				if (opt_number_value) {
					insert("IntegerLiteral", id, opt_number_value.get());
				}

				return id;
			}

			int visitVariable(const Ptr<const core::Variable>& var) override {
				int id = getFreshID(var);
				int type = this->visit(var->getType());
				unsigned var_id = var->getId();
				insert("Variable", id, type, var_id);
				return id;
			}

			int visitCallExpr(const Ptr<const core::CallExpr>& var) override {
				int id = getFreshID(var);
				int function_expr = this->visit(var->getFunctionExpr());
				insert("CallExpr", id, function_expr);
				make_node_list(var);
				return id;
			}

			int visitBindExpr(const Ptr<const core::BindExpr>& var) override {
				int id = getFreshID(var);
				int parameters = this->visit(var->getParameters());
				int call = this->visit(var->getCall());
				insert("BindExpr", id, parameters, call);
				return id;
			}

			int visitCastExpr(const Ptr<const core::CastExpr>& var) override {
				int id = getFreshID(var);
				int sub_expression = this->visit(var->getSubExpression());
				insert("CastExpr", id, sub_expression);
				return id;
			}

			int visitInitExpr(const Ptr<const core::InitExpr>& var) override {
				int id = getFreshID(var);
				int memory_expr = this->visit(var->getMemoryExpr());
				int init_exprs = this->visit(var->getInitExprs());
				insert("InitExpr", id, memory_expr, init_exprs);
				return id;
			}

			int visitJobExpr(const Ptr<const core::JobExpr>& var) override {
				int id = getFreshID(var);
				int thread_num_range = this->visit(var->getThreadNumRange());
				int body = this->visit(var->getBody());
				insert("JobExpr", id, thread_num_range, body);
				return id;
			}

			int visitLambdaExpr(const Ptr<const core::LambdaExpr>& var) override {
				int id = getFreshID(var);
				int type = this->visit(var->getType());
				int reference = this->visit(var->getReference());
				int definition = this->visit(var->getDefinition());
				insert("LambdaExpr", id, type, reference, definition);
				return id;
			}

			int visitLambdaReference(const Ptr<const core::LambdaReference>& var) override {
				int id = getFreshID(var);
				int type = this->visit(var->getType());
				const string& name = var->getName()->getValue();
				insert("LambdaReference", id, type, name);
				return id;
			}

			int visitMarkerExpr(const Ptr<const core::MarkerExpr>& var) override {
				// Special case: Markers are just piped through and have no ir.dl entry
				return this->visit(var->getSubExpression());
			}

			int visitTupleExpr(const Ptr<const core::TupleExpr>& var) override {
				int id = getFreshID(var);
				int expressions = this->visit(var->getExpressions());
				insert("TupleExpr", id, expressions);
				return id;
			}

			// -- Statement Nodes --

			int visitCompoundStmt(const Ptr<const core::CompoundStmt>& var) override {
				int id = getFreshID(var);
				insert("CompoundStmt", id);
				make_node_list(var);
				return id;
			}

			int visitDeclarationStmt(const Ptr<const core::DeclarationStmt>& var) override {
				int id = getFreshID(var);
				int declaration = this->visit(var->getDeclaration());
				int variable = this->visit(var->getVariable());
				insert("DeclarationStmt", id, declaration, variable);
				return id;
			}

			int visitIfStmt(const Ptr<const core::IfStmt>& var) override {
				int id = getFreshID(var);
				int condition = this->visit(var->getCondition());
				int then_body = this->visit(var->getThenBody());
				int else_body = this->visit(var->getElseBody());
				insert("IfStmt", id, condition, then_body, else_body);
				return id;
			}

			int visitBreakStmt(const Ptr<const core::BreakStmt>& var) override {
				int id = getFreshID(var);
				insert("BreakStmt", id);
				return id;
			}

			int visitContinueStmt(const Ptr<const core::ContinueStmt>& var) override {
				int id = getFreshID(var);
				insert("ContinueStmt", id);
				return id;
			}

			int visitForStmt(const Ptr<const core::ForStmt>& var) override {
				int id = getFreshID(var);
				int declaration = this->visit(var->getDeclaration());
				int end = this->visit(var->getEnd());
				int step = this->visit(var->getStep());
				int body = this->visit(var->getBody());
				insert("ForStmt", id, declaration, end, step, body);
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
				insert("ReturnStmt", id, return_decl);
				return id;
			}

			int visitSwitchStmt(const Ptr<const core::SwitchStmt>& var) override {
				int id = getFreshID(var);
				int switch_expr = this->visit(var->getSwitchExpr());
				int cases = this->visit(var->getCases());
				int default_case = this->visit(var->getDefaultCase());
				insert("SwitchStmt", id, switch_expr, cases, default_case);
				return id;
			}

			int visitThrowStmt(const Ptr<const core::ThrowStmt>& var) override {
				int id = getFreshID(var);
				int throw_expr = this->visit(var->getThrowExpr());
				insert("ThrowStmt", id, throw_expr);
				return id;
			}

			int visitTryCatchStmt(const Ptr<const core::TryCatchStmt>& var) override {
				int id = getFreshID(var);
				int body = this->visit(var->getBody());
				insert("TryCatchStmt", id, body);
				make_node_list(var);
				return id;
			}

			int visitWhileStmt(const Ptr<const core::WhileStmt>& var) override {
				int id = getFreshID(var);
				int condition = this->visit(var->getCondition());
				int body = this->visit(var->getBody());
				insert("WhileStmt", id, condition, body);
				return id;
			}

			// -- Support Nodes --

			int visitTypes(const Ptr<const core::Types>& types) override {
				int id = getFreshID(types);
				insert("Types", id);
				make_node_list(types);
				return id;
			}

			int visitParents(const Ptr<const core::Parents>& parents) override {
				int id = getFreshID(parents);
				insert("Parents", id);
				make_node_list(parents);
				return id;
			}

			int visitCatchClause(const Ptr<const core::CatchClause>& var) override {
				int id = getFreshID(var);
				int variable = this->visit(var->getVariable());
				int body = this->visit(var->getBody());
				insert("CatchClause", id, variable, body);
				return id;
			}

			int visitDeclaration(const Ptr<const core::Declaration>& decl) override {
				int id = getFreshID(decl);
				int type = this->visit(decl->getType());
				int initialization = this->visit(decl->getInitialization());
				insert("Declaration", id, type, initialization);
				return id;
			}

			int visitExpressions(const Ptr<const core::Expressions>& expressions) override {
				int id = getFreshID(expressions);
				insert("Expressions", id);
				make_node_list(expressions);
				return id;
			}

			int visitField(const Ptr<const core::Field>& field) override {
				int id = getFreshID(field);
				const string& name = field->getName()->getValue();
				int type = this->visit(field->getType());
				insert("Field", id, name, type);
				return id;
			}

			int visitFields(const Ptr<const core::Fields>& fields) override {
				int id = getFreshID(fields);
				insert("Fields", id);
				make_node_list(fields);
				return id;
			}

			int visitLambda(const Ptr<const core::Lambda>& lambda) override {
				int id = getFreshID(lambda);
				int type = this->visit(lambda->getType());
				int parameters = this->visit(lambda->getParameters());
				int body = this->visit(lambda->getBody());
				insert("Lambda", id, type, parameters, body);
				return id;
			}

			int visitLambdaBinding(const Ptr<const core::LambdaBinding>& binding) override {
				int id = getFreshID(binding);
				int reference = this->visit(binding->getReference());
				int lambda = this->visit(binding->getLambda());
				insert("LambdaBinding", id, reference, lambda);
				return id;
			}

			int visitLambdaDefinition(const Ptr<const core::LambdaDefinition>& def) override {
				int id = getFreshID(def);
				insert("LambdaDefinition", id);
				make_node_list(def);
				return id;
			}

			int visitMemberFunction(const Ptr<const core::MemberFunction>& fun) override {
				int id = getFreshID(fun);
				const string& name = fun->getName()->getValue();
				bool virtual_flag = fun->getVirtualFlag()->getValue();
				int implementation = this->visit(fun->getImplementation());
				insert("MemberFunction", id, name, virtual_flag, implementation);
				return id;
			}

			int visitMemberFunctions(const Ptr<const core::MemberFunctions>& member_funcs) override {
				int id = getFreshID(member_funcs);
				insert("MemberFunctions", id);
				make_node_list(member_funcs);
				return id;
			}

			int visitParameters(const Ptr<const core::Parameters>& params) override {
				int id = getFreshID(params);
				insert("Parameters", id);
				make_node_list(params);
				return id;
			}

			int visitParent(const Ptr<const core::Parent>& var) override {
				int id = getFreshID(var);
				bool vvirtual = var->getVirtual()->getValue();
				unsigned access_specifier_kind = var->getAccessSpecifierKind()->getValue();
				int type = this->visit(var->getType());
				insert("Parent", id, vvirtual, access_specifier_kind, type);
				return id;
			}

			int visitPureVirtualMemberFunction(const Ptr<const core::PureVirtualMemberFunction>& pvmf) override {
				int id = getFreshID(pvmf);
				const string& name = pvmf->getName()->getValue();
				int type = this->visit(pvmf->getType());
				insert("PureVirtualMemberFunction", id, name, type);
				return id;
			}

			int visitPureVirtualMemberFunctions(const Ptr<const core::PureVirtualMemberFunctions>& pvm_funcs) override {
				int id = getFreshID(pvm_funcs);
				insert("PureVirtualMemberFunctions", id);
				make_node_list(pvm_funcs);
				return id;
			}

			int visitSwitchCase(const Ptr<const core::SwitchCase>& var) override {
				int id = getFreshID(var);
				int guard = this->visit(var->getGuard());
				int body = this->visit(var->getBody());
				insert("SwitchCase", id, guard, body);
				return id;
			}

			int visitSwitchCases(const Ptr<const core::SwitchCases>& var) override {
				int id = getFreshID(var);
				insert("SwitchCases", id);
				make_node_list(var);
				return id;
			}

			int visitTagTypeDefinition(const Ptr<const core::TagTypeDefinition>& var) override {
				int id = getFreshID(var);
				insert("TagTypeDefinition", id);
				make_node_list(var);
				return id;
			}

			int visitTagTypeBinding(const Ptr<const core::TagTypeBinding>& var) override {
				int id = getFreshID(var);
				int tag = this->visit(var->getTag());
				int record = this->visit(var->getRecord());
				insert("TagTypeBinding", id, tag, record);
				return id;
			}


			// -- Unimplemented Nodes: Fail --

			int visitNode(const Ptr<const core::Node>& cur) override {
				assert_not_implemented() << "Unsupported node type: " << cur->getNodeType() << "\n";
				return 0;
			}

		private:

			void fill(souffle::tuple&) {}

			template<typename F, typename ... Rest>
			void fill(souffle::tuple& tuple, const F& first, const Rest& ... rest) {
				tuple << first;
				fill(tuple,rest...);
			}

			template<typename ... Args>
			void insert(const std::string& relationName, const Args& ... args ) {
				// get relation
				auto rel = analysis.getRelation(relationName);
				if (!rel) return;

				// insert data
				souffle::tuple tuple(rel);
				fill(tuple, args...);
				rel->insert(tuple);
			}

		};

		#undef make_node_list

	} // end anonymous namespace

	int extractFacts(souffle::Program& analysis, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer) {
		return FactExtractor<core::Pointer>(analysis,nodeIndexer).visit(root);
	}

	int extractAddressFacts(souffle::Program& analysis, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer) {
		return FactExtractor<core::Address>(analysis,nodeIndexer).visit(core::NodeAddress(root));
	}

	void checkForFailures(souffle::Program& analysis) {

		auto failures = analysis.getRelation("D474L06_utils_failure_dl_failure");
		assert_true(failures) << "Failure relation in analysis not found!";

		// extract failure messages
		std::vector<std::string> msgs;
		for(auto cur : *failures) {
			std::string msg;
			cur >> msg;
			msgs.push_back(msg);
		}

		// check if there have been failures
		if (msgs.empty()) return;

		// throw failure exception
		throw AnalysisFailure(msgs);

	}

} // end namespace framework
} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
