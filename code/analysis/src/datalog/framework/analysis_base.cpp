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

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace analysis {
namespace datalog {
namespace framework {

	namespace {

		using std::string;

		#define make_node_list(LOOP_OBJECT) do {                                           \
		                int counter = 0;                                                   \
		                for(const auto& cur : LOOP_OBJECT) {                               \
		                        insert("NodeList", id, counter++, visit(cur));             \
		                }                                                                  \
		        } while(0);


		class FactExtractor : public core::IRVisitor<int> {

			int node_counter = 0;
			souffle::Program& analysis;

		public:

			FactExtractor(souffle::Program& analysis)
				: core::IRVisitor<int>(true), analysis(analysis) {}

			int extractFacts(const core::NodePtr& rootNode) {
				return visit(rootNode);
			}

			// -- Type Nodes --

			int visitGenericType(const core::GenericTypePtr& type) override {
				int id = ++node_counter;
				const string& name = type->getName()->getValue();
				int parents = visit(type->getParents());
				int params = visit(type->getTypeParameter()); // TODO check this again
				insert("GenericType", id, name, parents, params);
				return id;
			}

			int visitTupleType(const core::TupleTypePtr& tuple) override {
				int id = ++node_counter;
				insert("TupleType", id);
				make_node_list(tuple);
				return id;
			}

			int visitFunctionType(const core::FunctionTypePtr& fun) override {
				int id = ++node_counter;
				int parameter_types = visit(fun->getParameterTypes());
				int return_type = visit(fun->getReturnType());
				uint kind = fun->getFunctionKind()->getValue();
				int instantiation_types = visit(fun->getInstantiationTypes());
				insert("FunctionType", id, parameter_types, return_type, kind, instantiation_types);
				return id;
			}

			int visitTypeVariable(const core::TypeVariablePtr& var) override {
				int id = ++node_counter;
				insert("TypeVariable", id, var->getVarName()->getValue());
				return id;
			}

			int visitVariadicTypeVariable(const core::VariadicTypeVariablePtr& var) override {
				int id = ++node_counter;
				const string& var_name = var->getVarName()->getValue();
				insert("VariadicTypeVariable", id, var_name);
				return id;
			}

			int visitGenericTypeVariable(const core::GenericTypeVariablePtr& var) override {
				int id = ++node_counter;
				const string& var_name = var->getVarName()->getValue();
				int type_parameter = visit(var->getTypeParameter());
				insert("GenericTypeVariable", id, var_name, type_parameter);
				return id;
			}

			int visitVariadicGenericTypeVariable(const core::VariadicGenericTypeVariablePtr& var) override {
				int id = ++node_counter;
				const string& var_name = var->getVarName()->getValue();
				int type_parameter = visit(var->getTypeParameter());
				insert("VariadicGenericTypeVariable", id, var_name, type_parameter);
				return id;
			}

			int visitNumericType(const core::NumericTypePtr& var) override {
				int id = ++node_counter;
				int node = visit(var->getValue());
				insert("NumericType", id, node);
				return id;
			}

			int visitTagType(const core::TagTypePtr& var) override {
				int id = ++node_counter;
				int tag = visit(var->getTag());
				int defintion = visit(var->getDefinition());
				insert("TagType", id, tag, defintion);
				return id;
			}

			int visitTagTypeReference(const core::TagTypeReferencePtr& var) override {
				int id = ++node_counter;
				const string& name = var->getName()->getValue();
				insert("TagTypeReference", id, name);
				return id;
			}

			int visitStruct(const core::StructPtr& var) override {
				int id = ++node_counter;
				const string& name = var->getName()->getValue();
				int fields = visit(var->getFields());
				int constructors = visit(var->getConstructors());
				int destructor = visit(var->getDestructor());
				bool destructor_virtual = var->getDestructorVirtual()->getValue();
				int member_functions = visit(var->getMemberFunctions());
				int pure_virtual_member_functions = visit(var->getPureVirtualMemberFunctions());
				int parents = visit(var->getParents());

				insert("Struct", id, name, fields, constructors, destructor, destructor_virtual,
				       member_functions, pure_virtual_member_functions, parents);

				return id;
			}

			int visitProgram(const core::ProgramPtr& var) override {
				int id = ++node_counter;
				insert("Program", id);
				make_node_list(var);
				return id;
			}

			int visitUnion(const core::UnionPtr& var) override {
				int id = ++node_counter;
				const string& name = var->getName()->getValue();
				int fields = visit(var->getFields());
				int constructors = visit(var->getConstructors());
				int destructor = visit(var->getDestructor());
				bool destructor_virtual = var->getDestructorVirtual()->getValue();
				int member_functions = visit(var->getMemberFunctions());
				int pure_virtual_member_functions = visit(var->getPureVirtualMemberFunctions());

				insert("Union", id, name, fields, constructors, destructor, destructor_virtual,
				       member_functions, pure_virtual_member_functions);

				return id;
			}

			// -- Expression Nodes --

			int visitLiteral(const core::LiteralPtr& var) override {
				int id = ++node_counter;
				int type = visit(var->getType());
				const string& string_value = var->getStringValue();
				insert("Literal", id, type, string_value);
				return id;
			}

			int visitVariable(const core::VariablePtr& var) override {
				int id = ++node_counter;
				int type = visit(var->getType());
				unsigned var_id = var->getId();
				insert("Variable", id, type, var_id);
				return id;
			}

			int visitCallExpr(const core::CallExprPtr& var) override {
				int id = ++node_counter;
				int function_expr = visit(var->getFunctionExpr());
				insert("CallExpr", id, function_expr);
				make_node_list(var);
				return id;
			}

			int visitBindExpr(const core::BindExprPtr& var) override {
				int id = ++node_counter;
				int parameters = visit(var->getParameters());
				int call = visit(var->getParameters());
				insert("BindExpr", id, parameters, call);
				return id;
			}

			int visitCastExpr(const core::CastExprPtr& var) override {
				int id = ++node_counter;
				int sub_expression = visit(var->getSubExpression());
				insert("CastExpr", id, sub_expression);
				return id;
			}

			int visitInitExpr(const core::InitExprPtr& var) override {
				int id = ++node_counter;
				int memory_expr = visit(var->getMemoryExpr());
				int init_exprs = visit(var->getInitExprs());
				insert("InitExpr", id, memory_expr, init_exprs);
				return id;
			}

			int visitJobExpr(const core::JobExprPtr& var) override {
				int id = ++node_counter;
				int thread_num_range = visit(var->getThreadNumRange());
				int body = visit(var->getBody());
				insert("JobExpr", id, thread_num_range, body);
				return id;
			}

			int visitLambdaExpr(const core::LambdaExprPtr& var) override {
				int id = ++node_counter;
				int type = visit(var->getType());
				int reference = visit(var->getReference());
				int definition = visit(var->getDefinition());
				insert("LambdaExpr", id, type, reference, definition);
				return id;
			}

			int visitLambdaReference(const core::LambdaReferencePtr& var) override {
				int id = ++node_counter;
				int type = visit(var->getType());
				const string& name = var->getName()->getValue();
				insert("LambdaReference", id, type, name);
				return id;
			}

			int visitMarkerExpr(const core::MarkerExprPtr& var) override {
				// Special case: Markers are just piped through and have no ir.dl entry
				return visit(var->getSubExpression());
			}

			int visitTupleExpr(const core::TupleExprPtr& var) override {
				int id = ++node_counter;
				int expressions = visit(var->getExpressions());
				insert("TupleExpr", id, expressions);
				return id;
			}

			// -- Statement Nodes --

			int visitCompoundStmt(const core::CompoundStmtPtr& var) override {
				int id = ++node_counter;
				insert("CompoundStmt", id);
				make_node_list(var);
				return id;
			}

			int visitDeclarationStmt(const core::DeclarationStmtPtr& var) override {
				int id = ++node_counter;
				int variable = visit(var->getVariable());
				int initialization = visit(var->getInitialization());
				insert("DeclarationStmt", id, variable, initialization);
				return id;
			}

			int visitIfStmt(const core::IfStmtPtr& var) override {
				int id = ++node_counter;
				int condition = visit(var->getCondition());
				int then_body = visit(var->getThenBody());
				int else_body = visit(var->getElseBody());
				insert("IfStmt", id, condition, then_body, else_body);
				return id;
			}

			int visitBreakStmt(const core::BreakStmtPtr& var) override {
				int id = ++node_counter;
				insert("BreakStmt", id);
				return id;
			}

			int visitContinueStmt(const core::ContinueStmtPtr& var) override {
				int id = ++node_counter;
				insert("ContinueStmt", id);
				return id;
			}

			int visitForStmt(const core::ForStmtPtr& var) override {
				int id = ++node_counter;
				int declaration = visit(var->getDeclaration());
				int end = visit(var->getEnd());
				int step = visit(var->getStep());
				int body = visit(var->getBody());
				insert("ForStmt", id, declaration, end, step, body);
				return id;
			}

			int visitGotoStmt(const core::GotoStmtPtr& var) override {
				assert_not_implemented() << "Goto statement not implemented!";
				return -1;
			}

			int visitLabelStmt(const core::LabelStmtPtr& var) override {
				assert_not_implemented() << "Label statement not implemented!";
				return -1;
			}

			int visitMarkerStmt(const core::MarkerStmtPtr& var) override {
				// Special case: Markers are just piped through and have no ir.dl entry
				return visit(var->getSubStatement());
			}

			int visitReturnStmt(const core::ReturnStmtPtr& var) override {
				int id = ++node_counter;
				int return_expr = visit(var->getReturnExpr());
				int return_var = visit(var->getReturnVar());
				insert("ReturnStmt", id, return_expr, return_var);
				return id;
			}

			int visitSwitchStmt(const core::SwitchStmtPtr& var) override {
				int id = ++node_counter;
				int switch_expr = visit(var->getSwitchExpr());
				int cases = visit(var->getCases());
				int default_case = visit(var->getDefaultCase());
				insert("SwitchStmt", id, switch_expr, cases, default_case);
				return id;
			}

			int visitThrowStmt(const core::ThrowStmtPtr& var) override {
				int id = ++node_counter;
				int throw_expr = visit(var->getThrowExpr());
				insert("ThrowStmt", id, throw_expr);
				return id;
			}

			int visitTryCatchStmt(const core::TryCatchStmtPtr& var) override {
				int id = ++node_counter;
				int body = visit(var->getBody());
				insert("TryCatchStmt", id, body);
				make_node_list(var);
				return id;
			}

			int visitWhileStmt(const core::WhileStmtPtr& var) override {
				int id = ++node_counter;
				int condition = visit(var->getCondition());
				int body = visit(var->getBody());
				insert("WhileStmt", id, condition, body);
				return id;
			}

			// -- Support Nodes --

			int visitTypes(const core::TypesPtr& types) override {
				int id = ++node_counter;
				insert("Types", id);
				make_node_list(types);
				return id;
			}

			int visitParents(const core::ParentsPtr& parents) override {
				int id = ++node_counter;
				insert("Parents", id);
				make_node_list(parents);
				return id;
			}

			int visitCatchClause(const core::CatchClausePtr& var) override {
				int id = ++node_counter;
				int variable = visit(var->getVariable());
				int body = visit(var->getBody());
				insert("CatchClause", id, variable, body);
				return id;
			}

			int visitExpressions(const core::ExpressionsPtr& expressions) override {
				int id = ++node_counter;
				insert("Expressions", id);
				make_node_list(expressions);
				return id;
			}

			int visitField(const core::FieldPtr& field) override {
				int id = ++node_counter;
				const string& name = field->getName()->getValue();
				int type = visit(field->getType());
				insert("Field", id, name, type);
				return id;
			}

			int visitFields(const core::FieldsPtr& fields) override {
				int id = ++node_counter;
				insert("Fields", id);
				make_node_list(fields);
				return id;
			}

			int visitLambda(const core::LambdaPtr& lambda) override {
				int id = ++node_counter;
				int type = visit(lambda->getType());
				int parameters = visit(lambda->getParameters());
				int body = visit(lambda->getBody());
				insert("Lambda", id, type, parameters, body);
				return id;
			}

			int visitLambdaBinding(const core::LambdaBindingPtr& binding) override {
				int id = ++node_counter;
				int reference = visit(binding->getReference());
				int lambda = visit(binding->getLambda());
				insert("LambdaBinding", id, reference, lambda);
				return id;
			}

			int visitLambdaDefinition(const core::LambdaDefinitionPtr& def) override {
				int id = ++node_counter;
				insert("LambdaDefinition", id);
				make_node_list(def);
				return id;
			}

			int visitMemberFunction(const core::MemberFunctionPtr& fun) override {
				int id = ++node_counter;
				const string& name = fun->getName()->getValue();
				bool virtual_flag = fun->getVirtualFlag()->getValue();
				int implementation = visit(fun->getImplementation());
				insert("MemberFunction", id, name, virtual_flag, implementation);
				return id;
			}

			int visitMemberFunctions(const core::MemberFunctionsPtr& member_funcs) override {
				int id = ++node_counter;
				insert("MemberFunctions", id);
				make_node_list(member_funcs);
				return id;
			}

			int visitParameters(const core::ParametersPtr& params) override {
				int id = ++node_counter;
				insert("Parameters", id);
				make_node_list(params);
				return id;
			}

			int visitParent(const core::ParentPtr& var) override {
				int id = ++node_counter;
				bool vvirtual = var->getVirtual()->getValue();
				unsigned access_specifier_kind = var->getAccessSpecifierKind()->getValue();
				int type = visit(var->getType());
				insert("Parent", id, vvirtual, access_specifier_kind, type);
				return id;
			}

			int visitPureVirtualMemberFunction(const core::PureVirtualMemberFunctionPtr& pvmf) override {
				int id = ++node_counter;
				const string& name = pvmf->getName()->getValue();
				int type = visit(pvmf->getType());
				insert("PureVirtualMemberFunction", id, name, type);
				return id;
			}

			int visitPureVirtualMemberFunctions(const core::PureVirtualMemberFunctionsPtr& pvm_funcs) override {
				int id = ++node_counter;
				insert("PureVirtualMemberFunctions", id);
				make_node_list(pvm_funcs);
				return id;
			}

			int visitSwitchCase(const core::SwitchCasePtr& var) override {
				int id = ++node_counter;
				int guard = visit(var->getGuard());
				int body = visit(var->getBody());
				insert("SwitchCase", id, guard, body);
				return id;
			}

			int visitSwitchCases(const core::SwitchCasesPtr& var) override {
				int id = ++node_counter;
				insert("SwitchCases", id);
				make_node_list(var);
				return id;
			}

			int visitTagTypeDefinition(const core::TagTypeDefinitionPtr& var) override {
				int id = ++node_counter;
				insert("TagTypeDefinition", id);
				make_node_list(var);
				return id;
			}

			int visitTagTypeBinding(const core::TagTypeBindingPtr& var) override {
				int id = ++node_counter;
				int tag = visit(var->getTag());
				int record = visit(var->getRecord());
				insert("TagTypeBinding", id, tag, record);
				return id;
			}


			// -- Unimplemented Nodes: Fail --

			int visitNode(const core::NodePtr& cur) override {
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

	int extractFacts(souffle::Program& analysis, const core::NodePtr& node) {
		return FactExtractor(analysis).visit(node);
	}

} // end namespace framework
} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
