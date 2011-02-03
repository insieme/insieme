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

#pragma once

#include <sstream>
#include <assert.h>
#include <unordered_map>
#include <memory>

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/statements.h"
#include "insieme/core/program.h"
#include "insieme/core/types.h"

#include "insieme/c_info/naming.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/hash_utils.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/simple_backend/simple_backend.h"
#include "insieme/simple_backend/code_management.h"
#include "insieme/simple_backend/name_manager.h"
#include "insieme/simple_backend/type_manager.h"
#include "insieme/simple_backend/function_manager.h"
#include "insieme/simple_backend/job_manager.h"

namespace insieme {
namespace simple_backend {

using namespace core;


// TODO more sane dependency handling / move forward declaration
class StmtConverter;
class VariableManager;


/**
 * A map from Entry points to Code sections returned by ConversionContext::convert.
 * Can be printed to any output stream
 */
class ConvertedCode : public TargetCode {

	/**
	 * A map of code fragments this converted code is consisting of.
	 */
	utils::map::PointerMap<ExpressionPtr, CodePtr> codeFragments;

public:

	/**
	 * A constructor for this class.
	 */
	ConvertedCode(const ProgramPtr& source) : TargetCode(source) { }

	/**
	 * This method allows to print the result to an output stream.
	 */
	virtual std::ostream& printTo(std::ostream& out) const;

	/**
	 * Adds a code fragment to the internally maintained list of fragments.
	 *
	 * @param source the source for this particular fragment
	 * @param fragment the the target code fragment to be stored
	 */
	void addFragment(const ExpressionPtr& source, CodePtr& fragment);

};

/**
 * Stores the persistent state objects required to perform a simple_backend conversion.
 * This includes a NameGenerator, a FunctionManager and a TypeManager.
 */
class Converter {

	// A list of managers required for the conversion process

	// TODO: think about replacing pure pointer with shared pointer
	StmtConverter* stmtConverter;
	NameManager* nameManager;
	TypeManager* typeManager;
	VariableManager* variableManager;
	FunctionManager* functionManager;
	JobManager* jobManager;
	NodeManager* nodeManager;

public:

	/**
	 * A default constructor for this converter. All internal managers will be set to null.
	 */
	Converter() {}

	/**
	 * A constructor allowing the explicit creation of a converter of this type. The given manager are used for
	 * the actual conversion.
	 *
	 * @param stmtConverter the actual converter implementation handling statements and expressions
	 * @param nameManager the manager used to pick and maintain names for the generated constructs (types, functions, variables, ...)
	 * @param typeManager the manager controlling the generation of types
	 * @param varManager the manager used for managing the scope of variables
	 * @param funcMan the function manager handling the creation of closures and their invocation
	 * @param nodeManager the node manager to be used for creating and maintaining intermediate IR nodes
	 */
	Converter(StmtConverter& stmtConverter, NameManager& nameManager, TypeManager& typeManager, VariableManager& varManager, FunctionManager& funcMan, NodeManager& nodeManager)
		: stmtConverter(&stmtConverter), nameManager(&nameManager), typeManager(&typeManager), variableManager(&varManager), functionManager(&funcMan), nodeManager(&nodeManager) { }


	TargetCodePtr convert(const core::ProgramPtr& prog);

	StmtConverter& getStmtConverter() {
		assert(stmtConverter);
		return *stmtConverter;
	}

	void setStmtConverter(StmtConverter* converter) {
		stmtConverter = converter;
	}

	NameManager& getNameManager() {
		assert(nameManager);
		return *nameManager;
	}

	void setNameManager(NameManager* manager) {
		nameManager = manager;
	}

	TypeManager& getTypeManager() {
		assert(typeManager);
		return *typeManager;
	}

	void setTypeManager(TypeManager* manager) {
		typeManager = manager;
	}

	VariableManager& getVariableManager() {
		assert(variableManager);
		return *variableManager;
	}

	void setVariableManager(VariableManager* manager) {
		variableManager = manager;
	}

	FunctionManager& getFunctionManager() {
		assert(functionManager);
		return *functionManager;
	}

	void setFunctionManager(FunctionManager* manager) {
		functionManager = manager;
	}

	JobManager& getJobManager() {
		assert(jobManager);
		return *jobManager;
	}

	void setJobManager(JobManager* manager) {
		jobManager = manager;
	}

	NodeManager& getNodeManager() {
		assert(nodeManager);
		return *nodeManager;
	}

	void setNodeManager(NodeManager* manager) {
		nodeManager = manager;
	}

	const lang::BasicGenerator& getLangBasic() {
		assert(nodeManager);
		return nodeManager->basic;
	}

};

class VariableManager {

public:

	enum MemoryLocation {
		NONE, 	/* < in case the variable is not referencing a memory cell */
		STACK, 	/* < the variable references a memory cell on the stack */
		HEAP 	/* < the variable references a memory cell on the heap */
	};

	struct VariableInfo {
		MemoryLocation location;
	};

private:

	typedef std::unordered_map<VariablePtr, VariableInfo, hash_target<VariablePtr>, equal_target<VariablePtr>> VariableInfoMap;

	VariableInfoMap variableMap;

public:

	VariableManager() : variableMap() {};

	const VariableInfo& getInfo(const VariablePtr& variable) const;
	void addInfo(const VariablePtr& variable, const VariableInfo& info);
	void removeInfo(const VariablePtr& variable);
	bool hasInfoFor(const VariablePtr& variable) const;

};



namespace detail {

	/**
	 * This class allows the conversion visitor to use special formats when printing the call of a specific function.
	 * For instance, this formatter allows to write the add operation + in infix notation.
	 */
	class Formatter {
	public:

		/**
		 * Performs the actual code formating. This method is pure abstract and
		 * has to be implemented within sub-classes.
		 *
		 * @param converter the converter and its context using this formatter
		 * @param call the call expression to be handled
		 */
		virtual void format(StmtConverter& converter, CodeStream& cStr, const CallExprPtr& call) =0;

	};

	/**
	 * Since formatter instances are polymorthic, they need to be handled via pointer or
	 * references. Further, the memory management needs to be considered. Therefore, formatter
	 * should be passed using this pointer type, which is based on a shared pointer.
	 */
	typedef std::shared_ptr<Formatter> FormatterPtr;

	/**
	 * The Lambda Formatter is a concrete generic implementation of the Formatter class. It uses
	 * a lambda expression passed in during the construction to format the actual output.
	 */
	template<typename Lambda>
	class LambdaFormatter : public Formatter {

		/**
		 * The lambda used to perform the formatting.
		 */
		Lambda lambda;

	public:

		/**
		 * Creates a new instance of this type printing the given literal using the
		 * given lambda during the formating.
		 *
		 * @param literal the literal to be handled by this formatter
		 * @param lambda the lambda performing the actual formatting
		 */
		LambdaFormatter(Lambda lambda) : lambda(lambda) {}

		/**
		 * Conducts the actual formatting of the given call expression.
		 *
		 * @param converter the converter and its context using this formatter
		 * @param call the call expression to be handled
		 */
		virtual void format(StmtConverter& converter, CodeStream& cStr, const CallExprPtr& call) {
			lambda(converter, cStr, call);
		}
	};

	/**
	 * A utility function to create LiteralFormatter instances without the need of
	 * specifying generic types. Those types will be inferred automatically.
	 *
	 * @param literal the literal to be handled by the requested formatter
	 * @return a new formatter handling the call expressions using the given lambda
	 */
	template<typename Lambda>
	FormatterPtr make_formatter(Lambda lambda) {
		return std::make_shared<LambdaFormatter<Lambda>>(lambda);
	}

	// define a type for handling formatter
	//typedef std::unordered_map<LiteralPtr, FormatterPtr, hash_target<LiteralPtr>, equal_target<LiteralPtr>> FormatTable;
	typedef std::unordered_map<string, FormatterPtr> FormatTable;

	// a forward declaration for a method assembling formater tables
	FormatTable initFormatTable(const lang::BasicGenerator&);

}

/** Central simple_backend conversion class, visits IR nodes and generates C code accordingly.
 ** */
class StmtConverter : private ASTVisitor<>, private boost::noncopyable {
protected:
	/**
	 * A reference to the central container maintaining all the instances of the required manager.
	 */
	Converter& cc;

	/**
	 * A pointer to the code fragment currently produced by this converter.
	 */
	CodePtr defCodePtr;

private:	

	/**
	 * The table handling operator specific formatting rules.
	 */
	detail::FormatTable formats;

//	template<typename Functor>
//	void runWithCodeStream(CodeStream& cs, Functor f) {
//		CodeStream& oldCodeStream = cStr;
//		cStr = cs;
//		f();
//		cs = oldCodeStream;
//	}

public:

	StmtConverter(Converter& context) : ASTVisitor<>(false), cc(context), formats(detail::initFormatTable(context.getLangBasic())) { };

	CodePtr getCode() const {
		return defCodePtr;
	}

	CodeStream& getCodeStream() const {
		return defCodePtr->getCodeStream();
	}

	Converter& getConversionContext() const {
		return cc;
	}

	/**
	 * Instructs this statement convert to process the given IR node and append
	 * the results to the given code fragment.
	 *
	 * @param node the node to be processes
	 * @param fragment the code fragment the code should be appended to
	 */
	void convert(const NodePtr& node, CodePtr& fragment) {
		// replace target-code fragment
		CodePtr current = defCodePtr;
		defCodePtr = fragment;

		// process subtree
		visit(node);

		// reset to old code fragment
		defCodePtr = current;
	}

	/**
	 * Converts the given node and writes the result into the current code fragment.
	 *
	 * @param node the node to be processes
	 */
	void convert(const NodePtr& node) {
		assert(defCodePtr);

		// process node
		visit(node);
	}

protected:

	void visitNode(const NodePtr& node) {
		getCodeStream() << "<?>" << toString(*node) << "</?>";
	}

	void visitProgram(const ProgramPtr&) {
		assert(0 && "ConvertVisitor should never encounter program node");
	}
	
	////////////////////////////////////////////////////////////////////////// Statements

	void visitBreakStmt(const BreakStmtPtr&) {
		getCodeStream() << "break";
	}

	void visitCompoundStmt(const CompoundStmtPtr& ptr);

	void visitContinueStmt(const ContinueStmtPtr&) {
		getCodeStream() << "continue";
	}

	void visitDeclarationStmt(const DeclarationStmtPtr& ptr);

	void visitForStmt(const ForStmtPtr& ptr);

	void visitIfStmt(const IfStmtPtr& ptr);

	void visitWhileStmt(const WhileStmtPtr& ptr);

	void visitReturnStmt(const ReturnStmtPtr& ptr);

	void visitSwitchStmt(const SwitchStmtPtr& ptr);


	////////////////////////////////////////////////////////////////////////// Expressions

	void visitCallExpr(const CallExprPtr& ptr);

	void visitCaptureInitExpr(const CaptureInitExprPtr& ptr);

	void visitCaptureInitExprInternal(const CaptureInitExprPtr& ptr, bool directCall);

	void visitCastExpr(const CastExprPtr& ptr);

	void visitJobExpr(const JobExprPtr& ptr);

	void visitLambdaExpr(const LambdaExprPtr& ptr);

	void visitLiteral(const LiteralPtr& ptr);

	void visitMarkerExpr(const MarkerExprPtr& ptr);

	void visitMarkerStmt(const MarkerStmtPtr& ptr);

	void visitStructExpr(const StructExprPtr& ptr);

	void visitUnionExpr(const UnionExprPtr& ptr);

	void visitTupleExpr(const TupleExprPtr& ptr) {
		// TODO: replace this with a C99 solution
		// TODO check when to use ref()/cref()
		CodeStream& cStr = getCodeStream();
		cStr << "std::make_tuple(";
		auto exps = ptr->getExpressions();
		if(exps.size() > 0) {
			visit(exps.front());
			for_each(exps.cbegin()+1, exps.cend(), [&](const ExpressionPtr& cur) {
				cStr << ", ";
				this->visit(cur);
			});
		}
		cStr << ")";
	}

	void visitMemberAccessExpr(const MemberAccessExprPtr& ptr);

	void visitVariable(const VariablePtr& ptr);

	void visitVectorExpr(const VectorExprPtr& ptr);

};


} // namespace simple_backend
} // namespace insieme
