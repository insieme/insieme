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

#include <strstream>
#include <assert.h>
#include <unordered_map>

#include <boost/algorithm/string/replace.hpp>

#include "ast_node.h"
#include "ast_visitor.h"
#include "statements.h"
#include "program.h"
#include "definition.h"

#include "container_utils.h"
#include "hash_utils.h"

namespace insieme {
namespace simple_backend {

using namespace ::insieme::core;

class CodeStream {
public:
	struct IndR { }; static IndR indR;
	struct IndL { }; static IndL indL;

private:
	std::stringstream ss;
	string indentString;

	template<typename T>
	friend CodeStream& operator<<(CodeStream& cstr, T& param);
	
	template<typename T>
	void append(T& param) {
		std::stringstream ssTmp;
		// TODO fix operator lookup
		//ssTmp << param;
		string tmp = ssTmp.str();
		boost::replace_all(tmp, "\n", string("\n") + indentString);
		ss << tmp;
	}
	void append(IndR param) {
		indentString += "\t";
	}
	void append(IndL param) {
		assert(indentString.length()>0 && "Trying to indent below level 0");
		indentString = indentString.substr(0, indentString.length()-1);
	}

public:
	CodeStream() : indentString("") {
	}

};

template<typename T>
CodeStream& operator<<(CodeStream& cstr, T& param) {
	cstr.append(param);
	return cstr;
}

class TypeConverter : public ASTVisitor<TypeConverter, string> {
public:
	string visitStructType(const StructTypePtr& ptr) {
		string ret = "struct " + ptr->getName() + "{\n";
		for_each(ptr->getEntries(), [](const NamedCompositeType::Entry& entry) {
			//entry.first
		});
	}
};

class NameGenerator {
	unsigned long num;

	std::unordered_map<NodePtr, string, hash_target<NodePtr>, equal_target<NodePtr>> nameMap; 

	//string getName(const NodePtr& ptr, const char* fragment) {
	//	auto it = nameMap.find(ptr);
	//	if(it != nameMap.end()) return string("__insieme_") + fragment + "_" + it->second;
	//	string
	//	nameMap.insert(make_pair())
	//} 
};

class ConvertVisitor : public ASTVisitor<ConvertVisitor> {
	CodeStream cStr;
	TypeConverter typeConv;

	void printTypeName(const TypePtr& typ) {
		// TODO print C type name for specified type to cStr
	}

public:
	ConvertVisitor() {};

	//void visitNode(const NodePtr& node) {
	//	std::cout << *node << std::endl;
	//}

	//void visitProgram(const ProgramPtr& ptr) {
	//	for_each(ptr->getDefinitions(), [](const DefinitionPtr& def) {
	//		def->getType();
	//	});
	//}
	
	////////////////////////////////////////////////////////////////////////// Statements

	void visitBreakStmt(const BreakStmtPtr&) {
		cStr << "break;\n";
	}

	void visitCompoundStmt(const CompoundStmtPtr& ptr) {
		cStr << "{" << CodeStream::indR << "\n";
		for_each(ptr->getChildList(), [&, this](const NodePtr& ptr) { this->visit(ptr); });
		cStr << CodeStream::indL << "}" << "\n";
	}

	void visitContinueStmt(const ContinueStmtPtr& ptr) {
		cStr << "continue;\n";
	}

	void visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
		cStr << ptr->getVarExpression()->getType()->getName() << " = ";
		visit(ptr->getInitialization());
		cStr << ";\n";
	}

	void visitForStmt(const ForStmtPtr& ptr) {
		auto decl = ptr->getDeclaration();
		auto ident = decl->getVarExpression()->getIdentifier();
		cStr << "for(";
		visit(decl);
		cStr << "; " << ident << " < ";
		visit(ptr->getEnd());
		cStr << "; " << ident << " += "; 
		visit(ptr->getStep());
		cStr << ") ";
		visit(ptr->getBody());
	}

	void visitIfStmt(const IfStmtPtr& ptr) {
		cStr << "if(";
		visit(ptr->getCondition());
		cStr << ") ";
		visit(ptr->getThenBody());
		cStr << "else ";
		visit(ptr->getElseBody());
	}

	void visitReturnStmt(const ReturnStmtPtr& ptr) {
		cStr << "return;\n";
	}

	void visitSwitchStmt(const SwitchStmtPtr& ptr) {
		cStr << "switch(";
		visit(ptr->getSwitchExpr());
		cStr << ") {\n";
		for_each(ptr->getCases(), [&](const SwitchStmt::Case& curCase) { // GCC sucks
			this->visit(curCase.first);
			this->cStr << ":" << CodeStream::indR << "\n";
			this->visit(curCase.second);
			this->cStr << "break;" << CodeStream::indL << "\n";
		});
		cStr << "}\n";
	}

	void visitWhileStmt(const WhileStmtPtr& ptr) {
		cStr << "while(";
		visit(ptr->getCondition());
		cStr << ") ";
		visit(ptr->getBody());
	}

	////////////////////////////////////////////////////////////////////////// Expressions

	void visitCallExpr(const CallExprPtr& ptr) {
		const std::vector<ExpressionPtr>& args = ptr->getArguments();
		visit(ptr->getFunctionExpr());
		cStr << "(";
		if(args.size()>0) {
			visit(args.front());
			for_each(args.cbegin()+1, args.cend(), [&, this](const ExpressionPtr& curArg) {
				this->cStr << ", ";
				this->visit(curArg);
			});
		}
		cStr << ")";
	}

	void visitCastExpr(const CastExprPtr& ptr) {
		cStr << "((";
		printTypeName(ptr->getType());
		cStr << ")(";
		visit(ptr->getSubExpression());
		cStr << "))";
	}

	void visitJobExpr(const JobExprPtr& ptr) {
		ptr->getLocalDecls();
	}

};

} // namespace simple_backend
} // namespace insieme