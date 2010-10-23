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

#include "ast_visitor.h"
#include "numeric_cast.h"
#include "string_utils.h"

#include <ostream>
#include <sstream>

using namespace insieme::core;

class DotVisitor : public ASTVisitor<> {

	std::ostream& out;
public:
	DotVisitor(std::ostream& out) : out(out) { }

	template <class ElemTy>
	void visitChildList(const std::vector<ElemTy>& children, const insieme::core::NodePtr& parent, const std::string& labelPrefix) {
		unsigned short elemCount=1;
		std::for_each(children.begin(), children.end(), [&](const ElemTy& curr) {
			out << (size_t)&*parent << " -> " << (size_t)&*curr <<
					"[label=\""<< labelPrefix <<
						(children.size() > 1 ? ("_" + insieme::utils::numeric_cast<std::string>(elemCount++)) : "") << "\"];" << std::endl;
		});
	}

	void visitGenericType(const insieme::core::GenericTypePtr& genTy) {
		std::ostringstream ss;
		// special handling for integer type parameters
		if(!genTy->getIntTypeParameter().empty()) {
			ss << "<" << join(", ", genTy->getIntTypeParameter(), [ ](std::ostream& out, const IntTypeParam& cur) {
				out << (cur.isConcrete() ? insieme::utils::numeric_cast<std::string>(cur.getValue()) : ""+cur.getSymbol());
			}) << ">";
		}
		out << (size_t)&*genTy << "\t[label=\"" << genTy->getFamilyName() << " " << ss.str() <<"\", shape=ellipse];" << std::endl;
		visitChildList(genTy->getTypeParameter(), genTy, "typeVar");
	}

	void visitFunctionType(const FunctionTypePtr& funcType) {
		out << (size_t)&*funcType << "\t[label=\"funcType\", shape=ellipse]" << std::endl;
		visitChildList(toVector(funcType->getReturnType()), funcType, "retTy");
		visitChildList(toVector(funcType->getArgumentType()), funcType, "argTy");
	}

	void visitTupleType(const TupleTypePtr& tupleTy) {
		out << (size_t)&*tupleTy << "\t[label=\"tupleType\", shape=ellipse]" << std::endl;
		visitChildList(tupleTy->getElementTypes(), tupleTy, "elemTy");
	}

	void visitNamedCompositeType(const NamedCompositeTypePtr& compTy) {
		std::string name;
		if(dynamic_pointer_cast<const StructType>(compTy))
			name = "structType";
		else
			name = "unionType";

		out << (size_t)&*compTy << "\t[label=\"" << name << "\", shape=ellipse]" << std::endl;
		// TODO
	}

	void visitStatement(const insieme::core::StatementPtr& stmt) {
		out << (size_t)&*stmt << "\t[label=\"STMT\", shape=box, style=filled]" << std::endl;
		const Node::ChildList& children = stmt->getChildList();
		std::for_each(children.begin(), children.end(), [&](const NodePtr& curr) {
			out << (size_t)&*stmt << " -> " << (size_t)&*curr << std::endl;
		});
	}

	void visitNode(const insieme::core::NodePtr& node) {
		out << (size_t)&*node << " [label=\"" << node->getNodeCategory() << "\"]" << std::endl;
		const Node::ChildList& children = node->getChildList();
		std::for_each(children.begin(), children.end(), [&](const NodePtr& curr) {
			out << (size_t)&*node << " -> " << (size_t)&*curr << std::endl;
		});
	}
};

void printDotGraph(const insieme::core::NodePtr& root, std::ostream& out) {
	DotVisitor dv(out);
	out << "digraph inspire {" << std::endl;
	insieme::core::visitAllOnce(root, dv);
	out << "}" << std::endl;
}
