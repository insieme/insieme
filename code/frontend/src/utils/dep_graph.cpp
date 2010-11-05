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

#define __STDC_CONSTANT_MACROS
#define __STDC_LIMIT_MACROS
#include "insieme/frontend/utils/dep_graph.h"

namespace insieme {
namespace frontend {
namespace utils {

std::ostream& operator<<(std::ostream& out, const clang::FunctionDecl* funcDecl) {
	return out << funcDecl->getNameAsString() << "(" << funcDecl->param_size() << ")";
}

std::ostream& operator<<(std::ostream& out, const clang::Type* type) {
	if(const clang::TagType* tagType = dyn_cast<const clang::TagType>(type))
		return out << tagType->getDecl()->getNameAsString();
	return out;
}

template <>
void DependencyGraph<const clang::Type*>::Handle(const clang::Type* type, const DependencyGraph<const clang::Type*>::VertexTy& v) {
	using namespace clang;

	assert(isa<const TagType>(type));

	const TagType* tagType = dyn_cast<const TagType>(type);
	RecordDecl* tag = dyn_cast<RecordDecl>(tagType->getDecl());
	for(RecordDecl::field_iterator it=tag->field_begin(), end=tag->field_end(); it != end; ++it) {
		const Type* fieldType = (*it)->getType().getTypePtr();
		if( const PointerType *ptrTy = dyn_cast<PointerType>(fieldType) )
			fieldType = ptrTy->getPointeeType().getTypePtr();
		else if( const ReferenceType *refTy = dyn_cast<ReferenceType>(fieldType) )
			fieldType = refTy->getPointeeType().getTypePtr();

		if( const TagType* tagTy = dyn_cast<TagType>(fieldType) ) {
			assert(isa<RecordDecl>(tagTy->getDecl()));
			addNode( tagTy, &v );
		}
	}
}

struct CallExprVisitor: public clang::StmtVisitor<CallExprVisitor> {

	typedef std::set<const clang::FunctionDecl*> CallGraph;
	CallGraph callGraph;

	CallExprVisitor() { }

	CallGraph getCallGraph(const clang::FunctionDecl* func) {
		assert(func->hasBody() && "Function in the dependency graph has no body");

		Visit(func->getBody());
		return callGraph;
	}

	void VisitCallExpr(clang::CallExpr* callExpr) {
		const clang::FunctionDecl* def;
		if(callExpr->getDirectCallee()->hasBody(def))
			callGraph.insert(def);
	}

	void VisitStmt(clang::Stmt* stmt) {
		std::for_each(stmt->child_begin(), stmt->child_end(),
			[ this ](clang::Stmt* curr) {
				if(curr) this->Visit(curr);
			});
	}
};

template <>
void DependencyGraph<const clang::FunctionDecl*>::Handle(const clang::FunctionDecl* func, const DependencyGraph<const clang::FunctionDecl*>::VertexTy& v) {
	CallExprVisitor callExprVis;
	CallExprVisitor::CallGraph&& graph = callExprVis.getCallGraph(func);

	std::for_each(graph.begin(), graph.end(),
			[ this, v ](const clang::FunctionDecl* currFunc) { this->addNode(currFunc, &v); }
	);
}

}
}
}
