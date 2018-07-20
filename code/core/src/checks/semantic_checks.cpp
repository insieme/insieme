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
 */

#include "insieme/core/checks/semantic_checks.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/annotations/default_delete.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/types/type_variable_deduction.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace checks {

	OptionalMessageList DefaultedDeletedPreTUMarkerCheck::visitNode(const NodeAddress& node) {
		OptionalMessageList res;
		IRBuilder builder(node->getNodeManager());

		if(annotations::isMarkedDefaultedPreTU(node)) {
			add(res, Message(node, EC_SEMANTIC_DEFAULTED_BODY_MARKER, "Found defaulted node annotation.", Message::ERROR));
		}
		if(annotations::isMarkedDeletedPreTU(node)) {
			add(res, Message(node, EC_SEMANTIC_DELETED_BODY_MARKER, "Found deleted node annotation.", Message::ERROR));
		}

		if(auto compound = node.getAddressedNode().isa<CompoundStmtPtr>()) {
			if(compound == builder.getDefaultedBodyPreTUMarker()) {
				add(res, Message(node, EC_SEMANTIC_DEFAULTED_BODY_MARKER, "Found defaulted body marker.", Message::ERROR));
			}
			if(compound == builder.getDeletedBodyPreTUMarker()) {
				add(res, Message(node, EC_SEMANTIC_DELETED_BODY_MARKER, "Found deleted body marker.", Message::ERROR));
			}
		}

		return res;
	}

	OptionalMessageList ForLoopSemanticsCheck::visitForStmt(const ForStmtAddress& curfor) {
		OptionalMessageList res;
		auto& mgr = curfor->getNodeManager();
		IRBuilder builder(mgr);

		// check body for free break/return
		core::visitDepthFirstPrunable(curfor->getBody(), [&](const core::NodeAddress& cur) -> bool {
			if(cur.isa<core::BreakStmtAddress>()) {
				add(res, Message(cur, EC_SEMANTIC_FREE_BREAK_INSIDE_FOR_LOOP,
				                 format("Free break statements are not allowed inside for loops. Consider while loop instead."), Message::ERROR));
			}
			if(cur.isa<core::ReturnStmtAddress>()) {
				add(res, Message(cur, EC_SEMANTIC_FREE_RETURN_INSIDE_FOR_LOOP,
				                 format("Free return statements are not allowed inside for loops. Consider while loop instead."), Message::ERROR));
			}

			if(cur.isa<core::LambdaExprAddress>() || cur.isa<core::ForStmtAddress>() || cur.isa<core::WhileStmtAddress>()
			   || cur.isa<core::SwitchStmtAddress>()) {
				return true;
			} else {
				return false;
			}
		});

		return res;
	}

	namespace {
		class ReturnStmtCheckVisitor : public IRVisitor<bool, Pointer, bool> {
			bool visitReturnStmt(const ReturnStmtPtr&, bool) override {
				LOG(DEBUG) << "Visit return!\n";
				return true;
			}

			bool visitThrowStmt(const ThrowStmtPtr&, bool) override {
				LOG(DEBUG) << "Visit throw!\n";
				return true;
			}

			bool visitIfStmt(const IfStmtPtr& ifst, bool inInfiniteLoop) override {
				LOG(DEBUG) << "Visit if ---- \n" << dumpColor(ifst) << "\n-----";
				bool ifSide = visit(ifst->getThenBody(), inInfiniteLoop);
				bool elseSide = visit(ifst->getElseBody(), inInfiniteLoop);
				return inInfiniteLoop ? (ifSide || elseSide) : (ifSide && elseSide);
			}

			bool visitSwitchStmt(const SwitchStmtPtr& switchst, bool inInfiniteLoop) override {
				LOG(DEBUG) << "Visit switch ---- \n" << dumpColor(switchst) << "\n-----";
				bool ok = true;
				auto cases = switchst->getCases();
				for(auto switchcase : cases) {
					bool caseOk = visit(switchcase->getBody(), inInfiniteLoop);
					ok = inInfiniteLoop ? (ok || caseOk) : (ok && caseOk);
				}
				bool defaultOk = visit(switchst->getDefaultCase(), inInfiniteLoop);
				ok = inInfiniteLoop ? (ok || defaultOk) : (ok && defaultOk);
				return ok;
			}

			bool visitWhileStmt(const WhileStmtPtr& whileStmt, bool) override {
				LOG(DEBUG) << "Visit while ---- \n" << dumpColor(whileStmt) << "\n-----";
				auto cond = whileStmt->getCondition();
				bool infiniteLoop = false;
				try {
					auto constraint = arithmetic::toConstraint(cond);
					infiniteLoop = constraint.isValid();
				} catch(const arithmetic::NotAConstraintException&) { /* not a constraint, not our problem */ }
				LOG(DEBUG) << " --> infinite? " << infiniteLoop;
				return visit(whileStmt->getBody(), infiniteLoop);
			}

			bool visitCompoundStmt(const CompoundStmtPtr& comp, bool inInfiniteLoop) override {
				LOG(DEBUG) << "Visit compound ---- \n" << dumpColor(comp) << "\n-----";
				auto elems = comp->getStatements();
				if(elems.empty()) { return false; }
				for(auto i = elems.cend() - 1; i != elems.cbegin(); --i) {
					bool r = visit(*i, inInfiniteLoop);
					if(r) { return true; }
				}
				return visit(elems.front(), inInfiniteLoop);
			}
		};
	}

	OptionalMessageList MissingReturnStmtCheck::visitLambdaExpr(const LambdaExprAddress& lambdaExpr) {
		OptionalMessageList res;

		auto& nodeMan = lambdaExpr->getNodeManager();
		auto& basic = nodeMan.getLangBasic();

		// check non-unit return types
		if(!basic.isUnit(lambdaExpr->getFunctionType()->getReturnType())) {
			// skip constructors and destructors
			const auto kind = lambdaExpr->getFunctionType()->getKind();
			if(kind != FK_CONSTRUCTOR && kind != FK_DESTRUCTOR) {
				LOG(DEBUG) << " --> engaging check\n";
				ReturnStmtCheckVisitor checker;
				if(!checker.visit(lambdaExpr->getBody(), false)) {
					LOG(DEBUG) << "MissingReturnStmtCheck failed for: \n" << dumpColor(lambdaExpr);
					VLOG(1) << " -> Text dump:\n" << dumpText(lambdaExpr);
					add(res, Message(lambdaExpr, EC_SEMANTIC_MISSING_RETURN_STMT, format("Not all control paths of non-unit lambdaExpr return a value."),
					                 Message::ERROR));
				}
			}
		}

		return res;
	}

	OptionalMessageList ValidInitExprMemLocationCheck::visitInitExpr(const InitExprAddress& initExpr) {
		OptionalMessageList res;

		auto memLocExpr = initExpr.getAddressedNode()->getMemoryExpr();
		auto& refExt = initExpr->getNodeManager().getLangExtension<lang::ReferenceExtension>();
		auto& basic = initExpr->getNodeManager().getLangBasic();

		if(refExt.isCallOfRefCast(memLocExpr)) memLocExpr = analysis::getArgument(memLocExpr, 0);
		if(refExt.isCallOfRefDeref(memLocExpr)) memLocExpr = analysis::getArgument(memLocExpr, 0);

		if(!memLocExpr.isa<VariablePtr>() && !memLocExpr.isa<LiteralPtr>() && !refExt.isCallOfRefTemp(memLocExpr) && !refExt.isCallOfRefNew(memLocExpr)
		   && !refExt.isCallOfRefMemberAccess(memLocExpr) && !refExt.isCallOfRefDecl(memLocExpr)
		   && !analysis::isCallOf(memLocExpr, basic.getCompositeMemberAccess())) {
			add(res,
				Message(initExpr, EC_SEMANTIC_INVALID_INIT_MEMLOC, format("InitExpr must initialize memory of a variable, literal, member, or temporary "
				                                                          "generated by ref_temp or ref_new or referenced by ref_decl.\n -> got: %s of type %s",
				                                                          *memLocExpr, *memLocExpr->getType()),
				        Message::ERROR));
		}

		return res;
	}

	OptionalMessageList ValidDeclarationCheck::visitDeclaration(const DeclarationAddress& decl) {
		OptionalMessageList res;

		// if it is not a materializing declaration
		if (!analysis::isMaterializingDecl(decl)) {

			// the init type and declared type need to be identical
			auto dT = decl->getType();
			auto iT = decl->getInitialization()->getType();

			if (!types::isSubTypeOf(iT,dT) && !(dT.isa<FunctionTypePtr>() && types::isMatchable(iT,dT))) {
				add(res,Message(
						decl,
						EC_SEMANTIC_INVALID_NON_MATERIALIZING_DECLARATION,
						format("Invalid non-materializing declaration imposing implicit type conversion.\n\tinit-type: %s\n\tdecl-type: %s", *iT, *dT),
						Message::ERROR
				));
			}

			// done in this case
			return res;
		}


		// now, that it is materializing, there has to be a reason for that
		auto init = decl.getAddressedNode()->getInitialization();

		// two possibilities: explicit or implicit

		// case 1: explicit - the materialized memory location is explicitly initialized
		if (analysis::getRefDeclCall(decl)) {	// so there is a ref_decl call

			// check for matching types of the init type and declared type
			auto declType = decl->getType();
			auto initType = init->getType();

			// mask out qualifiers (TODO: rethink this)
			lang::ReferenceType declRefType(declType);
			lang::ReferenceType initRefType(initType);


			if (!(declRefType.isPlain() && initRefType.isPlain() && analysis::equalTypes(declRefType.getElementType(),initRefType.getElementType()))) {
				add(res,Message(decl,
						EC_SEMANTIC_INVALID_MATERIALIZING_DECLARATION,
						format("Invalid conversion of types.\n\tinit-type: %s\n\tdecl-type: %s\n\tref-declt: %s", *init->getType(), *decl->getType(), *analysis::getRefDeclCall(decl)->getType()),
						Message::ERROR
				));
			}

			// done with the explicit cases
			return res;
		}

		// case 2: implicit - the materialized memory location is initialized through an implicit assignment or constructor call

		// -- some implicit constructor based initialization --

		auto innerDeclaredType = analysis::getReferencedType(decl->getType());
		if (analysis::hasConstructorAccepting(innerDeclaredType,init->getType())) return res;

		// also, assume that all generic types, tuple types, and tag type references have implicit copy and move constructors
		auto initType = init->getType();
		if (lang::isReference(initType) && (innerDeclaredType.isa<GenericTypePtr>() || innerDeclaredType.isa<TupleTypePtr>() || innerDeclaredType.isa<TagTypeReferencePtr>())) {
			lang::ReferenceType initRefType(initType);

			// check that the passed type is a sub-type of the declared type
			if (types::isSubTypeOf(initRefType.getElementType(),innerDeclaredType)) {

				// the init value is a const reference ..
				if (initRefType.isCppReference() && initRefType.isConst()) return res;

				// ... or a r-value reference
				if (initRefType.isCppRValueReference() && !initRefType.isConst()) return res;

			}

		}


		// -- simple, c-style assignment based initializations --

		// a value of type T can also initialize a ref<S,f,f,plain> whenever T is a subtype of S, and T is a trivial type
		if (analysis::isTrivial(initType)) {	// this covers e.g. scalars and PODs

			// parse decl type
			lang::ReferenceType declRef(decl->getType());

			// let a value of type T initialize a ref<S,f,f,plain> whenever T is a subtype of S
			if (declRef.isPlain() && types::isSubTypeOf(initType,declRef.getElementType())) return res;

		}

		// functions can be specialized
		if (initType.isa<FunctionTypePtr>()) {

			// parse decl type
			lang::ReferenceType declRef(decl->getType());

			// let a specialized function can be initialized by a more generic function
			if (declRef.isPlain() && types::isMatchable(declRef.getElementType(),initType)) return res;

		}

		// no reason found => this is not ok
		add(res,Message(decl,
				EC_SEMANTIC_INVALID_MATERIALIZING_DECLARATION,
				format("Materializing declaration unsupported by implicit or explicit constructor or conversion operations.\n\tinit-type: %s\n\tdecl-type: %s", *init->getType(), *decl->getType()),
				Message::ERROR
		));
		return res;
	}

	OptionalMessageList DebugCheck::visitNode(const NodeAddress& addr) {
		return {};
	}

} // end namespace check
} // end namespace core
} // end namespace insieme
