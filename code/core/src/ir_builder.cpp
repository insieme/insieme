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

#include "insieme/core/ir_builder.h"

#include <tuple>
#include <limits>
#include <set>
#include <cmath>

#include "insieme/core/ir_node.h"

#include "insieme/core/ir_values.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_program.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/types/unification.h"
#include "insieme/core/types/return_type_deduction.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/encoder/lists.h"

#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"

#include "insieme/core/parser3/ir_parser.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/datapath/datapath.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace core {

	const lang::BasicGenerator& IRBuilder::getLangBasic() const {
		return manager.getLangBasic();
	}

	namespace {

		/**
		 * Returns the list of variables referenced within an expression.
		 * This class is used when a code block needs to be transformed into a function
		 */
		struct VarRefFinder : public IRVisitor<bool> {
			VarRefFinder() : core::IRVisitor<bool>(false) {}

			bool visitVariable(const core::VariablePtr& varExpr) {
				usedVars.insert(varExpr);
				return true;
			}

			bool visitLambdaExpr(const core::LambdaExprPtr& lambdaExpr) {
				return true;
			}

			bool visitDeclarationStmt(const core::DeclarationStmtPtr& declStmt) {
				declaredVars.insert(declStmt->getVariable());
				return false;
			}

			bool visitNode(const NodePtr& node) {
				return false;
			}

			utils::set::PointerSet<VariablePtr> declaredVars;
			utils::set::PointerSet<VariablePtr> usedVars;
		};

		std::vector<VariablePtr> getReachingVariables(const core::NodePtr& root) {
			assert_true(root) << " no root node";
			VarRefFinder visitor;
			visitDepthFirstPrunable(root, visitor);

			auto nonDecls = utils::set::difference(visitor.usedVars, visitor.declaredVars);

			return std::vector<VariablePtr>(nonDecls.begin(), nonDecls.end());
		}
	}


	NodePtr IRBuilder::get(NodeType type, const NodeList& children) const {
		switch(type) {
		#define CONCRETE(KIND)                                                                                                                                 \
			case NT_##KIND:                                                                                                                                    \
				return get<NT_##KIND>(children);
		#include "insieme/core/ir_nodes.def"
		#undef CONCRETE
		}

		assert_fail() << "Unsupported node type added!";
		return NodePtr();
	}

	ProgramPtr IRBuilder::createProgram(const ExpressionList& entryPoints) const {
		return Program::get(manager, entryPoints);
	}

	namespace {

		/**
		 * Converts a eager map into a lazy map.
		 */
		IRBuilder::lazy_definition_map toLazy(const IRBuilder::eager_definition_map& map) {
			IRBuilder::lazy_definition_map res;
			for(const auto& cur : map) res.insert({cur.first, [=]() { return cur.second; }});
			return res;
		}

	}

	// ---------------------------- Parser Integration -----------------------------------

	NodePtr IRBuilder::parse(const string& code, const lazy_definition_map& symbols) const {
		return parser3::parse_any(manager, code, true, symbols);
	}

	NodePtr IRBuilder::parse(const string& code, const eager_definition_map& symbols) const {
		return parse(code, toLazy(symbols));
	}

	TypePtr IRBuilder::parseType(const string& code, const lazy_definition_map& symbols) const {
		return parser3::parse_type(manager, code, true, symbols);
	}

	TypePtr IRBuilder::parseType(const string& code, const eager_definition_map& symbols) const {
		return parseType(code, toLazy(symbols));
	}

	ExpressionPtr IRBuilder::parseExpr(const string& code, const lazy_definition_map& symbols) const {
		return parser3::parse_expr(manager, code, true, symbols);
	}

	ExpressionPtr IRBuilder::parseExpr(const string& code, const eager_definition_map& symbols) const {
		return parseExpr(code, toLazy(symbols));
	}

	StatementPtr IRBuilder::parseStmt(const string& code, const lazy_definition_map& symbols) const {
		return parser3::parse_stmt(manager, code, true, symbols);
	}

	StatementPtr IRBuilder::parseStmt(const string& code, const eager_definition_map& symbols) const {
		return parseStmt(code, toLazy(symbols));
	}

	ProgramPtr IRBuilder::parseProgram(const string& code, const lazy_definition_map& symbols) const {
		return parser3::parse_program(manager, code, true, symbols);
	}

	ProgramPtr IRBuilder::parseProgram(const string& code, const eager_definition_map& symbols) const {
		return parseProgram(code, toLazy(symbols));
	}

	vector<NodeAddress> IRBuilder::parseAddressesStatement(const string& code, const lazy_definition_map& symbols) const {
		return parser3::parse_addresses_statement(manager, code, true, symbols);
	}

	vector<NodeAddress> IRBuilder::parseAddressesStatement(const string& code, const eager_definition_map& symbols) const {
		return parseAddressesStatement(code, toLazy(symbols));
	}

	vector<NodeAddress> IRBuilder::parseAddressesProgram(const string& code, const lazy_definition_map& symbols) const {
		return parser3::parse_addresses_program(manager, code, true, symbols);
	}

	vector<NodeAddress> IRBuilder::parseAddressesProgram(const string& code, const eager_definition_map& symbols) const {
		return parseAddressesProgram(code, toLazy(symbols));
	}

	// ---------------------------- Standard Nodes -----------------------------------

	#include "ir_builder_impl.inl"

	StringValuePtr IRBuilder::stringValue(const char* str) const {
		return stringValue(string(str));
	}

	StringValuePtr IRBuilder::stringValue(const string& str) const {
		return StringValue::get(manager, str);
	}

	BoolValuePtr IRBuilder::boolValue(bool value) const {
		return BoolValue::get(manager, value);
	}

	CharValuePtr IRBuilder::charValue(char value) const {
		return CharValue::get(manager, value);
	}

	IntValuePtr IRBuilder::intValue(int value) const {
		return IntValue::get(manager, value);
	}

	UIntValuePtr IRBuilder::uintValue(unsigned value) const {
		return UIntValue::get(manager, value);
	}

	// ---------------------------- Convenience -------------------------------------

	bool IRBuilder::matchType(const std::string& typeStr, const core::TypePtr& irType) const {
		// the type used for caching parser results
		struct ParserCache : public std::map<string, TypePtr> {};
		typedef std::shared_ptr<ParserCache> ParserCachePtr;

		// lookup result within cache
		NodePtr cacheNode = manager.get(breakStmt()); // some node that is easy to find :)

		// get reference to cache
		if(!cacheNode->hasAttachedValue<ParserCachePtr>()) { cacheNode->attachValue(std::make_shared<ParserCache>()); }
		const ParserCachePtr& cache = cacheNode->getAttachedValue<ParserCachePtr>();

		// get cached result or parse
		TypePtr type;
		auto pos = cache->find(typeStr);
		if(pos == cache->end()) {
			type = parseType(typeStr);
			cache->insert(std::make_pair(typeStr, type));
		} else {
			type = pos->second;
		}

		// try unify the parsed type and the given type
		return types::unify(manager, type, irType);
	}

	GenericTypePtr IRBuilder::refType(const TypePtr& elementType, bool _const, bool _volatile) const {
		return lang::ReferenceType::create(elementType, _const, _volatile);
	}

	GenericTypePtr IRBuilder::ptrType(const TypePtr& elementType, bool _const, bool _volatile) const {
		return lang::PointerType::create(elementType, _const, _volatile);
	}

	GenericTypePtr IRBuilder::arrayType(const TypePtr& elementType) const {
		return lang::ArrayType::create(elementType);
	}

	GenericTypePtr IRBuilder::arrayType(const TypePtr& elementType, const LiteralPtr& size) const {
		return lang::ArrayType::create(elementType, size);
	}

	GenericTypePtr IRBuilder::arrayType(const TypePtr& elementType, const VariablePtr& size) const {
		return lang::ArrayType::create(elementType, size);
	}

	GenericTypePtr IRBuilder::arrayType(const TypePtr& elementType, size_t size) const {
		return lang::ArrayType::create(elementType, literal(toString(size), getLangBasic().getUIntInf()));
	}


	StructTypePtr IRBuilder::structType(const vector<std::pair<StringValuePtr, TypePtr>>& entries) const {
		return structType(::transform(entries, [&](const pair<StringValuePtr, TypePtr>& cur) { return namedType(cur.first, cur.second); }));
	}

	UnionTypePtr IRBuilder::unionType(const vector<std::pair<StringValuePtr, TypePtr>>& entries) const {
		return unionType(::transform(entries, [&](const pair<StringValuePtr, TypePtr>& cur) { return namedType(cur.first, cur.second); }));
	}

	StructTypePtr IRBuilder::structType(const vector<ParentPtr>& parents, const vector<NamedTypePtr>& entries) const {
		return structType(IRBuilder::parents(parents), entries);
	}

	StructTypePtr IRBuilder::structType(const vector<TypePtr>& parents, const vector<NamedTypePtr>& entries) const {
		return structType(IRBuilder::parents(parents), entries);
	}

	StructTypePtr IRBuilder::structType(const vector<ParentPtr>& parents, const vector<std::pair<StringValuePtr, TypePtr>>& entries) const {
		return structType(parents, ::transform(entries, [&](const pair<StringValuePtr, TypePtr>& cur) { return namedType(cur.first, cur.second); }));
	}

	StructTypePtr IRBuilder::structType(const vector<TypePtr>& parents, const vector<std::pair<StringValuePtr, TypePtr>>& entries) const {
		return structType(parents, ::transform(entries, [&](const pair<StringValuePtr, TypePtr>& cur) { return namedType(cur.first, cur.second); }));
	}

	StructTypePtr IRBuilder::structType(const StringValuePtr& name, const vector<ParentPtr>& parentsList, const vector<NamedTypePtr>& entries) const {
		return StructType::get(manager, name, parents(parentsList), entries);
	}


	NamedTypePtr IRBuilder::namedType(const string& name, const TypePtr& type) const {
		return namedType(stringValue(name), type);
	}

	NamedValuePtr IRBuilder::namedValue(const string& name, const ExpressionPtr& value) const {
		return namedValue(stringValue(name), value);
	}

	TupleExprPtr IRBuilder::tupleExpr(const vector<ExpressionPtr>& values) const {
		TupleTypePtr type = tupleType(extractTypes(values));
		return tupleExpr(type, Expressions::get(manager, values));
	}

	StructExprPtr IRBuilder::structExpr(const StructTypePtr& structType, const vector<NamedValuePtr>& values) const {
		return structExpr(structType, namedValues(values));
	}

	StructExprPtr IRBuilder::structExpr(const vector<std::pair<StringValuePtr, ExpressionPtr>>& members) const {
		vector<NamedTypePtr> types;
		vector<NamedValuePtr> values;
		for_each(members, [&](const pair<StringValuePtr, ExpressionPtr>& cur) {
			types.push_back(namedType(cur.first, cur.second->getType()));
			values.push_back(namedValue(cur.first, cur.second));
		});
		return structExpr(structType(types), namedValues(values));
	}

	StructExprPtr IRBuilder::structExpr(const vector<NamedValuePtr>& values) const {
		vector<NamedTypePtr> types;
		for_each(values, [&](const NamedValuePtr& cur) { types.push_back(namedType(cur->getName(), cur->getValue()->getType())); });
		return structExpr(structType(types), namedValues(values));
	}


	IfStmtPtr IRBuilder::ifStmt(const ExpressionPtr& condition, const StatementPtr& thenBody, const StatementPtr& elseBody) const {
		if(!elseBody) { return ifStmt(condition, wrapBody(thenBody), getNoOp()); }
		return ifStmt(condition, wrapBody(thenBody), wrapBody(elseBody));
	}

	WhileStmtPtr IRBuilder::whileStmt(const ExpressionPtr& condition, const StatementPtr& body) const {
		return whileStmt(condition, wrapBody(body));
	}

	ForStmtPtr IRBuilder::forStmt(const DeclarationStmtPtr& var, const ExpressionPtr& end, const ExpressionPtr& step, const StatementPtr& body) const {
		return forStmt(var->getVariable(), var->getInitialization(), end, step, wrapBody(body));
	}

	ForStmtPtr IRBuilder::forStmt(const VariablePtr& var, const ExpressionPtr& start, const ExpressionPtr& end, const ExpressionPtr& step,
	                              const StatementPtr& body) const {
		return forStmt(var, start, end, step, wrapBody(body));
	}

	ExpressionPtr IRBuilder::forStmtFinalValue(const ForStmtPtr& loopStmt) {
		assert_not_implemented() << "This hard-coded implementation should be reconsidered";

		core::TypePtr iterType = (analysis::isRefType(loopStmt->getIterator()->getType())) ? analysis::getReferencedType(loopStmt->getIterator()->getType())
		                                                                                   : loopStmt->getIterator()->getType();
		core::FunctionTypePtr ceilTy = functionType(toVector<core::TypePtr>(getLangBasic().getDouble()), getLangBasic().getDouble());

		core::ExpressionPtr finalVal = add( // start + ceil((end-start)/step) * step
		    loopStmt->getStart(),
		    mul(                                                                      // ceil((end-start)/step) * step
		        convert(callExpr(getLangBasic().getDouble(), literal(ceilTy, "ceil"), // ceil()
		                         div(                                                 // (end-start)/step
		                             convert(sub(sub(loopStmt->getEnd(), loopStmt->getStep()),
		                                         loopStmt->getStart()), // end - start
		                                     getLangBasic().getDouble()),
		                             convert(loopStmt->getStep(), getLangBasic().getDouble()))),
		                iterType),
		        convert(loopStmt->getStep(), loopStmt->getStart()->getType())));

		return finalVal;
	}

	SwitchStmtPtr IRBuilder::switchStmt(const ExpressionPtr& switchExpr, const vector<std::pair<ExpressionPtr, StatementPtr>>& cases,
	                                    const StatementPtr& defaultCase) const {
		CompoundStmtPtr defCase = (defaultCase) ? wrapBody(defaultCase) : getNoOp();

		vector<SwitchCasePtr> caseList = ::transform(
		    cases, [&](const pair<ExpressionPtr, StatementPtr>& cur) { return switchCase(static_pointer_cast<LiteralPtr>(cur.first), wrapBody(cur.second)); });

		return switchStmt(switchExpr, switchCases(caseList), defCase);
	}

	SwitchStmtPtr IRBuilder::switchStmt(const ExpressionPtr& switchExpr, const vector<SwitchCasePtr>& cases, const StatementPtr& defaultCase) const {
		return switchStmt(switchExpr, switchCases(cases), (defaultCase) ? wrapBody(defaultCase) : getNoOp());
	}

	FunctionTypePtr IRBuilder::toPlainFunctionType(const FunctionTypePtr& funType) const {
		if(funType->isPlain()) { return funType; }
		return functionType(funType->getParameterTypes(), funType->getReturnType(), FK_PLAIN);
	}

	FunctionTypePtr IRBuilder::toThickFunctionType(const FunctionTypePtr& funType) const {
		if(!funType->isPlain()) { return funType; }
		return functionType(funType->getParameterTypes(), funType->getReturnType(), FK_CLOSURE);
	}


	LiteralPtr IRBuilder::stringLit(const string& str) const {
		return literal(str, ptrType(getLangBasic().getChar(), true));
	}

	namespace {

		template <typename T>
		bool isInRange(const int val) {
			return std::numeric_limits<T>::min() <= val && val <= std::numeric_limits<T>::max();
		}
	}

	LiteralPtr IRBuilder::intLit(const int val, bool tight) const {
		if(!tight) { return literal(getLangBasic().getInt4(), toString(val)); }

		TypePtr type;
		if(isInRange<int8_t>(val)) {
			type = getLangBasic().getInt1();
		} else if(isInRange<int16_t>(val)) {
			type = getLangBasic().getInt2();
		} else {
			type = getLangBasic().getInt4();
		}
		return literal(type, toString(val));
	}

	LiteralPtr IRBuilder::uintLit(const unsigned int val, bool tight) const {
		if(!tight) { return literal(getLangBasic().getUInt4(), toString(val)); }

		TypePtr type;
		if(isInRange<uint8_t>(val)) {
			type = getLangBasic().getUInt1();
		} else if(isInRange<uint16_t>(val)) {
			type = getLangBasic().getUInt2();
		} else {
			type = getLangBasic().getUInt4();
		}
		return literal(type, toString(val));
	}

	LiteralPtr IRBuilder::integerLit(const int val, bool tight) const {
		if(val < 0) { return intLit(val, tight); }
		return uintLit((unsigned int)val, tight);
	}

	LiteralPtr IRBuilder::boolLit(bool value) const {
		return literal(getLangBasic().getBool(), (value) ? "true" : "false");
	}

	LiteralPtr IRBuilder::floatLit(const string& value) const {
		return literal(getLangBasic().getReal4(), value);
	}

	LiteralPtr IRBuilder::floatLit(float value) const {
		// special handling for de-normalized values
		if(std::fpclassify(value) == FP_SUBNORMAL) { return floatLit(format("%a", value)); }

		std::stringstream out;
		out << std::scientific << std::fixed << std::setprecision(std::numeric_limits<float>::digits10 + 1) << value << "f";
		assert_false(out.str().empty()) << "empty string? ";
		return floatLit(out.str());
	}

	LiteralPtr IRBuilder::doubleLit(const string& value) const {
		return literal(getLangBasic().getReal8(), value);
	}

	LiteralPtr IRBuilder::doubleLit(double value) const {
		// special handling for de-normalized values
		if(std::fpclassify(value) == FP_SUBNORMAL) { return floatLit(format("%a", value)); }

		std::stringstream out;
		out << std::scientific << std::fixed << std::setprecision(std::numeric_limits<float>::digits10 + 1) << value;
		assert_false(out.str().empty()) << "empty string? ";
		return doubleLit(out.str());
	}

	ExpressionPtr IRBuilder::undefined(const TypePtr& type) const {
		return callExpr(type, getLangBasic().getUndefined(), getTypeLiteral(type));
	}

	ExpressionPtr IRBuilder::undefinedVar(const TypePtr& type) const {
		if(analysis::isRefType(type)) {
			core::TypePtr elementType = core::analysis::getReferencedType(type);
			return refVar(undefined(elementType));
		}
		return undefined(type);
	}

	ExpressionPtr IRBuilder::undefinedNew(const TypePtr& type) const {
		if(analysis::isRefType(type)) {
			core::TypePtr elementType = core::analysis::getReferencedType(type);
			return refNew(undefined(elementType));
		}
		return undefined(type);
	}


	core::ExpressionPtr IRBuilder::getZero(const core::TypePtr& type) const {
		// if it is an integer ...
		if(manager.getLangBasic().isInt(type)) { return literal(type, "0"); }

		// if it is a real ..
		if(manager.getLangBasic().isReal(type)) {
			if(manager.getLangBasic().isFloat(type)) { return literal(type, "0.0f"); }
			return literal(type, "0.0");
		}

		// if it is the bool type
		if(manager.getLangBasic().isBool(type)) { return boolLit(false); }

		// if it is the char type
		if(manager.getLangBasic().isChar(type)) { return literal(type, "' '"); }

		// if it is a lock, keep it undefined
		if(manager.getLangExtension<lang::ParallelExtension>().isLock(type)) { return undefined(type); }

		// if it is a struct ...
		if(type->getNodeType() == core::NT_StructType) {
			// extract type and resolve members recursively
			StructTypePtr structType = static_pointer_cast<const StructType>(type);

			vector<NamedValuePtr> members;
			for_each(structType->getEntries(), [&](const NamedTypePtr& cur) { members.push_back(namedValue(cur->getName(), getZero(cur->getType()))); });

			return core::StructExpr::get(manager, structType, namedValues(members));
		}

		// if it is a union type ...
		if(type->getNodeType() == core::NT_UnionType) {
			UnionTypePtr unionType = type.as<UnionTypePtr>();

			// in case it is a an empty union
			if(unionType.empty() == 0) { return undefined(unionType); }

			// init the first member
			return unionExpr(unionType, unionType[0]->getName(), getZero(unionType[0]->getType()));
		}

		// if it is a ref type ...
		if(analysis::isRefType(type)) {
			// return NULL for the specific type
			auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
			return refReinterpret(refExt.getRefNull(), analysis::getReferencedType(type));
		}

		// if it is a function type -- used for function pointers
		if(type.isa<core::FunctionTypePtr>()) {
			// return NULL for the specific type
			auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
			return deref(refReinterpret(refExt.getRefNull(), type));
		}

		// add support for unit
		if(manager.getLangBasic().isUnit(type)) { return manager.getLangBasic().getUnitConstant(); }

		// FIXME:: this might not be enough, ferdinando fix it!
		if(type->getNodeType() == core::NT_RecType) {
			auto tmp = getZero(type.as<core::RecTypePtr>()->unroll());
			return core::transform::replaceNode(manager, ExpressionAddress(tmp)->getType(), type).as<ExpressionPtr>();
		}

		// for all other generic types we return a generic zero value
		if(type.isa<GenericTypePtr>()) { return callExpr(type, getLangBasic().getZero(), getTypeLiteral(type)); }

		// TODO: extend for more types
		LOG(FATAL) << "Encountered unsupported type: " << *type;
		assert_fail() << "Given type not supported yet!";

		// fall-back => return a literal 0 of the corresponding type
		return literal(type, "0");
	}

	ExpressionPtr IRBuilder::convert(const ExpressionPtr& src, const TypePtr& target) const {
		assert_true(src) << "Invalid null-input.";
		if(*src->getType() == *target) return src;
		assert_not_implemented() << "ToDo: implement conversions.";
		// use a cast node for this step
		return castExpr(target, src);
	}

	CallExprPtr IRBuilder::deref(const ExpressionPtr& subExpr) const {
		assert_pred1(analysis::isRefType, subExpr->getType());
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(analysis::getReferencedType(subExpr->getType()), refExt.getRefDeref(), subExpr);
	}

	ExpressionPtr IRBuilder::tryDeref(const ExpressionPtr& subExpr) const {
		if(!analysis::isRefType(subExpr->getType())) { return subExpr; }
		return deref(subExpr);
	}

	CallExprPtr IRBuilder::refVar(const ExpressionPtr& subExpr) const {
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(refType(subExpr->getType()), refExt.getRefVar(), subExpr);
	}

	CallExprPtr IRBuilder::refNew(const ExpressionPtr& subExpr) const {
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(refType(subExpr->getType()), refExt.getRefNew(), subExpr);
	}

	CallExprPtr IRBuilder::refDelete(const ExpressionPtr& subExpr) const {
		auto& basic = manager.getLangBasic();
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(basic.getUnit(), refExt.getRefDelete(), subExpr);
	}

	CallExprPtr IRBuilder::assign(const ExpressionPtr& target, const ExpressionPtr& value) const {
		assert_pred1(analysis::isRefType, target->getType());
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(manager.getLangBasic().getUnit(), refExt.getRefAssign(), target, value);
	}

	ExpressionPtr IRBuilder::refReinterpret(const ExpressionPtr& subExpr, const TypePtr& newElementType) const {
		assert_pred1(analysis::isRefType, subExpr->getType());
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(refType(newElementType), refExt.getRefReinterpret(), subExpr, getTypeLiteral(newElementType));
	}


	ExpressionPtr IRBuilder::invertSign(const ExpressionPtr& subExpr) const {
		// get the zero constant for this value
		ExpressionPtr zero = getZero(subExpr->getType());

		// we have to check if it is a literal. if
		// it is a literal we need to create a new
		// literal instead of a call expr
		if(LiteralPtr lit = subExpr.isa<LiteralPtr>()) {
			// if literal starts with a minus delete first character
			// to invert the sign else add a minus in front
			// of the literal. If it is has an optional plus delete it,
			// this has no effect.
			std::string newname = lit->getStringValue();
			if(newname[0] == '-' || newname[0] == '+') {
				newname.erase(0, 1);
			} else {
				newname.insert(0, "-");
			}
			// if we have a unsigned int the type maybe turns into a
			// signed type. This can only occur with integer types
			if(getLangBasic().isUnsignedInt(lit->getType())) {
				TypePtr type;
				if(getLangBasic().isUInt1(lit->getType())) {
					type = getLangBasic().getInt1();
				} else if(getLangBasic().isUInt2(lit->getType())) {
					type = getLangBasic().getInt2();
				} else if(getLangBasic().isUInt4(lit->getType())) {
					type = getLangBasic().getInt4();
				} else if(getLangBasic().isUInt8(lit->getType())) {
					type = getLangBasic().getInt8();
				} else if(getLangBasic().isUInt16(lit->getType())) {
					type = getLangBasic().getInt16();
				}
				assert_true(type) << "Cannot find unsigned int type size.";
				return literal(newname, type);
			}
			return literal(newname, lit->getType());
		}

		return callExpr(subExpr->getType(), manager.getLangBasic().getOperator(subExpr->getType(), lang::BasicGenerator::Sub), zero, subExpr);
	}

	ExpressionPtr IRBuilder::negateExpr(const ExpressionPtr& boolExpr) const {
		assert_true(manager.getLangBasic().isBool(boolExpr->getType())) << "Cannot negate a non boolean expression.";
		return callExpr(manager.getLangBasic().getBool(), manager.getLangBasic().getBoolLNot(), boolExpr);
	}


	CallExprPtr IRBuilder::arraySubscript(const ExpressionPtr& array, const ExpressionPtr& index) const {
		assert_not_implemented();
		return CallExprPtr();
	}
	CallExprPtr IRBuilder::arrayRefElem(const ExpressionPtr& array, const ExpressionPtr& index) const {
		assert_not_implemented() << "Todo: implement";
		return CallExprPtr();
	}

	CallExprPtr IRBuilder::arrayAccess(const ExpressionPtr& array, const ExpressionPtr& index) const {
		if(analysis::isRefType(array->getType())) { return arrayRefElem(array, index); }
		return arraySubscript(array, index);
	}

	DeclarationStmtPtr IRBuilder::declarationStmt(const ExpressionPtr& value) const {
		return declarationStmt(value->getType(), value);
	}

	DeclarationStmtPtr IRBuilder::declarationStmt(const TypePtr& type, const ExpressionPtr& value) const {
		return declarationStmt(variable(type), value);
	}


	CallExprPtr IRBuilder::acquireLock(const ExpressionPtr& lock) const {
		assert_true(analysis::isRefOf(lock, manager.getLangExtension<lang::ParallelExtension>().getLock())) << "Cannot lock a non-lock type.";
		return callExpr(manager.getLangBasic().getUnit(), manager.getLangExtension<lang::ParallelExtension>().getLockAcquire(), lock);
	}
	CallExprPtr IRBuilder::releaseLock(const ExpressionPtr& lock) const {
		assert_true(analysis::isRefOf(lock, manager.getLangExtension<lang::ParallelExtension>().getLock())) << "Cannot unlock a non-lock type.";
		return callExpr(manager.getLangBasic().getUnit(), manager.getLangExtension<lang::ParallelExtension>().getLockRelease(), lock);
	}
	CallExprPtr IRBuilder::initLock(const ExpressionPtr& lock) const {
		assert_true(analysis::isRefOf(lock, manager.getLangExtension<lang::ParallelExtension>().getLock())) << "Cannot init a non-lock type.";
		return callExpr(manager.getLangBasic().getUnit(), manager.getLangExtension<lang::ParallelExtension>().getLockInit(), lock);
	}


	CallExprPtr IRBuilder::atomicOp(const ExpressionPtr& location, const ExpressionPtr& testFunc, const ExpressionPtr& replaceFunc) {
		assert_true(core::analysis::isRefType(location->getType())) << "Atomic must be applied on ref.";
		// should also check types of testFunc and replaceFunc
		return callExpr(analysis::getReferencedType(location->getType()), manager.getLangExtension<lang::ParallelExtension>().getAtomic(), location, testFunc, replaceFunc);
	}

	CallExprPtr IRBuilder::atomicAssignment(const CallExprPtr& assignment) {
		// FIXME argument order
		const auto& basic = manager.getLangBasic();
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		auto& parExt = manager.getLangExtension<lang::ParallelExtension>();
		assert_true(refExt.isRefAssign(assignment->getFunctionExpr())) << "Trying to build atomic assignment from non-assigment";

		const auto &lhs = assignment->getArgument(0), &rhs = assignment->getArgument(1);
		const auto& lhsDeref = deref(lhs);
		CallExprPtr rhsCall = dynamic_pointer_cast<CallExprPtr>(rhs);
		assert_true(rhsCall) << "Unsupported atomic assignment structure";

		ExpressionPtr factor;
		if(*lhsDeref == *rhsCall->getArgument(0)) { factor = rhsCall->getArgument(1); }
		if(*lhsDeref == *rhsCall->getArgument(1)) { factor = rhsCall->getArgument(0); }
		assert_true(factor) << "LHS not found in RHS of atomic assignment";

		const auto& rhsFun = rhsCall->getFunctionExpr();
		if(basic.isAddOp(rhsFun)) { return callExpr(factor->getType(), parExt.getAtomicFetchAndAdd(), lhs, factor); }
		if(basic.isSubOp(rhsFun)) { return callExpr(factor->getType(), parExt.getAtomicFetchAndSub(), lhs, factor); }
		if(basic.isBitwiseAndOp(rhsFun)) { return callExpr(factor->getType(), parExt.getAtomicFetchAndAnd(), lhs, factor); }
		if(basic.isBitwiseOrOp(rhsFun)) { return callExpr(factor->getType(), parExt.getAtomicFetchAndOr(), lhs, factor); }
		if(basic.isBitwiseXorOp(rhsFun)) { return callExpr(factor->getType(), parExt.getAtomicFetchAndXor(), lhs, factor); }
		assert_fail() << "Unsupported atomic operation";
		return assignment;
	}

	CallExprPtr IRBuilder::atomicConditional(const IfStmtPtr& statement) {
		assert_fail() << "Not implemented";
		return CallExprPtr();
	}


	CallExprPtr IRBuilder::pickVariant(const ExpressionList& variants) const {
		assert_false(variants.empty()) << "Variant list must not be empty!";
		assert(all(variants, [&](const ExpressionPtr& cur) { return *cur->getType() == *variants[0]->getType(); })
		       && "All options have to have the same type.");
		return callExpr(variants[0]->getType(), manager.getLangBasic().getPick(),
		                encoder::toIR<ExpressionList, encoder::DirectExprListConverter>(manager, variants));
	}

	CallExprPtr IRBuilder::pickInRange(const ExpressionPtr& id, const ExpressionPtr& max, const ExpressionPtr& qualLB, const ExpressionPtr& qualUB,
	                                   const ExpressionPtr& qualS) const {
		vector<ExpressionPtr> args;
		args.push_back(id);
		args.push_back(max);
		if(!qualLB || !qualUB || !qualS) {
			args.push_back(intLit(0));
			args.push_back(intLit(0));
			args.push_back(intLit(0));
		} else {
			args.push_back(qualLB);
			args.push_back(qualUB);
			args.push_back(qualS);
		}
		return callExpr(manager.getLangBasic().getPickInRange(), args);
	}


	namespace {

		TypePtr deduceReturnTypeForCall(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
			// check function expression
			assert_eq(functionExpr->getType()->getNodeType(), NT_FunctionType) << "Function expression is not a function!";

			// extract function type
			FunctionTypePtr funType = static_pointer_cast<const FunctionType>(functionExpr->getType());
			assert_eq(funType->getParameterTypes().size(), arguments.size()) << "Invalid number of arguments!";

			// deduce return type
			core::TypeList argumentTypes;
			::transform(arguments, back_inserter(argumentTypes), [](const ExpressionPtr& cur) { return cur->getType(); });

			TypePtr res = types::deduceReturnType(funType, argumentTypes, false);

			assert_ne(TypePtr(), res) << "Unable to deduce return type!";

			return res;
		}

		/**
		 * Checks whether the given result type is matching the type expected when using automatic type inference.
		 */
		inline bool checkType(const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
			// check types
			if(*resultType != *deduceReturnTypeForCall(functionExpr, arguments)) {
				// print a warning if they are not matching
				LOG(WARNING) << "Potentially invalid return type for call specified - function type: " << toString(*functionExpr->getType())
				             << ", arguments: " << join(", ", arguments, print<deref<ExpressionPtr>>());
			}
			return true;
		}


		CallExprPtr createCall(const IRBuilder& builder, const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
			// check user-specified return type - only when compiled in debug mode
			// NOTE: the check returns true in any case, hence this assertion will never fail - its just a warning!
			// TODO: make this check faster
			//		assert_true(checkType(resultType, functionExpr, arguments)) << "Incorrect user-specified return type!";

			// create calling expression
			return builder.callExpr(resultType, functionExpr, arguments);
		}
	}

	CallExprPtr IRBuilder::callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) const {
		// use deduced return type to construct call
		return callExpr(deduceReturnTypeForCall(functionExpr, arguments), functionExpr, arguments);
	}
	CallExprPtr IRBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr) const {
		return createCall(*this, resultType, functionExpr, toVector<ExpressionPtr>());
	}

	LambdaPtr IRBuilder::lambda(const FunctionTypePtr& type, const ParametersPtr& params, const StatementPtr& body) const {
		return lambda(type, params, wrapBody(body));
	}

	LambdaPtr IRBuilder::lambda(const FunctionTypePtr& type, const VariableList& params, const StatementPtr& body) const {
		return lambda(type, params, wrapBody(body));
	}

	LambdaExprPtr IRBuilder::lambdaExpr(const StatementPtr& body, const ParametersPtr& params) const {
		return lambdaExpr(functionType(extractTypes(params->getParameters()), manager.getLangBasic().getUnit(), FK_PLAIN), params, wrapBody(body));
	}
	LambdaExprPtr IRBuilder::lambdaExpr(const StatementPtr& body, const VariableList& params) const {
		return lambdaExpr(body, parameters(params));
	}
	LambdaExprPtr IRBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const ParametersPtr& params) const {
		return lambdaExpr(functionType(extractTypes(params->getParameters()), returnType, FK_PLAIN), params, wrapBody(body));
	}
	LambdaExprPtr IRBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const VariableList& params) const {
		return lambdaExpr(returnType, body, parameters(params));
	}

	LambdaExprPtr IRBuilder::lambdaExpr(const FunctionTypePtr& type, const VariableList& params, const StatementPtr& body) const {
		return lambdaExpr(lambda(type, params, body));
	}

	BindExprPtr IRBuilder::bindExpr(const VariableList& params, const CallExprPtr& call) const {
		FunctionTypePtr type = functionType(extractTypes(params), call->getType(), FK_CLOSURE);
		return bindExpr(type, parameters(params), call);
	}

	JobExprPtr IRBuilder::jobExpr(const ExpressionPtr& threadNumRange, const ExpressionPtr& body) const {
		GenericTypePtr type = static_pointer_cast<GenericTypePtr>(manager.getLangBasic().getJob());
		return jobExpr(type, threadNumRange, body);
	}

	JobExprPtr IRBuilder::jobExpr(const ExpressionPtr& rangeLowerBound, const ExpressionPtr& rangeUpperBound, const ExpressionPtr& body) const {
		GenericTypePtr type = static_pointer_cast<GenericTypePtr>(manager.getLangBasic().getJob());
		const ExpressionPtr threadNumRange = getThreadNumRange(rangeLowerBound, rangeUpperBound);
		return jobExpr(type, threadNumRange, body);
	}

	namespace {

		bool isJobBody(const NodePtr& node) {
			// it has to be an expression
			if(node->getNodeCategory() != NC_Expression) { return false; }

			// the function has to be ()->unit or ()=>unit
			TypePtr type = node.as<ExpressionPtr>()->getType();
			if(type->getNodeType() != NT_FunctionType) { return false; }
			return type.as<FunctionTypePtr>()->getParameterTypes()->empty();
		}
	}

	JobExprPtr IRBuilder::jobExpr(const StatementPtr& stmt, int numThreads) const {
		ExpressionPtr jobBody = (isJobBody(stmt)) ? stmt.as<ExpressionPtr>() : transform::extractLambda(manager, stmt);

		return jobExpr((numThreads < 1) ? getThreadNumRange(1) : getThreadNumRange(numThreads, numThreads), jobBody);
	}

	MarkerExprPtr IRBuilder::markerExpr(const ExpressionPtr& subExpr, unsigned id) const {
		return markerExpr(subExpr, uintValue(id));
	}

	MarkerExprPtr IRBuilder::markerExpr(const ExpressionPtr& subExpr, const UIntValuePtr& id) const {
		return markerExpr(id, subExpr);
	}

	MarkerStmtPtr IRBuilder::markerStmt(const StatementPtr& subStmt, unsigned id) const {
		return markerStmt(subStmt, uintValue(id));
	}

	MarkerStmtPtr IRBuilder::markerStmt(const StatementPtr& subStmt, const UIntValuePtr& id) const {
		return markerStmt(id, subStmt);
	}

	CallExprPtr IRBuilder::getThreadNumRange(unsigned min) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateMinRange(), literal(type, toString(min)));
	}
	CallExprPtr IRBuilder::getThreadNumRange(unsigned min, unsigned max) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateBoundRange(), literal(type, toString(min)), literal(type, toString(max)));
	}

	CallExprPtr IRBuilder::getThreadNumRange(const ExpressionPtr& min) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateMinRange(), convert(min, type));
	}
	CallExprPtr IRBuilder::getThreadNumRange(const ExpressionPtr& min, const ExpressionPtr& max) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateBoundRange(), convert(min, type), convert(max, type));
	}


	CallExprPtr IRBuilder::getThreadGroup(ExpressionPtr level) const {
		if(!level) { level = uintLit(0); }
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getGetThreadGroup(), level);
	}
	CallExprPtr IRBuilder::getThreadGroupSize(ExpressionPtr level) const {
		if(!level) { level = uintLit(0); }
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getGetGroupSize(), level);
	}
	CallExprPtr IRBuilder::getThreadId(ExpressionPtr level) const {
		if(!level) { level = uintLit(0); }
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getGetThreadId(), level);
	}

	CallExprPtr IRBuilder::barrier(ExpressionPtr threadgroup) const {
		if(!threadgroup) { threadgroup = getThreadGroup(); }
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getBarrier(), threadgroup);
	}

	CallExprPtr IRBuilder::mergeAll() const {
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getMergeAll());
	}

	CallExprPtr IRBuilder::pfor(const ExpressionPtr& body, const ExpressionPtr& start, const ExpressionPtr& end, ExpressionPtr step) const {
		if(!step) { step = uintLit(1); }
		assert_true(manager.getLangBasic().isInt(start->getType()));
		assert_true(manager.getLangBasic().isInt(end->getType()));
		assert_true(manager.getLangBasic().isInt(step->getType()));
		auto ret = callExpr(getLangBasic().getUnit(), manager.getLangExtension<lang::ParallelExtension>().getPFor(), toVector<ExpressionPtr>(getThreadGroup(), start, end, step, body));
		// LOG(INFO) <<  "%%% generated pfor:\n "<< core::printer::PrettyPrinter(ret) << "\n";
		return ret;
	}

	CallExprPtr IRBuilder::pfor(const ForStmtPtr& initialFor) const {
		auto loopStart = initialFor->getStart();
		auto loopEnd = initialFor->getEnd();
		auto loopStep = initialFor->getStep();
		auto loopVarType = loopStart->getType();

		while(analysis::isRefType(loopVarType)) {
			loopVarType = analysis::getReferencedType(loopVarType);
		}

		// modify body to take iteration variables
		auto pforLambdaParamStart = variable(loopVarType);
		auto pforLambdaParamEnd = variable(loopVarType);
		auto pforLambdaParamStep = variable(loopVarType);

		auto adaptedFor = forStmt(initialFor->getIterator(), pforLambdaParamStart, pforLambdaParamEnd, pforLambdaParamStep, initialFor->getBody());

		BindExprPtr lambda = transform::extractLambda(manager, adaptedFor, toVector(pforLambdaParamStart, pforLambdaParamEnd, pforLambdaParamStep));

		return pfor(lambda, loopStart, loopEnd, loopStep);
	}

	CallExprPtr IRBuilder::parallel(const StatementPtr& stmt, int numThreads) const {
		auto& ext = manager.getLangExtension<lang::ParallelExtension>();
		return callExpr(ext.getThreadGroup(), ext.getParallel(), jobExpr(stmt, numThreads));
	}

	core::ExpressionPtr IRBuilder::createCallExprFromBody(StatementPtr body, TypePtr retTy, bool lazy) const {
		// Find the variables which are used in the body and not declared
		std::vector<VariablePtr>&& args = getReachingVariables(body);

		core::TypeList argsType;
		VariableList params;
		vector<ExpressionPtr> callArgs;

		vector<pair<ExpressionPtr, ExpressionPtr>> replVariableMap;
		for(const core::ExpressionPtr& curr : args) {
			assert_eq(curr->getNodeType(), core::NT_Variable);

			const core::VariablePtr& bodyVar = curr.as<core::VariablePtr>();
			const core::TypePtr& varType = bodyVar->getType();

			// substitute all value use of variables by a value paramenter, this avoids pointers in the
			// prototype and interacts better with the materialize read only routine
			if(analysis::isRefType(varType) && core::analysis::isReadOnly(body, bodyVar)) {
				core::VariablePtr&& parmVar = this->variable(analysis::getReferencedType(varType));
				argsType.push_back(parmVar->getType());
				params.push_back(parmVar);
				callArgs.push_back(this->deref(bodyVar));
				replVariableMap.push_back(std::make_pair(deref(bodyVar), parmVar));
				replVariableMap.push_back(std::make_pair(bodyVar, refVar(parmVar)));
			} else {
				// we create a new variable to replace the captured variable
				core::VariablePtr&& parmVar = this->variable(varType);
				argsType.push_back(varType);
				params.push_back(parmVar);
				callArgs.push_back(curr);
				replVariableMap.push_back(std::make_pair(bodyVar, parmVar));
			}
		}

		// Replace the variables in the body with the input parameters which have been created
		if(!replVariableMap.empty()) {
			for(auto replace : replVariableMap) {
				body = core::transform::replaceAllGen(manager, body, replace.first, replace.second);
			}
			// TODO: there is no suitable tool to replace expression by variable
			//    	body = core::static_pointer_cast<const core::Statement>(
			//    			core::transform::replaceVars(manager, body, replVariableMap)
			//    		);
		}

		core::LambdaExprPtr&& lambdaExpr = this->lambdaExpr(functionType(argsType, retTy, FK_PLAIN), params, wrapBody(body));
		core::CallExprPtr&& callExpr = this->callExpr(retTy, lambdaExpr, callArgs);

		if(!lazy) { return callExpr; }

		// build the expression body
		return bindExpr(std::vector<VariablePtr>(), callExpr);
	}

	CallExprPtr IRBuilder::accessMember(const ExpressionPtr& structExpr, const string& member) const {
		return accessMember(structExpr, stringValue(member));
	}

	CallExprPtr IRBuilder::accessMember(const ExpressionPtr& structExpr, const StringValuePtr& member) const {
		core::TypePtr type = structExpr->getType();
		if(type->getNodeType() == core::NT_RecType) { type = core::static_pointer_cast<const core::RecType>(type)->unroll(type.getNodeManager()); }

		// if it is a ref type, use refMember function
		if(analysis::isRefType(type)) { return refMember(structExpr, member); }

		assert_true((type->getNodeType() == core::NT_StructType || type->getNodeType() == core::NT_UnionType)) << "Cannot access non-struct type!";

		core::NamedCompositeTypePtr structType = static_pointer_cast<const core::NamedCompositeType>(type);
		core::TypePtr memberType = structType->getTypeOfMember(member);

		// create access instruction
		core::ExpressionPtr access = getLangBasic().getCompositeMemberAccess();
		return callExpr(memberType, access, structExpr, getIdentifierLiteral(member), getTypeLiteral(memberType));
	}

	CallExprPtr IRBuilder::refMember(const ExpressionPtr& structExpr, const string& member) const {
		return refMember(structExpr, stringValue(member));
	}

	CallExprPtr IRBuilder::refMember(const ExpressionPtr& structExpr, const StringValuePtr& member) const {
		core::TypePtr type = structExpr->getType();
		assert_pred1(analysis::isRefType, type) << "Cannot deref non-ref type";

		core::TypePtr elementType = analysis::getReferencedType(type);

		if(elementType->getNodeType() == core::NT_RecType) {
			elementType = core::static_pointer_cast<const core::RecType>(elementType)->unroll(elementType.getNodeManager());
		}

		// assert_true((elementType->getNodeType() == core::NT_StructType || elementType->getNodeType() == core::NT_UnionType)) << "Cannot access non-struct
		// type!";

		core::NamedCompositeTypePtr structType = static_pointer_cast<const core::NamedCompositeType>(elementType);
		core::TypePtr memberType = structType->getTypeOfMember(member);

		// create access instruction
		core::ExpressionPtr access = manager.getLangExtension<lang::ReferenceExtension>().getRefMemberAccess();
		return callExpr(refType(memberType), access, structExpr, getIdentifierLiteral(member), getTypeLiteral(memberType));
	}

	CallExprPtr IRBuilder::refParent(const ExpressionPtr& structExpr, const TypePtr& parent) const {
		// check some pre-conditions
		TypePtr type = structExpr->getType();
		assert_pred1(analysis::isRefType, type) << "Cannot deref non-ref type";
		type = analysis::getReferencedType(type);
		assert_true(type->getNodeType() == core::NT_StructType || type->getNodeType() == core::NT_GenericType || type->getNodeType() == core::NT_RecType);

		// compute result type
		core::TypePtr resType = refType(parent);

		// build up access operation
		auto narrow = manager.getLangExtension<lang::ReferenceExtension>().getRefNarrow();
		auto dataPath = datapath::DataPathBuilder(structExpr->getType()).parent(parent).getPath();
		return callExpr(resType, narrow, structExpr, dataPath, getTypeLiteral(parent));
	}

	CallExprPtr IRBuilder::accessComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const {
		unsigned idx = extractNumberFromExpression(component);
		return accessComponent(tupleExpr, idx);
	}

	CallExprPtr IRBuilder::accessComponent(ExpressionPtr tupleExpr, unsigned component) const {
		core::TypePtr type = tupleExpr->getType();
		assert_eq(type->getNodeType(), core::NT_TupleType) << "Cannot access non-tuple type!";

		core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(type);
		assert_lt(component, tupleType->getElementTypes().size()) << "Component out of range!";
		core::TypePtr componentType = tupleType->getElementTypes()[component];

		// create access instruction
		core::ExpressionPtr access = getLangBasic().getTupleMemberAccess();
		core::ExpressionPtr index = literal(getLangBasic().getUInt8(), utils::numeric_cast<string>(component));
		core::ExpressionPtr typeLiteral = getTypeLiteral(componentType);
		return callExpr(componentType, access, tupleExpr, index, typeLiteral);
	}

	CallExprPtr IRBuilder::refComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const {
		unsigned idx = extractNumberFromExpression(component);
		return refComponent(tupleExpr, idx);
	}
	CallExprPtr IRBuilder::refComponent(ExpressionPtr tupleExpr, unsigned component) const {
		core::TypePtr type = tupleExpr->getType();
		assert_pred1(analysis::isRefType, type) << "Cannot deref non ref type";

		core::TypePtr elementType = analysis::getReferencedType(type);
		assert_eq(elementType->getNodeType(), core::NT_TupleType) << "Cannot access non-tuple type!";

		core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(elementType);
		assert_lt(component, tupleType->getElementTypes().size()) << "Component out of range!";
		core::TypePtr componentType = tupleType->getElementTypes()[component];

		// create access instruction
		core::ExpressionPtr access = manager.getLangExtension<lang::ReferenceExtension>().getRefComponentAccess();
		core::ExpressionPtr index = literal(getLangBasic().getUInt8(), utils::numeric_cast<string>(component));
		core::ExpressionPtr typeLiteral = getTypeLiteral(componentType);
		return callExpr(refType(componentType), access, tupleExpr, index, typeLiteral);
	}


	CompoundStmtPtr IRBuilder::getNoOp() const {
		return compoundStmt();
	}

	bool IRBuilder::isNoOp(const NodePtr& p) const {
		return *p == *getNoOp();
	}

	TypePtr IRBuilder::getTypeLiteralType(const TypePtr& type) const {
		return genericType("type", toVector(type));
	}

	LiteralPtr IRBuilder::getTypeLiteral(const TypePtr& type) const {
		auto literalType = getTypeLiteralType(type);
		return literal(literalType, "type_literal");
	}

	LiteralPtr IRBuilder::getIdentifierLiteral(const string& value) const {
		return getIdentifierLiteral(stringValue(value));
	}

	LiteralPtr IRBuilder::getIdentifierLiteral(const core::StringValuePtr& value) const {
		return literal(getLangBasic().getIdentifier(), value);
	}

	ExpressionPtr IRBuilder::scalarToVector(const TypePtr& type, const ExpressionPtr& subExpr) const {
		assert_not_implemented();
		return ExpressionPtr();

		//	// if it is alread a vector => done
		//	if(subExpr->getType()->getNodeType() == NT_VectorType) {
		//		return subExpr;
		//	}
		//
		//	assert_true(getLangBasic().isScalarType(subExpr->getType())) << "Requested to convert non-scalar to scalar!";
		//	assert_eq(type->getNodeType(), NT_VectorType) << "Target type has to be a vector type!";
		//
		//
		//	VectorTypePtr vt = type.as<VectorTypePtr>();
		//
		//	// Convert casts form scalars to vectors to vector init exrpessions
		//	// get vector element type without ref
		//	core::TypePtr elementType = vt->getElementType();
		//	core::TypePtr targetType = elementType;// refs in arrays have been removed! (elementType->getNodeType() != core::NT_RefType) ?  vt->getElementType()
		//:
		//	//dynamic_pointer_cast<const core::RefType>(elementType)->getElementType();
		//
		//	core::ExpressionPtr arg = (subExpr->getType() == targetType) ? subExpr :
		//	                          convert(targetType, subExpr); // if the type of the sub expression is not equal the target type we need to cast it
		//
		//	core::ExpressionPtr&& retExpr = callExpr(type, getLangBasic().getVectorInitUniform(),
		//	                                (elementType->getNodeType() == core::NT_RefType
		//	                                 && arg->getNodeType() != core::NT_RefType)  ? refVar(arg) : arg,  // if we need a ref type and arg is no ref: add
		//ref
		//	                                getIntTypeParamLiteral(vt->getSize()));
		//
		//	return retExpr;


		//    // check for casts from salar pointers to vector pointers
		//    if(core::ArrayTypePtr&& array = dynamic_pointer_cast<const core::ArrayType>(type)) {
		////        core::RefTypePtr&& refType = dynamic_pointer_cast<const core::RefType>(array->getElementType());
		//        core::VectorTypePtr&& vt = dynamic_pointer_cast<const core::VectorType>(array->getElementType());
		//        core::ArrayTypePtr&& castedArray = dynamic_pointer_cast<const core::ArrayType>(subExpr->getType());
		//        if(castedArray && vt ){
		//            core::TypePtr elemTy = /*castedArray->getElementType()->getNodeType() == core::NodeType::NT_RefType ?
		//                    dynamic_pointer_cast<const core::RefType>(castedArray->getElementType())->getElementType() :*/ castedArray->getElementType();
		//
		//            if(elemTy) {
		//                // check if they have the same type
		//                assert(elemTy == vt->getElementType() && "cast from array to array of vectors only allowed within the same type");
		//
		//                return  callExpr(getLangBasic().getArrayElemToVec(), subExpr, getIntTypeParamLiteral(vt->getSize()));
		//            }
		//        }
		//    }

		//    // expression is either already a vector/array type or the type is not a vector type
		//    return subExpr;
	}


	// ------------------------ Operators ---------------------------

	namespace {
		template <typename... T>
		TypePtr infereExprTypeInternal(const ExpressionPtr& op, const T&... operators) {
			assert_eq(op->getType()->getNodeType(), NT_FunctionType) << "Operation is not a function!";
			FunctionTypePtr funType = static_pointer_cast<FunctionTypePtr>(op->getType());
			return types::tryDeduceReturnType(funType, extractTypes(toVector(operators...)));
		}
	}

	TypePtr IRBuilder::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a) const {
		return infereExprTypeInternal(op, a);
	}

	TypePtr IRBuilder::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b) const {
		return infereExprTypeInternal(op, a, b);
	}

	TypePtr IRBuilder::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& c) const {
		return infereExprTypeInternal(op, a, b, c);
	}


	namespace {

		GenericTypePtr toSigned(const IRBuilder& builder, const GenericTypePtr& type) {
			assert_true(builder.getLangBasic().isScalarType(type)) << "Can not alter non-scalar type to signed!";

			// check whether a modification is required
			if(builder.getLangBasic().isUnsignedInt(type)) {
				// alter to signed alternative
				return builder.genericType("int", toVector(type->getTypeParameter(0)));
			}
			return type;
		}
	}


	LiteralPtr IRBuilder::minus(const LiteralPtr& lit) const {
		assert_true(getLangBasic().isScalarType(lit->getType())) << "Can not change sign of non-scalar type!";

		// update type of literal to support unsigned
		TypePtr type = toSigned(*this, lit->getType().as<GenericTypePtr>());

		// update string value of literal
		string value = lit->getStringValue();
		assert_gt(value.size(), 0u);
		if(value[0] == '-') {
			value = value.substr(1);
		} else {
			value = "-" + value;
		}

		// create resulting literal
		return literal(value, type);
	}

	ExpressionPtr IRBuilder::minus(const ExpressionPtr& a) const {
		assert_true(getLangBasic().isScalarType(a->getType())) << "Can not change sign of non-scalar type!";

		// check literal type
		if(a->getNodeType() == NT_Literal) { return minus(a.as<LiteralPtr>()); }

		// update type of literal to support unsigned
		TypePtr type = toSigned(*this, a->getType().as<GenericTypePtr>());

		ExpressionPtr value = a;
		if(value->getType() != type) { value = convert(value, type); }

		// return 0 - a
		return sub(getZero(type), a);
	}


	// ---------------------------- Utilities ---------------------------------------


	unsigned IRBuilder::extractNumberFromExpression(ExpressionPtr& expr) const {
		unsigned idx = 0;
		// search for the literal in the second argument
		auto lambdaVisitor = makeLambdaVisitor([&idx, this](const NodePtr& node) -> bool {
			// check for literal, assuming it will always be a valid integer
			if(const LiteralPtr& lit = dynamic_pointer_cast<const Literal>(node)) {
				if(getLangBasic().isInt(lit->getType())) {
					idx = atoi(lit->getValue()->getValue().c_str());
					return true;
				}
			}
			return false;
		});

		if(!visitDepthFirstInterruptible(expr, lambdaVisitor)) {
			LOG(ERROR) << expr;
			assert_fail() << "Expression does not contain a literal a number";
		}

		return idx;
	}


	/**
	 * A utility function wrapping a given statement into a compound statement (if necessary).
	 */
	CompoundStmtPtr IRBuilder::wrapBody(const StatementPtr& stmt) const {
		if(stmt->getNodeType() == NT_CompoundStmt) { return static_pointer_cast<CompoundStmtPtr>(stmt); }
		return CompoundStmt::get(stmt->getNodeManager(), stmt);
	}

	ExpressionPtr IRBuilder::wrapLazy(const ExpressionPtr& expr) const {
		// if it is a expression, bind free variables
		VariableList list = analysis::getFreeVariables(expr);
		std::sort(list.begin(), list.end(), compare_target<VariablePtr>());
		ExpressionPtr res = lambdaExpr(expr->getType(), returnStmt(expr), list);

		// if there are no free variables ...
		if(list.empty()) {
			// ... no capturing is necessary
			return res;
		}

		// otherwise: bind the free variables
		return bindExpr(VariableList(), callExpr(expr->getType(), res, convertList<Expression>(list)));
	}

	CallExprPtr IRBuilder::print(const string& format, const ExpressionList& args) const {
		return print(stringLit(format), args);
	}

	CallExprPtr IRBuilder::print(const ExpressionPtr& format, const ExpressionList& args) const {
		auto& basic = getLangBasic();
		return callExpr(basic.getUnit(), basic.getPrint(), format, pack(args));
	}

	CallExprPtr IRBuilder::pack(const ExpressionList& values) const {
		auto& basic = getLangBasic();
		return callExpr(basic.getVarList(), basic.getVarlistPack(), tupleExpr(values));
	}

	CallExprPtr IRBuilder::select(const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& op) const {
		const auto& basic = manager.getLangBasic();
		const core::LiteralPtr& select = basic.getSelect();
		return callExpr(infereExprType(select, a, b, op), select, a, b, op);
	}

	CallExprPtr IRBuilder::select(const ExpressionPtr& a, const ExpressionPtr& b, lang::BasicGenerator::Operator op) const {
		return select(a, b, manager.getLangBasic().getOperator(a->getType(), op));
	}

	// helper for the pointwise operation
	CallExprPtr IRBuilder::pointwise(const ExpressionPtr& callee) const {
		assert_not_implemented();
		return CallExprPtr();

		//	const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
		//	assert_true(funTy) << "The argument of pointwise must be a function";
		//
		//	TypeList paramTys = funTy->getParameterTypeList();
		//
		//	assert_true(paramTys.size() <= 2 && paramTys.size() > 0) << "The function for pointwise must take one or two arguments";
		//
		////	FunctionTypePtr pointwiseTy; Use automatic type deduction since vector pointwise is not a function type any more and I have no idea how to build
		///the correct tpye
		//	ExpressionPtr pointwise;
		//	const auto& basic = manager.getLangBasic();
		//	if(paramTys.size() == 1) { // unary function
		//		TypePtr newParamTy = vectorType(paramTys.at(0), variableIntTypeParam('l'));
		////		pointwiseTy = functionType(toVector(newParamTy), vectorType(funTy->getReturnType(), variableIntTypeParam('l')));
		//		pointwise =  basic.getVectorPointwiseUnary();
		//	}
		//	else {   // binary functon
		//		TypePtr newParamTy1 = vectorType(paramTys.at(1), variableIntTypeParam('l'));
		//		TypePtr newParamTy2 = vectorType(paramTys.at(0), variableIntTypeParam('l'));
		////		pointwiseTy = functionType(toVector(newParamTy1, newParamTy2), vectorType(funTy->getReturnType(), variableIntTypeParam('l')));
		//
		////		pointwiseTy = functionType(toVector(newParamTy1, newParamTy2), vectorType(funTy->getReturnType(), variableIntTypeParam('l')));
		//		pointwise =  basic.getVectorPointwise();
		//	}
		//	return callExpr(/*pointwiseTy,*/ pointwise, callee);
	}

	// helper for accuraccy functions
	CallExprPtr IRBuilder::accuracyHigh(const ExpressionPtr& callee) const {
		const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
		assert_true(funTy) << "The argument of accuraccy high must be a function";
		int nArgs = funTy->getParameterTypeList().size();
		assert_true(nArgs <= 2 && nArgs > 0) << "The function for accuraccy high must take one or two arguments";

		const auto& basic = manager.getLangBasic();
		return nArgs == 1 ? callExpr(funTy, basic.getAccuracyFastUnary(), callee) : callExpr(funTy, basic.getAccuracyFastBinary(), callee);
	}
	CallExprPtr IRBuilder::accuracyBestEffort(const ExpressionPtr& callee) const {
		const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
		assert_true(funTy) << "The argument of accuraccy best effort must be a function";
		int nArgs = funTy->getParameterTypeList().size();
		assert_true(nArgs <= 2 && nArgs > 0) << "The function for accuraccy best effort must take one or two arguments";

		const auto& basic = manager.getLangBasic();
		return nArgs == 1 ? callExpr(funTy, basic.getAccuracyBestEffortUnary(), callee) : callExpr(funTy, basic.getAccuracyBestEffortBinary(), callee);
	}
	CallExprPtr IRBuilder::accuracyFast(const ExpressionPtr& callee) const {
		const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
		assert_true(funTy) << "The argument of accuraccy fast must be a function";
		int nArgs = funTy->getParameterTypeList().size();
		assert_true(nArgs <= 2 && nArgs > 0) << "The function for accuraccy fast must take one or two arguments";

		const auto& basic = manager.getLangBasic();
		return nArgs == 1 ? callExpr(basic.getAccuracyFastUnary(), callee) : callExpr(basic.getAccuracyFastBinary(), callee);
	}

	CallExprPtr IRBuilder::vectorPermute(const ExpressionPtr& dataVec, const ExpressionPtr& permutationVec) const {
		assert_not_implemented();
		return CallExprPtr();
		//	const RefTypePtr refTy = dataVec->getType().isa<RefTypePtr>();
		//
		//	const VectorTypePtr dataType = refTy ? refTy->getElementType().as<VectorTypePtr>() : dataVec->getType().as<VectorTypePtr>();
		//	assert_true(dataType) << "First argument of vector.permute must be a vector";
		//
		//	const auto& basic = manager.getLangBasic();
		//	const VectorTypePtr permuteType = dynamic_pointer_cast<const VectorType>(permutationVec->getType());
		//	assert_true(permuteType) << "Second argument of vector.permute must be a vector";
		//	assert_true(types::isSubTypeOf(permuteType->getElementType(),
		//	                               basic.getUIntInf())) << "The second argument of vector.permute must be of type vector<uint<#a>,#m>";
		//
		//	TypePtr retTy = vectorType(dataType->getElementType(), permuteType->getSize());
		//	ExpressionPtr fun;
		//	if(refTy) {
		//		retTy = refType(retTy);
		//		fun = basic.getVectorRefPermute();
		//	}
		//	else {
		//		fun = basic.getVectorPermute();
		//	}
		//
		//	return callExpr(retTy, fun, dataVec, permutationVec);
	}

} // namespace core
} // namespace insieme
