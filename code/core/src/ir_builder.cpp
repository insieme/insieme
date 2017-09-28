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

#include "insieme/core/ir_builder.h"

#include <tuple>
#include <limits>
#include <set>
#include <cmath>
#include <iomanip>

#include "insieme/core/ir_node.h"

#include "insieme/core/ir_values.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_program.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"

#include "insieme/core/types/unification.h"
#include "insieme/core/types/return_type_deduction.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/type_utils.h"

#include "insieme/core/annotations/default_delete.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/channel.h"
#include "insieme/core/lang/compound_operators.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/io.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/lang/varargs_extension.h"

#include "insieme/core/datapath/datapath.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/parser/ir_parser.h"
#include "insieme/core/parser/detail/driver.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace core {

	const lang::BasicGenerator& IRBuilderBaseModule::getLangBasic() const {
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

	}


	NodePtr IRBuilderBaseModule::get(NodeType type, const NodeList& children) const {
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

	ProgramPtr IRBuilderBaseModule::createProgram(const ExpressionList& entryPoints) const {
		return Program::get(manager, entryPoints);
	}

	namespace {

		/**
		 * Converts a eager map into a lazy map.
		 */
		IRBuilderBaseModule::LazyDefinitionMap toLazy(const IRBuilderBaseModule::EagerDefinitionMap& map) {
			IRBuilderBaseModule::LazyDefinitionMap res;
			for(const auto& cur : map) res.insert({cur.first, [=]() { return cur.second; }});
			return res;
		}

		IRBuilderBaseModule::LazyDefinitionMap addStandardSymbols(NodeManager& mgr, const IRBuilderBaseModule::LazyDefinitionMap& init = IRBuilderBaseModule::LazyDefinitionMap()) {
			IRBuilderBaseModule::LazyDefinitionMap res;

			// load standard modules
			for(const auto& cur : mgr.getLangExtension<lang::ArrayExtension>().getSymbols()) res.insert(cur);
			for(const auto& cur : mgr.getLangExtension<lang::ReferenceExtension>().getSymbols()) res.insert(cur);
			for(const auto& cur : mgr.getLangExtension<lang::ParallelExtension>().getSymbols()) res.insert(cur);
			for(const auto& cur : mgr.getLangExtension<lang::PointerExtension>().getSymbols()) res.insert(cur);
			for(const auto& cur : mgr.getLangExtension<lang::InputOutputExtension>().getSymbols()) res.insert(cur);

			// add initializer symbols (overrides standard if necessary)
			for(const auto& cur : init) res.insert(cur);

			// done
			return res;
		}

		parser::TypeAliasMap getStandardAliasMap(NodeManager& mgr) {
			parser::TypeAliasMap res;

			// load standard modules
			for(const auto& cur : mgr.getLangExtension<lang::ArrayExtension>().getTypeAliases()) res.insert(cur);
			for(const auto& cur : mgr.getLangExtension<lang::ReferenceExtension>().getTypeAliases()) res.insert(cur);
			for(const auto& cur : mgr.getLangExtension<lang::ParallelExtension>().getTypeAliases()) res.insert(cur);
			for(const auto& cur : mgr.getLangExtension<lang::PointerExtension>().getTypeAliases()) res.insert(cur);
			for(const auto& cur : mgr.getLangExtension<lang::InputOutputExtension>().getTypeAliases()) res.insert(cur);

			// done
			return res;
		}
	}

	// ---------------------------- Parser Integration -----------------------------------

	NodePtr IRBuilderBaseModule::parse(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseAny(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	NodePtr IRBuilderBaseModule::parse(const string& code, const EagerDefinitionMap& symbols) const {
		return parse(code, toLazy(symbols));
	}

	TypePtr IRBuilderBaseModule::parseType(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseType(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	TypePtr IRBuilderBaseModule::parseType(const string& code, const EagerDefinitionMap& symbols) const {
		return parseType(code, toLazy(symbols));
	}

	ExpressionPtr IRBuilderBaseModule::parseExpr(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseExpr(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	ExpressionPtr IRBuilderBaseModule::parseExpr(const string& code, const EagerDefinitionMap& symbols) const {
		return parseExpr(code, toLazy(symbols));
	}

	StatementPtr IRBuilderBaseModule::parseStmt(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseStmt(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	StatementPtr IRBuilderBaseModule::parseStmt(const string& code, const EagerDefinitionMap& symbols) const {
		return parseStmt(code, toLazy(symbols));
	}

	ProgramPtr IRBuilderBaseModule::parseProgram(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseProgram(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	ProgramPtr IRBuilderBaseModule::parseProgram(const string& code, const EagerDefinitionMap& symbols) const {
		return parseProgram(code, toLazy(symbols));
	}

	vector<NodeAddress> IRBuilderBaseModule::parseAddressesType(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseAddressesType(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	vector<NodeAddress> IRBuilderBaseModule::parseAddressesType(const string& code, const EagerDefinitionMap& symbols) const {
		return parseAddressesType(code, toLazy(symbols));
	}

	vector<NodeAddress> IRBuilderBaseModule::parseAddressesExpression(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseAddressesExpression(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	vector<NodeAddress> IRBuilderBaseModule::parseAddressesExpression(const string& code, const EagerDefinitionMap& symbols) const {
		return parseAddressesExpression(code, toLazy(symbols));
	}

	vector<NodeAddress> IRBuilderBaseModule::parseAddressesStatement(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseAddressesStatement(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	vector<NodeAddress> IRBuilderBaseModule::parseAddressesStatement(const string& code, const EagerDefinitionMap& symbols) const {
		return parseAddressesStatement(code, toLazy(symbols));
	}

	vector<NodeAddress> IRBuilderBaseModule::parseAddressesProgram(const string& code, const LazyDefinitionMap& symbols) const {
		return parser::parseAddressesProgram(manager, code, true, addStandardSymbols(manager, symbols), getStandardAliasMap(manager));
	}

	vector<NodeAddress> IRBuilderBaseModule::parseAddressesProgram(const string& code, const EagerDefinitionMap& symbols) const {
		return parseAddressesProgram(code, toLazy(symbols));
	}

	// ---------------------------- Standard Nodes -----------------------------------

	#include "insieme/core/generated/ir_builder_impl.inl"

	StringValuePtr IRBuilderBaseModule::stringValue(const char* str) const {
		return stringValue(string(str));
	}

	StringValuePtr IRBuilderBaseModule::stringValue(const string& str) const {
		return StringValue::get(manager, str);
	}

	BoolValuePtr IRBuilderBaseModule::boolValue(bool value) const {
		return BoolValue::get(manager, value);
	}

	CharValuePtr IRBuilderBaseModule::charValue(char value) const {
		return CharValue::get(manager, value);
	}

	IntValuePtr IRBuilderBaseModule::intValue(int value) const {
		return IntValue::get(manager, value);
	}

	UIntValuePtr IRBuilderBaseModule::uintValue(unsigned value) const {
		return UIntValue::get(manager, value);
	}

	// ---------------------------- Convenience -------------------------------------

	bool IRBuilderBaseModule::matchType(const std::string& typeStr, const core::TypePtr& irType) const {
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

	GenericTypePtr IRBuilderBaseModule::refType(const TypePtr& elementType, bool _const, bool _volatile, lang::ReferenceType::Kind kind) const {
		return lang::ReferenceType::create(elementType, _const, _volatile, kind);
	}

	TypePtr IRBuilderBaseModule::ptrType(const TypePtr& elementType, bool _const, bool _volatile) const {
		return lang::PointerType::create(elementType, _const, _volatile);
	}

	GenericTypePtr IRBuilderBaseModule::channelType(const TypePtr& elementType, const ExpressionPtr& size) const {
		return lang::ChannelType::create(elementType, size);
	}

	GenericTypePtr IRBuilderBaseModule::arrayType(const TypePtr& elementType) const {
		return lang::ArrayType::create(elementType);
	}

	GenericTypePtr IRBuilderBaseModule::arrayType(const TypePtr& elementType, const LiteralPtr& size) const {
		return lang::ArrayType::create(elementType, size);
	}

	GenericTypePtr IRBuilderBaseModule::arrayType(const TypePtr& elementType, const VariablePtr& size) const {
		return lang::ArrayType::create(elementType, size);
	}

	GenericTypePtr IRBuilderBaseModule::arrayType(const TypePtr& elementType, size_t size) const {
		return lang::ArrayType::create(elementType, size);
	}


	CompoundStmtPtr IRBuilderBaseModule::getDefaultedBodyPreTUMarker() const {
		return compoundStmt(literal(genericType("Insieme_defaulted_body_compound_marker"), "Insieme_defaulted_body_compound_marker"));
	}

	CompoundStmtPtr IRBuilderBaseModule::getDeletedBodyPreTUMarker() const {
		return compoundStmt(literal(genericType("Insieme_deleted_body_compound_marker"), "Insieme_deleted_body_compound_marker"));
	}


	TagTypePtr IRBuilderBaseModule::structType(const vector<std::pair<StringValuePtr, TypePtr>>& fields) const {
		return structType(::transform(fields, [&](const pair<StringValuePtr, TypePtr>& cur) { return field(cur.first, cur.second); }));
	}

	TagTypePtr IRBuilderBaseModule::unionType(const vector<FieldPtr>& fields) const {
		auto tag = tagTypeReference("");
		return tagType(tag, tagTypeDefinition({ { tag, unionRecord(fields) } }));
	}

	TagTypePtr IRBuilderBaseModule::unionType(const StringValuePtr& name, const vector<FieldPtr>& fields) const {
		auto tag = tagTypeReference(name);
		return tagType(tag, tagTypeDefinition({ { tag, unionRecord(name, fields) } }));
	}

	TagTypePtr IRBuilderBaseModule::unionType(const vector<std::pair<StringValuePtr, TypePtr>>& union_fields) const {
		auto fields = ::transform(union_fields, [&](const std::pair<StringValuePtr, TypePtr>& cur)->FieldPtr { return field(cur.first, cur.second); });
		return unionType(fields);
	}

	TagTypePtr IRBuilderBaseModule::unionType(const string& name, const FieldList& filds,
	                                          const ExpressionList& ctors, const ExpressionPtr& dtor, const bool dtorIsVirtual,
	                                          const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) const {
		return unionType(stringValue(name), fields(filds),
		                 expressions(convertList<Expression>(ctors)), dtor, boolValue(dtorIsVirtual),
		                 memberFunctions(mfuns), pureVirtualMemberFunctions(pvmfuns));
	}

	TagTypePtr IRBuilderBaseModule::unionType(const StringValuePtr& name, const FieldsPtr& fields,
	                                          const ExpressionsPtr& ctors, const ExpressionPtr& dtor, const BoolValuePtr& dtorIsVirtual,
	                                          const MemberFunctionsPtr& mfuns, const PureVirtualMemberFunctionsPtr& pvmfuns) const {
		auto tag = tagTypeReference(name);
				return tagType(tag, tagTypeDefinition({ { tag, unionRecord(name, fields, ctors, dtor, dtorIsVirtual, mfuns, pvmfuns) } }));
	}


	TagTypePtr IRBuilderBaseModule::structType(const vector<ParentPtr>& parents, const vector<FieldPtr>& fields) const {
		return structType(stringValue(""), IRBuilderBaseModule::parents(parents), fields);
	}

	TagTypePtr IRBuilderBaseModule::structType(const vector<TypePtr>& parents, const vector<FieldPtr>& fields) const {
		return structType(stringValue(""), IRBuilderBaseModule::parents(parents), fields);
	}

	TagTypePtr IRBuilderBaseModule::structType(const vector<ParentPtr>& parents, const vector<std::pair<StringValuePtr, TypePtr>>& fields) const {
		return structType(parents, ::transform(fields, [&](const pair<StringValuePtr, TypePtr>& cur) { return field(cur.first, cur.second); }));
	}

	TagTypePtr IRBuilderBaseModule::structType(const vector<TypePtr>& parents, const vector<std::pair<StringValuePtr, TypePtr>>& fields) const {
		return structType(parents, ::transform(fields, [&](const pair<StringValuePtr, TypePtr>& cur) { return field(cur.first, cur.second); }));
	}

	TagTypePtr IRBuilderBaseModule::structType(const vector<FieldPtr>& fields) const {
		return structType("", fields);
	}

	TagTypePtr IRBuilderBaseModule::structType(const string& name, const vector<FieldPtr>& fields) const {
		return structType(stringValue(name), fields);
	}

	TagTypePtr IRBuilderBaseModule::structType(const StringValuePtr& name, const vector<FieldPtr>& fields) const {
		return structType(name, {}, fields);
	}

	TagTypePtr IRBuilderBaseModule::structType(const StringValuePtr& name, const vector<ParentPtr>& parentsList, const vector<FieldPtr>& fields) const {
		auto tag = tagTypeReference(name);
		return tagType(tag, tagTypeDefinition({ { tag, structRecord(name, parents(parentsList), fields) } }));
	}

	TagTypePtr IRBuilderBaseModule::structType(const string& name, const ParentList& prents, const FieldList& filds,
	                                           const ExpressionList& ctors, const ExpressionPtr& dtor, const bool dtorIsVirtual,
	                                           const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) const {
		return structType(stringValue(name), parents(prents), fields(filds),
		                  expressions(convertList<Expression>(ctors)), dtor, boolValue(dtorIsVirtual),
		                  memberFunctions(mfuns), pureVirtualMemberFunctions(pvmfuns));
	}

	TagTypePtr IRBuilderBaseModule::structType(const StringValuePtr& name, const ParentsPtr& parents, const FieldsPtr& fields, const ExpressionsPtr& ctors,
	                                           const ExpressionPtr& dtor, const BoolValuePtr& dtorIsVirtual, const MemberFunctionsPtr& mfuns,
	                                           const PureVirtualMemberFunctionsPtr& pvmfuns) const {
		auto tag = tagTypeReference(name);
		return tagType(tag, tagTypeDefinition({ { tag, structRecord(name, parents, fields, ctors, dtor, dtorIsVirtual, mfuns, pvmfuns) } }));
	}

	LiteralPtr IRBuilderBaseModule::getLiteralForMember(const FunctionTypePtr& functionType, const std::string& memberName) const {
		if (!functionType->isMember()) {
			assert_fail() << "Builder: Requesting literal for member" << memberName << ", but it is not a member function type:\n" << dumpColor(functionType);
			return LiteralPtr();
		}
		std::string recordName;
		const auto ft = analysis::getObjectType(functionType);
		if (TagTypeReferencePtr ttref = ft.isa<TagTypeReferencePtr>()) recordName = ttref->getName()->getValue();
		if (GenericTypePtr gt = ft.isa<GenericTypePtr>()) recordName = gt->getName()->getValue();
		return literal(recordName + "::" + memberName, functionType);
	}

	LiteralPtr IRBuilderBaseModule::getLiteralForConstructor(const FunctionTypePtr& functionType) const {
		return getLiteralForMember(functionType, "ctor");
	}

	LiteralPtr IRBuilderBaseModule::getLiteralForDestructor(const FunctionTypePtr& functionType) const {
		return getLiteralForMember(functionType, "dtor");
	}

	LiteralPtr IRBuilderBaseModule::getLiteralForMemberFunction(const FunctionTypePtr& functionType, const std::string& memberName) const {
		return getLiteralForMember(functionType, memberName);
	}

	FieldPtr IRBuilderBaseModule::field(const string& name, const TypePtr& type) const {
		return field(stringValue(name), type);
	}

	TupleExprPtr IRBuilderBaseModule::tupleExpr(const vector<ExpressionPtr>& values) const {
		TupleTypePtr type = tupleType(extractTypes(values));
		return tupleExpr(type, Expressions::get(manager, values));
	}

	IfStmtPtr IRBuilderBaseModule::ifStmt(const ExpressionPtr& condition, const StatementPtr& thenBody, const StatementPtr& elseBody) const {
		if(!elseBody) { return ifStmt(condition, wrapBody(thenBody), getNoOp()); }
		return ifStmt(condition, wrapBody(thenBody), wrapBody(elseBody));
	}

	WhileStmtPtr IRBuilderBaseModule::whileStmt(const ExpressionPtr& condition, const StatementPtr& body) const {
		return whileStmt(condition, wrapBody(body));
	}

	ForStmtPtr IRBuilderBaseModule::forStmt(const DeclarationStmtPtr& var, const ExpressionPtr& end, const ExpressionPtr& step, const StatementPtr& body) const {
		return forStmt(var->getVariable(), var->getInitialization(), end, step, wrapBody(body));
	}

	ForStmtPtr IRBuilderBaseModule::forStmt(const VariablePtr& var, const ExpressionPtr& start, const ExpressionPtr& end, const ExpressionPtr& step,
	                              const StatementPtr& body) const {
		return forStmt(var, start, end, step, wrapBody(body));
	}

	SwitchStmtPtr IRBuilderBaseModule::switchStmt(const ExpressionPtr& switchExpr, const vector<std::pair<ExpressionPtr, StatementPtr>>& cases,
	                                    const StatementPtr& defaultCase) const {
		CompoundStmtPtr defCase = (defaultCase) ? wrapBody(defaultCase) : getNoOp();

		vector<SwitchCasePtr> caseList = ::transform(
		    cases, [&](const pair<ExpressionPtr, StatementPtr>& cur) { return switchCase(static_pointer_cast<LiteralPtr>(cur.first), wrapBody(cur.second)); });

		return switchStmt(switchExpr, switchCases(caseList), defCase);
	}

	SwitchStmtPtr IRBuilderBaseModule::switchStmt(const ExpressionPtr& switchExpr, const vector<SwitchCasePtr>& cases, const StatementPtr& defaultCase) const {
		return switchStmt(switchExpr, switchCases(cases), (defaultCase) ? wrapBody(defaultCase) : getNoOp());
	}

	FunctionTypePtr IRBuilderBaseModule::toPlainFunctionType(const FunctionTypePtr& funType) const {
		if(funType->isPlain()) { return funType; }
		return functionType(funType->getParameterTypes(), funType->getReturnType(), FK_PLAIN);
	}

	FunctionTypePtr IRBuilderBaseModule::toThickFunctionType(const FunctionTypePtr& funType) const {
		if(!funType->isPlain()) { return funType; }
		return functionType(funType->getParameterTypes(), funType->getReturnType(), FK_CLOSURE);
	}

	ExpressionPtr IRBuilderBaseModule::unitConsume(const ExpressionPtr& toConsume) const {
		return callExpr(getLangBasic().getUnit(), getLangBasic().getUnitConsume(), toConsume);
	}

	LiteralPtr IRBuilderBaseModule::stringLit(const string& str, const bool isConst) const {
		if (str.length() == 0 || str[0] != '"') {
			return stringLit("\"" + str + "\"", isConst);
		}

		// We calculate the payload length, which is:
		// * the length of the given string - 2 (because we added double quotes) + 1 (because clang counts the terminating \0 character)
		// * also we need to subtract 1 for each encountered escaped sequence, since they got encoded like this in the frontend
		int payloadLength = str.length() - 2 + 1 - std::count(str.begin(), str.end(), '\\');

		return literal(str, refType(arrayType(getLangBasic().getChar(), payloadLength), isConst));
	}

	namespace {

		template <typename T>
		bool isInRange(const int val) {
			return std::numeric_limits<T>::min() <= val && val <= std::numeric_limits<T>::max();
		}
	}

	LiteralPtr IRBuilderBaseModule::intLit(const int val, bool tight) const {
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

	LiteralPtr IRBuilderBaseModule::uintLit(const unsigned int val, bool tight) const {
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

	LiteralPtr IRBuilderBaseModule::integerLit(const int val, bool tight) const {
		if(val < 0) { return intLit(val, tight); }
		return uintLit((unsigned int)val, tight);
	}

	LiteralPtr IRBuilderBaseModule::boolLit(bool value) const {
		return (value) ? getLangBasic().getTrue() : getLangBasic().getFalse();
	}

	LiteralPtr IRBuilderBaseModule::floatLit(const string& value) const {
		return literal(getLangBasic().getReal4(), value);
	}

	LiteralPtr IRBuilderBaseModule::floatLit(float value) const {
		// special handling for de-normalized values
		if(std::fpclassify(value) == FP_SUBNORMAL) { return floatLit(format("%a", value)); }

		std::stringstream out;
		out << std::scientific << std::fixed << std::setprecision(std::numeric_limits<float>::digits10 + 1) << value << "f";
		assert_false(out.str().empty()) << "empty string? ";
		return floatLit(out.str());
	}

	LiteralPtr IRBuilderBaseModule::doubleLit(const string& value) const {
		return literal(getLangBasic().getReal8(), value);
	}

	LiteralPtr IRBuilderBaseModule::doubleLit(double value) const {
		// special handling for de-normalized values
		if(std::fpclassify(value) == FP_SUBNORMAL) { return floatLit(format("%a", value)); }

		std::stringstream out;
		out << std::scientific << std::fixed << std::setprecision(std::numeric_limits<float>::digits10 + 1) << value;
		assert_false(out.str().empty()) << "empty string? ";
		return doubleLit(out.str());
	}

	ExpressionPtr IRBuilderBaseModule::undefinedNew(const TypePtr& type) const {
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		if(analysis::isRefType(type)) {
			auto elementType = analysis::getReferencedType(type);
			return lang::buildRefCast(callExpr(refType(elementType), refExt.getRefNew(), getTypeLiteral(elementType)), type);
		}
		return callExpr(refType(type), refExt.getRefNew(), getTypeLiteral(type));
	}

	ExpressionPtr IRBuilderBaseModule::deref(const ExpressionPtr& subExpr) const {
		return lang::buildRefDeref(subExpr);
	}

	ExpressionPtr IRBuilderBaseModule::tryDeref(const ExpressionPtr& subExpr) const {
		if(!analysis::isRefType(subExpr->getType())) { return subExpr; }
		return deref(subExpr);
	}

	CallExprPtr IRBuilderBaseModule::refTemp(const TypePtr& type) const {
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(refType(type), refExt.getRefTemp(), getTypeLiteral(type));
	}

	CallExprPtr IRBuilderBaseModule::refTemp(const ExpressionPtr& subExpr) const {
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(refType(subExpr->getType()), refExt.getRefTempInit(), subExpr);
	}

	CallExprPtr IRBuilderBaseModule::refNew(const ExpressionPtr& subExpr) const {
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(refType(subExpr->getType()), refExt.getRefNewInit(), subExpr);
	}

	CallExprPtr IRBuilderBaseModule::refDelete(const ExpressionPtr& subExpr) const {
		auto& basic = manager.getLangBasic();
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(basic.getUnit(), refExt.getRefDelete(), subExpr);
	}

	CallExprPtr IRBuilderBaseModule::assign(const ExpressionPtr& target, const ExpressionPtr& value) const {
		assert_pred1(analysis::isRefType, target->getType());
		auto& basic = manager.getLangBasic();
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(basic.getUnit(), refExt.getRefAssign(), target, value);
	}

	ExpressionPtr IRBuilderBaseModule::refReinterpret(const ExpressionPtr& subExpr, const TypePtr& newElementType) const {
		assert_pred1(analysis::isRefType, subExpr->getType());
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
		return callExpr(refType(newElementType), refExt.getRefReinterpret(), subExpr, getTypeLiteral(newElementType));
	}


	ExpressionPtr IRBuilderBaseModule::invertSign(const ExpressionPtr& subExpr) const {
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

	ExpressionPtr IRBuilderBaseModule::negateExpr(const ExpressionPtr& boolExpr) const {
		assert_true(manager.getLangBasic().isBool(boolExpr->getType())) << "Cannot negate a non boolean expression.";
		return callExpr(manager.getLangBasic().getBool(), manager.getLangBasic().getBoolLNot(), boolExpr);
	}


	CallExprPtr IRBuilderBaseModule::arraySubscript(const ExpressionPtr& array, const ExpressionPtr& index) const {
		// check that the access is valid
		assert_pred1(lang::isArray, array);
		// build expression accessing the addressed element in the given array
		auto& refExt = getExtension<lang::ArrayExtension>();
		return callExpr(refExt.getArraySubscript(), array, index);
	}
	CallExprPtr IRBuilderBaseModule::arrayRefElem(const ExpressionPtr& array, const ExpressionPtr& index) const {
		// check that the access is valid
		assert_pred1(lang::isReference, array);
		assert_pred1(lang::isArray, analysis::getReferencedType(array));
		// build expression accessing the addressed element in the given array
		auto& refExt = getExtension<lang::ReferenceExtension>();
		return callExpr(refExt.getRefArrayElement(), array, index);
	}

	CallExprPtr IRBuilderBaseModule::arrayAccess(const ExpressionPtr& array, const ExpressionPtr& index) const {
		if(analysis::isRefType(array->getType())) { return arrayRefElem(array, index); }
		return arraySubscript(array, index);
	}

	DeclarationStmtPtr IRBuilderBaseModule::declarationStmt(const ExpressionPtr& value) const {
		return declarationStmt(value->getType(), value);
	}
	DeclarationStmtPtr IRBuilderBaseModule::declarationStmt(const TypePtr& type, const ExpressionPtr& value) const {
		return declarationStmt(variable(type), value);
	}
	DeclarationStmtPtr IRBuilderBaseModule::declarationStmt(const VariablePtr& value) const {
		if(!lang::isReference(value)) {
			return declarationStmt(value, getZero(value->getType()));
		}
		return declarationStmt(value, lang::buildRefDecl(value->getType()));
	}

	ReturnStmtPtr IRBuilderBaseModule::returnStmt(const ExpressionPtr& retVal) const {
		return returnStmt(retVal, retVal->getType());
	}

	ReturnStmtPtr IRBuilderBaseModule::returnStmt() const {
		return returnStmt(manager.getLangBasic().getUnitConstant());
	}

	CallExprPtr IRBuilderBaseModule::acquireLock(const ExpressionPtr& lock) const {
		assert_true(analysis::isRefOf(lock, manager.getLangExtension<lang::ParallelExtension>().getLock())) << "Cannot lock a non-lock type: " << dumpColor(lock) << " of type " << dumpColor(lock->getType());
		return callExpr(manager.getLangBasic().getUnit(), manager.getLangExtension<lang::ParallelExtension>().getLockAcquire(), lock);
	}
	CallExprPtr IRBuilderBaseModule::tryAcquireLock(const ExpressionPtr& lock) const {
		assert_true(analysis::isRefOf(lock, manager.getLangExtension<lang::ParallelExtension>().getLock())) << "Cannot tryLock a non-lock type: "  << dumpColor(lock) << " of type " << dumpColor(lock->getType());
		return callExpr(manager.getLangBasic().getBool(), manager.getLangExtension<lang::ParallelExtension>().getLockTryAcquire(), lock);
	}
	CallExprPtr IRBuilderBaseModule::releaseLock(const ExpressionPtr& lock) const {
		assert_true(analysis::isRefOf(lock, manager.getLangExtension<lang::ParallelExtension>().getLock())) << "Cannot unlock a non-lock type: " << dumpColor(lock) << " of type " << dumpColor(lock->getType());
		return callExpr(manager.getLangBasic().getUnit(), manager.getLangExtension<lang::ParallelExtension>().getLockRelease(), lock);
	}
	CallExprPtr IRBuilderBaseModule::initLock(const ExpressionPtr& lock) const {
		assert_true(analysis::isRefOf(lock, manager.getLangExtension<lang::ParallelExtension>().getLock())) << "Cannot init a non-lock type: " << dumpColor(lock) << " of type " << dumpColor(lock->getType());
		return callExpr(manager.getLangBasic().getUnit(), manager.getLangExtension<lang::ParallelExtension>().getLockInit(), lock);
	}

	CallExprPtr IRBuilderBaseModule::atomicOp(const ExpressionPtr& location, const ExpressionPtr& testFunc, const ExpressionPtr& replaceFunc) {
		assert_true(core::analysis::isRefType(location->getType())) << "Atomic must be applied on ref.";
		// should also check types of testFunc and replaceFunc
		return callExpr(analysis::getReferencedType(location->getType()), manager.getLangExtension<lang::ParallelExtension>().getAtomic(), location, testFunc, replaceFunc);
	}

	CallExprPtr IRBuilderBaseModule::atomicAssignment(const CallExprPtr& assignment) {
		const auto& basic = manager.getLangBasic();
		auto& parExt = manager.getLangExtension<lang::ParallelExtension>();
		auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();

		assert_true(assignment->getNumArguments() >= 1) << "Unsupported atomic assignment structure - call needs at least one argument";

		// handle pre/post increment
		if(lang::isRefMathOp(assignment)) {
			const auto& lhs = assignment->getArgument(0);
			const auto one = literal("1", analysis::getReferencedType(lhs));
			if(refExt.isCallOfGenPreInc(assignment)) return callExpr(parExt.getAtomicAddAndFetch(), lhs, one);
			if(refExt.isCallOfGenPostInc(assignment)) return callExpr(parExt.getAtomicFetchAndAdd(), lhs, one);
			if(refExt.isCallOfGenPreDec(assignment)) return callExpr(parExt.getAtomicSubAndFetch(), lhs, one);
			if(refExt.isCallOfGenPostDec(assignment)) return callExpr(parExt.getAtomicFetchAndSub(), lhs, one);
		}

		// handle compound assignment
		if(lang::isCompoundAssignmentOperation(assignment)) {
			auto& cE = manager.getLangExtension<lang::CompoundOpsExtension>();
			const auto& lhs = assignment->getArgument(0);
			const auto& rhs = assignment->getArgument(1);
			if(cE.isCallOfCompAssignAdd(assignment)) return callExpr(parExt.getAtomicAddAndFetch(), lhs, rhs);
			if(cE.isCallOfCompAssignSubtract(assignment)) return callExpr(parExt.getAtomicSubAndFetch(), lhs, rhs);
			if(cE.isCallOfCompAssignBitwiseAnd(assignment)) return callExpr(parExt.getAtomicAndAndFetch(), lhs, rhs);
			if(cE.isCallOfCompAssignBitwiseOr(assignment)) return callExpr(parExt.getAtomicOrAndFetch(), lhs, rhs);
			if(cE.isCallOfCompAssignBitwiseXor(assignment)) return callExpr(parExt.getAtomicXorAndFetch(), lhs, rhs);
			assert_fail() << "Unsupported atomic operation:\n" << dumpColor(assignment);
		}

		assert_true(refExt.isRefAssign(assignment->getFunctionExpr()))
		    << "Trying to build atomic assignment from non-assigment:" << dumpColor(assignment);

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

	CallExprPtr IRBuilderBaseModule::atomicConditional(const IfStmtPtr& statement) {
		assert_fail() << "Not implemented";
		return CallExprPtr();
	}


	CallExprPtr IRBuilderBaseModule::pickVariant(const ExpressionList& variants) const {
		assert_false(variants.empty()) << "Variant list must not be empty!";
		assert(all(variants, [&](const ExpressionPtr& cur) { return *cur->getType() == *variants[0]->getType(); })
		       && "All options have to have the same type.");
		return callExpr(variants[0]->getType(), manager.getLangBasic().getPick(),
		                encoder::toIR<ExpressionList, encoder::DirectExprListConverter>(manager, variants));
	}

	CallExprPtr IRBuilderBaseModule::pickInRange(const ExpressionPtr& id, const ExpressionPtr& max, const ExpressionPtr& qualLB, const ExpressionPtr& qualUB,
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
			assert_eq(functionExpr->getType()->getNodeType(), NT_FunctionType) << "Function expression is not a function!" << "\n - fun expr: " << *functionExpr
				<< "\n - of type: " << *functionExpr->getType() << "\n - with node type: " << functionExpr->getType()->getNodeType();

			// extract function type
			FunctionTypePtr funType = static_pointer_cast<const FunctionType>(functionExpr->getType());

			// deduce return type
			core::TypeList argumentTypes;
			::transform(arguments, back_inserter(argumentTypes), [](const ExpressionPtr& cur) { return cur->getType(); });

			TypePtr res = types::deduceReturnType(funType, argumentTypes, false);

			assert_true(res) << "Unable to deduce return type!\n"
					"Function Type:  " << *funType << "\n"
					"Argument Types:   " << join(",", argumentTypes, print<deref<TypePtr>>()) << "\n";

			return res;
		}

		CallExprPtr createCall(const IRBuilderBaseModule& builder, const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
			// create calling expression
			return builder.callExpr(resultType, functionExpr, arguments);
		}
	}

	CallExprPtr IRBuilderBaseModule::callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) const {
		// use deduced return type to construct call
		return callExpr(deduceReturnTypeForCall(functionExpr, arguments), functionExpr, arguments);
	}
	CallExprPtr IRBuilderBaseModule::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr) const {
		return createCall(*this, resultType, functionExpr, toVector<ExpressionPtr>());
	}
	CallExprPtr IRBuilderBaseModule::callExpr(const ExpressionPtr& functionExpr, const DeclarationList& arguments) const {
		auto args = ::transform(arguments, [](const DeclarationPtr& d){ return transform::extractInitExprFromDecl(d); });
		return callExpr(deduceReturnTypeForCall(functionExpr, args), functionExpr, arguments);
	}

	LambdaPtr IRBuilderBaseModule::lambda(const FunctionTypePtr& type, const ParametersPtr& params, const StatementPtr& body) const {
		return lambda(type, params, wrapBody(body));
	}

	LambdaPtr IRBuilderBaseModule::lambda(const FunctionTypePtr& type, const VariableList& params, const StatementPtr& body) const {
		return lambda(type, params, wrapBody(body));
	}

	LambdaExprPtr IRBuilderBaseModule::lambdaExpr(const TypePtr& returnType, const VariableList& params, const StatementPtr& body, const std::string& name, FunctionKind kind) const {
		auto paramVarTypes = extractTypes(params);
		assert_true(::all(paramVarTypes, lang::isReference)) << "All parameters need to be reference types, given: " << paramVarTypes;
		auto paramTypes = ::transform(paramVarTypes, transform::dematerialize);
		return lambdaExpr(functionType(paramTypes, returnType, kind), params, wrapBody(body), name);
	}

	LambdaExprPtr IRBuilderBaseModule::lambdaExpr(const FunctionTypePtr& type, const VariableList& params, const StatementPtr& body, const std::string& name) const {
		assert_true(::all(extractTypes(params), lang::isReference)) << "All parameters need to be reference types, given: " << extractTypes(params);
		return lambdaExpr(lambda(type, params, body), name);
	}

	BindExprPtr IRBuilderBaseModule::bindExpr(const VariableList& params, const CallExprPtr& call) const {
		FunctionTypePtr type = functionType(extractTypes(params), call->getType(), FK_CLOSURE);
		return bindExpr(type, parameters(params), call);
	}

	JobExprPtr IRBuilderBaseModule::jobExpr(const ExpressionPtr& threadNumRange, const ExpressionPtr& body) const {
		GenericTypePtr type = static_pointer_cast<GenericTypePtr>(manager.getLangBasic().getJob());
		return jobExpr(type, threadNumRange, body);
	}

	JobExprPtr IRBuilderBaseModule::jobExpr(const ExpressionPtr& rangeLowerBound, const ExpressionPtr& rangeUpperBound, const ExpressionPtr& body) const {
		GenericTypePtr type = static_pointer_cast<GenericTypePtr>(manager.getLangBasic().getJob());
		const ExpressionPtr threadNumRange = getThreadNumRange(rangeLowerBound, rangeUpperBound);
		return jobExpr(type, threadNumRange, body);
	}

	JobExprPtr IRBuilderBaseModule::jobExpr(const ExpressionPtr& rangeLowerBound, const ExpressionPtr& rangeUpperBound, const ExpressionPtr& rangeMod,
	                                        const ExpressionPtr& body) const {
		GenericTypePtr type = static_pointer_cast<GenericTypePtr>(manager.getLangBasic().getJob());
		const ExpressionPtr threadNumRange = getThreadNumRange(rangeLowerBound, rangeUpperBound, rangeMod);
		return jobExpr(type, threadNumRange, body);
	}

	JobExprPtr IRBuilderBaseModule::jobExprUnbounded(const ExpressionPtr& rangeLowerBound, const ExpressionPtr& body) const {
		GenericTypePtr type = static_pointer_cast<GenericTypePtr>(manager.getLangBasic().getJob());
		const ExpressionPtr threadNumRange = getThreadNumRange(rangeLowerBound);
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

	JobExprPtr IRBuilderBaseModule::jobExpr(const StatementPtr& stmt, int numThreads) const {
		ExpressionPtr jobBody = (isJobBody(stmt)) ? stmt.as<ExpressionPtr>() : transform::extractLambda(manager, stmt);

		return jobExpr((numThreads < 1) ? getThreadNumRange(1) : getThreadNumRange(numThreads, numThreads), jobBody);
	}

	MarkerExprPtr IRBuilderBaseModule::markerExpr(const ExpressionPtr& subExpr, unsigned id) const {
		return markerExpr(subExpr, uintValue(id));
	}

	MarkerExprPtr IRBuilderBaseModule::markerExpr(const ExpressionPtr& subExpr, const UIntValuePtr& id) const {
		return markerExpr(id, subExpr);
	}

	MarkerStmtPtr IRBuilderBaseModule::markerStmt(const StatementPtr& subStmt, unsigned id) const {
		return markerStmt(subStmt, uintValue(id));
	}

	MarkerStmtPtr IRBuilderBaseModule::markerStmt(const StatementPtr& subStmt, const UIntValuePtr& id) const {
		return markerStmt(id, subStmt);
	}

	CallExprPtr IRBuilderBaseModule::getThreadNumRange(unsigned min) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateMinRange(), literal(type, toString(min)));
	}
	CallExprPtr IRBuilderBaseModule::getThreadNumRange(unsigned min, unsigned max) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateBoundRange(), literal(type, toString(min)), literal(type, toString(max)));
	}

	CallExprPtr IRBuilderBaseModule::getThreadNumRange(const ExpressionPtr& min) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateMinRange(), numericCast(min, type));
	}
	CallExprPtr IRBuilderBaseModule::getThreadNumRange(const ExpressionPtr& min, const ExpressionPtr& max) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateBoundRange(), numericCast(min, type), numericCast(max, type));
	}
	CallExprPtr IRBuilderBaseModule::getThreadNumRange(const ExpressionPtr& min, const ExpressionPtr& max, const ExpressionPtr& mod) const {
		TypePtr type = manager.getLangBasic().getUInt8();
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getCreateBoundRangeMod(), numericCast(min, type), numericCast(max, type),
		                numericCast(mod, type));
	}

	CallExprPtr IRBuilderBaseModule::getThreadGroup(ExpressionPtr level) const {
		if(!level) { level = uintLit(0); }
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getGetThreadGroup(), level);
	}
	CallExprPtr IRBuilderBaseModule::getThreadGroupSize(ExpressionPtr level) const {
		if(!level) { level = uintLit(0); }
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getGetGroupSize(), level);
	}
	CallExprPtr IRBuilderBaseModule::getThreadId(ExpressionPtr level) const {
		if(!level) { level = uintLit(0); }
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getGetThreadId(), level);
	}
	CallExprPtr IRBuilderBaseModule::getDefaultThreads() const {
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getGetDefaultThreads());
	}

	CallExprPtr IRBuilderBaseModule::barrier(ExpressionPtr threadgroup) const {
		if(!threadgroup) { threadgroup = getThreadGroup(); }
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getBarrier(), threadgroup);
	}

	CallExprPtr IRBuilderBaseModule::mergeAll() const {
		return callExpr(manager.getLangExtension<lang::ParallelExtension>().getMergeAll());
	}

	CallExprPtr IRBuilderBaseModule::pfor(const ExpressionPtr& body, const ExpressionPtr& start, const ExpressionPtr& end, ExpressionPtr step) const {
		if(!step) { step = uintLit(1); }
		assert_true(manager.getLangBasic().isInt(start->getType()));
		assert_true(manager.getLangBasic().isInt(end->getType()));
		assert_true(manager.getLangBasic().isInt(step->getType()));
		auto ret = callExpr(getLangBasic().getUnit(), manager.getLangExtension<lang::ParallelExtension>().getPFor(), toVector<ExpressionPtr>(getThreadGroup(), start, end, step, body));
		// LOG(INFO) <<  "%%% generated pfor:\n "<< core::printer::PrettyPrinter(ret) << "\n";
		return ret;
	}

	CallExprPtr IRBuilderBaseModule::pfor(const ForStmtPtr& initialFor) const {
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

	CallExprPtr IRBuilderBaseModule::parallel(const StatementPtr& stmt, int numThreads) const {
		auto& ext = manager.getLangExtension<lang::ParallelExtension>();
		return callExpr(ext.getThreadGroup(), ext.getParallel(), jobExpr(stmt, numThreads));
	}

	CallExprPtr IRBuilderBaseModule::accessMember(const ExpressionPtr& structExpr, const string& member) const {
		return accessMember(structExpr, stringValue(member));
	}

	CallExprPtr IRBuilderBaseModule::accessMember(const ExpressionPtr& structExpr, const StringValuePtr& member) const {
		core::TypePtr type = structExpr->getType();

		// if it is a ref type, use refMember function
		if(analysis::isRefType(type)) { return refMember(structExpr, member); }

		// must be a tag type at this point
		auto tagType = type.isa<TagTypePtr>();
		assert_true(tagType) << "Cannot access non-tag type!";

		// handle recursions
		if (tagType->isRecursive()) {
			tagType = tagType->peel();
		}

		// get the type of the selected field
		TypePtr memberType = tagType->getFieldType(member);

		// create access instruction
		ExpressionPtr access = getLangBasic().getCompositeMemberAccess();
		return callExpr(memberType, access, structExpr, getIdentifierLiteral(member), getTypeLiteral(memberType));
	}

	CallExprPtr IRBuilderBaseModule::refMember(const ExpressionPtr& structExpr, const string& member) const {
		return refMember(structExpr, stringValue(member));
	}

	CallExprPtr IRBuilderBaseModule::refMember(const ExpressionPtr& structExpr, const StringValuePtr& member) const {
		core::TypePtr type = structExpr->getType();
		assert_pred1(analysis::isRefType, type) << "Cannot deref non-ref type";

		core::TypePtr elementType = analysis::getReferencedType(type);

		// handle recursive types
		auto tagType = elementType.isa<TagTypePtr>();
		assert_true(tagType) << "Can not access member of non-tag-type!";
		if (tagType->isRecursive()) {
			tagType = tagType->peel();
		}

		// get member of addressed type
		core::TypePtr memberType = tagType->getFieldType(member);
		assert_true(memberType) << "No member " << member->getValue() << " found in\n" << dumpPretty(structExpr->getType());

		// create access instruction
		core::ExpressionPtr access = manager.getLangExtension<lang::ReferenceExtension>().getRefMemberAccess();
		return callExpr(access, structExpr, getIdentifierLiteral(member), getTypeLiteral(memberType));
	}

	CallExprPtr IRBuilderBaseModule::accessComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const {
		unsigned idx = extractNumberFromExpression(component);
		return accessComponent(tupleExpr, idx);
	}

	CallExprPtr IRBuilderBaseModule::accessComponent(ExpressionPtr tupleExpr, unsigned component) const {
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

	CallExprPtr IRBuilderBaseModule::refComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const {
		unsigned idx = extractNumberFromExpression(component);
		return refComponent(tupleExpr, idx);
	}
	CallExprPtr IRBuilderBaseModule::refComponent(ExpressionPtr tupleExpr, unsigned component) const {
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
		return callExpr(access, tupleExpr, index, typeLiteral);
	}


	CompoundStmtPtr IRBuilderBaseModule::getNoOp() const {
		return compoundStmt();
	}

	bool IRBuilderBaseModule::isNoOp(const NodePtr& p) const {
		return *p == *getNoOp();
	}

	TypePtr IRBuilderBaseModule::getTypeLiteralType(const TypePtr& type) const {
		return genericType("type", toVector(type));
	}

	TypePtr IRBuilderBaseModule::numericType(int64_t value) const {
		return numericType(literal(format("%d", value), getLangBasic().getIntInf()));
	}

	LiteralPtr IRBuilderBaseModule::getTypeLiteral(const TypePtr& type) const {
		auto literalType = getTypeLiteralType(type);
		return literal(literalType, "type_literal");
	}

	LiteralPtr IRBuilderBaseModule::getIdentifierLiteral(const string& value) const {
		return getIdentifierLiteral(stringValue(value));
	}

	LiteralPtr IRBuilderBaseModule::getIdentifierLiteral(const core::StringValuePtr& value) const {
		return literal(getLangBasic().getIdentifier(), value);
	}

	ExpressionPtr IRBuilderBaseModule::scalarToVector(const TypePtr& type, const ExpressionPtr& subExpr) const {
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

	TypePtr IRBuilderBaseModule::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a) const {
		return infereExprTypeInternal(op, a);
	}

	TypePtr IRBuilderBaseModule::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b) const {
		return infereExprTypeInternal(op, a, b);
	}

	TypePtr IRBuilderBaseModule::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& c) const {
		return infereExprTypeInternal(op, a, b, c);
	}


	namespace {

		GenericTypePtr toSigned(const IRBuilderBaseModule& builder, const GenericTypePtr& type) {
			assert_true(builder.getLangBasic().isScalarType(type)) << "Can not alter non-scalar type to signed!";

			// check whether a modification is required
			if(builder.getLangBasic().isUnsignedInt(type)) {
				// alter to signed alternative
				return builder.genericType("int", toVector(type->getTypeParameter(0)));
			}
			return type;
		}
	}


	LiteralPtr IRBuilderBaseModule::minus(const LiteralPtr& lit) const {
		assert_true(getLangBasic().isScalarType(lit->getType())) << "Can not change sign of non-scalar type!";

		// update type of literal to support signed
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

	ExpressionPtr IRBuilderBaseModule::minus(const ExpressionPtr& a) const {
		assert_true(getLangBasic().isScalarType(a->getType())) << "Can not change sign of non-scalar type!";

		// check literal type
		if(a->getNodeType() == NT_Literal) { return minus(a.as<LiteralPtr>()); }

		// update type of literal to support signed
		TypePtr type = toSigned(*this, a->getType().as<GenericTypePtr>());

		ExpressionPtr value = numericCast(a,type);

		// return 0 - a
		return sub(getZero(type), a);
	}

	ExpressionPtr IRBuilderBaseModule::numericCast(const core::ExpressionPtr& expr, const core::TypePtr& targetType) const {
		if(expr->getType() == targetType) return expr;

		// special case for enum to numeric
		if(lang::isEnum(expr)) {
			return numericCast(core::lang::buildEnumToInt(expr), targetType);
		}
		// special case for numeric to enum
		if(lang::isEnum(targetType)) {
			return core::lang::buildEnumFromInt(targetType, expr);
		}

		return callExpr(targetType, getLangBasic().getNumericCast(), expr, getTypeLiteral(targetType));
	}

	core::ExpressionPtr IRBuilderBaseModule::getZero(const core::TypePtr& type) const {
			// if it is an integer ...
			if(manager.getLangBasic().isInt(type)) { return literal(type, "0"); }

			// if it is a real ..
			if(manager.getLangBasic().isReal(type)) { return literal(type, "0.0"); }

			// if it is the bool type
			if(manager.getLangBasic().isBool(type)) { return boolLit(false); }

			// if it is the char type
			if(manager.getLangBasic().isChar(type)) { return literal(type, "'\\0'"); }

			// if it is a ref type ...
			if(analysis::isRefType(type)) {
					// return NULL for the specific type
					return lang::buildRefNull(type);
			}

			// if it is a pointer type ...
			if(lang::isPointer(type)) {
				return lang::buildPtrNull(type);
			}

			// if it is an enum ...
			if(lang::isEnum(type)) {
				auto& enumExt = manager.getLangExtension<lang::EnumExtension>();
				// this is what gcc does: { enum { A=1 } var1; cout << var1; }. Prints 0.
				return callExpr(type, enumExt.getEnumFromInt(), getTypeLiteral(type), getZero(lang::getEnumIntType(type)));
			}

			// if it is a struct ...
			if(auto structType = analysis::isStruct(type)) {
				ExpressionList initers;
				for_each(structType->getFields(), [&](const FieldPtr& cur) { initers.push_back(getZero(cur->getType())); });
				return deref(initExpr(lang::buildRefTemp(type), initers));
			}

			// if it is a union type ...
			if(auto unionType = analysis::isUnion(type)) {
				// in case it is a an empty union
				if(unionType->getFields().empty()) { assert_fail() << "Empty Unions are not allowed!"; }

				// init the first member
				auto first = unionType->getFields()[0];
				return deref(initExpr(lang::buildRefTemp(type), getZero(first->getType())));
			}

			// if it is a function type -- used for function pointers
			if(type.isa<core::FunctionTypePtr>()) {
					// return NULL for the specific type
					auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
					return deref(refReinterpret(refExt.getRefNull(), type));
			}

			// add support for unit
			if(manager.getLangBasic().isUnit(type)) { return manager.getLangBasic().getUnitConstant(); }

			// for array types
			if(lang::isArray(type)) {
				return deref(initExpr(lang::buildRefTemp(type), getZero(lang::getArrayElementType(type))));
			}

			// for all other generic types we return a generic zero value
			if(type.isa<GenericTypePtr>()) {
				return callExpr(type, getLangBasic().getZero(), getTypeLiteral(type));
			}

			// assume this is an intercepted member initializer, and use zero literal
			if(type.isa<TypeVariablePtr>()) {
				return callExpr(type, getLangBasic().getZero(), getTypeLiteral(type));
			}

			// TODO: extend for more types
			LOG(FATAL) << "Encountered unsupported type: " << *type;
			assert_fail() << "Given type not supported yet!";

			// fall-back => return a literal 0 of the corresponding type
			return literal(type, "0");
	}

	// ---------------------------- Utilities ---------------------------------------


	unsigned IRBuilderBaseModule::extractNumberFromExpression(ExpressionPtr& expr) const {
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
	CompoundStmtPtr IRBuilderBaseModule::wrapBody(const StatementPtr& stmt) const {
		if(stmt->getNodeType() == NT_CompoundStmt) { return static_pointer_cast<CompoundStmtPtr>(stmt); }
		return CompoundStmt::get(stmt->getNodeManager(), stmt);
	}

	ExpressionPtr IRBuilderBaseModule::wrapLazy(const ExpressionPtr& expr) const {
		// if it is a expression, bind free variables
		VariableList list = analysis::getFreeVariables(expr);
		std::sort(list.begin(), list.end(), compare_target<VariablePtr>());

		// convert all variables to references
		auto ingredients = transform::materialize({list, returnStmt(expr)});
		auto funType = functionType(extractTypes(list), expr->getType());
		// replace return statement types
		ingredients.body = core::transform::transformBottomUpGen(ingredients.body, [&funType, this](const core::ReturnStmtPtr& ret) {
			auto retT = ret->getReturnType();
			auto replacementT = core::transform::materialize(funType->getReturnType());
			return returnStmt(ret->getReturnExpr(), replacementT);
		});

		ExpressionPtr res = lambdaExpr(funType, ingredients.params, ingredients.body);

		// if there are no free variables ...
		if(list.empty()) {
			// ... no capturing is necessary
			return res;
		}

		// otherwise: bind the free variables
		return bindExpr(VariableList(), callExpr(expr->getType(), res, convertList<Expression>(list)));
	}

	CallExprPtr IRBuilderBaseModule::print(const string& format, const ExpressionList& args) const {
		return print(stringLit(format), args);
	}

	CallExprPtr IRBuilderBaseModule::print(const ExpressionPtr& format, const ExpressionList& args) const {
		auto& basic = getLangBasic();
		auto& ext = getExtension<lang::InputOutputExtension>();
		return callExpr(basic.getUnit(), ext.getPrint(), format, pack(args));
	}

	CallExprPtr IRBuilderBaseModule::pack(const ExpressionList& values) const {
		auto& vaExt = manager.getLangExtension<lang::VarArgsExtension>();
		return callExpr(vaExt.getVarList(), vaExt.getVarlistPack(), tupleExpr(values));
	}

	CallExprPtr IRBuilderBaseModule::select(const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& op) const {
		const auto& basic = manager.getLangBasic();
		const core::LiteralPtr& select = basic.getSelect();
		return callExpr(infereExprType(select, a, b, op), select, a, b, op);
	}

	CallExprPtr IRBuilderBaseModule::select(const ExpressionPtr& a, const ExpressionPtr& b, lang::BasicGenerator::Operator op) const {
		return select(a, b, manager.getLangBasic().getOperator(a->getType(), op));
	}

	// helper for the pointwise operation
	CallExprPtr IRBuilderBaseModule::pointwise(const ExpressionPtr& callee) const {
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
	CallExprPtr IRBuilderBaseModule::accuracyHigh(const ExpressionPtr& callee) const {
		const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
		assert_true(funTy) << "The argument of accuraccy high must be a function";
		int nArgs = funTy->getParameterTypeList().size();
		assert_true(nArgs <= 2 && nArgs > 0) << "The function for accuraccy high must take one or two arguments";

		const auto& basic = manager.getLangBasic();
		return nArgs == 1 ? callExpr(funTy, basic.getAccuracyFastUnary(), callee) : callExpr(funTy, basic.getAccuracyFastBinary(), callee);
	}
	CallExprPtr IRBuilderBaseModule::accuracyBestEffort(const ExpressionPtr& callee) const {
		const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
		assert_true(funTy) << "The argument of accuraccy best effort must be a function";
		int nArgs = funTy->getParameterTypeList().size();
		assert_true(nArgs <= 2 && nArgs > 0) << "The function for accuraccy best effort must take one or two arguments";

		const auto& basic = manager.getLangBasic();
		return nArgs == 1 ? callExpr(funTy, basic.getAccuracyBestEffortUnary(), callee) : callExpr(funTy, basic.getAccuracyBestEffortBinary(), callee);
	}
	CallExprPtr IRBuilderBaseModule::accuracyFast(const ExpressionPtr& callee) const {
		const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
		assert_true(funTy) << "The argument of accuraccy fast must be a function";
		int nArgs = funTy->getParameterTypeList().size();
		assert_true(nArgs <= 2 && nArgs > 0) << "The function for accuraccy fast must take one or two arguments";

		const auto& basic = manager.getLangBasic();
		return nArgs == 1 ? callExpr(basic.getAccuracyFastUnary(), callee) : callExpr(basic.getAccuracyFastBinary(), callee);
	}

	CallExprPtr IRBuilderBaseModule::vectorPermute(const ExpressionPtr& dataVec, const ExpressionPtr& permutationVec) const {
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
