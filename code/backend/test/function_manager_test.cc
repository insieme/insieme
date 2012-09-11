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

#include <gtest/gtest.h>

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_builder.h"

#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/variable_manager.h"

#include "insieme/backend/c_ast/c_ast_printer.h"


namespace insieme {
namespace backend {

namespace {
	class TestNameManager : public SimpleNameManager {
	public:
		TestNameManager() : SimpleNameManager("test") {};
		virtual string getName(const core::NodePtr& ptr, const string& fragment) {
			return "name";
		}
	};
}


// a small test verifying that the given substr is contained within the given string
bool containsSubString(const string& str, const string& substr) {
	return str.find(substr) != string::npos;
}

// a small test verifying that the given substr is contained within the given string
bool notContainsSubString(const string& str, const string& substr) {
	return !containsSubString(str, substr);
}

TEST(FunctionManager, Literals) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	TestNameManager nameManager;
	c_ast::SharedCodeFragmentManager fragmentManager = c_ast::CodeFragmentManager::createShared();

	Converter converter;
	TypeManager typeManager(converter);
	converter.setTypeManager(&typeManager);
	converter.setNameManager(&nameManager);
	converter.setNodeManager(&nodeManager);
	converter.setFragmentManager(fragmentManager);

	FunctionManager funManager(converter);
	converter.setFunctionManager(&funManager);


	// create a function type
	core::TypePtr int4 = basic.getInt4();
	core::TypePtr real4 = basic.getFloat();
	core::TypePtr boolean = basic.getBool();

	core::FunctionTypePtr funType = builder.functionType(toVector(real4, boolean), int4);
	EXPECT_EQ("((real<4>,bool)->int<4>)", toString(*funType));

	// create a literal with that type
	core::LiteralPtr literal = builder.literal(funType, "myFun");

	// obtain information
	FunctionInfo info = funManager.getInfo(literal);

	// to be tested:
	//  - name of the function
	//  - prototype of the function
	//  - lambda wrapper

	EXPECT_EQ("myFun", toC(info.function->name));
	EXPECT_TRUE((bool)info.prototype);
	EXPECT_TRUE((bool)info.lambdaWrapperName);
	EXPECT_TRUE((bool)info.lambdaWrapper);

	EXPECT_EQ("myFun_wrap", toC(info.lambdaWrapperName));

	EXPECT_PRED2(containsSubString, toC(info.prototype), "int32_t myFun(float p1, bool p2);");
	EXPECT_EQ("int32_t myFun_wrap(name* closure, float p1, bool p2) {\n    return myFun(p1, p2);\n}\n", toC(info.lambdaWrapper));

	// check get value (value to be used when passing function as an argument)
	ConversionContext context(converter);
	EXPECT_EQ("&myFun", toC(funManager.getValue(literal, context)));

	// TODO: check the call creation
}


TEST(FunctionManager, Lambda) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	Converter converter;
	converter.setNodeManager(&nodeManager);

	TestNameManager nameManager;
	converter.setNameManager(&nameManager);

	c_ast::SharedCodeFragmentManager fragmentManager = c_ast::CodeFragmentManager::createShared();
	converter.setFragmentManager(fragmentManager);

	TypeManager typeManager(converter);
	converter.setTypeManager(&typeManager);

	StmtConverter stmtConverter(converter);
	converter.setStmtConverter(&stmtConverter);

	FunctionManager funManager(converter);
	converter.setFunctionManager(&funManager);


	// create a function type
	core::TypePtr int4 = basic.getInt4();
	core::TypePtr real4 = basic.getFloat();
	core::TypePtr boolean = basic.getBool();

	core::FunctionTypePtr funType = builder.functionType(toVector(real4, boolean), int4);
	EXPECT_EQ("((real<4>,bool)->int<4>)", toString(*funType));

	// create parameters
	vector<core::VariablePtr> params;
	params.push_back(builder.variable(real4, 1));
	params.push_back(builder.variable(boolean, 2));

	// create body
	core::StatementPtr body = builder.returnStmt(builder.intLit(12));

	// create the lambda
	core::LambdaExprPtr lambda = builder.lambdaExpr(funType, params, body);

	// obtain information
	LambdaInfo info = funManager.getInfo(lambda);

	// to be tested:
	//  - function
	//  - prototype = definition
	//  - lambda wrapper

	EXPECT_EQ("name", toC(info.function->name));
	EXPECT_TRUE((bool)info.prototype);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.lambdaWrapper);
	EXPECT_TRUE((bool)info.lambdaWrapperName);

	EXPECT_EQ("name_wrap", toC(info.lambdaWrapperName));
	EXPECT_PRED2(containsSubString, toC(info.prototype), "int32_t name(float name, bool name)");
	EXPECT_PRED2(containsSubString, toC(info.definition), "int32_t name(float name, bool name) {\n    return 12;\n}");
	EXPECT_PRED2(containsSubString, toC(info.lambdaWrapper), "int32_t name_wrap(name* closure, float p1, bool p2) {\n    return name(p1, p2);\n}");

	// since function is not recursive, no seperation of prototype and definition should be required
	EXPECT_EQ(info.prototype, info.definition);

	// check get value (value to be used when passing function as an argument)
	ConversionContext context(converter);
	EXPECT_EQ("&name", toC(funManager.getValue(lambda, context)));

	// TODO: check for call
}


TEST(FunctionManager, MutualRecursiveLambda) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	Converter converter;
	converter.setNodeManager(&nodeManager);

	TestNameManager nameManager;
	converter.setNameManager(&nameManager);

	c_ast::SharedCodeFragmentManager fragmentManager = c_ast::CodeFragmentManager::createShared();
	converter.setFragmentManager(fragmentManager);

	TypeManager typeManager(converter);
	converter.setTypeManager(&typeManager);

	StmtConverter stmtConverter(converter);
	converter.setStmtConverter(&stmtConverter);

	FunctionManager funManager(converter);
	converter.setFunctionManager(&funManager);

	// create a function type
	core::TypePtr int4 = basic.getInt4();
	core::TypePtr boolean = basic.getBool();

	core::FunctionTypePtr funType = builder.functionType(toVector(int4), boolean);
	EXPECT_EQ("((int<4>)->bool)", toString(*funType));

	// create parameters
	core::VariablePtr param = builder.variable(int4, 3);
	vector<core::VariablePtr> params = toVector(param);

	// create function variables
	core::VariablePtr varEven = builder.variable(funType, 1);
	core::VariablePtr varOdd = builder.variable(funType, 2);

	// create body
	core::StatementPtr bodyEven = builder.returnStmt(builder.callExpr(varOdd, param));
	core::StatementPtr bodyOdd = builder.returnStmt(builder.callExpr(varOdd, param));

	// create the lambda
	vector<core::LambdaBindingPtr> definitions;
	definitions.push_back(builder.lambdaBinding(varEven, builder.lambda(funType, params, bodyEven)));
	definitions.push_back(builder.lambdaBinding(varOdd, builder.lambda(funType, params, bodyOdd)));
	core::LambdaDefinitionPtr lambdaDef = builder.lambdaDefinition(definitions);

	core::LambdaExprPtr lambda = builder.lambdaExpr(varEven, lambdaDef);

	// obtain information
	LambdaInfo info = funManager.getInfo(lambda);

	// to be tested:
	//  - function
	//  - prototype = definition
	//  - lambda wrapper

	EXPECT_EQ("name", toC(info.function->name));
	EXPECT_TRUE((bool)info.prototype);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.lambdaWrapper);

	EXPECT_PRED2(containsSubString, toC(info.prototype), "bool name(int32_t p1);");
	EXPECT_PRED2(containsSubString, toC(info.definition), "bool name(int32_t name) {\n");
	EXPECT_PRED2(containsSubString, toC(info.lambdaWrapper), "bool name_wrap(name* closure, int32_t p1) {\n    return name(p1);\n}");

	// TODO: check create call and get value
}


TEST(FunctionManager, Bind) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	Converter converter;
	converter.setNodeManager(&nodeManager);

	TestNameManager nameManager;
	converter.setNameManager(&nameManager);

	c_ast::SharedCodeFragmentManager fragmentManager = c_ast::CodeFragmentManager::createShared();
	converter.setFragmentManager(fragmentManager);

	TypeManager typeManager(converter);
	converter.setTypeManager(&typeManager);

	StmtConverter stmtConverter(converter);
	converter.setStmtConverter(&stmtConverter);

	FunctionManager funManager(converter);
	converter.setFunctionManager(&funManager);

	// ----------------- Create a bind -----------

	core::TypePtr int4 = basic.getInt4();
	core::TypePtr real4 = basic.getFloat();
	core::TypePtr refInt4 = builder.refType(int4);
	core::TypePtr boolean = basic.getBool();

	core::VariablePtr p1 = builder.variable(int4, 1);
	core::VariablePtr p2 = builder.variable(real4, 2);
	core::LiteralPtr  p3 = builder.literal(refInt4, "v3");

	core::LiteralPtr fun = builder.literal(builder.functionType(toVector(real4, refInt4, int4), boolean), "fun");
	core::CallExprPtr call = builder.callExpr(boolean, fun, p2, p3, p1);
	core::BindExprPtr bind = builder.bindExpr(toVector(p2,p1), call);

	EXPECT_EQ("bind(v2,v1){fun(v2, v3, v1)}", toString(*bind));

	// ----------------- Convert using function manager -----------

	BindInfo info = funManager.getInfo(bind);

	// check presence of all members
	EXPECT_TRUE((bool)info.closureName);
	EXPECT_EQ("name_closure", toC(info.closureName));

	EXPECT_TRUE((bool)info.mapperName);
	EXPECT_EQ("name_mapper", toC(info.mapperName));

	EXPECT_TRUE((bool)info.constructorName);
	EXPECT_EQ("name_ctr", toC(info.constructorName));

	EXPECT_TRUE((bool)info.closureType);
	EXPECT_TRUE((bool)info.definitions);

	EXPECT_EQ("name_closure", toC(info.closureType));

	string def = toC(info.definitions);
	EXPECT_PRED2(containsSubString, def, "bool(* call)(struct _name_closure*,float,int32_t);");
	EXPECT_PRED2(containsSubString, def, "bool(* nested)(float,int32_t*,int32_t);");
	EXPECT_PRED2(containsSubString, def, "int32_t* c2;");
	EXPECT_PRED2(containsSubString, def, "} name_closure;");

	EXPECT_PRED2(containsSubString, def,
		"bool name_mapper(name_closure* closure, float c1, int32_t c3) {\n"
		"    return closure->nested(c1, closure->c2, c3);\n"
		"}"
	);

	EXPECT_PRED2(containsSubString, def,
		"static inline name_closure* name_ctr(name_closure* closure, bool(* nested)(float,int32_t*,int32_t), int32_t* c2) {\n"
		"    *closure = (name_closure){&name_mapper, nested, c2};\n"
		"    return closure;\n"
		"}"
	);

	// full code example - just make sure that no dependency cycle has been produced
	toString(c_ast::CCode(fragmentManager, bind, info.definitions));

	// check get value (value to be used when passing function as an argument)
	ConversionContext context(converter);

	EXPECT_EQ(
			"name_ctr((name_closure*)alloca(sizeof(name_closure)), &fun, &v3)",
			toC(funManager.getValue(bind, context))
	);
	EXPECT_TRUE(context.getDependencies().find(info.definitions) != context.getDependencies().end());

}


TEST(FunctionManager, NestedBind) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	Converter converter;
	converter.setNodeManager(&nodeManager);

	TestNameManager nameManager;
	converter.setNameManager(&nameManager);

	c_ast::SharedCodeFragmentManager fragmentManager = c_ast::CodeFragmentManager::createShared();
	converter.setFragmentManager(fragmentManager);

	TypeManager typeManager(converter);
	converter.setTypeManager(&typeManager);

	StmtConverter stmtConverter(converter);
	converter.setStmtConverter(&stmtConverter);

	FunctionManager funManager(converter);
	converter.setFunctionManager(&funManager);

	// ----------------- Create a bind -----------

	core::TypePtr int4 = basic.getInt4();
	core::TypePtr real4 = basic.getFloat();
	core::TypePtr refInt4 = builder.refType(int4);
	core::TypePtr boolean = basic.getBool();

	core::VariablePtr p1 = builder.variable(int4, 1);
	core::VariablePtr p2 = builder.variable(real4, 2);
	core::LiteralPtr  p3 = builder.literal(refInt4, "v3");

	core::LiteralPtr fun = builder.literal(builder.functionType(toVector(real4, refInt4, int4), boolean), "fun");
	core::CallExprPtr call = builder.callExpr(boolean, fun, p2, p3, p1);
	core::BindExprPtr innerBind = builder.bindExpr(toVector(p2,p1), call);

	EXPECT_EQ("bind(v2,v1){fun(v2, v3, v1)}", toString(*innerBind));

	// ----------------- Create another bind -----------

	core::VariablePtr p4 = builder.variable(int4, 4);
	core::LiteralPtr  p5 = builder.literal(real4, "v5");
	core::CallExprPtr call2 = builder.callExpr(boolean, innerBind, p5, p4);
	core::BindExprPtr bind = builder.bindExpr(toVector(p4), call2);

	EXPECT_EQ("bind(v4){bind(v2,v1){fun(v2, v3, v1)}(v5, v4)}", toString(*bind));

	// ----------------- Convert using function manager -----------

	BindInfo info = funManager.getInfo(bind);

	// check presence of all members
	EXPECT_TRUE((bool)info.closureName);
	EXPECT_EQ("name_closure", toC(info.closureName));

	EXPECT_TRUE((bool)info.mapperName);
	EXPECT_EQ("name_mapper", toC(info.mapperName));

	EXPECT_TRUE((bool)info.constructorName);
	EXPECT_EQ("name_ctr", toC(info.constructorName));

	EXPECT_TRUE((bool)info.closureType);
	EXPECT_TRUE((bool)info.definitions);

	EXPECT_EQ("name_closure", toC(info.closureType));

	string def = toC(info.definitions);
	EXPECT_PRED2(containsSubString, def, "bool(* call)(struct _name_closure*,int32_t);");
	EXPECT_PRED2(containsSubString, def, "name* nested;");
	EXPECT_PRED2(containsSubString, def, "float c1;");
	EXPECT_PRED2(containsSubString, def, "} name_closure;");

	EXPECT_PRED2(containsSubString, def,
		"bool name_mapper(name_closure* closure, int32_t c2) {\n"
		"    return closure->nested->call(closure->nested, closure->c1, c2);\n"
		"}"
	);

	EXPECT_PRED2(containsSubString, def,
		"static inline name_closure* name_ctr(name_closure* closure, name* nested, float c1) {\n"
		"    *closure = (name_closure){&name_mapper, nested, c1};\n"
		"    return closure;\n"
		"}"
	);

	// full code example - just make sure that no dependency cycle has been produced
	toString(c_ast::CCode(fragmentManager, bind, info.definitions));

	// check get value (value to be used when passing function as an argument)
	ConversionContext context(converter);

	EXPECT_EQ(
			"name_ctr((name_closure*)alloca(sizeof(name_closure)), name_ctr((name_closure*)alloca(sizeof(name_closure)), &fun, &v3), v5)",
			toC(funManager.getValue(bind, context))
	);
	EXPECT_TRUE(context.getDependencies().find(info.definitions) != context.getDependencies().end());

}

} // end namespace backend
} // end namespace insieme

