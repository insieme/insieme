ArrayTypePtr ASTBuilder::arrayType(const TypePtr& elementType,  const unsigned short dim ) { return ArrayType::get(manager, elementType, dim ); }
BoolLiteralPtr ASTBuilder::boolLiteral(bool value) { return BoolLiteral::get(manager, value); }
BoolTypePtr ASTBuilder::boolType() { return BoolType::get(manager); }
BreakStmtPtr ASTBuilder::breakStmt() { return BreakStmt::get(manager); }
CallExprPtr ASTBuilder::callExpr(const TypePtr& type,  const ExpressionPtr& functionExpr,  const vector<ExpressionPtr>& arguments) { return CallExpr::get(manager, type, functionExpr, arguments); }
CastExprPtr ASTBuilder::castExpr(const TypePtr& type,  const ExpressionPtr& subExpression) { return CastExpr::get(manager, type, subExpression); }
ChannelTypePtr ASTBuilder::channelType(const TypePtr& elementType,  const unsigned short size) { return ChannelType::get(manager, elementType, size); }
CompoundStmtPtr ASTBuilder::compoundStmt() { return CompoundStmt::get(manager); }
CompoundStmtPtr ASTBuilder::compoundStmt(const vector<StatementPtr>& stmts) { return CompoundStmt::get(manager, stmts); }
CompoundStmtPtr ASTBuilder::compoundStmt(const StatementPtr& stmt) { return CompoundStmt::get(manager, stmt); }
ContinueStmtPtr ASTBuilder::continueStmt() { return ContinueStmt::get(manager); }
DeclarationStmtPtr ASTBuilder::declarationStmt(const TypePtr& type,  const Identifier& id,  const ExpressionPtr& initExpression) { return DeclarationStmt::get(manager, type, id, initExpression); }
DefinitionPtr ASTBuilder::definition(const Identifier& name,  const TypePtr& type,  const ExpressionPtr& definition ,  bool external ) { return Definition::get(manager, name, type, definition , external ); }
FloatLiteralPtr ASTBuilder::floatLiteral(const string& from,  unsigned short bytes ) { return FloatLiteral::get(manager, from, bytes ); }
FloatLiteralPtr ASTBuilder::floatLiteral(double value,  unsigned short bytes ) { return FloatLiteral::get(manager, value, bytes ); }
FloatTypePtr ASTBuilder::floatType(const unsigned short numBytes ) { return FloatType::get(manager, numBytes ); }
ForStmtPtr ASTBuilder::forStmt(const DeclarationStmtPtr& declaration,  const StatementPtr& body,  const ExpressionPtr& end,  const ExpressionPtr& step ) { return ForStmt::get(manager, declaration, body, end, step ); }
FunctionTypePtr ASTBuilder::functionType(const TypePtr& argumentType,  const TypePtr& returnType) { return FunctionType::get(manager, argumentType, returnType); }
IfStmtPtr ASTBuilder::ifStmt(const ExpressionPtr& condition,  const StatementPtr& body,  const StatementPtr& elseBody ) { return IfStmt::get(manager, condition, body, elseBody ); }
IntLiteralPtr ASTBuilder::intLiteral(int value,  unsigned short bytes ) { return IntLiteral::get(manager, value, bytes ); }
IntTypePtr ASTBuilder::intType(const unsigned short numBytes ) { return IntType::get(manager, numBytes ); }
LambdaExprPtr ASTBuilder::lambdaExpr(const TypePtr& type,  const ParamList& params,  const StatementPtr& body) { return LambdaExpr::get(manager, type, params, body); }
ParamExprPtr ASTBuilder::paramExpr(const TypePtr& type,  const Identifier &id) { return ParamExpr::get(manager, type, id); }
RefTypePtr ASTBuilder::refType(const TypePtr& elementType) { return RefType::get(manager, elementType); }
ReturnStmtPtr ASTBuilder::returnStmt(const ExpressionPtr& returnExpression) { return ReturnStmt::get(manager, returnExpression); }
StructTypePtr ASTBuilder::structType(const Entries& entries) { return StructType::get(manager, entries); }
TupleExprPtr ASTBuilder::tupleExpr(const vector<ExpressionPtr>& expressions) { return TupleExpr::get(manager, expressions); }
TupleTypePtr ASTBuilder::tupleType(const ElementTypeList& elementTypes) { return TupleType::get(manager, elementTypes); }
TypeVariablePtr ASTBuilder::typeVariable(const string& name) { return TypeVariable::get(manager, name); }
UnionTypePtr ASTBuilder::unionType(const Entries& entries) { return UnionType::get(manager, entries); }
UnitTypePtr ASTBuilder::unitType() { return UnitType::get(manager); }
VarExprPtr ASTBuilder::varExpr(const TypePtr& type,  const Identifier &id) { return VarExpr::get(manager, type, id); }
VectorTypePtr ASTBuilder::vectorType(const TypePtr& elementType,  const unsigned short size) { return VectorType::get(manager, elementType, size); }
WhileStmtPtr ASTBuilder::whileStmt(const ExpressionPtr& condition,  const StatementPtr& body) { return WhileStmt::get(manager, condition, body); }
