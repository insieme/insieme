ArrayTypePtr       ASTBuilder::arrayType(const TypePtr& elementType, const unsigned short dim) const { return ArrayType::get(manager, elementType, dim); }
BoolLiteralPtr     ASTBuilder::boolLiteral(bool value) const { return BoolLiteral::get(manager, value); }
BoolTypePtr        ASTBuilder::boolType() const { return BoolType::get(manager); }
BreakStmtPtr       ASTBuilder::breakStmt() const { return BreakStmt::get(manager); }
CallExprPtr        ASTBuilder::callExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) const { return CallExpr::get(manager, type, functionExpr, arguments); }
CastExprPtr        ASTBuilder::castExpr(const TypePtr& type, const ExpressionPtr& subExpression) const { return CastExpr::get(manager, type, subExpression); }
ChannelTypePtr     ASTBuilder::channelType(const TypePtr& elementType, const unsigned short size) const { return ChannelType::get(manager, elementType, size); }
CompoundStmtPtr    ASTBuilder::compoundStmt() const { return CompoundStmt::get(manager); }
CompoundStmtPtr    ASTBuilder::compoundStmt(const StatementPtr& stmt) const { return CompoundStmt::get(manager, stmt); }
CompoundStmtPtr    ASTBuilder::compoundStmt(const vector<StatementPtr>& stmts) const { return CompoundStmt::get(manager, stmts); }
ContinueStmtPtr    ASTBuilder::continueStmt() const { return ContinueStmt::get(manager); }
DeclarationStmtPtr ASTBuilder::declarationStmt(const TypePtr& type, const Identifier& id, const ExpressionPtr& initExpression) const { return DeclarationStmt::get(manager, type, id, initExpression); }
DefinitionPtr      ASTBuilder::definition(const Identifier& name, const TypePtr& type, const ExpressionPtr& definition, bool external) const { return Definition::get(manager, name, type, definition, external); }
FloatLiteralPtr    ASTBuilder::floatLiteral(const string& from, unsigned short bytes) const { return FloatLiteral::get(manager, from, bytes); }
FloatLiteralPtr    ASTBuilder::floatLiteral(double value, unsigned short bytes) const { return FloatLiteral::get(manager, value, bytes); }
FloatLiteralPtr    ASTBuilder::floatLiteral(double value, const TypePtr& type) const { return FloatLiteral::get(manager, value, type); }
FloatLiteralPtr    ASTBuilder::floatLiteral(const string& from, const TypePtr& type) const { return FloatLiteral::get(manager, from, type); }
FloatTypePtr       ASTBuilder::floatType(const unsigned short numBytes) const { return FloatType::get(manager, numBytes); }
ForStmtPtr         ASTBuilder::forStmt(const DeclarationStmtPtr& declaration, const StatementPtr& body, const ExpressionPtr& end, const ExpressionPtr& step) const { return ForStmt::get(manager, declaration, body, end, step); }
FunctionTypePtr    ASTBuilder::functionType(const TypePtr& argumentType, const TypePtr& returnType) const { return FunctionType::get(manager, argumentType, returnType); }
GenericTypePtr     ASTBuilder::genericType(const string& name, const vector<TypePtr>& typeParams, const vector<IntTypeParam>& intTypeParams, const TypePtr& baseType) const { return GenericType::get(manager, name, typeParams, intTypeParams, baseType); }
IfStmtPtr          ASTBuilder::ifStmt(const ExpressionPtr& condition, const StatementPtr& body, const StatementPtr& elseBody) const { return IfStmt::get(manager, condition, body, elseBody); }
IntLiteralPtr      ASTBuilder::intLiteral(int value, unsigned short bytes) const { return IntLiteral::get(manager, value, bytes); }
IntLiteralPtr      ASTBuilder::intLiteral(int value, const TypePtr& type) const { return IntLiteral::get(manager, value, type); }
IntTypePtr         ASTBuilder::intType(const unsigned short numBytes) const { return IntType::get(manager, numBytes); }
JobExprPtr         ASTBuilder::jobExpr(const StatementPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecs) const { return JobExpr::get(manager, defaultStmt, guardedStmts, localDecs); }
LambdaExprPtr      ASTBuilder::lambdaExpr(const TypePtr& type, const ParamList& params, const StatementPtr& body) const { return LambdaExpr::get(manager, type, params, body); }
ParamExprPtr       ASTBuilder::paramExpr(const TypePtr& type, const Identifier &id) const { return ParamExpr::get(manager, type, id); }
RefTypePtr         ASTBuilder::refType(const TypePtr& elementType) const { return RefType::get(manager, elementType); }
ReturnStmtPtr      ASTBuilder::returnStmt(const ExpressionPtr& returnExpression) const { return ReturnStmt::get(manager, returnExpression); }
StructExprPtr      ASTBuilder::structExpr(const Members& members) const { return StructExpr::get(manager, members); }
StructTypePtr      ASTBuilder::structType(const Entries& entries) const { return StructType::get(manager, entries); }
TupleExprPtr       ASTBuilder::tupleExpr(const vector<ExpressionPtr>& expressions) const { return TupleExpr::get(manager, expressions); }
TupleTypePtr       ASTBuilder::tupleType(const ElementTypeList& elementTypes) const { return TupleType::get(manager, elementTypes); }
TypeVariablePtr    ASTBuilder::typeVariable(const string& name) const { return TypeVariable::get(manager, name); }
UnionExprPtr       ASTBuilder::unionExpr(const Members& members) const { return UnionExpr::get(manager, members); }
UnionTypePtr       ASTBuilder::unionType(const Entries& entries) const { return UnionType::get(manager, entries); }
UnitTypePtr        ASTBuilder::unitType() const { return UnitType::get(manager); }
VarExprPtr         ASTBuilder::varExpr(const TypePtr& type, const Identifier &id) const { return VarExpr::get(manager, type, id); }
VectorTypePtr      ASTBuilder::vectorType(const TypePtr& elementType, const IntTypeParam& size) const { return VectorType::get(manager, elementType, size); }
VectorTypePtr      ASTBuilder::vectorType(const TypePtr& elementType, const unsigned short size) const { return VectorType::get(manager, elementType, size); }
WhileStmtPtr       ASTBuilder::whileStmt(const ExpressionPtr& condition, const StatementPtr& body) const { return WhileStmt::get(manager, condition, body); }
