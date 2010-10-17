ArrayTypePtr       ASTBuilder::arrayType(const TypePtr& elementType, const IntTypeParam& dim) const { return ArrayType::get(manager, elementType, dim); }
BreakStmtPtr       ASTBuilder::breakStmt() const { return BreakStmt::get(manager); }
CallExprPtr        ASTBuilder::callExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) const { return CallExpr::get(manager, type, functionExpr, arguments); }
CallExprPtr        ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) const { return CallExpr::get(manager, functionExpr, arguments); }
CastExprPtr        ASTBuilder::castExpr(const TypePtr& type, const ExpressionPtr& subExpression) const { return CastExpr::get(manager, type, subExpression); }
ChannelTypePtr     ASTBuilder::channelType(const TypePtr& elementType, const IntTypeParam& size) const { return ChannelType::get(manager, elementType, size); }
CompoundStmtPtr    ASTBuilder::compoundStmt() const { return CompoundStmt::get(manager); }
CompoundStmtPtr    ASTBuilder::compoundStmt(const vector<StatementPtr>& stmts) const { return CompoundStmt::get(manager, stmts); }
CompoundStmtPtr    ASTBuilder::compoundStmt(const StatementPtr& stmt) const { return CompoundStmt::get(manager, stmt); }
ContinueStmtPtr    ASTBuilder::continueStmt() const { return ContinueStmt::get(manager); }
DeclarationStmtPtr ASTBuilder::declarationStmt(const TypePtr& type, const Identifier& id, const ExpressionPtr& initExpression) const { return DeclarationStmt::get(manager, type, id, initExpression); }
ForStmtPtr         ASTBuilder::forStmt(const DeclarationStmtPtr& declaration, const StatementPtr& body, const ExpressionPtr& end, const ExpressionPtr& step) const { return ForStmt::get(manager, declaration, body, end, step); }
FunctionTypePtr    ASTBuilder::functionType(const TypePtr& argumentType, const TypePtr& returnType) const { return FunctionType::get(manager, argumentType, returnType); }
GenericTypePtr     ASTBuilder::genericType(const Identifier& name, const vector<TypePtr>& typeParams, const vector<IntTypeParam>& intTypeParams, const TypePtr& baseType) const { return GenericType::get(manager, name, typeParams, intTypeParams, baseType); }
IfStmtPtr          ASTBuilder::ifStmt(const ExpressionPtr& condition, const StatementPtr& thenBody) const { return IfStmt::get(manager, condition, thenBody); }
IfStmtPtr          ASTBuilder::ifStmt(const ExpressionPtr& condition, const StatementPtr& thenBody, const StatementPtr& elseBody) const { return IfStmt::get(manager, condition, thenBody, elseBody); }
JobExprPtr         ASTBuilder::jobExpr(const StatementPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecs) const { return JobExpr::get(manager, defaultStmt, guardedStmts, localDecs); }
LambdaExprPtr      ASTBuilder::lambdaExpr(const TypePtr& type, const ParamList& params, const StatementPtr& body) const { return LambdaExpr::get(manager, type, params, body); }
LiteralPtr         ASTBuilder::literal(const string& value, const TypePtr& type) const { return Literal::get(manager, value, type); }
ParamExprPtr       ASTBuilder::paramExpr(const TypePtr& type, const Identifier &id) const { return ParamExpr::get(manager, type, id); }
RecLambdaDefinitionPtr ASTBuilder::recLambdaDefinition(const RecFunDefs& definitions) const { return RecLambdaDefinition::get(manager, definitions); }
RecLambdaExprPtr   ASTBuilder::recLambdaExpr(const VarExprPtr& variable, const RecLambdaDefinitionPtr& definition) const { return RecLambdaExpr::get(manager, variable, definition); }
RecTypeDefinitionPtr ASTBuilder::recTypeDefinition(const RecTypeDefs& definitions) const { return RecTypeDefinition::get(manager, definitions); }
RecTypePtr         ASTBuilder::recType(const TypeVariablePtr& typeVariable, const RecTypeDefinitionPtr& definition) const { return RecType::get(manager, typeVariable, definition); }
RefTypePtr         ASTBuilder::refType(const TypePtr& elementType) const { return RefType::get(manager, elementType); }
ReturnStmtPtr      ASTBuilder::returnStmt(const ExpressionPtr& returnExpression) const { return ReturnStmt::get(manager, returnExpression); }
StructExprPtr      ASTBuilder::structExpr(const Members& members) const { return StructExpr::get(manager, members); }
StructTypePtr      ASTBuilder::structType(const Entries& entries) const { return StructType::get(manager, entries); }
SwitchStmtPtr      ASTBuilder::switchStmt(const ExpressionPtr& switchExpr, const vector<SwitchStmt::Case>& cases, const StatementPtr& defaultCase) const { return SwitchStmt::get(manager, switchExpr, cases, defaultCase); }
SwitchStmtPtr      ASTBuilder::switchStmt(const ExpressionPtr& switchExpr, const vector<SwitchStmt::Case>& cases) const { return SwitchStmt::get(manager, switchExpr, cases); }
TupleExprPtr       ASTBuilder::tupleExpr(const vector<ExpressionPtr>& expressions) const { return TupleExpr::get(manager, expressions); }
TupleTypePtr       ASTBuilder::tupleType(const ElementTypeList& elementTypes) const { return TupleType::get(manager, elementTypes); }
TypeVariablePtr    ASTBuilder::typeVariable(const string& name) const { return TypeVariable::get(manager, name); }
UnionExprPtr       ASTBuilder::unionExpr(const Members& members) const { return UnionExpr::get(manager, members); }
UnionTypePtr       ASTBuilder::unionType(const Entries& entries) const { return UnionType::get(manager, entries); }
VarExprPtr         ASTBuilder::varExpr(const TypePtr& type, const Identifier &id) const { return VarExpr::get(manager, type, id); }
VectorExprPtr      ASTBuilder::vectorExpr(const vector<ExpressionPtr>& expressions) const { return VectorExpr::get(manager, expressions); }
VectorTypePtr      ASTBuilder::vectorType(const TypePtr& elementType, const IntTypeParam& size) const { return VectorType::get(manager, elementType, size); }
WhileStmtPtr       ASTBuilder::whileStmt(const ExpressionPtr& condition, const StatementPtr& body) const { return WhileStmt::get(manager, condition, body); }
