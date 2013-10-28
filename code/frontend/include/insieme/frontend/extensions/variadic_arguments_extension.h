#include "insieme/frontend/extensions/clang_stage_plugin.h"


class VariadicArgumentsPlugin : public insieme::frontend::extensions::ClangStagePlugin {
    	virtual core::ExpressionPtr Visit(const clang::Expr* expr, frontend::conversion::Converter& convFact) {
            if(llvm::isa<clang::CallExpr>(expr)) {
                core::IRBuilder builder = convFact.getIRBuilder();
                const clang::CallExpr* callexpr = llvm::cast<clang::CallExpr>(expr);
                if(!callexpr->getDirectCallee())
                    return nullptr;
                //find the correct name for the builtin
                std::string name = callexpr->getDirectCallee()->getNameAsString();
                bool found=false;
                if(name.find("va_start") != std::string::npos) {
                    name = "va_start";
                    found=true;
                }
                if(name.find("va_end") != std::string::npos) {
                    name = "va_end";
                    found=true;
                }
                if(name.find("va_arg") != std::string::npos) {
                    name = "va_arg";
                    found=true;
                }
                if(name.find("va_copy") != std::string::npos) {
                    name = "va_copy";
                    found=true;
                }
                if(!found)
                    return nullptr;

                //convert it with the standard expression visitor
                core::ExpressionPtr ex = convFact.getExprConverter()->VisitCallExpr(callexpr);
                assert(ex.isa<core::CallExprPtr>() && "this is no call expression.");

                auto funExpr = ex.as<core::CallExprPtr>()->getFunctionExpr();
                auto args = ex.as<core::CallExprPtr>()->getArguments();
  				vector<core::ExpressionPtr> newArgs(args);

                //if(core::analysis::isCallOf(args[0], gen.getRefVectorToRefArray()))
                    //newArgs[0] = args[0].as<core::CallExprPtr>()->getArguments();

                auto type = funExpr->getType().as<core::FunctionTypePtr>();
                auto typeAddr = core::FunctionTypeAddress(type);
                auto paramTypeAddr = typeAddr->getParameterType(0);
                auto newFunType = insieme::core::transform::replaceNode(builder.getNodeManager(), paramTypeAddr, builder.refType(builder.genericType("va_list")));
                auto lit = builder.literal(name,newFunType.as<core::FunctionTypePtr>());

                auto argument = args[0];
                auto argumentAddress = core::ExpressionAddress(argument);
                auto argumentTypeAddress = argumentAddress->getType();
                auto newArg = insieme::core::transform::replaceNode(builder.getNodeManager(), argumentTypeAddress, builder.refType(builder.genericType("va_list")));

                newArgs[0] = newArg.as<core::ExpressionPtr>();

                ex = builder.callExpr(type->getReturnType(), lit, newArgs);
                return ex;
            }
            return nullptr;
}

};
