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

//#include "program.h"

#include "clang_compiler.h"
#include "pragma_handler.h"
//#include "utils/source_locations.h"
#include "clang_config.h"
#include "clang/AST/ASTContext.h"

//#include <iostream>
#include <clang/AST/Decl.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Attr.h>
#include <clang/Sema/AttributeList.h>
#include <clang/AST/Expr.h>

using namespace insieme;
using namespace insieme::frontend;
using namespace insieme::core;

#define CHECK_LOCATION(loc, srcMgr, line, col) \
    EXPECT_EQ(util::Line(loc, srcMgr), (size_t)line); \
    EXPECT_EQ(util::Column(loc, srcMgr), (size_t)col);

/*old version for __attribute((address_space(x)))
const char* addressSpaceStr[] = {
        "private",
        "local",
        "global",
        "constant"
};

enum AddressSpace {
        PRIVATE,
        LOCAL,
        GLOBAL,
        CONSTANT
};

const llvm::StringRef addrStr [] = {
    llvm::StringRef("__private"),
    llvm::StringRef("__local"),
    llvm::StringRef("__global"),
    llvm::StringRef("__constant")
};
*/

// new version for __attribute((annotate("x")))

class AddrSpace {
public:
    const llvm::StringRef Private;
    const llvm::StringRef Local;
    const llvm::StringRef Global;
    const llvm::StringRef Constant;

    AddrSpace() : Private("__private"), Local("__local"), Global("__global"), Constant("__constant") {};
};

//counter for address matching function
unsigned int addressSpaceMatches = 0;

const char* kindStr[] = {
        "Alias",
        "AlignMac68k",
        "Aligned",
        "AlwaysInline",
        "AnalyzerNoReturn",
        "Annotate",
        "AsmLabel",
        "BaseCheck",
        "Blocks",
        "CDecl",
        "CFReturnsNotRetained",
        "CFReturnsRetained",
        "CarriesDependency",
        "Cleanup",
        "Const",
        "Constructor",
        "DLLExport",
        "DLLImport",
        "Deprecated",
        "Destructor",
        "FastCall",
        "Final",
        "Format",
        "FormatArg",
        "GNUInline",
        "Hiding",
        "IBAction",
        "IBOutlet",
        "IBOutletCollection",
        "InitPriority",
        "MSP430Interrupt",
        "Malloc",
        "MaxFieldAlignment",
        "NSReturnsNotRetained",
        "NSReturnsRetained",
        "NoDebug",
        "NoInline",
        "NoInstrumentFunction",
        "NoReturn",
        "NoThrow",
        "NonNull",
        "ObjCException",
        "ObjCNSObject",
        "Overloadable",
        "Override",
        "Ownership",
        "Packed",
        "Pascal",
        "Pure",
        "Regparm",
        "ReqdWorkGroupSize",
        "Section",
        "Sentinel",
        "StdCall",
        "ThisCall",
        "TransparentUnion",
        "Unavailable",
        "Unused",
        "Used",
        "VecReturn",
        "Visibility",
        "WarnUnusedResult",
        "Weak",
        "WeakImport",
        "WeakRef",
        "X86ForceAlignArgPointer"
};

//attribute specific handlers to get attribute parameters
using namespace clang;
const Attr* parseAttribute(const Attr* attr)
{
    switch(attr->getKind()){
    case attr::Kind::Annotate:
    {
        const clang::AnnotateAttr* aa = (const AnnotateAttr*)attr;
        llvm::StringRef sr = aa->getAnnotation();
        std::cout << "annotation: " << sr.str() << std::endl;
        return aa;
    }
    case attr::Kind::ReqdWorkGroupSize:
    {
        const ReqdWorkGroupSizeAttr* rwgsa = (const ReqdWorkGroupSizeAttr*)attr;
        std::cout << "ReqdWorkGroupSize: "  << rwgsa->getXDim() << rwgsa->getYDim() << rwgsa->getZDim() << std::endl;
        return rwgsa;
    }
    case attr::Kind::Packed:
    {
        std::cout << "Packed" << std::endl;
        return (const PackedAttr*)attr;
    }
    case attr::Kind::Aligned:
    {
        const AlignedAttr* aa = (const AlignedAttr*)attr;
        // std::cout << "Aligned: "  << aa->getAlignment() << std::endl;
        return aa;
    }
    default:
    {
        std::cout << "Not implemented: " << kindStr[attr->getKind()] << std::endl;
        return attr;
    }
    }
}

void checkForAttrs(clang::Decl* decl)
{
    if(decl->hasAttrs())
    {
        decl->dump();
        fprintf(stderr, " has an attribute\n");
//        parseAttribute(decl->getAttrs());
    }
    else {
        decl->dump();
        fprintf(stderr, " has no attribute\n");
    }
}

bool scanStruct(RecordDecl* decl){
    if(decl->getTagKind() != TagTypeKind::TTK_Struct)
        return false;

    attr::Kind expectedKinds[] = {attr::Kind::Aligned, attr::Kind::Packed};
    unsigned int expectedParameters[] = {64};
    unsigned int matches = 0, args = 0;
    ASTContext& ctx = decl->getASTContext();

    decl->field_begin();

    for(RecordDecl::field_iterator I = decl->field_begin(), E = decl->field_end(); I != E; ++I) {

        if(!I->hasAttrs())
            continue;

        AttrVec attrvec = I->getAttrs();
        EXPECT_EQ(static_cast<unsigned>(1), attrvec.size());
        const Attr* attr = attrvec[0];

        EXPECT_EQ(expectedKinds[matches++], attr->getKind());
        if(attr->getKind() == attr::Kind::Aligned) {
            EXPECT_EQ(expectedParameters[args++], ((AlignedAttr*)attr)->getAlignment(ctx));
        }
    }

    return true;
}

void checkAddressSpace(const Attr* attr, unsigned int& matches){
    AddrSpace addrspace;
    //expected address spaces: GLOBAL, CONSTANT, LOCAL, PRIVATE
    llvm::StringRef expected[6] = {addrspace.Global, addrspace.Constant, addrspace.Local, addrspace.Private,
            addrspace.Local, addrspace.Private};

    const clang::AnnotateAttr* aa = (const AnnotateAttr*)attr;
    llvm::StringRef sr = aa->getAnnotation();

//    std::cout << expected[matches].str() << " - " << sr.str() << std::endl;

    EXPECT_EQ(expected[matches++].str(), sr.str());
}

void checkAttributes(const Attr* attr, ASTContext& ctx, unsigned int& matches, unsigned int& args){
    attr::Kind expectedKinds[] = {attr::Kind::Packed, attr::Kind::Aligned, attr::Kind::Aligned};
    unsigned int expectedParameters[] = {16, 64};

    EXPECT_EQ(expectedKinds[matches++], attr->getKind());

    if(attr->getKind() == attr::Kind::Aligned) {
        EXPECT_EQ(expectedParameters[args++], ((AlignedAttr*)attr)->getAlignment(ctx));
    }
}

void scanStmt(clang::Stmt* stmt, clang::ASTContext& ctx) {
    unsigned int matches = 0, args = 0;

    for(clang::StmtIterator si = stmt->child_begin(), se = stmt->child_end(); si != se; ++si) {
    //                clang::Stmt* body_stmt = dyn_cast<clang::Stmt> (*si);
        //check attributes of declarations
/*        clang::Stmt* retain = si->Retain();
        printf("parsing stmt %s\n", retain->getStmtClassName());
*/
/*        fprintf(stderr, "----------new dump-----------%d\n", isa<clang::DeclStmt>(*si));
        retain->dump();
*/
        if(isa<clang::DeclStmt> (*si)){

            clang::DeclStmt* declstmt = dyn_cast<clang::DeclStmt>(*si);


            if(declstmt->isSingleDecl()){
                clang::Decl* decl = declstmt->getSingleDecl();
                clang::Stmt* body = decl->getBody();
//                clang::CompoundStmt* cs = decl->getCompoundBody();

                if(isa<clang::RecordDecl> (*decl)){
                    scanStruct((clang::RecordDecl*)decl);
                }

                if(body) {
                    scanStmt(body, ctx);
                }
//                else
//                    fprintf(stderr, "bodyless ");
//                checkForAttrs(decl);

                if(!decl->hasAttrs())
                    continue;

                AttrVec attrvec = decl->getAttrs();

                for(Attr *const*ai = attrvec.begin(), *const*ae = attrvec.end(); ai != ae; ++ai)
                {
                    if((*ai)->getKind() == attr::Kind::Annotate)
                        checkAddressSpace(*ai, addressSpaceMatches);
                    else
                        checkAttributes(*ai, ctx, matches, args);
                }

            }
            else {
                clang::DeclGroupRef dgr = declstmt->getDeclGroup();

                clang::DeclGroup& declgroup = dgr.getDeclGroup();
                for(size_t i = 0; i < declgroup.size(); ++i){
//                    checkForAttrs(declgroup[i]);
                    if(!declgroup[i]->hasAttrs())
                        continue;

                    const AttrVec attrvec = declgroup[i]->getAttrs();

                    EXPECT_EQ(static_cast<unsigned>(1), attrvec.size());
                    for(Attr *const*ai = attrvec.begin(), *const*ae = attrvec.end(); ai != ae; ++ai)
                    {
                        if((*ai)->getKind() == attr::Kind::Annotate)
                            checkAddressSpace(*ai, addressSpaceMatches);
                        else
                            checkAttributes(*ai, ctx, matches, args);
                    }

                    clang::Stmt* body = declgroup[i]->getBody();
                    if(body) {
                        printf("parsing stmt %s\n", body->getStmtClassName());
                        scanStmt(body, ctx);
                    }
                }
            }
        }
        else {
            //nothing to do for now
        }
    }
}

//Check the size of the variable
TEST(KernelMatcherTest, CheckBuildinVector) {
	SharedNodeManager manager = std::make_shared<NodeManager>();
	frontend::Program p(manager);

	p.addTranslationUnit( std::string(SRC_DIR) + "/kernel_matcher.cl" );
    clang::ASTContext& ctx = (*p.getTranslationUnits().begin())->getCompiler().getASTContext();
    std::vector<clang::Type*> types = ctx.getTypes();

    for(size_t i = 0; i < types.size(); ++i)
    {
        clang::Type* t = types.at(i);

        if(t->isVectorType() && t->hasIntegerRepresentation())
        {

            clang::ExtVectorType* evt = (clang::ExtVectorType*)t;
            clang::QualType qt = evt->getCanonicalTypeInternal();
            qt.dump();
            clang::ExtVectorType* evt2 = (clang::ExtVectorType*)(qt.getTypePtr());
//            printf("------------>found a vector %s: %d\n",t->getTypeClassName(), evt2->getNumElements());
            //check the size of the vector
            if(evt->getTypeClass() == clang::Type::TypeClass::Typedef)
                EXPECT_EQ(static_cast<unsigned int>(4), evt2->getNumElements());

//            EXPECT_TRUE(evt->isAccessorWithinNumElements('w'));
        }
    }
}

// Check if memory spaces are as expected default, __global, __constant, __local, __private
/* moved to new version using __attribute((annotate("x")))
TEST(KernelMatcherTest, ReadMemorySpaces) {
    unsigned int expected_asp[] = {GLOBAL, CONSTANT, LOCAL, PRIVATE, 0, 0, 0};
    unsigned int asp = 0;
    for(size_t i = 0; i < types.size(); ++i)
    {
        clang::Type* t = types.at(i);

*//* does not work as expected - now done by ScanStruct
        if(t->isStructureType())
        {

            const clang::RecordType* rt = t->getAsStructureType();
            const clang::RecordDecl* rd = rt->getDecl();

                printf("------------------> %d - %d\n", rt->hasUnsignedIntegerRepresentation(), rt->isAnyPointerType());
                if(!rd->isEmbeddedInDeclarator()){
                clang::Stmt* stmt = rd->getBody();
                if(stmt)
                scanStmt(stmt, ctx);
            }
        }
*//*
clang::QualType qt = t->getCanonicalTypeInternal();
        //read the address space attribute of all pointers
        if(t->isPointerType()) {
            clang::QualType qt = t->getCanonicalTypeInternal();
            //skip return Type of kernel function which has to be void
            if(qt->isVoidPointerType())
                continue;
            printf("pointee: %d!!!", qt.getAddressSpace());
*//*
            const char* expected = addressSpaceStr[expected_asp[asp]];
            const char* actual = addressSpaceStr[t->getPointeeType().getAddressSpace()];
            fprintf(stderr, "%d address Spacse %s - %s\n", asp,  expected, actual);
*//*
            EXPECT_EQ(expected_asp[asp++], t->getPointeeType().getAddressSpace());
//            std::cout << qt.getAsString() << " ADDRESS_SPACE: " << t->getPointeeType().getAddressSpace() << std::endl;
        }
    }
}*/

// Check the given attributes
TEST(KernelMatcherTest, ReadAttributes) {
	SharedNodeManager manager = std::make_shared<NodeManager>();
	frontend::Program p(manager);

	p.addTranslationUnit( std::string(SRC_DIR) + "/kernel_matcher.cl" );
    clang::ASTContext& ctx = (*p.getTranslationUnits().begin())->getCompiler().getASTContext();
    std::vector<clang::Type*> types = ctx.getTypes();

    clang::DeclContext* declRef = clang::TranslationUnitDecl::castToDeclContext(ctx.getTranslationUnitDecl());

    for(clang::DeclContext::decl_iterator I = declRef->decls_begin(), E = declRef->decls_end(); I != E; ++I) {
        if(isa<clang::FunctionDecl> (*I)) {
            //continue; // skip non function declarations

            // Top-level Function declaration
            clang::FunctionDecl* func_decl = dyn_cast<clang::FunctionDecl>(*I);

            //parse only the kernlel function kfct
            if(!func_decl->getName().equals(llvm::StringRef("kfct")))
                continue;

            //Function declaration should have RequiredWorkGroupSize(1,2,3) and annotate("__kernel") attribute
            EXPECT_TRUE(func_decl->hasAttrs());
            if(func_decl->hasAttrs()) {
                const clang::AttrVec attrVec = func_decl->getAttrs();
                EXPECT_EQ(static_cast<unsigned>(2), attrVec.size());
                Attr* attr = attrVec[0];
//                std::cout << kindStr[attr->getKind()] << std::endl;
//                parseAttribute(attr);
                EXPECT_EQ(attr::Kind::ReqdWorkGroupSize, attr->getKind());
                if(attr->getKind() == attr::Kind::ReqdWorkGroupSize)
                {
                    EXPECT_EQ(static_cast<unsigned>(1), ((const ReqdWorkGroupSizeAttr*)attr)->getXDim());
                    EXPECT_EQ(static_cast<unsigned>(2), ((const ReqdWorkGroupSizeAttr*)attr)->getYDim());
                    EXPECT_EQ(static_cast<unsigned>(3), ((const ReqdWorkGroupSizeAttr*)attr)->getZDim());
                }

                //get annotate string
                attr = attrVec[1];
                AnnotateAttr* aa = (AnnotateAttr*)attr;
                llvm::StringRef sr = aa->getAnnotation();

                //convert string to char[] and make sure that "kernel" is converted to "__kernel"
                char fctModifier[16];
                fctModifier[0] = '_';
                fctModifier[1] = '_';

                int offset = 0;

                if(sr[0] == '_' && sr[1] == '_')
                {
                    offset = 2;
                }
                for(int i = 0; i < 6; ++i)
                {
                    fctModifier[i+2] = sr[i+offset];
                }
                fctModifier[8] = '\0';

                EXPECT_STREQ("__kernel", fctModifier);
//                printf("second attr kind: %s(%s)\n", kindStr[attr->getKind()], fctModifier);
            }

/*
            std::cout << "is an aliasAttr: " << clang::AliasAttr::classof(attr) << std::endl;
            std::cout << "is an annotateAttr: " << clang::AnnotateAttr::classof(attr) << std::endl;
            std::cout << "is an asmLabelAttr: " << clang::AsmLabelAttr::classof(attr) << std::endl;
            std::cout << "is a formatAttr: " << clang::FormatAttr::classof(attr) << std::endl;
            std::cout << "is a sectionAttr: " << clang::SectionAttr::classof(attr) << std::endl;
*/
/*           const clang::Attr* attr2 = attr->getNext();
            std::cout << "hi" << std::endl;
            std::cout << kindStr[attr2->getKind()] << std::endl;
*/

            //check address space of arguments
            for(unsigned int i = 0; i < func_decl->getNumParams(); ++i)
            {
                clang::ParmVarDecl* pd = func_decl->getParamDecl(i);
                AttrVec attrvec = pd->getAttrs();
                for(Attr *const*ai = attrvec.begin(), *const*ae = attrvec.end(); ai != ae; ++ai)
                {
                    EXPECT_EQ(attr::Kind::Annotate, (*ai)->getKind());
                    if((*ai)->getKind() == attr::Kind::Annotate)
                        checkAddressSpace(*ai, addressSpaceMatches);
                }
           }

            clang::Stmt* func_body = func_decl->getBody();
            EXPECT_TRUE(func_body);
            if(!func_body)
            {
                printf("functions declarations with no definition\n");
                continue; // skip functions declarations with no definition
            }

            scanStmt(func_body, ctx);
            if(isa<Expr> (func_body)) {
                printf("I'm an ExtVectorElementExpr\n");
            }
        }
    }
//    fprintf(stderr, "done\n");
}
