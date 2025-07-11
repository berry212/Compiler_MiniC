///
/// @file Antlr4CSTVisitor.cpp
/// @brief Antlr4的具体语法树的遍历产生AST
/// @author zenglj (zenglj@live.com)
/// @version 1.1
/// @date 2024-11-23
///
/// @copyright Copyright (c) 2024
///
/// @par 修改日志:
/// <table>
/// <tr><th>Date       <th>Version <th>Author  <th>Description
/// <tr><td>2024-09-29 <td>1.0     <td>zenglj  <td>新建
/// <tr><td>2024-11-23 <td>1.1     <td>zenglj  <td>表达式版增强
/// </table>
///

#include <cstddef>
#include <string>
#include <vector>

#include "Antlr4CSTVisitor.h"
#include "AST.h"
#include "AttrType.h"
#include "Instruction.h"

#define Instanceof(res, type, var) auto res = dynamic_cast<type>(var)

/// @brief 构造函数
MiniCCSTVisitor::MiniCCSTVisitor()
{}

/// @brief 析构函数
MiniCCSTVisitor::~MiniCCSTVisitor()
{}

/// @brief 遍历CST产生AST
/// @param root CST语法树的根结点
/// @return AST的根节点
ast_node * MiniCCSTVisitor::run(MiniCParser::CompileUnitContext * root)
{
    return std::any_cast<ast_node *>(visitCompileUnit(root));
}

/// @brief 非终结运算符compileUnit的遍历
/// @param ctx CST上下文
std::any MiniCCSTVisitor::visitCompileUnit(MiniCParser::CompileUnitContext * ctx)
{
    // compileUnit: (funcDef | varDecl)* EOF

    // 请注意这里必须先遍历全局变量后遍历函数。肯定可以确保全局变量先声明后使用的规则，但有些情况却不能检查出。
    // 事实上可能函数A后全局变量B后函数C，这时在函数A中是不能使用变量B的，需要报语义错误，但目前的处理不会。
    // 因此在进行语义检查时，可能追加检查行号和列号，如果函数的行号/列号在全局变量的行号/列号的前面则需要报语义错误
    // TODO 请追加实现。

    ast_node * temp_node;
    ast_node * compileUnitNode = create_contain_node(ast_operator_type::AST_OP_COMPILE_UNIT);

    // 可能多个变量，因此必须循环遍历
    for (auto varCtx: ctx->varDecl()) {

        // 变量函数定义
        temp_node = std::any_cast<ast_node *>(visitVarDecl(varCtx));
        (void) compileUnitNode->insert_son_node(temp_node);
    }

    // 可能有多个函数，因此必须循环遍历
    for (auto funcCtx: ctx->funcDef()) {

        // 变量函数定义
        temp_node = std::any_cast<ast_node *>(visitFuncDef(funcCtx));
        (void) compileUnitNode->insert_son_node(temp_node);
    }

    return compileUnitNode;
}

/// @brief 非终结运算符funcDef的遍历
/// @param ctx CST上下文
std::any MiniCCSTVisitor::visitFuncDef(MiniCParser::FuncDefContext * ctx)
{
    // 识别的文法产生式：funcDef: (T_INT | T_VOID) T_ID T_L_PAREN formalParamList? T_R_PAREN block;

    // 函数返回类型，可以是INT或VOID
    type_attr funcReturnType{BasicType::TYPE_VOID, -1};

    if (ctx->T_INT()) {
        funcReturnType.type = BasicType::TYPE_INT;
        funcReturnType.lineno = ctx->T_INT()->getSymbol()->getLine();
    } else if (ctx->T_VOID()) {
        funcReturnType.type = BasicType::TYPE_VOID;
        funcReturnType.lineno = ctx->T_VOID()->getSymbol()->getLine();
    }

    // 创建函数名的标识符终结符节点，终结符
    char * id = strdup(ctx->T_ID()->getText().c_str());

    var_id_attr funcId{id, (int64_t) ctx->T_ID()->getSymbol()->getLine()};

    // 处理形参列表
    ast_node * formalParamsNode = nullptr;
    if (ctx->formalParamList()) {
        // 有形参，创建形参节点
        formalParamsNode = std::any_cast<ast_node *>(visitFormalParamList(ctx->formalParamList()));
    }

    // 遍历block结点创建函数体节点，非终结符
    auto blockNode = std::any_cast<ast_node *>(visitBlock(ctx->block()));

    // 创建函数定义的节点，孩子有类型，函数名，语句块和形参(实际上无)
    // create_func_def函数内会释放funcId中指向的标识符空间，切记，之后不要再释放，之前一定要是通过strdup函数或者malloc分配的空间
    return create_func_def(funcReturnType, funcId, blockNode, formalParamsNode);
}

/// @brief 非终结运算符block的遍历
/// @param ctx CST上下文
std::any MiniCCSTVisitor::visitBlock(MiniCParser::BlockContext * ctx)
{
    // 识别的文法产生式：block : T_L_BRACE blockItemList? T_R_BRACE';
    if (!ctx->blockItemList()) {
        // 语句块没有语句

        // 为了方便创建一个空的Block节点
        return create_contain_node(ast_operator_type::AST_OP_BLOCK);
    }

    // 语句块含有语句

    // 内部创建Block节点，并把语句加入，这里不需要创建Block节点
    return visitBlockItemList(ctx->blockItemList());
}

/// @brief 非终结运算符blockItemList的遍历
/// @param ctx CST上下文
std::any MiniCCSTVisitor::visitBlockItemList(MiniCParser::BlockItemListContext * ctx)
{
    // 识别的文法产生式：blockItemList : blockItem +;
    // 正闭包 循环 至少一个blockItem
    auto block_node = create_contain_node(ast_operator_type::AST_OP_BLOCK);

    for (auto blockItemCtx: ctx->blockItem()) {

        // 非终结符，需遍历
        auto blockItem = std::any_cast<ast_node *>(visitBlockItem(blockItemCtx));

        // 插入到块节点中
        (void) block_node->insert_son_node(blockItem);
    }

    return block_node;
}

///
/// @brief 非终结运算符blockItem的遍历
/// @param ctx CST上下文
///
std::any MiniCCSTVisitor::visitBlockItem(MiniCParser::BlockItemContext * ctx)
{
    // 识别的文法产生式：blockItem : statement | varDecl
    if (ctx->statement()) {
        // 语句识别
        return visitStatement(ctx->statement());
    } else if (ctx->varDecl()) {
        return visitVarDecl(ctx->varDecl());
    }

    return nullptr;
}

/// @brief 非终结运算符statement中的遍历
/// @param ctx CST上下文
std::any MiniCCSTVisitor::visitStatement(MiniCParser::StatementContext * ctx)
{
    // 识别的文法产生式：statement: T_ID T_ASSIGN expr T_SEMICOLON  # assignStatement
    // | T_RETURN expr T_SEMICOLON # returnStatement
    // | block  # blockStatement
    // | expr ? T_SEMICOLON #expressionStatement;
    if (Instanceof(assignCtx, MiniCParser::AssignStatementContext *, ctx)) {
        return visitAssignStatement(assignCtx);
    } else if (Instanceof(returnCtx, MiniCParser::ReturnStatementContext *, ctx)) {
        return visitReturnStatement(returnCtx);
    } else if (Instanceof(blockCtx, MiniCParser::BlockStatementContext *, ctx)) {
        return visitBlockStatement(blockCtx);
    } else if (Instanceof(exprCtx, MiniCParser::ExpressionStatementContext *, ctx)) {
        return visitExpressionStatement(exprCtx);
    } else if (Instanceof(ifCtx, MiniCParser::IfStmtContext *, ctx)) {
        return visitIfStmt(ifCtx);
    } else if (Instanceof(whileCtx, MiniCParser::WhileStmtContext *, ctx)) {
        return visitWhileStmt(whileCtx);
    } else if (Instanceof(forCtx, MiniCParser::ForStmtContext *, ctx)) {
        return visitForStmt(forCtx);
    } else if (Instanceof(breakCtx, MiniCParser::BreakStmtContext *, ctx)) {
        return visitBreakStmt(breakCtx);
    } else if (Instanceof(continueCtx, MiniCParser::ContinueStmtContext *, ctx)) {
        return visitContinueStmt(continueCtx);
    } else if (Instanceof(NopContext, MiniCParser::NopContext *, ctx)) {
        return visitNop(NopContext);
    }

    return nullptr;
}

///
/// @brief 非终结运算符statement中的returnStatement的遍历
/// @param ctx CST上下文
///
std::any MiniCCSTVisitor::visitReturnStatement(MiniCParser::ReturnStatementContext * ctx)
{
    // 识别的文法产生式：returnStatement -> T_RETURN expr T_SEMICOLON

    if (ctx->expr()) {
        // 非终结符，表达式expr遍历
        auto exprNode = std::any_cast<ast_node *>(visitExpr(ctx->expr()));
        // 创建返回节点，其孩子为Expr
        return create_contain_node(ast_operator_type::AST_OP_RETURN, exprNode);
    } else {
        // 空的return语句（用于void函数）
        // 创建返回节点，没有子节点
        return create_contain_node(ast_operator_type::AST_OP_RETURN);
    }
}

/// @brief 非终结运算符expr的遍历
/// @param ctx CST上下文
std::any MiniCCSTVisitor::visitExpr(MiniCParser::ExprContext * ctx)
{
    // 识别产生式：expr: logicOrExp | lVal T_ASSIGN logicOrExp;

    if (ctx->lVal() && ctx->T_ASSIGN() && ctx->logicOrExp()) {
        // 赋值表达式
        auto lvalNode = std::any_cast<ast_node *>(visitLVal(ctx->lVal()));
        auto exprNode = std::any_cast<ast_node *>(visitLogicOrExp(ctx->logicOrExp()));

        // 创建赋值节点
        return ast_node::New(ast_operator_type::AST_OP_ASSIGN, lvalNode, exprNode, nullptr);
    } else if (ctx->logicOrExp()) {
        // 普通表达式
        return visitLogicOrExp(ctx->logicOrExp());
    }

    return nullptr;
}

std::any MiniCCSTVisitor::visitAssignStatement(MiniCParser::AssignStatementContext * ctx)
{
    // 识别文法产生式：assignStatement: lVal T_ASSIGN expr T_SEMICOLON

    // 赋值左侧左值Lval遍历产生节点
    auto lvalNode = std::any_cast<ast_node *>(visitLVal(ctx->lVal()));

    // 赋值右侧expr遍历
    auto exprNode = std::any_cast<ast_node *>(visitExpr(ctx->expr()));

    // 创建一个AST_OP_ASSIGN类型的中间节点，孩子为Lval和Expr
    return ast_node::New(ast_operator_type::AST_OP_ASSIGN, lvalNode, exprNode, nullptr);
}

std::any MiniCCSTVisitor::visitBlockStatement(MiniCParser::BlockStatementContext * ctx)
{
    // 识别文法产生式 blockStatement: block

    return visitBlock(ctx->block());
}

std::any MiniCCSTVisitor::visitAddExp(MiniCParser::AddExpContext * ctx)
{
    // 识别的文法产生式：addExp : unaryExp (addOp unaryExp)*;

    if (ctx->addOp().empty()) {

        // 没有addOp运算符，则说明闭包识别为0，只识别了第一个非终结符unaryExp
        return visitMulExp(ctx->mulExp()[0]);
    }

    ast_node *left, *right;

    // 存在addOp运算符，自
    auto opsCtxVec = ctx->addOp();

    // 有操作符，肯定会进循环，使得right设置正确的值
    for (int k = 0; k < (int) opsCtxVec.size(); k++) {

        // 获取运算符
        ast_operator_type op = std::any_cast<ast_operator_type>(visitAddOp(opsCtxVec[k]));

        if (k == 0) {

            // 左操作数
            left = std::any_cast<ast_node *>(visitMulExp(ctx->mulExp()[k]));
        }

        // 右操作数
        right = std::any_cast<ast_node *>(visitMulExp(ctx->mulExp()[k + 1]));

        // 新建结点作为下一个运算符的右操作符
        left = ast_node::New(op, left, right, nullptr);
    }

    return left;
}

std::any MiniCCSTVisitor::visitMulExp(MiniCParser::MulExpContext * ctx)
{
    if (ctx->mulOp().empty()) {
        // 没有addOp运算符，则说明闭包识别为0，只识别了第一个非终结符unaryExp
        return visitUnaryExp(ctx->unaryExp()[0]);
    }

    ast_node *left, *right;

    // 存在addOp运算符，自
    auto opsCtxVec = ctx->mulOp();

    // 有操作符，肯定会进循环，使得right设置正确的值
    for (int k = 0; k < (int) opsCtxVec.size(); k++) {

        // 获取运算符
        ast_operator_type op = std::any_cast<ast_operator_type>(visitMulOp(opsCtxVec[k]));

        if (k == 0) {

            // 左操作数
            left = std::any_cast<ast_node *>(visitUnaryExp(ctx->unaryExp()[k]));
        }

        // 右操作数
        right = std::any_cast<ast_node *>(visitUnaryExp(ctx->unaryExp()[k + 1]));

        // 新建结点作为下一个运算符的右操作符
        left = ast_node::New(op, left, right, nullptr);
    }

    return left;
}

std::any MiniCCSTVisitor::visitMulOp(MiniCParser::MulOpContext * ctx)
{
    if (ctx->T_DIV()) {
        return ast_operator_type::AST_OP_DIV;
    } else if (ctx->T_MUL()) {
        return ast_operator_type::AST_OP_MUL;
    } else if (ctx->T_MOD()) {
        return ast_operator_type::AST_OP_MOD;
    } else {
        return nullptr;
    }
}

/// @brief 非终结运算符addOp的遍历
/// @param ctx CST上下文
std::any MiniCCSTVisitor::visitAddOp(MiniCParser::AddOpContext * ctx)
{
    // 识别的文法产生式：addOp : T_ADD | T_SUB

    if (ctx->T_ADD()) {
        return ast_operator_type::AST_OP_ADD;
    } else {
        return ast_operator_type::AST_OP_SUB;
    }
}

std::any MiniCCSTVisitor::visitUnaryExp(MiniCParser::UnaryExpContext * ctx)
{
    // 识别文法产生式：unaryExp: primaryExp | T_ID T_L_PAREN realParamList? T_R_PAREN;

    if (ctx->primaryExp()) {
        // 普通表达式
        return visitPrimaryExp(ctx->primaryExp());
    } else if (ctx->T_ID()) {

        // 创建函数调用名终结符节点
        ast_node * funcname_node = ast_node::New(ctx->T_ID()->getText(), (int64_t) ctx->T_ID()->getSymbol()->getLine());

        // 实参列表
        ast_node * paramListNode = nullptr;

        // 函数调用
        if (ctx->realParamList()) {
            // 有参数
            paramListNode = std::any_cast<ast_node *>(visitRealParamList(ctx->realParamList()));
        }

        // 创建函数调用节点，其孩子为被调用函数名和实参，
        return create_func_call(funcname_node, paramListNode);
    } else if (ctx->unaryOp()) {
        // 一元运算符处理 - 取负运算
        ast_operator_type op = std::any_cast<ast_operator_type>(visitUnaryOp(ctx->unaryOp()));
        ast_node * expr = std::any_cast<ast_node *>(visitUnaryExp(ctx->unaryExp()));

        // 创建一元取负节点，其孩子为表达式
        return create_contain_node(op, expr);
    } else if (ctx->T_LNOT()) {
        // 逻辑非操作
        ast_node * expr = std::any_cast<ast_node *>(visitUnaryExp(ctx->unaryExp()));

        // 创建逻辑非节点
        return create_contain_node(ast_operator_type::AST_OP_LNOT, expr);
    } else if (ctx->T_INC() && ctx->lVal() && ctx->children.size() == 2 && ctx->children[0] == ctx->T_INC()) {
        // 前置自增 ++lVal (T_INC在前面)
        ast_node * lvalNode = std::any_cast<ast_node *>(visitLVal(ctx->lVal()));
        return create_contain_node(ast_operator_type::AST_OP_PRE_INC, lvalNode);
    } else if (ctx->T_DEC() && ctx->lVal() && ctx->children.size() == 2 && ctx->children[0] == ctx->T_DEC()) {
        // 前置自减 --lVal (T_DEC在前面)
        ast_node * lvalNode = std::any_cast<ast_node *>(visitLVal(ctx->lVal()));
        return create_contain_node(ast_operator_type::AST_OP_PRE_DEC, lvalNode);
    } else if (ctx->lVal() && ctx->T_INC() && ctx->children.size() == 2 && ctx->children[0] == ctx->lVal()) {
        // 后置自增 lVal++ (lVal在前面)
        ast_node * lvalNode = std::any_cast<ast_node *>(visitLVal(ctx->lVal()));
        return create_contain_node(ast_operator_type::AST_OP_POST_INC, lvalNode);
    } else if (ctx->lVal() && ctx->T_DEC() && ctx->children.size() == 2 && ctx->children[0] == ctx->lVal()) {
        // 后置自减 lVal-- (lVal在前面)
        ast_node * lvalNode = std::any_cast<ast_node *>(visitLVal(ctx->lVal()));
        return create_contain_node(ast_operator_type::AST_OP_POST_DEC, lvalNode);
    }
    return nullptr;
}

std::any MiniCCSTVisitor::visitUnaryOp(MiniCParser::UnaryOpContext * ctx)
{
    return ast_operator_type::AST_OP_NEG;
}

std::any MiniCCSTVisitor::visitPrimaryExp(MiniCParser::PrimaryExpContext * ctx)
{
    // 识别文法产生式 primaryExp: T_L_PAREN expr T_R_PAREN | T_DIGIT | lVal;

    ast_node * node = nullptr;

    if (ctx->T_DIGIT()) {
        // 无符号整型字面量
        // 识别 primaryExp: T_DIGIT

        uint32_t val = (uint32_t) stoull(ctx->T_DIGIT()->getText(), nullptr, 0);
        int64_t lineNo = (int64_t) ctx->T_DIGIT()->getSymbol()->getLine();
        node = ast_node::New(digit_int_attr{val, lineNo});
    } else if (ctx->lVal()) {
        // 具有左值的表达式
        // 识别 primaryExp: lVal
        node = std::any_cast<ast_node *>(visitLVal(ctx->lVal()));
    } else if (ctx->expr()) {
        // 带有括号的表达式
        // primaryExp: T_L_PAREN expr T_R_PAREN
        node = std::any_cast<ast_node *>(visitExpr(ctx->expr()));
    }

    return node;
}

std::any MiniCCSTVisitor::visitLVal(MiniCParser::LValContext * ctx)
{
    // 识别文法产生式：lVal: T_ID (T_L_BRACKET expr T_R_BRACKET)*;

    // 获取ID的名字和行号
    auto varId = ctx->T_ID()->getText();
    int64_t lineNo = (int64_t) ctx->T_ID()->getSymbol()->getLine();

    // 创建基础变量节点
    ast_node * varNode = ast_node::New(varId, lineNo);

    // 检查是否有数组索引
    if (ctx->expr().empty()) {
        // 没有数组索引，直接返回变量节点
        return varNode;
    }

    // 创建一个数组访问节点
    ast_node * arrayAccessNode = create_contain_node(ast_operator_type::AST_OP_ARRAY_ACCESS);

    // 添加数组名作为第一个子节点
    arrayAccessNode->insert_son_node(varNode);

    // 添加所有索引作为后续子节点
    for (auto exprCtx: ctx->expr()) {
        ast_node * indexNode = std::any_cast<ast_node *>(visitExpr(exprCtx));
        arrayAccessNode->insert_son_node(indexNode);
    }

    return arrayAccessNode;
}

std::any MiniCCSTVisitor::visitVarDecl(MiniCParser::VarDeclContext * ctx)
{
    // varDecl: basicType varDef (T_COMMA varDef)* T_SEMICOLON;

    // 声明语句节点
    ast_node * stmt_node = create_contain_node(ast_operator_type::AST_OP_DECL_STMT);

    // 类型节点
    type_attr typeAttr = std::any_cast<type_attr>(visitBasicType(ctx->basicType()));

    for (auto & varCtx: ctx->varDef()) {
        // 变量名节点，也可能返回赋值语句节点，因为加入了变量定义初始化
        ast_node * id_node = std::any_cast<ast_node *>(visitVarDef(varCtx));

        // 创建类型节点
        ast_node * type_node = create_type_node(typeAttr);

        // 创建变量定义节点
        ast_node * decl_node = ast_node::New(ast_operator_type::AST_OP_VAR_DECL, type_node, id_node, nullptr);

        // 插入到变量声明语句
        (void) stmt_node->insert_son_node(decl_node);
    }

    return stmt_node;
}

std::any MiniCCSTVisitor::visitVarDef(MiniCParser::VarDefContext * ctx)
{
    // 识别的文法产生式：varDef: T_ID arrayDeclarator? (T_ASSIGN expr)?;

    auto varId = ctx->T_ID()->getText();

    // 获取行号
    int64_t lineNo = (int64_t) ctx->T_ID()->getSymbol()->getLine();
    // 创建变量名节点
    ast_node * id_node = ast_node::New(varId, lineNo);

    // 检查是否有数组声明符
    if (ctx->arrayDeclarator()) {
        // 有数组声明符，获取数组声明符节点
        std::vector<int> dim = std::any_cast<std::vector<int>>(visitArrayDeclarator(ctx->arrayDeclarator()));

        // 创建数组变量节点，将变量名和数组声明符结合
        // TODO 简化AST?
        id_node = ast_node::New(ast_operator_type::AST_OP_ARRAY_VAR, id_node, nullptr);
        for (auto i: dim) {
            // 为每个维度创建一个常数节点并添加到数组变量节点
            ast_node * dim_node = ast_node::New(digit_int_attr{(uint32_t) i, lineNo});
            id_node->insert_son_node(dim_node);
        }
    }

    // 检查是否有初始化表达式
    if (ctx->T_ASSIGN() && ctx->expr()) {
        ast_node * expr_node = std::any_cast<ast_node *>(visitExpr(ctx->expr()));

        // 创建赋值节点，其孩子为变量名节点（可能包含数组声明符）和表达式节点
        return ast_node::New(ast_operator_type::AST_OP_ASSIGN, id_node, expr_node, nullptr);
    }

    // 无初始化表达式，直接返回变量名节点（可能包含数组声明符）
    return id_node;
}

std::any MiniCCSTVisitor::visitRealParamList(MiniCParser::RealParamListContext * ctx)
{
    // 识别的文法产生式：realParamList : expr (T_COMMA expr)*;

    auto paramListNode = create_contain_node(ast_operator_type::AST_OP_FUNC_REAL_PARAMS);

    for (auto paramCtx: ctx->expr()) {

        auto paramNode = std::any_cast<ast_node *>(visitExpr(paramCtx));

        paramListNode->insert_son_node(paramNode);
    }

    return paramListNode;
}

std::any MiniCCSTVisitor::visitExpressionStatement(MiniCParser::ExpressionStatementContext * ctx)
{
    // 识别文法产生式  expr ? T_SEMICOLON #expressionStatement;
    if (ctx->expr()) {
        // 表达式语句

        // 遍历expr非终结符，创建表达式节点后返回
        return visitExpr(ctx->expr());
    } else {
        // 空语句

        // 直接返回空指针，需要再把语句加入到语句块时要注意判断，空语句不要加入
        return nullptr;
    }
}

// 逻辑或表达式
std::any MiniCCSTVisitor::visitLogicOrExp(MiniCParser::LogicOrExpContext * ctx)
{
    if (ctx->logicAndExp().size() == 1) {
        return visitLogicAndExp(ctx->logicAndExp(0));
    }

    ast_node *left, *right;

    for (int i = 0; i < ctx->logicAndExp().size() - 1; i++) {
        if (i == 0) {
            left = std::any_cast<ast_node *>(visitLogicAndExp(ctx->logicAndExp(i)));
        }

        right = std::any_cast<ast_node *>(visitLogicAndExp(ctx->logicAndExp(i + 1)));

        // 创建逻辑或节点
        left = ast_node::New(ast_operator_type::AST_OP_LOR, left, right, nullptr);
    }

    return left;
}

// 逻辑与表达式
std::any MiniCCSTVisitor::visitLogicAndExp(MiniCParser::LogicAndExpContext * ctx)
{
    if (ctx->eqExp().size() == 1) {
        return visitEqExp(ctx->eqExp(0));
    }

    ast_node *left, *right;

    for (int i = 0; i < ctx->eqExp().size() - 1; i++) {
        if (i == 0) {
            left = std::any_cast<ast_node *>(visitEqExp(ctx->eqExp(i)));
        }

        right = std::any_cast<ast_node *>(visitEqExp(ctx->eqExp(i + 1)));

        // 创建逻辑与节点
        left = ast_node::New(ast_operator_type::AST_OP_LAND, left, right, nullptr);
    }

    return left;
}

// 相等性表达式
std::any MiniCCSTVisitor::visitEqExp(MiniCParser::EqExpContext * ctx)
{
    if (ctx->relExp().size() == 1) {
        return visitRelExp(ctx->relExp(0));
    }

    ast_node *left, *right;

    for (int i = 0; i < ctx->relExp().size() - 1; i++) {
        // 获取操作符
        ast_operator_type op = std::any_cast<ast_operator_type>(visitEqOp(ctx->eqOp(i)));

        if (i == 0) {
            left = std::any_cast<ast_node *>(visitRelExp(ctx->relExp(i)));
        }

        right = std::any_cast<ast_node *>(visitRelExp(ctx->relExp(i + 1)));

        // 创建相等比较节点
        left = ast_node::New(op, left, right, nullptr);
    }

    return left;
}

// 关系表达式
std::any MiniCCSTVisitor::visitRelExp(MiniCParser::RelExpContext * ctx)
{
    if (ctx->addExp().size() == 1) {
        return visitAddExp(ctx->addExp(0));
    }

    ast_node *left, *right;

    for (int i = 0; i < ctx->addExp().size() - 1; i++) {
        // 获取操作符
        ast_operator_type op = std::any_cast<ast_operator_type>(visitRelOp(ctx->relOp(i)));

        if (i == 0) {
            left = std::any_cast<ast_node *>(visitAddExp(ctx->addExp(i)));
        }

        right = std::any_cast<ast_node *>(visitAddExp(ctx->addExp(i + 1)));

        // 创建关系比较节点
        left = ast_node::New(op, left, right, nullptr);
    }

    return left;
}

// 相等运算符
std::any MiniCCSTVisitor::visitEqOp(MiniCParser::EqOpContext * ctx)
{
    if (ctx->T_EQ()) {
        return ast_operator_type::AST_OP_EQ;
    } else if (ctx->T_NE()) {
        return ast_operator_type::AST_OP_NE;
    }
    return nullptr;
}

// 关系运算符
std::any MiniCCSTVisitor::visitRelOp(MiniCParser::RelOpContext * ctx)
{
    if (ctx->T_GT()) {
        return ast_operator_type::AST_OP_GT;
    } else if (ctx->T_LT()) {
        return ast_operator_type::AST_OP_LT;
    } else if (ctx->T_GE()) {
        return ast_operator_type::AST_OP_GE;
    } else if (ctx->T_LE()) {
        return ast_operator_type::AST_OP_LE;
    }
    return nullptr;
}

// if语句
std::any MiniCCSTVisitor::visitIfStmt(MiniCParser::IfStmtContext * ctx)
{
    // 条件表达式
    auto condNode = std::any_cast<ast_node *>(visitExpr(ctx->ifStatement()->expr()));

    // 真分支语句
    std::any thenResult = visitStatement(ctx->ifStatement()->statement(0));
    ast_node * thenNode = nullptr;

    // 尝试转换，如果转换失败或结果为nullptr，则创建空语句块
    try {
        thenNode = std::any_cast<ast_node *>(thenResult);
    } catch (const std::bad_any_cast &) {
        // 转换失败，说明返回类型不是 ast_node*, 创建一个空的语句块作为then分支
        thenNode = create_contain_node(ast_operator_type::AST_OP_BLOCK);
    }

    // 创建if节点
    ast_node * ifNode;

    // 处理可选的else部分
    if (ctx->ifStatement()->T_ELSE()) { // 检查是否有第二个语句(else分支)
        // 有else分支
        auto elseNode = std::any_cast<ast_node *>(visitStatement(ctx->ifStatement()->statement(1)));
        ifNode = ast_node::New(ast_operator_type::AST_OP_IF, condNode, thenNode, elseNode, nullptr);
    } else {
        // 没有else分支
        ifNode = ast_node::New(ast_operator_type::AST_OP_IF, condNode, thenNode, nullptr);
    }

    return ifNode;
}

// while语句
std::any MiniCCSTVisitor::visitWhileStmt(MiniCParser::WhileStmtContext * ctx)
{
    // 条件表达式
    auto condNode = std::any_cast<ast_node *>(visitExpr(ctx->whileStatement()->expr()));

    // 循环体语句
    std::any bodyResult = visitStatement(ctx->whileStatement()->statement());
    ast_node * bodyNode = nullptr;

    // 尝试转换，如果转换失败或结果为nullptr，则创建空语句块
    try {
        bodyNode = std::any_cast<ast_node *>(bodyResult);
    } catch (const std::bad_any_cast &) {
        // 转换失败，说明返回类型不是 ast_node*
        bodyNode = create_contain_node(ast_operator_type::AST_OP_BLOCK);
    }

    // 创建while节点
    return ast_node::New(ast_operator_type::AST_OP_WHILE, condNode, bodyNode, nullptr);
}

// break语句
std::any MiniCCSTVisitor::visitBreakStmt(MiniCParser::BreakStmtContext * ctx)
{
    // 创建break节点
    return ast_node::New(ast_operator_type::AST_OP_BREAK, nullptr);
}

// continue语句
std::any MiniCCSTVisitor::visitContinueStmt(MiniCParser::ContinueStmtContext * ctx)
{
    // 创建continue节点
    return ast_node::New(ast_operator_type::AST_OP_CONTINUE, nullptr);
}

std::any MiniCCSTVisitor::visitFormalParamList(MiniCParser::FormalParamListContext * ctx)
{
    // 识别的文法产生式：formalParamList: formalParam (T_COMMA formalParam)*;
    // 创建形参列表节点
    ast_node * formalParamsNode = create_contain_node(ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS);

    // 遍历所有形参并添加到形参列表节点
    for (auto paramCtx: ctx->formalParam()) {
        // 访问每个形参，获取形参节点
        ast_node * paramNode = std::any_cast<ast_node *>(visitFormalParam(paramCtx));

        // 添加到形参列表节点
        formalParamsNode->insert_son_node(paramNode);
    }

    return formalParamsNode;
}

std::any MiniCCSTVisitor::visitFormalParam(MiniCParser::FormalParamContext * ctx)
{
    // 识别的文法产生式：formalParam: basicType T_ID arrayDeclarator?;

    // 获取形参类型
    type_attr paramType = std::any_cast<type_attr>(visitBasicType(ctx->basicType()));

    // 获取形参名称和行号
    auto paramName = ctx->T_ID()->getText();
    int64_t paramLineNo = (int64_t) ctx->T_ID()->getSymbol()->getLine();

    // 创建形参类型节点
    ast_node * paramTypeNode = create_type_node(paramType);

    // 创建形参名称节点
    ast_node * paramIdNode = ast_node::New(paramName, paramLineNo);

    // 检查是否有数组声明符
    if (ctx->arrayDeclarator()) {
        // 有数组声明符，获取数组声明符节点
        std::vector<int> dim = std::any_cast<std::vector<int>>(visitArrayDeclarator(ctx->arrayDeclarator()));
        // 创建数组形参节点，将形参名和数组声明符结合
        paramIdNode = ast_node::New(ast_operator_type::AST_OP_ARRAY_VAR, paramIdNode, nullptr);
        for (auto i: dim) {
            // 为每个维度创建一个常数节点并添加到数组形参节点
            ast_node * dim_node = ast_node::New(digit_int_attr{(uint32_t) i, paramLineNo});
            paramIdNode->insert_son_node(dim_node);
        }
    }

    // 创建形参声明节点
    ast_node * paramDeclNode =
        ast_node::New(ast_operator_type::AST_OP_FUNC_FORMAL_PARAM, paramTypeNode, paramIdNode, nullptr);

    return paramDeclNode;
}

std::any MiniCCSTVisitor::visitArrayDeclarator(MiniCParser::ArrayDeclaratorContext * ctx)
{
    // 识别的文法产生式：arrayDeclarator: (T_L_BRACKET expr T_R_BRACKET)+;
    std::vector<int> dim;

    // 如果是形参数组声明且第一个维度为空，需要特殊处理
    // 检查是否所有中括号数量比表达式数量多，说明第一个维度是空的
    if (ctx->T_L_BRACKET().size() > ctx->expr().size()) {
        // 如果第一个维度未被处理，添加一个0作为第一维
        dim.push_back(0);
    }

    // 遍历所有的数组维度表达式
    for (size_t i = 0; i < ctx->expr().size(); i++) {
        auto exprCtx = ctx->expr()[i];
        ast_node * exprNode = std::any_cast<ast_node *>(visitExpr(exprCtx));
        dim.push_back(static_cast<int>(exprNode->integer_val));
    }

    return dim;
}

std::any MiniCCSTVisitor::visitBasicType(MiniCParser::BasicTypeContext * ctx)
{
    // 识别的文法产生式：basicType: T_INT;

    type_attr typeAttr;

    if (ctx->T_INT()) {
        typeAttr.type = BasicType::TYPE_INT;
        typeAttr.lineno = (int64_t) ctx->T_INT()->getSymbol()->getLine();
    }

    return typeAttr;
}

std::any MiniCCSTVisitor::visitNop(MiniCParser::NopContext * ctx)
{
    return create_contain_node(ast_operator_type::AST_OP_BLOCK);
}

// for语句
std::any MiniCCSTVisitor::visitForStmt(MiniCParser::ForStmtContext * ctx)
{
    return visitForStatement(ctx->forStatement());
}

// for语句的具体实现
std::any MiniCCSTVisitor::visitForStatement(MiniCParser::ForStatementContext * ctx)
{
    // 识别文法产生式：T_FOR T_L_PAREN forInit? T_SEMICOLON expr? T_SEMICOLON forUpdate? T_R_PAREN statement

    ast_node * initNode = nullptr;
    ast_node * condNode = nullptr;
    ast_node * updateNode = nullptr;
    ast_node * bodyNode = nullptr;

    // 处理初始化部分
    if (ctx->forInit()) {
        initNode = std::any_cast<ast_node *>(visitForInit(ctx->forInit()));
    }

    // 处理条件部分
    if (ctx->expr()) {
        condNode = std::any_cast<ast_node *>(visitExpr(ctx->expr()));
    }

    // 处理步进部分
    if (ctx->forUpdate()) {
        updateNode = std::any_cast<ast_node *>(visitForUpdate(ctx->forUpdate()));
    }

    // 处理循环体
    std::any bodyResult = visitStatement(ctx->statement());
    try {
        bodyNode = std::any_cast<ast_node *>(bodyResult);
    } catch (const std::bad_any_cast &) {
        // 转换失败，创建空语句块
        bodyNode = create_contain_node(ast_operator_type::AST_OP_BLOCK);
    }

    // 创建for循环节点
    ast_node * forNode = create_contain_node(ast_operator_type::AST_OP_FOR);

    if (initNode) {
        forNode->insert_son_node(initNode);
    }
    if (condNode) {
        forNode->insert_son_node(condNode);
    }
    if (updateNode) {
        forNode->insert_son_node(updateNode);
    }
    if (bodyNode) {
        forNode->insert_son_node(bodyNode);
    }

    return forNode;
}

std::any MiniCCSTVisitor::visitForInit(MiniCParser::ForInitContext * ctx)
{
    ast_node * initContentNode = nullptr;

    if (ctx->varDeclNoSemi()) {
        initContentNode = std::any_cast<ast_node *>(visitVarDeclNoSemi(ctx->varDeclNoSemi()));
    } else if (ctx->expr()) {
        initContentNode = std::any_cast<ast_node *>(visitExpr(ctx->expr()));
    }

    // 创建FOR_INIT类型的容器节点
    ast_node * forInitNode = create_contain_node(ast_operator_type::AST_OP_FOR_INIT);

    // 如果有初始化内容，添加为子节点
    if (initContentNode) {
        forInitNode->insert_son_node(initContentNode);
    }

    return forInitNode;
}

// for循环步进部分
std::any MiniCCSTVisitor::visitForUpdate(MiniCParser::ForUpdateContext * ctx)
{
    // 识别文法产生式：forUpdate: expr (T_COMMA expr)*;

    ast_node * updateListNode = create_contain_node(ast_operator_type::AST_OP_FOR_UPDATE);

    for (auto exprCtx: ctx->expr()) {
        ast_node * exprNode = std::any_cast<ast_node *>(visitExpr(exprCtx));
        updateListNode->insert_son_node(exprNode);
    }

    return updateListNode;
}

// 新增：处理无分号的变量声明
std::any MiniCCSTVisitor::visitVarDeclNoSemi(MiniCParser::VarDeclNoSemiContext * ctx)
{
    // varDeclNoSemi: basicType varDef (T_COMMA varDef)*;

    // 声明语句节点
    ast_node * stmt_node = create_contain_node(ast_operator_type::AST_OP_DECL_STMT);

    // 类型节点
    type_attr typeAttr = std::any_cast<type_attr>(visitBasicType(ctx->basicType()));

    for (auto & varCtx: ctx->varDef()) {
        // 变量名节点，也可能返回赋值语句节点，因为加入了变量定义初始化
        ast_node * id_node = std::any_cast<ast_node *>(visitVarDef(varCtx));

        // 创建类型节点
        ast_node * type_node = create_type_node(typeAttr);

        // 创建变量定义节点
        ast_node * decl_node = ast_node::New(ast_operator_type::AST_OP_VAR_DECL, type_node, id_node, nullptr);

        // 插入到变量声明语句
        (void) stmt_node->insert_son_node(decl_node);
    }

    return stmt_node;
}