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

#include "Antlr4CSTVisitor.h"
#include "AST.h"
#include "AttrType.h"

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
    } else if (Instanceof(breakCtx, MiniCParser::BreakStmtContext *, ctx)) {
        return visitBreakStmt(breakCtx);
    } else if (Instanceof(continueCtx, MiniCParser::ContinueStmtContext *, ctx)) {
        return visitContinueStmt(continueCtx);
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

    // 非终结符，表达式expr遍历
    auto exprNode = std::any_cast<ast_node *>(visitExpr(ctx->expr()));

    // 创建返回节点，其孩子为Expr
    return create_contain_node(ast_operator_type::AST_OP_RETURN, exprNode);
}

/// @brief 非终结运算符expr的遍历
/// @param ctx CST上下文
std::any MiniCCSTVisitor::visitExpr(MiniCParser::ExprContext * ctx)
{
    // 识别产生式：expr: addExp;

    return visitLogicOrExp(ctx->logicOrExp());
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
    // 识别文法产生式：lVal: T_ID;
    // 获取ID的名字
    auto varId = ctx->T_ID()->getText();

    // 获取行号
    int64_t lineNo = (int64_t) ctx->T_ID()->getSymbol()->getLine();

    return ast_node::New(varId, lineNo);
}

std::any MiniCCSTVisitor::visitVarDecl(MiniCParser::VarDeclContext * ctx)
{
    // varDecl: basicType varDef (T_COMMA varDef)* T_SEMICOLON;

    // 声明语句节点
    ast_node * stmt_node = create_contain_node(ast_operator_type::AST_OP_DECL_STMT);

    // 类型节点
    type_attr typeAttr = std::any_cast<type_attr>(visitBasicType(ctx->basicType()));

    for (auto & varCtx: ctx->varDef()) {
        // 变量名节点
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
    // varDef: T_ID;

    auto varId = ctx->T_ID()->getText();

    // 获取行号
    int64_t lineNo = (int64_t) ctx->T_ID()->getSymbol()->getLine();

    return ast_node::New(varId, lineNo);
}

std::any MiniCCSTVisitor::visitBasicType(MiniCParser::BasicTypeContext * ctx)
{
    // basicType: T_INT;
    type_attr attr{BasicType::TYPE_VOID, -1};
    if (ctx->T_INT()) {
        attr.type = BasicType::TYPE_INT;
        attr.lineno = (int64_t) ctx->T_INT()->getSymbol()->getLine();
    }

    return attr;
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
    auto thenNode = std::any_cast<ast_node *>(visitStatement(ctx->ifStatement()->statement(0)));
    
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
    auto bodyNode = std::any_cast<ast_node *>(visitStatement(ctx->whileStatement()->statement()));

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
    // 识别的文法产生式：formalParam: basicType T_ID;

    // 获取形参类型
    type_attr paramType = std::any_cast<type_attr>(visitBasicType(ctx->basicType()));

    // 获取形参名称和行号
    auto paramName = ctx->T_ID()->getText();
    int64_t paramLineNo = (int64_t) ctx->T_ID()->getSymbol()->getLine();

    // 创建形参类型节点
    ast_node * paramTypeNode = create_type_node(paramType);

    // 创建形参名称节点
    ast_node * paramIdNode = ast_node::New(paramName, paramLineNo);

    // 创建形参声明节点
    ast_node * paramDeclNode =
        ast_node::New(ast_operator_type::AST_OP_FUNC_FORMAL_PARAM, paramTypeNode, paramIdNode, nullptr);

    return paramDeclNode;
}