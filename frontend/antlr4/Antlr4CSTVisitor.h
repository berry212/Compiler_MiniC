///
/// @file Antlr4CSTVisitor.h
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
#pragma once

#include "AST.h"
#include "MiniCBaseVisitor.h"

/// @brief 遍历具体语法树产生抽象语法树
class MiniCCSTVisitor : public MiniCBaseVisitor {

public:
    /// @brief 构造函数
    MiniCCSTVisitor();

    /// @brief 析构函数
    virtual ~MiniCCSTVisitor();

    /// @brief 遍历CST产生AST
    /// @param root CST语法树的根结点
    /// @return AST的根节点
    ast_node * run(MiniCParser::CompileUnitContext * root);

protected:
    /* 下面的函数都是从MiniCBaseVisitor继承下来的虚拟函数，需要重载实现 */

    /// @brief 非终结运算符compileUnit的遍历
    /// @param ctx CST上下文
    /// @return AST的节点
    std::any visitCompileUnit(MiniCParser::CompileUnitContext * ctx) override;

    /// @brief 非终结运算符funcDef的遍历
    /// @param ctx CST上下文
    /// @return AST的节点
    std::any visitFuncDef(MiniCParser::FuncDefContext * ctx) override;

    /// @brief 非终结运算符block的遍历
    /// @param ctx CST上下文
    /// @return AST的节点
    std::any visitBlock(MiniCParser::BlockContext * ctx) override;

    /// @brief 非终结运算符blockItemList的遍历
    /// @param ctx CST上下文
    /// @return AST的节点
    std::any visitBlockItemList(MiniCParser::BlockItemListContext * ctx) override;

    /// @brief 非终结运算符blockItem的遍历
    /// @param ctx CST上下文
    /// @return AST的节点
    std::any visitBlockItem(MiniCParser::BlockItemContext * ctx) override;

    /// @brief 非终结运算符statement中的遍历
    /// @param ctx CST上下文
    /// @return AST的节点
    std::any visitStatement(MiniCParser::StatementContext * ctx);

    /// @brief 非终结运算符statement中的returnStatement的遍历
    /// @param ctx CST上下文
    /// @return AST的节点
    std::any visitReturnStatement(MiniCParser::ReturnStatementContext * ctx) override;

    /// @brief 非终结运算符expr的遍历
    /// @param ctx CST上下文
    /// @return AST的节点
    std::any visitExpr(MiniCParser::ExprContext * ctx) override;

    ///
    /// @brief 内部产生的非终结符assignStatement的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitAssignStatement(MiniCParser::AssignStatementContext * ctx) override;

    ///
    /// @brief 内部产生的非终结符blockStatement的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitBlockStatement(MiniCParser::BlockStatementContext * ctx) override;

    ///
    /// @brief 非终结符AddExp的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitAddExp(MiniCParser::AddExpContext * ctx) override;

    ///
    /// @brief 非终结符addOp的分析
    /// @param ctx CST上下文
    /// @return std::any 类型
    ///
    std::any visitAddOp(MiniCParser::AddOpContext * ctx) override;

    ///
    /// @brief 非终结符unaryExp的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitUnaryExp(MiniCParser::UnaryExpContext * ctx) override;

    ///
    /// @brief 非终结符PrimaryExp的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitPrimaryExp(MiniCParser::PrimaryExpContext * ctx) override;

    ///
    /// @brief 非终结符LVal的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitLVal(MiniCParser::LValContext * ctx) override;

    ///
    /// @brief 非终结符VarDecl的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitVarDecl(MiniCParser::VarDeclContext * ctx) override;

    ///
    /// @brief 非终结符VarDecl的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitVarDef(MiniCParser::VarDefContext * ctx) override;

    ///
    /// @brief 非终结符BasicType的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitBasicType(MiniCParser::BasicTypeContext * ctx) override;

    ///
    /// @brief 非终结符RealParamList的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitRealParamList(MiniCParser::RealParamListContext * ctx) override;

    ///
    /// @brief 非终结符ExpressionStatement的分析
    /// @param ctx CST上下文
    /// @return std::any AST的节点
    ///
    std::any visitExpressionStatement(MiniCParser::ExpressionStatementContext * context) override;
	
	/// @brief 非终结符mulExp分析 乘法除法取模 
	/// @param ctx CST上下文
	/// @return std::any AST的节点
	std::any visitMulExp(MiniCParser::MulExpContext *ctx) override;

    /// @brief 非终结符mulOp
    /// @param ctx 
    /// @return 
    std::any visitMulOp(MiniCParser::MulOpContext * ctx) override;

    /// @brief 非终结符UnaryOp
    /// @param ctx 
    /// @return
    std::any visitUnaryOp(MiniCParser::UnaryOpContext * ctx) override;

    // 新增控制流语句的访问方法
    std::any visitIfStmt(MiniCParser::IfStmtContext * ctx) override;
    std::any visitWhileStmt(MiniCParser::WhileStmtContext * ctx) override;
    std::any visitBreakStmt(MiniCParser::BreakStmtContext * ctx) override;
    std::any visitContinueStmt(MiniCParser::ContinueStmtContext * ctx) override;

    // 新增逻辑和关系表达式的访问方法
    std::any visitLogicOrExp(MiniCParser::LogicOrExpContext * ctx) override;
    std::any visitLogicAndExp(MiniCParser::LogicAndExpContext * ctx) override;
    std::any visitEqExp(MiniCParser::EqExpContext * ctx) override;
    std::any visitRelExp(MiniCParser::RelExpContext * ctx) override;
    std::any visitEqOp(MiniCParser::EqOpContext * ctx) override;
    std::any visitRelOp(MiniCParser::RelOpContext * ctx) override;

    // 形参列表相关方法
    std::any visitFormalParamList(MiniCParser::FormalParamListContext * ctx) override;
    std::any visitFormalParam(MiniCParser::FormalParamContext * ctx) override;

    std::any visitNop(MiniCParser::NopContext * ctx) override;
    std::any visitArrayDeclarator(MiniCParser::ArrayDeclaratorContext * ctx) override;

    // 新增for语句
    std::any visitForStmt(MiniCParser::ForStmtContext * ctx) override;
    // 新增for循环相关的访问方法
    std::any visitForStatement(MiniCParser::ForStatementContext * ctx) override;
    std::any visitForInit(MiniCParser::ForInitContext * ctx) override;
    std::any visitForUpdate(MiniCParser::ForUpdateContext * ctx) override;
    std::any visitVarDeclNoSemi(MiniCParser::VarDeclNoSemiContext * ctx) override;
};
