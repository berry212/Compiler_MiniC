///
/// @file IRGenerator.cpp
/// @brief AST遍历产生线性IR的源文件
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
#include <climits>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <regex>
#include <unordered_map>
#include <vector>
#include <iostream>

#include "AST.h"
#include "BranchInstruction.h"
#include "Common.h"
#include "Function.h"
#include "IRCode.h"
#include "IRGenerator.h"
#include "Instruction.h"
#include "IntegerType.h"
#include "LocalVariable.h"
#include "Module.h"
#include "EntryInstruction.h"
#include "LabelInstruction.h"
#include "ExitInstruction.h"
#include "FuncCallInstruction.h"
#include "BinaryInstruction.h"
#include "MoveInstruction.h"
#include "GotoInstruction.h"
#include "ArgInstruction.h"
#include "ArrayType.h"
#include "MemMoveInstruction.h"
#include "Type.h"
#include "Value.h"
#include "PointerType.h"

/// @brief 构造函数
/// @param _root AST的根
/// @param _module 符号表
IRGenerator::IRGenerator(ast_node * _root, Module * _module) : root(_root), module(_module)
{
    /*数组*/
    ast2ir_handlers[ast_operator_type::AST_OP_ARRAY_ACCESS] = &IRGenerator::ir_array_access;
    ast2ir_handlers[ast_operator_type::AST_OP_ARRAY_VAR] = &IRGenerator::ir_array_var;

    /* 关系运算符 */
    ast2ir_handlers[ast_operator_type::AST_OP_GT] = &IRGenerator::ir_gt;
    ast2ir_handlers[ast_operator_type::AST_OP_LT] = &IRGenerator::ir_lt;
    ast2ir_handlers[ast_operator_type::AST_OP_GE] = &IRGenerator::ir_ge;
    ast2ir_handlers[ast_operator_type::AST_OP_LE] = &IRGenerator::ir_le;
    ast2ir_handlers[ast_operator_type::AST_OP_EQ] = &IRGenerator::ir_eq;
    ast2ir_handlers[ast_operator_type::AST_OP_NE] = &IRGenerator::ir_ne;

    /* 逻辑运算符 */
    ast2ir_handlers[ast_operator_type::AST_OP_LAND] = &IRGenerator::ir_logical_and;
    ast2ir_handlers[ast_operator_type::AST_OP_LOR] = &IRGenerator::ir_logical_or;
    ast2ir_handlers[ast_operator_type::AST_OP_LNOT] = &IRGenerator::ir_logical_not;

    /* 控制流语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_IF] = &IRGenerator::ir_if;
    ast2ir_handlers[ast_operator_type::AST_OP_WHILE] = &IRGenerator::ir_while;
    ast2ir_handlers[ast_operator_type::AST_OP_BREAK] = &IRGenerator::ir_break;
    ast2ir_handlers[ast_operator_type::AST_OP_CONTINUE] = &IRGenerator::ir_continue;

    /*乘除取余取负*/
    ast2ir_handlers[ast_operator_type::AST_OP_MUL] = &IRGenerator::ir_mul;
    ast2ir_handlers[ast_operator_type::AST_OP_DIV] = &IRGenerator::ir_div;
    ast2ir_handlers[ast_operator_type::AST_OP_MOD] = &IRGenerator::ir_mod;
    ast2ir_handlers[ast_operator_type::AST_OP_NEG] = &IRGenerator::ir_neg;

    /* 叶子节点 */
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_LITERAL_UINT] = &IRGenerator::ir_leaf_node_uint;
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_VAR_ID] = &IRGenerator::ir_leaf_node_var_id;
    ast2ir_handlers[ast_operator_type::AST_OP_LEAF_TYPE] = &IRGenerator::ir_leaf_node_type;

    /* 表达式运算， 加减 */
    ast2ir_handlers[ast_operator_type::AST_OP_SUB] = &IRGenerator::ir_sub;
    ast2ir_handlers[ast_operator_type::AST_OP_ADD] = &IRGenerator::ir_add;

    /* 语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_ASSIGN] = &IRGenerator::ir_assign;
    ast2ir_handlers[ast_operator_type::AST_OP_RETURN] = &IRGenerator::ir_return;

    /* 函数调用 */
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_CALL] = &IRGenerator::ir_function_call;

    /* 函数定义 */
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_DEF] = &IRGenerator::ir_function_define;
    ast2ir_handlers[ast_operator_type::AST_OP_FUNC_FORMAL_PARAMS] = &IRGenerator::ir_function_formal_params;

    /* 变量定义语句 */
    ast2ir_handlers[ast_operator_type::AST_OP_DECL_STMT] = &IRGenerator::ir_declare_statment;
    ast2ir_handlers[ast_operator_type::AST_OP_VAR_DECL] = &IRGenerator::ir_variable_declare;

    /* 语句块 */
    ast2ir_handlers[ast_operator_type::AST_OP_BLOCK] = &IRGenerator::ir_block;

    /* 编译单元 */
    ast2ir_handlers[ast_operator_type::AST_OP_COMPILE_UNIT] = &IRGenerator::ir_compile_unit;
}

/// @brief 遍历抽象语法树产生线性IR，保存到IRCode中
/// @param root 抽象语法树
/// @param IRCode 线性IR
/// @return true: 成功 false: 失败
bool IRGenerator::run()
{
    ast_node * node;

    // 从根节点进行遍历
    node = ir_visit_ast_node(root);

    return node != nullptr;
}

/// @brief 根据AST的节点运算符查找对应的翻译函数并执行翻译动作
/// @param node AST节点
/// @return 成功返回node节点，否则返回nullptr
ast_node * IRGenerator::ir_visit_ast_node(ast_node * node)
{
    // 空节点
    if (nullptr == node) {
        return nullptr;
    }

    bool result;

    std::unordered_map<ast_operator_type, ast2ir_handler_t>::const_iterator pIter;
    pIter = ast2ir_handlers.find(node->node_type);
    if (pIter == ast2ir_handlers.end()) {
        // 没有找到，则说明当前不支持
        result = (this->ir_default)(node);
    } else {
        result = (this->*(pIter->second))(node);
    }

    if (!result) {
        // 语义解析错误，则出错返回
        node = nullptr;
    }

    return node;
}

/// @brief 未知节点类型的节点处理
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_default(ast_node * node)
{
    // 未知的节点
    printf("Unkown node(%d)\n", (int) node->node_type);
    return true;
}

/// @brief 编译单元AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_compile_unit(ast_node * node)
{
    module->setCurrentFunction(nullptr);

    for (auto son: node->sons) {

        // 遍历编译单元，要么是函数定义，要么是语句
        ast_node * son_node = ir_visit_ast_node(son);
        if (!son_node) {
            // TODO 自行追加语义错误处理
            return false;
        }
    }

    return true;
}

/// @brief 函数定义AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_define(ast_node * node)
{
    bool result;

    // 创建一个函数，用于当前函数处理
    if (module->getCurrentFunction()) {
        // 函数中嵌套定义函数，这是不允许的，错误退出
        minic_log(LOG_ERROR, "函数中嵌套定义函数不允许");
        return false;
    }

    // 函数定义的AST包含四个孩子
    // 第一个孩子：函数返回类型
    // 第二个孩子：函数名字
    // 第三个孩子：形参列表
    // 第四个孩子：函数体即block
    ast_node * type_node = node->sons[0];
    ast_node * name_node = node->sons[1];
    ast_node * param_node = node->sons[2];
    ast_node * block_node = node->sons[3];

    // 先分析形参列表并创建形参
    std::vector<FormalParam *> params;
    for (auto son: param_node->sons) {
        // 每个形参包含两个孩子：类型和变量名
        ast_node * param_type_node = son->sons[0];
        ast_node * param_name_node = son->sons[1];

        // 检查形参是否是数组类型
        if (param_name_node->node_type == ast_operator_type::AST_OP_ARRAY_VAR) {
            // 数组形参处理
            ast_node * array_name_node = param_name_node->sons[0];
            std::vector<int32_t> dimensions;

            // 收集维度信息
            for (size_t j = 1; j < param_name_node->sons.size(); j++) {
                // 添加维度信息，数组参数的第一维可以省略
                if (j == 1) {
                    dimensions.push_back(0); // 使用0表示不定大小的维度
                } else {
                    dimensions.push_back(param_name_node->sons[j]->integer_val);
                }
            }

            // 创建数组类型
            Type * elemType = param_type_node->type;
            Type * arrayType = new ArrayType(elemType, dimensions);

            // 创建形参对象，使用数组名和数组类型
            FormalParam * param = new FormalParam(arrayType, array_name_node->name);

            // 添加形参到列表
            params.push_back(param);
        } else {
            // 普通形参处理
            FormalParam * param = new FormalParam(param_type_node->type, param_name_node->name);

            // 添加形参到列表
            params.push_back(param);
        }
    }

    // 创建一个新的函数定义
    Function * newFunc = module->newFunction(name_node->name, type_node->type, params);
    if (!newFunc) {
        // 新定义的函数已经存在，则失败返回
        minic_log(LOG_ERROR, "函数(%s)重复定义", name_node->name.c_str());
        return false;
    }

    // 当前函数设置有效，变更为当前的函数
    module->setCurrentFunction(newFunc);

    // 进入函数的作用域 (stack 再加一层std::unordered_map<std::string, Value *>)
    module->enterScope();

    // 获取函数的IR代码列表，用于后面追加指令用，注意这里用的是引用传值
    InterCode & irCode = newFunc->getInterCode();

    // 创建并加入Entry入口指令
    irCode.addInst(new EntryInstruction(newFunc));

    // 创建出口指令并不加入出口指令，等函数内的指令处理完毕后加入出口指令
    LabelInstruction * exitLabelInst = new LabelInstruction(newFunc);

    // 函数出口指令保存到函数信息中，因为在语义分析函数体时return语句需要跳转到函数尾部，需要这个label指令
    newFunc->setExitLabel(exitLabelInst);

    // 处理形参
    result = ir_function_formal_params(param_node);
    if (!result) {
        // 形参解析失败
        minic_log(LOG_ERROR, "形参解析失败");
        return false;
    }
    node->blockInsts.addInst(param_node->blockInsts);

    // 新建一个Value，用于保存函数的返回值，如果没有返回值可不用申请
    LocalVariable * retValue = nullptr;
    if (!type_node->type->isVoidType()) {
        // 保存函数返回值变量到函数信息中，在return语句翻译时需要设置值到这个变量中
        retValue = static_cast<LocalVariable *>(module->newVarValue(type_node->type));
        newFunc->setReturnValue(retValue);

        // 如果是main函数，初始化返回值为0
        if (name_node->name == "main") {
            // 创建0常量
            ConstInt * zeroVal = module->newConstInt(0);
            // 生成赋值指令
            MoveInstruction * initRetInst = new MoveInstruction(newFunc, retValue, zeroVal);
            node->blockInsts.addInst(initRetInst);
        }
    }

    // 函数内已经进入作用域，内部不再需要做变量的作用域管理，即在block中不需要再创建新的作用域
    block_node->needScope = false;

    // 遍历block
    result = ir_block(block_node);
    if (!result) {
        // block解析失败
        minic_log(LOG_ERROR, "函数体解析失败");
        return false;
    }

    // IR指令追加到当前的节点中
    node->blockInsts.addInst(block_node->blockInsts);

    // node节点的指令移动到函数的IR指令列表中
    irCode.addInst(node->blockInsts);

    // 添加函数出口Label指令，主要用于return语句跳转到这里进行函数的退出
    irCode.addInst(exitLabelInst);

    // 函数出口指令
    irCode.addInst(new ExitInstruction(newFunc, retValue));

    // 恢复成外部函数
    module->setCurrentFunction(nullptr);

    // 退出函数的作用域
    module->leaveScope();

    return true;
}

/// @brief 创建形参对象并添加到当前函数，为形参创建局部变量，并将形参对象赋值给局部变量
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_formal_params(ast_node * node)
{
    // 获取当前正在处理的函数
    Function * currentFunc = module->getCurrentFunction();
    if (!currentFunc) {
        minic_log(LOG_ERROR, "翻译形参时当前函数为空");
        return false;
    }

    // 获取函数的形参列表
    auto & params = currentFunc->getParams();

    // 确保AST节点的子节点数量与函数形参数量一致
    if (node->sons.size() != params.size()) {
        minic_log(LOG_ERROR, "形参数量不匹配");
        return false;
    }

    // 遍历所有形参
    for (size_t i = 0; i < params.size(); i++) {
        // 获取已存在的形参对象
        FormalParam * param = params[i];

        // 获取AST节点中的形参信息
        ast_node * son = node->sons[i];
        ast_node * type_node = son->sons[0];
        ast_node * name_node = son->sons[1];

        // 检查形参是否是数组类型
        if (name_node->node_type == ast_operator_type::AST_OP_ARRAY_VAR) {
            // 数组形参处理
            ast_node * array_name_node = name_node->sons[0];
            std::vector<int32_t> dimensions;

            // 收集数组维度信息
            for (size_t j = 1; j < name_node->sons.size(); j++) {
                ast_node * dim_node = name_node->sons[j];

                // 在第一轮将最高维度设置为 0
                if (dim_node->node_type == ast_operator_type::AST_OP_LEAF_LITERAL_UINT && j == 1) {
                    dimensions.push_back(0); 
                    continue;
                }
                
                if (dim_node->node_type == ast_operator_type::AST_OP_LEAF_LITERAL_UINT) {
                    dimensions.push_back(static_cast<int32_t>(dim_node->integer_val));
                } else {
                    minic_log(LOG_ERROR, "数组形参维度必须是整数常量");
                    return false;
                }
            }

            // 创建数组类型
            Type * elemType = type_node->type;
            Type * arrayType = new ArrayType(elemType, dimensions);

            // 创建数组变量并加入符号表
            LocalVariable * arrayVar =
                static_cast<LocalVariable *>(module->newVarValue(arrayType, array_name_node->name));
            if (!arrayVar) {
                minic_log(LOG_ERROR, "为数组形参创建局部变量失败");
                return false;
            }

            // 将形参值赋给局部数组变量
            MoveInstruction * moveInst = new MoveInstruction(currentFunc, arrayVar, param);
            node->blockInsts.addInst(moveInst);
        } else { // 如果不是数组类型, 按照普通变量处理
            // 普通形参处理
            LocalVariable * localVar =
                static_cast<LocalVariable *>(module->newVarValue(type_node->type, name_node->name));
            if (!localVar) {
                minic_log(LOG_ERROR, "为形参创建局部变量失败");
                return false;
            }

            // 在函数入口点生成将形参值赋给局部变量的指令
            MoveInstruction * moveInst = new MoveInstruction(currentFunc, localVar, param);
            node->blockInsts.addInst(moveInst);
        }
    }

    return true;
}

/// @brief 函数调用AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_function_call(ast_node * node)
{
    std::vector<Value *> realParams;

    // 获取当前正在处理的函数
    Function * currentFunc = module->getCurrentFunction();

    // 函数调用的节点包含两个节点：
    // 第一个节点：函数名节点
    // 第二个节点：实参列表节点

    std::string funcName = node->sons[0]->name;
    int64_t lineno = node->sons[0]->line_no;

    ast_node * paramsNode = node->sons[1];

    // 根据函数名查找函数，看是否存在。若不存在则出错
    // 这里约定函数必须先定义后使用
    auto calledFunction = module->findFunction(funcName);
    if (nullptr == calledFunction) {
        minic_log(LOG_ERROR, "函数(%s)未定义或声明", funcName.c_str());
        return false;
    }

    // 当前函数存在函数调用
    currentFunc->setExistFuncCall(true);

    // 处理实参
    if (!paramsNode->sons.empty()) {
        int32_t argsCount = (int32_t) paramsNode->sons.size();

        // 当前函数中调用函数实参个数最大值统计，实际上是统计实参传参需在栈中分配的大小
        if (argsCount > currentFunc->getMaxFuncCallArgCnt()) {
            currentFunc->setMaxFuncCallArgCnt(argsCount);
        }

        // 遍历参数列表，子节点是表达式
        // 这里自左往右计算表达式
        for (auto son: paramsNode->sons) {
            // 遍历每个实参表达式，进行翻译
            ast_node * tempExpr = ir_visit_ast_node(son);
            if (!tempExpr) {
                return false;
            }
            // TODO need fix arg
            if (tempExpr->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS &&
                tempExpr->sons.size() - 1 == // 数组访问的维度数 == 数组的维度数 => 是元素而不是地址，需要解引用
                    static_cast<ArrayType *>(ir_visit_ast_node(son->sons[0])->val->getType())->getNumDimensions()) {
                tempExpr = dereference(tempExpr);
            }
            realParams.push_back(tempExpr->val);
            node->blockInsts.addInst(tempExpr->blockInsts);

            // 为每个实参生成ARG指令
            ArgInstruction * argInst = new ArgInstruction(currentFunc, tempExpr->val);
            node->blockInsts.addInst(argInst);
        }
    }

    // 检查实参和形参数量是否一致
    if (realParams.size() != calledFunction->getParams().size()) {
        minic_log(LOG_ERROR,
                  "第%lld行的被调用函数(%s)参数个数不匹配，期望%zu个，实际%zu个",
                  (long long) lineno,
                  funcName.c_str(),
                  calledFunction->getParams().size(),
                  realParams.size());
        return false;
    }

    // 返回调用有返回值，则需要分配临时变量，用于保存函数调用的返回值
    Type * returnType = calledFunction->getReturnType();

    // 创建函数调用指令
    FuncCallInstruction * funcCallInst = new FuncCallInstruction(currentFunc, calledFunction, realParams, returnType);
    node->blockInsts.addInst(funcCallInst);

    // 如果函数有返回值(非void类型)，为返回值创建一个临时变量
    if (!returnType->isVoidType()) {
        // 创建临时变量存储返回值
        LocalVariable * returnVar = static_cast<LocalVariable *>(module->newVarValue(returnType));

        // 将函数调用结果赋给临时变量
        MoveInstruction * moveInst = new MoveInstruction(currentFunc, returnVar, funcCallInst);
        node->blockInsts.addInst(moveInst);

        // 函数调用结果Value保存到node中，方便上层节点使用
        node->val = returnVar;
    } else {
        // void函数调用没有返回值
        node->val = nullptr;
    }
    return true;
}

/// @brief 语句块（含函数体）AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_block(ast_node * node)
{
    // 进入作用域
    if (node->needScope) {
        module->enterScope();
    }

    std::vector<ast_node *>::iterator pIter;
    for (pIter = node->sons.begin(); pIter != node->sons.end(); ++pIter) {

        // 遍历Block的每个语句，进行显示或者运算
        ast_node * temp = ir_visit_ast_node(*pIter);
        if (!temp) {
            return false;
        }

        node->blockInsts.addInst(temp->blockInsts);
    }

    // 离开作用域
    if (node->needScope) {
        module->leaveScope();
    }

    return true;
}

/// @brief 整数加法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_add(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 加法节点，左结合，先计算左节点，后计算右节点

    // 加法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 加法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * addInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_ADD_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(addInst);

    node->val = addInst;

    return true;
}

/// @brief 整数减法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_sub(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 加法节点，左结合，先计算左节点，后计算右节点

    // 加法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 加法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * subInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_SUB_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(subInst);

    node->val = subInst;

    return true;
}

/// @brief 赋值AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_assign(ast_node * node)
{
    ast_node * son1_node = node->sons[0];
    ast_node * son2_node = node->sons[1];

    // 赋值节点，自右往左运算

    // 赋值运算符的左侧操作数
    ast_node * left = ir_visit_ast_node(son1_node);
    if (!left) {
        // 某个变量没有定值
        // 这里缺省设置变量不存在则创建，因此这里不会错误
        return false;
    }

    // 赋值运算符的右侧操作数
    ast_node * right = ir_visit_ast_node(son2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理
    // 获取当前函数
    Function * currentFunc = module->getCurrentFunction();

    // 添加右侧表达式的指令
    node->blockInsts.addInst(right->blockInsts);
    // 添加左侧表达式的指令
    node->blockInsts.addInst(left->blockInsts);

    // 判断左右操作数是否是内存地址（数组访问等）
    bool leftIsMem = node->sons[0]->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS;
    bool rightIsMem = node->sons[1]->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS;

    Instruction * movInst = nullptr;

    if (!leftIsMem && !rightIsMem) {
        // 情况1: 两边都是非内存值，直接赋值
        movInst = new MoveInstruction(currentFunc, left->val, right->val);

    } else if (leftIsMem && !rightIsMem) {
        // 情况2: 左侧是内存地址，右侧是非内存值，直接存储
        movInst = new MemMoveInstruction(currentFunc, IRInstOperator::IRINST_OP_STORE, left->val, right->val);
    } else if (!leftIsMem && rightIsMem) {
        // 情况3: 右侧是内存地址，左侧是非内存值，直接加载
        movInst = new MemMoveInstruction(currentFunc, IRInstOperator::IRINST_OP_LOAD, left->val, right->val);
    } else {
        // 情况4: 两边都是内存地址，需要先将右侧加载到临时变量
        PointerType * ptrType = static_cast<PointerType *>(right->val->getType());
        Type * elemType = const_cast<Type *>(ptrType->getPointeeType());
        LocalVariable * temp = static_cast<LocalVariable *>(module->newVarValue(elemType));
        movInst = new MemMoveInstruction(currentFunc, IRInstOperator::IRINST_OP_LOAD, temp, right->val);
        node->blockInsts.addInst(movInst);
        // 然后将临时变量存储到左侧内存
        movInst = new MemMoveInstruction(currentFunc, IRInstOperator::IRINST_OP_STORE, left->val, temp);
    }

    // 添加生成的指令
    node->blockInsts.addInst(movInst);

    // 这里赋值语句的结果是被赋值的表达式
    node->val = left->val;

    return true;
}

/// @brief return节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_return(ast_node * node)
{
    ast_node * right = nullptr;
    Function * currentFunc = module->getCurrentFunction();
    LocalVariable * returnValue = currentFunc->getReturnValue();

    // return语句可能没有表达式，也可能有，因此这里必须进行区分判断
    if (!node->sons.empty()) {
        // 有返回值的return语句
        right = ir_visit_ast_node(node->sons[0]);
        if (!right) {
            return false;
        }

        // 检查返回类型与函数返回类型是否匹配
        if (currentFunc->getReturnType()->isVoidType()) {
            minic_log(LOG_ERROR, "void函数不能有返回值");
            return false;
        }

        // 添加right节点的指令与赋值指令到当前节点
        node->blockInsts.addInst(right->blockInsts);

        // 如果函数返回类型不是void，则需要将返回值赋给返回值变量
        if (returnValue) {
            if (right->node_type == ast_operator_type::AST_OP_ARRAY_ACCESS) {
                MemMoveInstruction * movInst =
                    new MemMoveInstruction(currentFunc, IRInstOperator::IRINST_OP_LOAD, returnValue, right->val);
                node->blockInsts.addInst(movInst);
            } else {
                MoveInstruction * movInst = new MoveInstruction(currentFunc, returnValue, right->val);
                node->blockInsts.addInst(movInst);
            }
            // MoveInstruction * movInst = new MoveInstruction(currentFunc, returnValue, right->val);
            // node->blockInsts.addInst(movInst);
        }
    } else {
        // 空return语句，检查是否是void函数
        if (!currentFunc->getReturnType()->isVoidType()) {
            minic_log(LOG_ERROR, "非void函数需要返回值");
        }
    }

    // 跳转到函数的尾部出口指令上
    node->blockInsts.addInst(new GotoInstruction(currentFunc, currentFunc->getExitLabel()));

    return true;
}

/// @brief 类型叶子节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_type(ast_node * node)
{
    // 不需要做什么，直接从节点中获取即可。

    return true;
}

/// @brief 标识符叶子节点翻译成线性中间IR，变量声明的不走这个语句
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_var_id(ast_node * node)
{
    Value * val;

    // 查找ID型Value
    // 变量，则需要在符号表中查找对应的值

    val = module->findVarValue(node->name);

    node->val = val;

    return true;
}

/// @brief 无符号整数字面量叶子节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_leaf_node_uint(ast_node * node)
{
    ConstInt * val;

    // 新建一个整数常量Value
    val = module->newConstInt((int32_t) node->integer_val);

    node->val = val; // 线性IR的value, integer_val是AST阶段生成的

    return true;
}

/// @brief 变量声明语句节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_declare_statment(ast_node * node)
{
    bool result = false;

    for (auto & child: node->sons) {

        // 遍历每个变量声明
        result = ir_variable_declare(child);
        if (!result) {
            break;
        }
        // 添加子节点的指令块到当前节点
        node->blockInsts.addInst(child->blockInsts);
    }

    return result;
}

/// @brief 变量定声明节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_variable_declare(ast_node * node)
{
    // TODO 这里可强化类型等检查

    ast_node * type_node = node->sons[0];
    ast_node * second_node = node->sons[1];

    if (second_node->node_type == ast_operator_type::AST_OP_ARRAY_VAR) {
        // AST_OP_ARRAY_VAR节点的第一个子节点是数组名，后续子节点是各维度的大小
        ast_node * array_name_node = second_node->sons[0];
        std::vector<int32_t> dimensions;

        // 收集各维度大小
        for (size_t i = 1; i < second_node->sons.size(); i++) {
            ast_node * dim_node = second_node->sons[i];
            if (dim_node->node_type == ast_operator_type::AST_OP_LEAF_LITERAL_UINT) {
                // 将维度值添加到维度列表
                dimensions.push_back(static_cast<int32_t>(dim_node->integer_val));
            } else {
                minic_log(LOG_ERROR, "数组维度必须是整数常量");
                return false;
            }
        }

        // 创建数组类型
        // 注意：这里假设已经有一个ArrayType类，如果没有需要先实现它
        Type * elemType = type_node->type;
        Type * arrayType = new ArrayType(elemType, dimensions);

        // 创建数组变量并加入符号表
        Value * arrayVar = module->newVarValue(arrayType, array_name_node->name);
        node->val = arrayVar;

    } else if (second_node->node_type == ast_operator_type::AST_OP_ASSIGN) {
        // 从赋值节点中获取变量名节点（左侧）
        ast_node * id_node = second_node->sons[0];

        // 创建变量
        Value * var = module->newVarValue(type_node->type, id_node->name);
        node->val = var;

        // 处理赋值表达式（右侧）
        ast_node * expr_node = second_node->sons[1];
        ast_node * expr = ir_visit_ast_node(expr_node);

        // 获取当前函数
        Function * currentFunc = module->getCurrentFunction();

        // 创建并添加赋值指令
        MoveInstruction * movInst = new MoveInstruction(currentFunc, var, expr->val);

        // 将表达式的指令和赋值指令添加到当前节点
        node->blockInsts.addInst(expr->blockInsts);
        node->blockInsts.addInst(movInst);
    } else {
        // 无初始化表达式，直接创建变量
        node->val = module->newVarValue(type_node->type, second_node->name);
    }

    return true;
}

/// @brief 整数乘法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_mul(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 乘法节点，左结合，先计算左节点，后计算右节点

    // 乘法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 乘法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * mulInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_MUL_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(mulInst);

    node->val = mulInst;

    return true;
}

/// @brief 整数除法AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_div(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 除法节点，左结合，先计算左节点，后计算右节点

    // 除法的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 除法的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * divInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_DIV_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(divInst);

    node->val = divInst;

    return true;
}

/// @brief 整数取余AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_mod(ast_node * node)
{
    ast_node * src1_node = node->sons[0];
    ast_node * src2_node = node->sons[1];

    // 取余节点，左结合，先计算左节点，后计算右节点

    // 取余的左边操作数
    ast_node * left = ir_visit_ast_node(src1_node);
    if (!left) {
        // 某个变量没有定值
        return false;
    }

    // 取余的右边操作数
    ast_node * right = ir_visit_ast_node(src2_node);
    if (!right) {
        // 某个变量没有定值
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 这里只处理整型的数据，如需支持实数，则需要针对类型进行处理

    BinaryInstruction * modInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_MOD_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(modInst);

    node->val = modInst;

    return true;
}

/// @brief 整数取负AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_neg(ast_node * node)
{
    // 取负运算符是一元运算符，只有一个子节点
    ast_node * src_node = node->sons[0];

    // 取负的操作数
    ast_node * operand = ir_visit_ast_node(src_node);
    if (!operand) {
        // 操作数没有定值
        return false;
    }

    // 创建0常量，用于生成 0-operand 的减法指令
    ConstInt * zero = module->newConstInt(0);

    // 检查操作数是否是指针，如果是则解引用
    dereference(operand);
    node->blockInsts.addInst(operand->blockInsts);

    // 使用减法指令实现取负运算：0 - operand
    BinaryInstruction * negInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_SUB_I,
                                                        zero,
                                                        operand->val,
                                                        IntegerType::getTypeInt());

    // 创建临时变量保存IR的值，以及线性IR指令
    node->blockInsts.addInst(operand->blockInsts);
    node->blockInsts.addInst(negInst);

    node->val = negInst;

    return true;
}

/// @brief 大于关系运算符AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_gt(ast_node * node)
{
    ast_node * left_node = node->sons[0];
    ast_node * right_node = node->sons[1];

    // 计算左操作数
    ast_node * left = ir_visit_ast_node(left_node);
    if (!left) {
        return false;
    }

    // 计算右操作数
    ast_node * right = ir_visit_ast_node(right_node);
    if (!right) {
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 创建比较指令
    BinaryInstruction * cmpInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_GT_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeBool());

    // 将指令添加到当前节点
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(cmpInst);

    node->val = cmpInst;
    return true;
}

/// @brief 小于关系运算符AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_lt(ast_node * node)
{
    ast_node * left_node = node->sons[0];
    ast_node * right_node = node->sons[1];

    // 计算左操作数
    ast_node * left = ir_visit_ast_node(left_node);
    if (!left) {
        return false;
    }

    // 计算右操作数
    ast_node * right = ir_visit_ast_node(right_node);
    if (!right) {
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 创建比较指令
    BinaryInstruction * cmpInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_LT_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeBool());

    // 将指令添加到当前节点
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(cmpInst);

    node->val = cmpInst;
    return true;
}

/// @brief 大于等于关系运算符AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_ge(ast_node * node)
{
    ast_node * left_node = node->sons[0];
    ast_node * right_node = node->sons[1];

    // 计算左操作数
    ast_node * left = ir_visit_ast_node(left_node);
    if (!left) {
        return false;
    }

    // 计算右操作数
    ast_node * right = ir_visit_ast_node(right_node);
    if (!right) {
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 创建比较指令
    BinaryInstruction * cmpInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_GE_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeBool());

    // 将指令添加到当前节点
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(cmpInst);

    node->val = cmpInst;
    return true;
}

/// @brief 小于等于关系运算符AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_le(ast_node * node)
{
    ast_node * left_node = node->sons[0];
    ast_node * right_node = node->sons[1];

    // 计算左操作数
    ast_node * left = ir_visit_ast_node(left_node);
    if (!left) {
        return false;
    }

    // 计算右操作数
    ast_node * right = ir_visit_ast_node(right_node);
    if (!right) {
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 创建比较指令
    BinaryInstruction * cmpInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_LE_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeBool());

    // 将指令添加到当前节点
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(cmpInst);

    node->val = cmpInst;
    return true;
}

/// @brief 等于关系运算符AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_eq(ast_node * node)
{
    ast_node * left_node = node->sons[0];
    ast_node * right_node = node->sons[1];

    // 计算左操作数
    ast_node * left = ir_visit_ast_node(left_node);
    if (!left) {
        return false;
    }

    // 计算右操作数
    ast_node * right = ir_visit_ast_node(right_node);
    if (!right) {
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 创建比较指令
    BinaryInstruction * cmpInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_EQ_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeBool());

    // 将指令添加到当前节点
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(cmpInst);

    node->val = cmpInst;
    return true;
}

/// @brief 不等于关系运算符AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_ne(ast_node * node)
{
    ast_node * left_node = node->sons[0];
    ast_node * right_node = node->sons[1];

    // 计算左操作数
    ast_node * left = ir_visit_ast_node(left_node);
    if (!left) {
        return false;
    }

    // 计算右操作数
    ast_node * right = ir_visit_ast_node(right_node);
    if (!right) {
        return false;
    }

    // 检查操作数是否是指针，如果是则解引用
    dereference(left);
    node->blockInsts.addInst(left->blockInsts);
    dereference(right);
    node->blockInsts.addInst(right->blockInsts);

    // 创建比较指令
    BinaryInstruction * cmpInst = new BinaryInstruction(module->getCurrentFunction(),
                                                        IRInstOperator::IRINST_OP_NE_I,
                                                        left->val,
                                                        right->val,
                                                        IntegerType::getTypeBool());

    // 将指令添加到当前节点
    node->blockInsts.addInst(left->blockInsts);
    node->blockInsts.addInst(right->blockInsts);
    node->blockInsts.addInst(cmpInst);

    node->val = cmpInst;
    return true;
}

/// @brief 逻辑与AST节点翻译成线性中间IR (短路求值) 逻辑运算结果只有0 1 保存在 node->val
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_logical_and(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();

    // 创建标签：右操作数执行块、AND结果为假的块、AND结束块
    LabelInstruction * trueLabel = node->trueLabel;
    LabelInstruction * falseLabel = node->falseLabel;
    bool needOwnLabels = !trueLabel || !falseLabel;

    if (needOwnLabels) {
        trueLabel = new LabelInstruction(currentFunc);
        falseLabel = new LabelInstruction(currentFunc);
    }

    // 使用上层标签，只需要创建右操作数标签
    LabelInstruction * rightSideLabel = new LabelInstruction(currentFunc);

    // 计算左操作数
    ast_node * left_node = node->sons[0];
    left_node->trueLabel = rightSideLabel;
    left_node->falseLabel = falseLabel;
    ast_node * left = ir_visit_ast_node(left_node);
    if (!left) {
        return false;
    }

    // 如果left是逻辑表达式，则会自动生成跳转指令（他拿着我们的真假标签）
    node->blockInsts.addInst(left->blockInsts);
    // 如果左操作数是一个值，他只会做计算，因此我们要自己生成跳转指令
    if (left_node->node_type != ast_operator_type::AST_OP_LAND &&
        left_node->node_type != ast_operator_type::AST_OP_LOR &&
        left_node->node_type != ast_operator_type::AST_OP_LNOT) {
        BranchInstruction * leftBranch = new BranchInstruction(currentFunc,
                                                               left->val,
                                                               rightSideLabel, // 为真跳转到右操作数
                                                               falseLabel);    // 为假跳转到假标签
        node->blockInsts.addInst(leftBranch);
    }

    // 右操作数标签
    node->blockInsts.addInst(rightSideLabel);
    // 计算右操作数
    ast_node * right_node = node->sons[1];
    right_node->trueLabel = trueLabel;
    right_node->falseLabel = falseLabel;
    ast_node * right = ir_visit_ast_node(right_node);
    if (!right) {
        return false;
    }
    // 右操作数指令块
    node->blockInsts.addInst(right->blockInsts);
    if (right_node->node_type != ast_operator_type::AST_OP_LAND &&
        right_node->node_type != ast_operator_type::AST_OP_LOR &&
        right_node->node_type != ast_operator_type::AST_OP_LNOT) {
        BranchInstruction * rightBranch = new BranchInstruction(currentFunc,
                                                                right->val,
                                                                trueLabel,   // true
                                                                falseLabel); // false
        node->blockInsts.addInst(rightBranch);
    }

    // 如果需要自己的标签，生成结果变量和相关代码
    if (needOwnLabels) {
        LabelInstruction * endLabel = new LabelInstruction(currentFunc);

        // 创建结果变量 - 必须是局部变量，不能直接赋值
        LocalVariable * resultVar = static_cast<LocalVariable *>(module->newVarValue(IntegerType::getTypeInt()));

        // 创建常量
        ConstInt * zeroVal = module->newConstInt(0);
        ConstInt * oneVal = module->newConstInt(1);

        // 真结果标签
        node->blockInsts.addInst(trueLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, oneVal));
        node->blockInsts.addInst(new GotoInstruction(currentFunc, endLabel));

        // 假结果标签
        node->blockInsts.addInst(falseLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, zeroVal));

        node->blockInsts.addInst(endLabel);

        // 设置结果 - 必须是Variable，不能是其他类型
        node->val = resultVar;
    }
    return true;
}

/// @brief 逻辑或AST节点翻译成线性中间IR (短路求值)
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_logical_or(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();

    // 创建标签：右操作数执行块、AND结果为假的块、AND结束块
    LabelInstruction * trueLabel = node->trueLabel;
    LabelInstruction * falseLabel = node->falseLabel;
    bool needOwnLabels = !trueLabel || !falseLabel;

    if (needOwnLabels) {
        trueLabel = new LabelInstruction(currentFunc);
        falseLabel = new LabelInstruction(currentFunc);
    }

    // 使用上层标签，只需要创建右操作数标签
    LabelInstruction * rightSideLabel = new LabelInstruction(currentFunc);

    // 计算左操作数
    ast_node * left_node = node->sons[0];
    left_node->trueLabel = trueLabel;
    left_node->falseLabel = rightSideLabel;
    ast_node * left = ir_visit_ast_node(left_node);
    if (!left) {
        return false;
    }

    // 如果left是逻辑表达式，则会自动生成跳转指令（他拿着我们的真假标签）
    node->blockInsts.addInst(left->blockInsts);
    // 如果左操作数是一个值，他只会做计算，因此我们要自己生成跳转指令
    if (left_node->node_type != ast_operator_type::AST_OP_LAND &&
        left_node->node_type != ast_operator_type::AST_OP_LOR &&
        left_node->node_type != ast_operator_type::AST_OP_LNOT) {
        BranchInstruction * leftBranch = new BranchInstruction(currentFunc,
                                                               left->val,
                                                               trueLabel,       // 为真跳转到真标签
                                                               rightSideLabel); // 为假跳转到右部标签
        node->blockInsts.addInst(leftBranch);
    }

    // 右操作数标签
    node->blockInsts.addInst(rightSideLabel);
    // 计算右操作数
    ast_node * right_node = node->sons[1];
    right_node->trueLabel = trueLabel;
    right_node->falseLabel = falseLabel;
    ast_node * right = ir_visit_ast_node(right_node);
    if (!right) {
        return false;
    }
    // 右操作数指令块
    node->blockInsts.addInst(right->blockInsts);
    if (right_node->node_type != ast_operator_type::AST_OP_LAND &&
        right_node->node_type != ast_operator_type::AST_OP_LOR &&
        right_node->node_type != ast_operator_type::AST_OP_LNOT) {
        BranchInstruction * rightBranch = new BranchInstruction(currentFunc,
                                                                right->val,
                                                                trueLabel,   // true
                                                                falseLabel); // false
        node->blockInsts.addInst(rightBranch);
    }

    // 如果需要自己的标签，生成结果变量和相关代码
    if (needOwnLabels) {
        LabelInstruction * endLabel = new LabelInstruction(currentFunc);

        // 创建结果变量 - 必须是局部变量，不能直接赋值
        LocalVariable * resultVar = static_cast<LocalVariable *>(module->newVarValue(IntegerType::getTypeInt()));

        // 创建常量
        ConstInt * zeroVal = module->newConstInt(0);
        ConstInt * oneVal = module->newConstInt(1);

        // 真结果标签
        node->blockInsts.addInst(trueLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, oneVal));
        node->blockInsts.addInst(new GotoInstruction(currentFunc, endLabel));

        // 假结果标签
        node->blockInsts.addInst(falseLabel);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, zeroVal));

        node->blockInsts.addInst(endLabel);

        // 设置结果 - 必须是Variable，不能是其他类型
        node->val = resultVar;
    }
    return true;
}

/// @brief 逻辑非AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_logical_not(ast_node * node)
{
    // 检查是否有上层提供的真假标签
    LabelInstruction * trueLabel = node->trueLabel;
    LabelInstruction * falseLabel = node->falseLabel;

    // 计算操作数
    ast_node * operand_node = node->sons[0];

    if (trueLabel && falseLabel) {
        // 如果有上层标签，实现逻辑非只需要调换真假标签
        operand_node->trueLabel = falseLabel; // 操作数为真时跳到假标签
        operand_node->falseLabel = trueLabel; // 操作数为假时跳到真标签

        ast_node * operand = ir_visit_ast_node(operand_node);
        if (!operand) {
            return false;
        }

        // 添加操作数的指令块
        node->blockInsts.addInst(operand->blockInsts);

        // 如果操作数不是逻辑表达式，需要创建分支指令，颠倒标签实现逻辑非
        if (operand_node->node_type != ast_operator_type::AST_OP_LAND &&
            operand_node->node_type != ast_operator_type::AST_OP_LOR &&
            operand_node->node_type != ast_operator_type::AST_OP_LNOT) {
            BranchInstruction * branch = new BranchInstruction(module->getCurrentFunction(),
                                                               operand->val,
                                                               falseLabel, // 如果为真，跳到假标签
                                                               trueLabel); // 如果为假，跳到真标签
            node->blockInsts.addInst(branch);
        }
    } else {
        // 没有上层标签，需要创建自己的标签和结果变量
        Function * currentFunc = module->getCurrentFunction();
        LabelInstruction * trueLabel_not = new LabelInstruction(currentFunc);
        LabelInstruction * falseLabel_not = new LabelInstruction(currentFunc);
        LabelInstruction * endLabel_not = new LabelInstruction(currentFunc);

        // 结果变量
        LocalVariable * resultVar = static_cast<LocalVariable *>(module->newVarValue(IntegerType::getTypeInt()));

        // 创建常量
        ConstInt * zeroVal = module->newConstInt(0);
        ConstInt * oneVal = module->newConstInt(1);

        // 检查操作数是否为逻辑表达式
        if (operand_node->node_type == ast_operator_type::AST_OP_LAND ||
            operand_node->node_type == ast_operator_type::AST_OP_LOR ||
            operand_node->node_type == ast_operator_type::AST_OP_LNOT) {

            // 操作数是逻辑表达式，传递调换后的真假标签
            operand_node->trueLabel = falseLabel_not; // 操作数为真时跳到假标签
            operand_node->falseLabel = trueLabel_not; // 操作数为假时跳到真标签

            ast_node * operand = ir_visit_ast_node(operand_node);
            if (!operand) {
                return false;
            }

            // 添加操作数的指令块（包含跳转指令）
            node->blockInsts.addInst(operand->blockInsts);

        } else {
            // 操作数不是逻辑表达式，需要自己生成跳转指令
            ast_node * operand = ir_visit_ast_node(operand_node);
            if (!operand) {
                return false;
            }

            // 添加操作数的指令块
            node->blockInsts.addInst(operand->blockInsts);

            // 创建分支指令，颠倒标签实现逻辑非
            BranchInstruction * branch = new BranchInstruction(currentFunc,
                                                               operand->val,
                                                               falseLabel_not, // 如果为真，跳到假标签
                                                               trueLabel_not); // 如果为假，跳到真标签
            node->blockInsts.addInst(branch);
        }

        // 此时程序的控制流应该按照代码逻辑，跳转到真假标签
        // 真标签 - 设置为1
        node->blockInsts.addInst(trueLabel_not);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, oneVal));
        node->blockInsts.addInst(new GotoInstruction(currentFunc, endLabel_not));

        // 假标签 - 设置为0
        node->blockInsts.addInst(falseLabel_not);
        node->blockInsts.addInst(new MoveInstruction(currentFunc, resultVar, zeroVal));

        // 结束标签
        node->blockInsts.addInst(endLabel_not);

        // 设置结果
        node->val = resultVar;
    }

    return true;
}

/// @brief if语句AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_if(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();

    // 创建if语句的标签：then块、else块(如果有)、if结束块
    LabelInstruction * thenLabel = new LabelInstruction(currentFunc);
    LabelInstruction * elseLabel = new LabelInstruction(currentFunc);
    LabelInstruction * endLabel = new LabelInstruction(currentFunc);

    // 计算条件表达式 (待实现对于值的if跳转)
    ast_node * cond_node = node->sons[0];
    cond_node->trueLabel = thenLabel;  // 条件为真跳转到then块
    cond_node->falseLabel = elseLabel; // 条件为假跳转到else块
    ast_node * cond = ir_visit_ast_node(cond_node);
    if (!cond) {
        return false;
    }

    // 计算then块
    ast_node * then_node = node->sons[1];
    ast_node * then_block = ir_visit_ast_node(then_node);
    if (!then_block) {
        return false;
    }

    // 判断是否有else块
    bool has_else = node->sons.size() > 2;
    ast_node * else_block = nullptr;

    // 计算else块
    if (has_else) {
        ast_node * else_node = node->sons[2];
        else_block = ir_visit_ast_node(else_node);
        if (!else_block) {
            return false;
        }
    }

    // 组装指令块
    node->blockInsts.addInst(cond->blockInsts);

    // 注意，如果if的cond_node不是逻辑表达式，我们需要利用他的值来进行跳转
    // 如果是逻辑表达式，在他们自身的处理逻辑中就会创建好跳转指令
    if (cond_node->node_type != ast_operator_type::AST_OP_LAND &&
        cond_node->node_type != ast_operator_type::AST_OP_LOR &&
        cond_node->node_type != ast_operator_type::AST_OP_LNOT) {
        // 条件判断：如果条件为真，跳转到then块；否则跳转到else块
        BranchInstruction * branch = new BranchInstruction(currentFunc,
                                                           cond->val,
                                                           thenLabel,  // true
                                                           elseLabel); // false
        node->blockInsts.addInst(branch);
    }

    // then块
    node->blockInsts.addInst(thenLabel);
    node->blockInsts.addInst(then_block->blockInsts);
    node->blockInsts.addInst(new GotoInstruction(currentFunc, endLabel));

    // else块
    node->blockInsts.addInst(elseLabel);
    if (has_else) {
        node->blockInsts.addInst(else_block->blockInsts);
    }

    // 结束标签
    node->blockInsts.addInst(endLabel);

    return true;
}

/// @brief while语句AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_while(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();

    // 创建while循环的标签：条件块、循环体块、循环结束块
    LabelInstruction * condLabel = new LabelInstruction(currentFunc);
    LabelInstruction * bodyLabel = new LabelInstruction(currentFunc);
    LabelInstruction * endLabel = new LabelInstruction(currentFunc);

    // 保存循环的开始和结束标签，用于break和continue语句
    LabelInstruction * oldContinueTarget = currentFunc->getContinueTarget();
    LabelInstruction * oldBreakTarget = currentFunc->getBreakTarget();

    // 设置当前循环的标签
    currentFunc->setContinueTarget(condLabel); // continue跳转到条件判断
    currentFunc->setBreakTarget(endLabel);     // break跳转到循环结束

    // 条件判断块
    node->blockInsts.addInst(condLabel);

    // 计算条件表达式，传递标签用于逻辑表达式的短路求值
    ast_node * cond_node = node->sons[0];
    cond_node->trueLabel = bodyLabel; // 条件为真跳转到循环体
    cond_node->falseLabel = endLabel; // 条件为假跳转到循环结束
    ast_node * cond = ir_visit_ast_node(cond_node);
    if (!cond) {
        return false;
    }

    node->blockInsts.addInst(cond->blockInsts);

    // 注意，如果while的cond_node不是逻辑表达式，我们需要利用他的值来进行跳转
    // 如果是逻辑表达式，在他们自身的处理逻辑中就会创建好跳转指令
    if (cond_node->node_type != ast_operator_type::AST_OP_LAND &&
        cond_node->node_type != ast_operator_type::AST_OP_LOR &&
        cond_node->node_type != ast_operator_type::AST_OP_LNOT) {
        // 条件判断：如果条件为真，跳转到循环体；否则跳转到循环结束
        BranchInstruction * branch = new BranchInstruction(currentFunc,
                                                           cond->val,
                                                           bodyLabel, // true
                                                           endLabel); // false
        node->blockInsts.addInst(branch);
    }

    // 循环体
    node->blockInsts.addInst(bodyLabel);
    ast_node * body_node = node->sons[1];
    ast_node * body = ir_visit_ast_node(body_node);
    if (!body) {
        return false;
    }
    node->blockInsts.addInst(body->blockInsts);

    // 循环体结束后跳回条件判断
    node->blockInsts.addInst(new GotoInstruction(currentFunc, condLabel));

    // 循环结束标签
    node->blockInsts.addInst(endLabel);

    // 恢复之前的break和continue标签
    currentFunc->setContinueTarget(oldContinueTarget);
    currentFunc->setBreakTarget(oldBreakTarget);

    return true;
}

/// @brief break语句AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_break(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();

    // 获取当前循环的break目标标签
    LabelInstruction * breakTarget = currentFunc->getBreakTarget();

    if (!breakTarget) {
        minic_log(LOG_ERROR, "break语句必须在循环内部使用");
        return false;
    }

    // 创建跳转到break目标的指令
    node->blockInsts.addInst(new GotoInstruction(currentFunc, breakTarget));

    return true;
}

/// @brief continue语句AST节点翻译成线性中间IR
/// @param node AST节点
/// @return 翻译是否成功，true：成功，false：失败
bool IRGenerator::ir_continue(ast_node * node)
{
    Function * currentFunc = module->getCurrentFunction();

    // 获取当前循环的continue目标标签
    LabelInstruction * continueTarget = currentFunc->getContinueTarget();

    if (!continueTarget) {
        minic_log(LOG_ERROR, "continue语句必须在循环内部使用");
        return false;
    }

    // 创建跳转到continue目标的指令
    node->blockInsts.addInst(new GotoInstruction(currentFunc, continueTarget));

    return true;
}

bool IRGenerator::ir_array_var(ast_node * node)
{
    return true;
}

bool IRGenerator::ir_array_access(ast_node * node)
{
    // 获取当前函数
    Function * currentFunc = module->getCurrentFunction();

    // 数组访问节点的第一个子节点是数组名，后面的子节点是索引表达式
    if (node->sons.size() < 2) {
        minic_log(LOG_ERROR, "数组访问语法错误：缺少索引");
        return false;
    }

    // 获取数组变量
    ast_node * array_node = ir_visit_ast_node(node->sons[0]);
    if (!array_node || !array_node->val) {
        minic_log(LOG_ERROR, "数组变量未定义或不可访问");
        return false;
    }

    // 检查是否是数组类型
    if (!array_node->val->getType()->isArrayType()) {
        minic_log(LOG_ERROR, "变量不是数组类型，无法进行索引访问");
        return false;
    }

    // 添加数组变量的指令到当前节点
    node->blockInsts.addInst(array_node->blockInsts);

    // 获取数组类型
    ArrayType * arrayType = static_cast<ArrayType *>(array_node->val->getType());
    std::vector<int32_t> dimensions = arrayType->getDimensions();

    // 检查索引维度是否正确
    if (node->sons.size() - 1 > dimensions.size()) {
        minic_log(LOG_ERROR, "数组维度访问错误：索引数量过多");
        return false;
    }

    // 第一步：计算每个维度的基础偏移量
    std::vector<int32_t> dimOffset;
    int32_t elemSize = arrayType->getElementType()->getSize(); // 此处已经开始计算int 4字节大小了
    int32_t totalSize = elemSize;

    // 从最内层维度开始计算每个维度的大小, 得到的 dimOffset 也是第一个元素是最高维度大小
    for (int i = dimensions.size() - 1; i >= 0; i--) {
        dimOffset.insert(dimOffset.begin(), totalSize);
        totalSize *= dimensions[i];
    }

    ConstInt * zero = module->newConstInt(0);
    // 第二步：计算每个索引的偏移量并累加
    Value * currentOffset = zero; // 初始为零的偏移量
    for (size_t i = 1; i < node->sons.size(); i++) {
        // 索引从1开始，因为0是数组名
        size_t access_offset = dimOffset.size() - (node->sons.size() - 1); // 源数组维度数 - 需要访问的维度数
        size_t dimOffsetIdx = dimOffset.size() - i - access_offset;        // 倒序访问 + 和 arrayIdx 左对齐
        size_t arrayIdx = node->sons.size() - i;                           // 倒序访问

        // 处理索引表达式
        ast_node * index_expr = ir_visit_ast_node(node->sons[arrayIdx]);
        // 检测 index_expr 是否是 array_access节点
        index_expr = dereference(index_expr);
        if (!index_expr) {
            minic_log(LOG_ERROR, "数组索引表达式错误");
            return false;
        }

        // 添加索引表达式的指令
        node->blockInsts.addInst(index_expr->blockInsts);

        // 获取当前维度的大小因子
        ConstInt * dimSizeFactor = module->newConstInt(dimOffset[dimOffsetIdx]);

        // 计算: index * dimSize
        BinaryInstruction * mulInst = new BinaryInstruction(currentFunc,
                                                            IRInstOperator::IRINST_OP_MUL_I,
                                                            index_expr->val,
                                                            dimSizeFactor,
                                                            IntegerType::getTypeInt());
        node->blockInsts.addInst(mulInst);

        // 累加偏移量: currentOffset += index * dimSize
        BinaryInstruction * addInst = new BinaryInstruction(currentFunc,
                                                            IRInstOperator::IRINST_OP_ADD_I,
                                                            currentOffset,
                                                            mulInst,
                                                            IntegerType::getTypeInt());
        node->blockInsts.addInst(addInst);

        // 更新当前偏移量值，用于下一次迭代
        currentOffset = addInst;
    }
    // 循环结束后，一次性将最终的偏移量赋值给offsetVar
    Value * offsetVar = currentOffset;

    // 第三步：计算数组元素的最终地址

    // 计算: arrayBaseAddr + offset
    Type * ptrType = new PointerType(arrayType->getElementType());
    BinaryInstruction * addrInst =
        new BinaryInstruction(currentFunc, IRInstOperator::IRINST_OP_ADD_I, array_node->val, offsetVar, ptrType);
    node->blockInsts.addInst(addrInst);
    node->val = addrInst;

    return true;
}

ast_node * IRGenerator::dereference(ast_node * node)
{
    // 默认指针都是一维
    Value * ptr = node->val;
    if (ptr->getType()->isPointerType()) {
        PointerType * ptrType = static_cast<PointerType *>(ptr->getType());
        Type * elementType = const_cast<Type *>(ptrType->getPointeeType());

        Function * currentFunc = module->getCurrentFunction();
        LocalVariable * tmpVar = static_cast<LocalVariable *>(module->newVarValue(elementType));

        MemMoveInstruction * loadInst =
            new MemMoveInstruction(currentFunc, IRInstOperator::IRINST_OP_LOAD, tmpVar, ptr);
        node->blockInsts.addInst(loadInst);
        node->val = loadInst->getOperand(0);

        return node;
    }
    return node;
}