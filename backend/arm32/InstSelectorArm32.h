﻿///
/// @file InstSelectorArm32.h
/// @brief 指令选择器-ARM32
/// @author zenglj (zenglj@live.com)
/// @version 1.0
/// @date 2024-11-21
///
/// @copyright Copyright (c) 2024
///
/// @par 修改日志:
/// <table>
/// <tr><th>Date       <th>Version <th>Author  <th>Description
/// <tr><td>2024-11-21 <td>1.0     <td>zenglj  <td>新做
/// </table>
///
#pragma once

#include <map>
#include <vector>

#include "Function.h"
#include "ILocArm32.h"
#include "Instruction.h"
#include "PlatformArm32.h"
#include "SimpleRegisterAllocator.h"
#include "RegVariable.h"

using namespace std;

/// @brief 指令选择器-ARM32
class InstSelectorArm32 {

    /// @brief 所有的IR指令
    std::vector<Instruction *> & ir;

    /// @brief 指令变换
    ILocArm32 & iloc;

    /// @brief 要处理的函数
    Function * func;

protected:
    /// @brief 指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate(Instruction * inst);

    /// @brief NOP翻译成ARM32汇编
    /// @param inst IR指令
    void translate_nop(Instruction * inst);

    /// @brief 函数入口指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate_entry(Instruction * inst);

    /// @brief 函数出口指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate_exit(Instruction * inst);

    /// @brief 赋值指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate_assign(Instruction * inst);

    /// @brief Label指令指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate_label(Instruction * inst);

    /// @brief goto指令指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate_goto(Instruction * inst);

    /// @brief 整数加法指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate_add_int32(Instruction * inst);

    /// @brief 整数减法指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate_sub_int32(Instruction * inst);

    /// @brief 二元操作指令翻译成ARM32汇编
    /// @param inst IR指令
    /// @param operator_name 操作码
    void translate_two_operator(Instruction * inst, string operator_name);

    /// @brief 函数调用指令翻译成ARM32汇编
    /// @param inst IR指令
    void translate_call(Instruction * inst);

    ///
    /// @brief 实参指令翻译成ARM32汇编
    /// @param inst
    ///
    void translate_arg(Instruction * inst);

    /*新增指令*/
    void translate_mul_int32(Instruction * inst);
    void translate_div_int32(Instruction * inst);
    void translate_mod_int32(Instruction * inst);

    // 条件分支指令
    void translate_branch(Instruction * inst);

    // 条件运算指令
    void translate_cmp_int32(Instruction * inst, const string & cond);
    void translate_gt_int32(Instruction * inst);
    void translate_lt_int32(Instruction * inst);
    void translate_ge_int32(Instruction * inst);
    void translate_le_int32(Instruction * inst);
    void translate_eq_int32(Instruction * inst);
    void translate_ne_int32(Instruction * inst);

    // 内存指令处理
    void translate_load(Instruction * inst);
    void translate_store(Instruction * inst);

    // 辅助函数 - 生成条件判断代码
    void gen_condition_code(Instruction * inst, const string & cond);

    ///
    /// @brief 输出IR指令
    ///
    void outputIRInstruction(Instruction * inst);

    /// @brief IR翻译动作函数原型
    typedef void (InstSelectorArm32::*translate_handler)(Instruction *);

    /// @brief IR动作处理函数清单
    map<IRInstOperator, translate_handler> translator_handlers;

    ///
    /// @brief 简单的朴素寄存器分配方法
    ///
    SimpleRegisterAllocator & simpleRegisterAllocator;

    ///
    /// @brief 函数实参累计
    ///
    int32_t argCount = 0;

    /// @brief 累计的实参个数
    int32_t realArgCount = 0;

    ///
    /// @brief 显示IR指令内容
    ///
    bool showLinearIR = false;

public:
    /// @brief 构造函数
    /// @param _irCode IR指令
    /// @param _func 函数
    /// @param _iloc 后端指令
    InstSelectorArm32(std::vector<Instruction *> & _irCode,
                      ILocArm32 & _iloc,
                      Function * _func,
                      SimpleRegisterAllocator & allocator);

    ///
    /// @brief 析构函数
    ///
    ~InstSelectorArm32();

    ///
    /// @brief 设置是否输出线性IR的内容
    /// @param show true显示，false显示
    ///
    void setShowLinearIR(bool show)
    {
        showLinearIR = show;
    }

    /// @brief 指令选择
    void run();
};
