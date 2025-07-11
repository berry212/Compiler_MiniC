﻿///
/// @file InstSelectorArm32.cpp
/// @brief 指令选择器-ARM32的实现
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
#include <cstdio>

#include "BranchInstruction.h"
#include "Common.h"
#include "ILocArm32.h"
#include "InstSelectorArm32.h"
#include "PlatformArm32.h"

#include "PointerType.h"
#include "RegVariable.h"
#include "Function.h"

#include "LabelInstruction.h"
#include "GotoInstruction.h"
#include "FuncCallInstruction.h"
#include "MoveInstruction.h"

/// @brief 构造函数
/// @param _irCode 指令
/// @param _iloc ILoc
/// @param _func 函数
InstSelectorArm32::InstSelectorArm32(vector<Instruction *> & _irCode,
                                     ILocArm32 & _iloc,
                                     Function * _func,
                                     SimpleRegisterAllocator & allocator)
    : ir(_irCode), iloc(_iloc), func(_func), simpleRegisterAllocator(allocator)
{
    translator_handlers[IRInstOperator::IRINST_OP_ENTRY] = &InstSelectorArm32::translate_entry;
    translator_handlers[IRInstOperator::IRINST_OP_EXIT] = &InstSelectorArm32::translate_exit;

    translator_handlers[IRInstOperator::IRINST_OP_LABEL] = &InstSelectorArm32::translate_label;
    translator_handlers[IRInstOperator::IRINST_OP_GOTO] = &InstSelectorArm32::translate_goto;

    translator_handlers[IRInstOperator::IRINST_OP_ASSIGN] = &InstSelectorArm32::translate_assign;

    translator_handlers[IRInstOperator::IRINST_OP_ADD_I] = &InstSelectorArm32::translate_add_int32;
    translator_handlers[IRInstOperator::IRINST_OP_SUB_I] = &InstSelectorArm32::translate_sub_int32;

    translator_handlers[IRInstOperator::IRINST_OP_FUNC_CALL] = &InstSelectorArm32::translate_call;
    translator_handlers[IRInstOperator::IRINST_OP_ARG] = &InstSelectorArm32::translate_arg;

    /*新增指令*/
    translator_handlers[IRInstOperator::IRINST_OP_MUL_I] = &InstSelectorArm32::translate_mul_int32;
    translator_handlers[IRInstOperator::IRINST_OP_DIV_I] = &InstSelectorArm32::translate_div_int32;
    translator_handlers[IRInstOperator::IRINST_OP_MOD_I] = &InstSelectorArm32::translate_mod_int32;

    // 添加条件分支指令处理器
    translator_handlers[IRInstOperator::IRINST_OP_BR] = &InstSelectorArm32::translate_branch;

    // 添加关系运算符处理器
    translator_handlers[IRInstOperator::IRINST_OP_GT_I] = &InstSelectorArm32::translate_gt_int32;
    translator_handlers[IRInstOperator::IRINST_OP_LT_I] = &InstSelectorArm32::translate_lt_int32;
    translator_handlers[IRInstOperator::IRINST_OP_GE_I] = &InstSelectorArm32::translate_ge_int32;
    translator_handlers[IRInstOperator::IRINST_OP_LE_I] = &InstSelectorArm32::translate_le_int32;
    translator_handlers[IRInstOperator::IRINST_OP_EQ_I] = &InstSelectorArm32::translate_eq_int32;
    translator_handlers[IRInstOperator::IRINST_OP_NE_I] = &InstSelectorArm32::translate_ne_int32;

    // 内存访问
    translator_handlers[IRInstOperator::IRINST_OP_LOAD] = &InstSelectorArm32::translate_load;
    translator_handlers[IRInstOperator::IRINST_OP_STORE] = &InstSelectorArm32::translate_store;
}

///
/// @brief 析构函数
///
InstSelectorArm32::~InstSelectorArm32()
{}

/// @brief 指令选择执行
void InstSelectorArm32::run()
{
    for (auto inst: ir) {

        // 逐个指令进行翻译
        if (!inst->isDead()) {
            translate(inst);
        }
    }
}

/// @brief 指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate(Instruction * inst)
{
    // 操作符
    IRInstOperator op = inst->getOp();

    map<IRInstOperator, translate_handler>::const_iterator pIter;
    pIter = translator_handlers.find(op);
    if (pIter == translator_handlers.end()) {
        // 没有找到，则说明当前不支持
        printf("Translate: Operator(%d) not support", (int) op);
        return;
    }

    // 开启时输出IR指令作为注释
    if (showLinearIR) {
        outputIRInstruction(inst);
    }

    (this->*(pIter->second))(inst);
}

///
/// @brief 输出IR指令
///
void InstSelectorArm32::outputIRInstruction(Instruction * inst)
{
    std::string irStr;
    inst->toString(irStr);
    if (!irStr.empty()) {
        iloc.comment(irStr);
    }
}

/// @brief NOP翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_nop(Instruction * inst)
{
    (void) inst;
    iloc.nop();
}

/// @brief Label指令指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_label(Instruction * inst)
{
    Instanceof(labelInst, LabelInstruction *, inst);

    iloc.label(labelInst->getName());
}

/// @brief goto指令指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_goto(Instruction * inst)
{
    Instanceof(gotoInst, GotoInstruction *, inst);

    // 无条件跳转
    iloc.jump(gotoInst->getTarget()->getName());
}

/// @brief 函数入口指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_entry(Instruction * inst)
{
    // 查看保护的寄存器
    auto & protectedRegNo = func->getProtectedReg();
    auto & protectedRegStr = func->getProtectedRegStr();

    bool first = true;
    for (auto regno: protectedRegNo) {
        if (first) {
            protectedRegStr = PlatformArm32::regName[regno];
            first = false;
        } else {
            protectedRegStr += "," + PlatformArm32::regName[regno];
        }
    }

    if (!protectedRegStr.empty()) {
        iloc.inst("push", "{" + protectedRegStr + "}");
    }

    // 为fun分配栈帧，含局部变量、函数调用值传递的空间等
    iloc.allocStack(func, ARM32_TMP_REG_NO);
}

/// @brief 函数出口指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_exit(Instruction * inst)
{
    if (inst->getOperandsNum()) {
        // 存在返回值
        Value * retVal = inst->getOperand(0);

        // 赋值给寄存器R0
        iloc.load_var(0, retVal);
    }

    // 恢复栈空间
    iloc.inst("mov", "sp", "fp");

    // 保护寄存器的恢复
    auto & protectedRegStr = func->getProtectedRegStr();
    if (!protectedRegStr.empty()) {
        iloc.inst("pop", "{" + protectedRegStr + "}");
    }

    iloc.inst("bx", "lr");
}

/// @brief 赋值指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_assign(Instruction * inst)
{
    Value * result = inst->getOperand(0);
    Value * arg1 = inst->getOperand(1);

    int32_t arg1_regId = arg1->getRegId();
    int32_t result_regId = result->getRegId();

    if (arg1_regId != -1) {
        // 寄存器 => 内存
        // 寄存器 => 寄存器

        // r8 -> rs 可能用到r9
        iloc.store_var(arg1_regId, result, ARM32_TMP_REG_NO);
    } else if (result_regId != -1) {
        // 内存变量 => 寄存器

        iloc.load_var(result_regId, arg1);
    } else {
        // 内存变量 => 内存变量

        int32_t temp_regno = simpleRegisterAllocator.Allocate();

        // arg1 -> r8
        iloc.load_var(temp_regno, arg1);

        // r8 -> rs 可能用到r9
        iloc.store_var(temp_regno, result, ARM32_TMP_REG_NO);

        simpleRegisterAllocator.free(temp_regno);
    }
}

/// @brief 二元操作指令翻译成ARM32汇编
/// @param inst IR指令
/// @param operator_name 操作码
/// @param rs_reg_no 结果寄存器号
/// @param op1_reg_no 源操作数1寄存器号
/// @param op2_reg_no 源操作数2寄存器号
void InstSelectorArm32::translate_two_operator(Instruction * inst, string operator_name)
{
    Value * result = inst;
    Value * arg1 = inst->getOperand(0);
    Value * arg2 = inst->getOperand(1);

    int32_t arg1_reg_no = arg1->getRegId();
    int32_t arg2_reg_no = arg2->getRegId();
    int32_t result_reg_no = inst->getRegId();
    int32_t load_result_reg_no, load_arg1_reg_no, load_arg2_reg_no;

    // 看arg1是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg1_reg_no == -1) {

        // 分配一个寄存器r8
        load_arg1_reg_no = simpleRegisterAllocator.Allocate(arg1);

        // arg1 -> r8，这里可能由于偏移不满足指令的要求，需要额外分配寄存器
        iloc.load_var(load_arg1_reg_no, arg1);
    } else {
        load_arg1_reg_no = arg1_reg_no;
    }

    // 看arg2是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg2_reg_no == -1) {

        // 分配一个寄存器r9
        load_arg2_reg_no = simpleRegisterAllocator.Allocate(arg2);

        // arg2 -> r9
        iloc.load_var(load_arg2_reg_no, arg2);
    } else {
        load_arg2_reg_no = arg2_reg_no;
    }

    // 看结果变量是否是寄存器，若不是则需要分配一个新的寄存器来保存运算的结果
    if (result_reg_no == -1) {
        // 分配一个寄存器r10，用于暂存结果
        load_result_reg_no = simpleRegisterAllocator.Allocate(result);
    } else {
        load_result_reg_no = result_reg_no;
    }

    // r8 + r9 -> r10
    iloc.inst(operator_name,
              PlatformArm32::regName[load_result_reg_no],
              PlatformArm32::regName[load_arg1_reg_no],
              PlatformArm32::regName[load_arg2_reg_no]);

    // 结果不是寄存器，则需要把rs_reg_name保存到结果变量中
    if (result_reg_no == -1) {

        // 这里使用预留的临时寄存器，因为立即数可能过大，必须借助寄存器才可操作。

        // r10 -> result
        iloc.store_var(load_result_reg_no, result, ARM32_TMP_REG_NO);
    }

    // 释放寄存器
    simpleRegisterAllocator.free(arg1);
    simpleRegisterAllocator.free(arg2);
    simpleRegisterAllocator.free(result);
}

/// @brief 整数加法指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_add_int32(Instruction * inst)
{
    translate_two_operator(inst, "add");
}

/// @brief 整数减法指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_sub_int32(Instruction * inst)
{
    translate_two_operator(inst, "sub");
}

/// @brief 函数调用指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_call(Instruction * inst)
{
    FuncCallInstruction * callInst = dynamic_cast<FuncCallInstruction *>(inst);

    int32_t operandNum = callInst->getOperandsNum();

    if (operandNum != realArgCount) {

        // 两者不一致 也可能没有ARG指令，正常
        if (realArgCount != 0) {

            minic_log(LOG_ERROR, "ARG指令的个数与调用函数个数不一致");
        }
    }

    if (operandNum) {

        // 强制占用这几个寄存器参数传递的寄存器
        simpleRegisterAllocator.Allocate(0);
        simpleRegisterAllocator.Allocate(1);
        simpleRegisterAllocator.Allocate(2);
        simpleRegisterAllocator.Allocate(3);

        // // 前四个的后面参数采用栈传递
        // int esp = 0;
        // for (int32_t k = 4; k < operandNum; k++) {

        //     auto arg = callInst->getOperand(k);

        //     // 新建一个内存变量，用于栈传值到形参变量中
        //     MemVariable * newVal = func->newMemVariable((Type *) PointerType::get(arg->getType()));
        //     newVal->setMemoryAddr(ARM32_SP_REG_NO, esp);
        //     esp += 4;

        //     Instruction * assignInst = new MoveInstruction(func, newVal, arg);

        //     // 翻译赋值指令
        //     translate_assign(assignInst);

        //     delete assignInst;
        // }

        // for (int32_t k = 0; k < operandNum && k < 4; k++) {

        //     auto arg = callInst->getOperand(k);

        //     // 检查实参的类型是否是临时变量。
        //     // 如果是临时变量，该变量可更改为寄存器变量即可，或者设置寄存器号
        //     // 如果不是，则必须开辟一个寄存器变量，然后赋值即可

        //     Instruction * assignInst = new MoveInstruction(func, PlatformArm32::intRegVal[k], arg);

        //     // 翻译赋值指令
        //     translate_assign(assignInst);

        //     delete assignInst;
        // }
    }

    iloc.call_fun(callInst->getName());

    if (operandNum) {
        simpleRegisterAllocator.free(0);
        simpleRegisterAllocator.free(1);
        simpleRegisterAllocator.free(2);
        simpleRegisterAllocator.free(3);
    }

    // // 赋值指令
    // if (callInst->hasResultValue()) {

    //     // 新建一个赋值操作
    //     Instruction * assignInst = new MoveInstruction(func, callInst, PlatformArm32::intRegVal[0]);

    //     // 翻译赋值指令
    //     translate_assign(assignInst);

    //     delete assignInst;
    // }

    // 函数调用后清零，使得下次可正常统计
    realArgCount = 0;
}

///
/// @brief 实参指令翻译成ARM32汇编
/// @param inst
///
void InstSelectorArm32::translate_arg(Instruction * inst)
{
    // // 翻译之前必须确保源操作数要么是寄存器，要么是内存，否则出错。
    // Value * src = inst->getOperand(0);

    // // 当前统计的ARG指令个数
    // int32_t regId = src->getRegId();

    // if (realArgCount < 4) {
    //     // 前四个参数
    //     if (regId != -1) {
    //         if (regId != realArgCount) {
    //             // 肯定寄存器分配有误
    //             minic_log(LOG_ERROR, "第%d个ARG指令对象寄存器分配有误: %d", argCount + 1, regId);
    //         }
    //     } else {
    //         minic_log(LOG_ERROR, "第%d个ARG指令对象不是寄存器", argCount + 1);
    //     }
    // } else {
    //     // 必须是内存分配，若不是则出错
    //     int32_t baseRegId;
    //     bool result = src->getMemoryAddr(&baseRegId);
    //     if ((!result) || (baseRegId != ARM32_SP_REG_NO)) {

    //         minic_log(LOG_ERROR, "第%d个ARG指令对象不是SP寄存器寻址", argCount + 1);
    //     }
    // }

    // realArgCount++;
}

/// @brief 整数乘法指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_mul_int32(Instruction * inst)
{
    Value * result = inst;
    Value * arg1 = inst->getOperand(0);
    Value * arg2 = inst->getOperand(1);

    int32_t arg1_reg_no = arg1->getRegId();
    int32_t arg2_reg_no = arg2->getRegId();
    int32_t result_reg_no = inst->getRegId();
    int32_t load_result_reg_no, load_arg1_reg_no, load_arg2_reg_no;

    // 看arg1是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg1_reg_no == -1) {
        // 分配一个寄存器r8
        load_arg1_reg_no = simpleRegisterAllocator.Allocate(arg1);

        // arg1 -> r8，这里可能由于偏移不满足指令的要求，需要额外分配寄存器
        iloc.load_var(load_arg1_reg_no, arg1);
    } else {
        load_arg1_reg_no = arg1_reg_no;
    }

    // 看arg2是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg2_reg_no == -1) {
        // 分配一个寄存器r9
        load_arg2_reg_no = simpleRegisterAllocator.Allocate(arg2);

        // arg2 -> r9
        iloc.load_var(load_arg2_reg_no, arg2);
    } else {
        load_arg2_reg_no = arg2_reg_no;
    }

    // 看结果变量是否是寄存器，若不是则需要分配一个新的寄存器来保存运算的结果
    if (result_reg_no == -1) {
        // 分配一个寄存器r10，用于暂存结果
        load_result_reg_no = simpleRegisterAllocator.Allocate(result);
    } else {
        load_result_reg_no = result_reg_no;
    }

    // 执行乘法操作 r8 * r9 -> r10
    iloc.mul(load_result_reg_no, load_arg1_reg_no, load_arg2_reg_no);

    // 结果不是寄存器，则需要把临时结果保存到结果变量中
    if (result_reg_no == -1) {
        // r10 -> result
        iloc.store_var(load_result_reg_no, result, ARM32_TMP_REG_NO);
    }

    // 释放寄存器
    simpleRegisterAllocator.free(arg1);
    simpleRegisterAllocator.free(arg2);
    simpleRegisterAllocator.free(result);
}

/*新增指令处理函数*/
/// @brief 整数除法指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_div_int32(Instruction * inst)
{
    Value * result = inst;
    Value * arg1 = inst->getOperand(0);
    Value * arg2 = inst->getOperand(1);

    int32_t arg1_reg_no = arg1->getRegId();
    int32_t arg2_reg_no = arg2->getRegId();
    int32_t result_reg_no = inst->getRegId();
    int32_t load_result_reg_no, load_arg1_reg_no, load_arg2_reg_no;

    // 看arg1是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg1_reg_no == -1) {
        // 分配一个寄存器
        load_arg1_reg_no = simpleRegisterAllocator.Allocate(arg1);

        // arg1 -> 寄存器
        iloc.load_var(load_arg1_reg_no, arg1);
    } else {
        load_arg1_reg_no = arg1_reg_no;
    }

    // 看arg2是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg2_reg_no == -1) {
        // 分配一个寄存器
        load_arg2_reg_no = simpleRegisterAllocator.Allocate(arg2);

        // arg2 -> 寄存器
        iloc.load_var(load_arg2_reg_no, arg2);
    } else {
        load_arg2_reg_no = arg2_reg_no;
    }

    // 看结果变量是否是寄存器，若不是则需要分配一个新的寄存器来保存运算的结果
    if (result_reg_no == -1) {
        // 分配一个寄存器，用于暂存结果
        load_result_reg_no = simpleRegisterAllocator.Allocate(result);
    } else {
        load_result_reg_no = result_reg_no;
    }

    // 执行除法操作
    iloc.idiv(load_result_reg_no, load_arg1_reg_no, load_arg2_reg_no);

    // 结果不是寄存器，则需要把临时结果保存到结果变量中
    if (result_reg_no == -1) {
        iloc.store_var(load_result_reg_no, result, ARM32_TMP_REG_NO);
    }

    // 释放寄存器
    simpleRegisterAllocator.free(arg1);
    simpleRegisterAllocator.free(arg2);
    simpleRegisterAllocator.free(result);
}

/// @brief 整数取模指令翻译成ARM32汇编
/// @param inst IR指令
void InstSelectorArm32::translate_mod_int32(Instruction * inst)
{
    Value * result = inst;
    Value * arg1 = inst->getOperand(0);
    Value * arg2 = inst->getOperand(1);

    int32_t arg1_reg_no = arg1->getRegId();
    int32_t arg2_reg_no = arg2->getRegId();
    int32_t result_reg_no = inst->getRegId();
    int32_t load_result_reg_no, load_arg1_reg_no, load_arg2_reg_no;
    int32_t tmp_reg_no = ARM32_TMP_REG_NO; // 临时寄存器

    // 看arg1是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg1_reg_no == -1) {
        // 分配一个寄存器
        load_arg1_reg_no = simpleRegisterAllocator.Allocate(arg1);

        // arg1 -> 寄存器
        iloc.load_var(load_arg1_reg_no, arg1);
    } else {
        load_arg1_reg_no = arg1_reg_no;
    }

    // 看arg2是否是寄存器，若是则寄存器寻址，否则要load变量到寄存器中
    if (arg2_reg_no == -1) {
        // 分配一个寄存器
        load_arg2_reg_no = simpleRegisterAllocator.Allocate(arg2);

        // arg2 -> 寄存器
        iloc.load_var(load_arg2_reg_no, arg2);
    } else {
        load_arg2_reg_no = arg2_reg_no;
    }

    // 看结果变量是否是寄存器，若不是则需要分配一个新的寄存器来保存运算的结果
    if (result_reg_no == -1) {
        // 分配一个寄存器，用于暂存结果
        load_result_reg_no = simpleRegisterAllocator.Allocate(result);
    } else {
        load_result_reg_no = result_reg_no;
    }

    // 强制占用临时寄存器，确保可用
    simpleRegisterAllocator.Allocate(tmp_reg_no);

    // 使用额外指令计算模运算
    iloc.idiv(tmp_reg_no, load_arg1_reg_no, load_arg2_reg_no); // tmp = a/b
    iloc.mul(tmp_reg_no, tmp_reg_no, load_arg2_reg_no);        // tmp = (tmp)*b
    iloc.inst("sub",
              PlatformArm32::regName[load_result_reg_no],
              PlatformArm32::regName[load_arg1_reg_no],
              PlatformArm32::regName[tmp_reg_no]); // dst = a - tmp

    // 结果不是寄存器，则需要把临时结果保存到结果变量中
    if (result_reg_no == -1) {
        iloc.store_var(load_result_reg_no, result, ARM32_TMP_REG_NO);
    }

    // 释放寄存器
    simpleRegisterAllocator.free(arg1);
    simpleRegisterAllocator.free(arg2);
    simpleRegisterAllocator.free(result);
    simpleRegisterAllocator.free(tmp_reg_no);
}

// 条件分支指令 - 处理BranchInstruction
void InstSelectorArm32::translate_branch(Instruction * inst)
{
    // 动态转换为BranchInstruction
    Instanceof(branchInst, BranchInstruction *, inst);

    Value * condition = branchInst->getCondition();
    LabelInstruction * trueLabel = branchInst->getTrueLabel();
    LabelInstruction * falseLabel = branchInst->getFalseLabel();

    // 获取条件的寄存器
    int32_t cond_reg_no = condition->getRegId();
    int32_t load_cond_reg_no;

    // 如果条件没有在寄存器中，加载到寄存器
    if (cond_reg_no == -1) {
        load_cond_reg_no = simpleRegisterAllocator.Allocate(condition);
        iloc.load_var(load_cond_reg_no, condition);
    } else {
        load_cond_reg_no = cond_reg_no;
    }

    // 比较条件值与0
    iloc.inst("cmp", PlatformArm32::regName[load_cond_reg_no], "#0");

    // 如果条件为假(等于0)，跳转到falseLabel
    iloc.inst("beq", falseLabel->getName());

    // 如果执行到这里，条件为真，跳转到trueLabel
    // 注意：在某些情况下，可以省略此跳转（例如，如果trueLabel紧跟在当前指令之后）
    // 但为了简单性和一致性，我们总是生成此跳转
    iloc.inst("b", trueLabel->getName());

    // 释放寄存器
    simpleRegisterAllocator.free(condition);
}

// 条件运算辅助函数 - 生成通用条件判断代码
void InstSelectorArm32::gen_condition_code(Instruction * inst, const string & cond)
{
    Value * result = inst;
    Value * left = inst->getOperand(0);
    Value * right = inst->getOperand(1);

    int32_t left_reg_no = left->getRegId();
    int32_t right_reg_no = right->getRegId();
    int32_t result_reg_no = result->getRegId();
    int32_t load_left_reg_no, load_right_reg_no, load_result_reg_no;

    // 加载左操作数到寄存器
    if (left_reg_no == -1) {
        load_left_reg_no = simpleRegisterAllocator.Allocate(left);
        iloc.load_var(load_left_reg_no, left);
    } else {
        load_left_reg_no = left_reg_no;
    }

    // 加载右操作数到寄存器
    if (right_reg_no == -1) {
        load_right_reg_no = simpleRegisterAllocator.Allocate(right);
        iloc.load_var(load_right_reg_no, right);
    } else {
        load_right_reg_no = right_reg_no;
    }

    // 分配结果寄存器
    if (result_reg_no == -1) {
        load_result_reg_no = simpleRegisterAllocator.Allocate(result);
    } else {
        load_result_reg_no = result_reg_no;
    }

    // 比较左右操作数
    iloc.inst("cmp", PlatformArm32::regName[load_left_reg_no], PlatformArm32::regName[load_right_reg_no]);

    // 初始化结果为0(假)
    iloc.inst("mov", PlatformArm32::regName[load_result_reg_no], "#0");

    // 如果条件满足则设为1(真)
    iloc.inst("mov" + cond, PlatformArm32::regName[load_result_reg_no], "#1");

    // 保存结果如果不是寄存器变量
    if (result_reg_no == -1) {
        iloc.store_var(load_result_reg_no, result, ARM32_TMP_REG_NO);
    }

    // 释放寄存器
    simpleRegisterAllocator.free(left);
    simpleRegisterAllocator.free(right);
    simpleRegisterAllocator.free(result);
}

// 大于运算
void InstSelectorArm32::translate_gt_int32(Instruction * inst)
{
    gen_condition_code(inst, "gt");
}

// 小于运算
void InstSelectorArm32::translate_lt_int32(Instruction * inst)
{
    gen_condition_code(inst, "lt");
}

// 大于等于运算
void InstSelectorArm32::translate_ge_int32(Instruction * inst)
{
    gen_condition_code(inst, "ge");
}

// 小于等于运算
void InstSelectorArm32::translate_le_int32(Instruction * inst)
{
    gen_condition_code(inst, "le");
}

// 等于运算
void InstSelectorArm32::translate_eq_int32(Instruction * inst)
{
    gen_condition_code(inst, "eq");
}

// 不等于运算
void InstSelectorArm32::translate_ne_int32(Instruction * inst)
{
    gen_condition_code(inst, "ne");
}

/// @brief load指令翻译成ARM32汇编 - 从内存加载数据
/// @param inst IR指令
void InstSelectorArm32::translate_load(Instruction * inst)
{
    // load指令格式: %dest = *%src_ptr
    Value * dest = inst->getOperand(0);    // 目标变量
    Value * src_ptr = inst->getOperand(1); // 源指针

    int32_t src_ptr_reg_no = src_ptr->getRegId();
    int32_t load_src_ptr_reg_no;

    // 加载源指针到寄存器（获取内存地址）
    if (src_ptr_reg_no == -1) {
        load_src_ptr_reg_no = simpleRegisterAllocator.Allocate(src_ptr);
        iloc.load_var(load_src_ptr_reg_no, src_ptr);
    } else {
        load_src_ptr_reg_no = src_ptr_reg_no;
    }

    // 从指针指向的内存地址加载数据到临时寄存器
    int32_t temp_reg_no = simpleRegisterAllocator.Allocate(dest);

    // 使用 ILocArm32 提供的 load_base 方法
    // 偏移为0，相当于 ldr temp_reg, [src_ptr_reg]
    iloc.load_base(temp_reg_no, load_src_ptr_reg_no, 0);

    // 将结果存储到目标变量
    iloc.store_var(temp_reg_no, dest, ARM32_TMP_REG_NO);

    // 释放寄存器
    simpleRegisterAllocator.free(src_ptr);
    simpleRegisterAllocator.free(dest);
}

/// @brief store指令翻译成ARM32汇编 - 向内存存储数据
/// @param inst IR指令
void InstSelectorArm32::translate_store(Instruction * inst)
{
    // store指令格式: *%dest_ptr = %src
    Value * dest_ptr = inst->getOperand(0); // 目标指针
    Value * src = inst->getOperand(1);      // 源数据

    int32_t dest_ptr_reg_no = dest_ptr->getRegId();
    int32_t src_reg_no = src->getRegId();
    int32_t load_dest_ptr_reg_no, load_src_reg_no;

    // 加载目标指针到寄存器（获取内存地址）
    if (dest_ptr_reg_no == -1) {
        load_dest_ptr_reg_no = simpleRegisterAllocator.Allocate(dest_ptr);
        iloc.load_var(load_dest_ptr_reg_no, dest_ptr);
    } else {
        load_dest_ptr_reg_no = dest_ptr_reg_no;
    }

    // 加载源数据到寄存器
    if (src_reg_no == -1) {
        load_src_reg_no = simpleRegisterAllocator.Allocate(src);
        iloc.load_var(load_src_reg_no, src);
    } else {
        load_src_reg_no = src_reg_no;
    }

    // 使用 ILocArm32 提供的 store_base 方法
    // 偏移为0，相当于 str src_reg, [dest_ptr_reg]
    iloc.store_base(load_src_reg_no, load_dest_ptr_reg_no, 0, ARM32_TMP_REG_NO);

    // 释放寄存器
    simpleRegisterAllocator.free(dest_ptr);
    simpleRegisterAllocator.free(src);
}