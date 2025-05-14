#pragma once

#include "Instruction.h"
#include "LabelInstruction.h"
#include "VoidType.h"
/// @brief 条件分支指令
class BranchInstruction : public Instruction {
public:
    /// @brief 构造函数
    /// @param func 所属函数
    /// @param condition 条件值
    /// @param trueLabel 条件为真时跳转的标签
    /// @param falseLabel 条件为假时跳转的标签
    BranchInstruction(Function * func, Value * condition, LabelInstruction * trueLabel, LabelInstruction * falseLabel)
        : Instruction(func, IRInstOperator::IRINST_OP_BR, VoidType::getType()), condition(condition), trueLabel(trueLabel),
          falseLabel(falseLabel)
    {
        // 添加操作数
        addOperand(condition);
    }

    /// @brief 获取条件
    /// @return 条件值
    [[nodiscard]] Value * getCondition() const
    {
        return condition;
    }

    /// @brief 获取真分支标签
    /// @return 真分支标签
    [[nodiscard]] LabelInstruction * getTrueLabel() const
    {
        return trueLabel;
    }

    /// @brief 获取假分支标签
    /// @return 假分支标签
    [[nodiscard]] LabelInstruction * getFalseLabel() const
    {
        return falseLabel;
    }

    /// @brief 转换为字符串
    /// @return 指令的字符串表示
    void toString(std::string & str) override;

private:
    /// @brief 条件值
    Value * condition;

    /// @brief 真分支标签
    LabelInstruction * trueLabel;

    /// @brief 假分支标签
    LabelInstruction * falseLabel;
};
