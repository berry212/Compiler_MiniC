///
/// @file MemMoveInstruction.cpp
/// @brief 内存读写操作指令，包括加载（load）和存储（store）
///
/// @author chen
/// @version 1.0
/// @date 2025-6-1
///
/// @copyright Copyright (c) 2024
///


#include "Common.h"
#include "Instruction.h"
#include "VoidType.h"
#include "MemMoveInstruction.h"

///
/// @brief 构造函数
/// @param _func 所属的函数
/// @param _accessType 访问类型(加载/存储)
/// @param _dest 目标操作数
/// @param _source 源操作数
///
MemMoveInstruction::MemMoveInstruction(Function * _func, IRInstOperator _IROP, Value * _dest, Value * _source)
    : Instruction(_func, _IROP, VoidType::getType()), accessType(_IROP)
{
    addOperand(_dest);
    addOperand(_source);
}

/// @brief 转换成字符串显示
/// @param str 转换后的字符串
void MemMoveInstruction::toString(std::string & str)
{
    Value * dest = getOperand(0);
    Value * source = getOperand(1);

    if (accessType == IRInstOperator::IRINST_OP_LOAD) {
        // 加载指令: %t1 = *%t2
        str = dest->getIRName() + " = *" + source->getIRName();
    } else if (accessType == IRInstOperator::IRINST_OP_STORE) {
        // 存储指令: *%t1 = %t2
        str = "*" + dest->getIRName() + " = " + source->getIRName();
    } else {
        minic_log(LOG_ERROR, "内存指令访问类型错误");
    }
}

/// @brief 获取访问类型
/// @return 访问类型
IRInstOperator MemMoveInstruction::getAccessType() const
{
    return accessType;
}