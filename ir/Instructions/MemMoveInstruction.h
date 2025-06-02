///
/// @file MemMoveInstruction.h
/// @brief 内存读写操作指令，包括加载（load）和存储（store）
///
/// @author chen
/// @version 1.0
/// @date 2025-6-1
///
/// @copyright Copyright (c) 2024
///

#pragma once

#include <string>

#include "Value.h"
#include "Instruction.h"
#include "MoveInstruction.h"

class Function;

///
/// @brief 内存数据传输指令
///
class MemMoveInstruction : public Instruction {
public:
    ///
    /// @brief 构造函数
    /// @param _func 所属的函数
    /// @param _accessType 访问类型(加载/存储)
    /// @param _dest 目标操作数
    /// @param _source 源操作数
    ///
    MemMoveInstruction(Function * _func, IRInstOperator _IROP, Value * _dest, Value * _source);

    /// @brief 转换成字符串
    void toString(std::string & str) override;

    /// @brief 获取访问类型
    /// @return 访问类型
    [[nodiscard]] IRInstOperator getAccessType() const;

private:
    IRInstOperator accessType; ///< 内存访问类型
};