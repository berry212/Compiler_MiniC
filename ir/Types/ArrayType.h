///
/// @file ArrayType.h
/// @brief 数组类型描述类
/// @author chen
/// @version 1.0
/// @date 2024-12-05
///
/// @copyright Copyright (c) 2024
///
/// @par 修改日志:
/// <table>
/// <tr><th>Date       <th>Version <th>Author  <th>Description
/// <tr><td>2024-12-05 <td>1.0     <td>chen    <td>新建
/// </table>
///

#pragma once

#include <string>
#include <vector>
#include "Type.h"

///
/// @brief 数组类型
///
class ArrayType : public Type {
public:
    /// @brief ArrayType的构造函数
    /// @param[in] elemType 数组元素的类型
    /// @param[in] dimensions 数组各维度的大小
    ///
    /// 该构造函数将Type的ID设置为ArrayTyID，并保存元素类型和维度信息
    explicit ArrayType(Type * elemType, const std::vector<int32_t> & dimensions);

    ///
    /// @brief 获取数组元素类型
    /// @return 元素类型
    ///
    [[nodiscard]] Type * getElementType() const;

    ///
    /// @brief 获取数组维度信息
    /// @return 维度大小的向量
    ///
    [[nodiscard]] const std::vector<int32_t> & getDimensions() const;

    ///
    /// @brief 获取指定维度的大小
    /// @param dim 维度索引
    /// @return 该维度的大小，如果索引无效则返回-1
    ///
    [[nodiscard]] int32_t getDimensionSize(size_t dim) const;

    ///
    /// @brief 获取数组的维数
    /// @return 维数
    ///
    [[nodiscard]] size_t getNumDimensions() const;

    ///
    /// @brief 获取数组总大小（元素数量）
    /// @return 元素总数
    ///
    [[nodiscard]] int32_t getTotalElements() const;

    ///
    /// @brief 获得类型所占内存空间大小(字节数)
    /// @return 内存大小
    ///
    [[nodiscard]] int32_t getSize() const override;

    ///
    /// @brief 转换为字符串表示
    /// @return 字符串表示
    ///
    [[nodiscard]] std::string toString() const override;

    ///
    /// @brief 获取元素在特定索引位置的偏移量(元素数)
    /// @param indices 各维度的索引值
    /// @return 元素的偏移量
    ///
    [[nodiscard]] int32_t getElementOffset(const std::vector<int32_t> & indices) const;

    ///
    /// @brief 判断是否是数组类型
    /// @return 始终返回true
    ///
    [[nodiscard]] bool isArrayType() const
    {
        return true;
    }

    ///
    /// @brief 获取数组类型
    /// @param elemType 元素类型
    /// @param dimensions 维度信息
    /// @return ArrayType* 数组类型指针
    ///
    static ArrayType * getArrayType(Type * elemType, const std::vector<int32_t> & dimensions);

private:
    Type * elementType;              ///< 数组元素类型
    std::vector<int32_t> dimensions; ///< 数组各维度大小
};