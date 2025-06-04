///
/// @file ArrayType.cpp
/// @brief 数组类型描述类实现
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

#include "ArrayType.h"

ArrayType::ArrayType(Type * elemType, const std::vector<int32_t> & dimensions)
    : Type(ArrayTyID), elementType(elemType), dimensions(dimensions)
{}

Type * ArrayType::getElementType() const
{
    return elementType;
}

const std::vector<int32_t> & ArrayType::getDimensions() const
{
    return dimensions;
}

int32_t ArrayType::getDimensionSize(size_t dim) const
{
    if (dim < dimensions.size()) {
        return dimensions[dim];
    }
    return -1;
}

size_t ArrayType::getNumDimensions() const
{
    return dimensions.size();
}

int32_t ArrayType::getTotalElements() const
{
    int32_t totalSize = 1;
    for (auto dim: dimensions) {
        totalSize *= dim;
    }
    return totalSize;
}

int32_t ArrayType::getSize() const
{
    int32_t elemSize = elementType->getSize();
    return getTotalElements() * elemSize;
}

std::string ArrayType::toString() const
{
    std::string result = elementType->toString();
    for (auto dim: dimensions) {
        result += "[" + std::to_string(dim) + "]";
    }
    return result;
}

int32_t ArrayType::getElementOffset(const std::vector<int32_t> & indices) const
{
    if (indices.size() > dimensions.size()) {
        return -1; // 索引维数超过数组维数
    }

    int32_t offset = 0;
    int32_t multiplier = 1;

    // 从最后一维开始计算，如三维数组[2][3][4]，索引(i,j,k)的偏移量为 i*3*4 + j*4 + k
    for (int i = (int) indices.size() - 1; i >= 0; --i) {
        offset += indices[i] * multiplier;
        if (i > 0) {
            multiplier *= dimensions[i];
        }
    }

    return offset;
}

ArrayType * ArrayType::getArrayType(Type * elemType, const std::vector<int32_t> & dimensions)
{
    return new ArrayType(elemType, dimensions);
}