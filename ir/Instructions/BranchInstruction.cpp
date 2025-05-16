#include "BranchInstruction.h"
#include <string>

void BranchInstruction::toString(std::string& str)
{
    str = "bc " + condition->getIRName() + ", " + trueLabel->getIRName() + ", " + falseLabel->getIRName();
}