#include "BranchInstruction.h"
#include <string>

void BranchInstruction::toString(std::string& str)
{
    str = "br " + condition->getIRName() + ", " + trueLabel->getIRName() + ", " + falseLabel->getIRName();
}