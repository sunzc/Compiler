#include "RegManager.h"
#include "Instruction.h"

// define 
int REG_RA = 900;
int REG_BP = 901;
int REG_SP = 902;
// Return Value stored in R/F903
int REG_RV = 903;
// Return Label Register in MainLoop
int REG_RL = 904;
int labelCount = 0;

/**
 * push in-use caller-save regs, first Int Reg, then Float Reg
 */
std::string RegManager::pushCallerSaveRegs() {
	std::string code;
	Instruction::Operand *arg1, *arg2, *dest;
	ArithIns *ai;
	MovIns *mi;
	int intReg, floatReg;
	auto iit = inUseCallerSIR_.begin();
	auto fit = inUseCallerSFR_.begin();

	for (;iit != inUseCallerSIR_.end(); iit++) {
		intReg = (*iit);

		// inst: STI intReg SP
		arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, intReg);
		arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		mi = new MovIns(MovIns::MovInsType::STI, arg1, arg2);
		code += mi->toString();

		// inst: SUB SP 1 SP
		arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, 1);
		dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		ai = new ArithIns(ArithIns::ArithInsType::SUB, arg1, arg2, dest);
		code += ai->toString();
	}

	for (;fit != inUseCallerSFR_.end(); fit++) {
		floatReg = (*fit);

		// inst: STF floatReg SP
		arg1 = new Instruction::Operand(Instruction::Operand::OperandType::FLOAT_REG, floatReg);
		arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		mi = new MovIns(MovIns::MovInsType::STF, arg1, arg2);
		code += mi->toString();

		// inst: SUB SP 1 SP
		arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, 1);
		dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		ai = new ArithIns(ArithIns::ArithInsType::SUB, arg1, arg2, dest);
		code += ai->toString();
	}

	return code;

}

/**
 * pop in-use caller-save regs, first float Reg, then int Reg
 */
std::string RegManager::popCallerSaveRegs() {
	std::string code;
	Instruction::Operand *arg1, *arg2, *dest;
	ArithIns *ai;
	MovIns *mi;
	int intReg, floatReg;
	auto iit = inUseCallerSIR_.rbegin();
	auto fit = inUseCallerSFR_.rbegin();

	for (;fit != inUseCallerSFR_.rend(); fit++) {
		floatReg = (*fit);

		// inst: ADD SP 1 SP
		arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, 1);
		dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		ai = new ArithIns(ArithIns::ArithInsType::ADD, arg1, arg2, dest);
		code += ai->toString();

		// inst: LDF floatReg SP
		arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		arg2 = new Instruction::Operand(Instruction::Operand::OperandType::FLOAT_REG, floatReg);
		mi = new MovIns(MovIns::MovInsType::LDF, arg1, arg2);
		code += mi->toString();
	}

	for (;iit != inUseCallerSIR_.rend(); iit++) {
		intReg = (*iit);

		// inst: ADD SP 1 SP
		arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, 1);
		dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		ai = new ArithIns(ArithIns::ArithInsType::ADD, arg1, arg2, dest);
		code += ai->toString();

		// inst: LDI SP intReg
		arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
		arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, intReg);
		mi = new MovIns(MovIns::MovInsType::LDI, arg1, arg2);
		code += mi->toString();
	}

	return code;
}

RegManager::RegManager() {
	int i;

	for(i = CALLER_START; i <= CALLER_END; i++) {
		freeCallerSIR_.push_back(i);
		freeCallerSFR_.push_back(i);
	}

	for(i = CALLEE_START; i <= CALLEE_END; i++) {
		freeCalleeSIR_.push_back(i);
		freeCalleeSFR_.push_back(i);
	}
}

int RegManager::getReg(bool isCallerSave, bool isFloat) {
	int ret;

	if (isCallerSave && isFloat) {
		ret = freeCallerSFR_.front();
		freeCallerSFR_.pop_front();
		inUseCallerSFR_.push_back(ret);
	} else if (isCallerSave && !isFloat) {
		ret = freeCallerSIR_.front();
		freeCallerSIR_.pop_front();
		inUseCallerSIR_.push_back(ret);
	} else if (!isCallerSave && isFloat) {
		ret = freeCalleeSFR_.front();
		freeCalleeSFR_.pop_front();
		inUseCalleeSFR_.push_back(ret);
	} else {
		ret = freeCalleeSIR_.front();
		freeCalleeSIR_.pop_front();
		inUseCalleeSIR_.push_back(ret);
	}

	return ret;
}

bool RegManager::isCallerSR(int regNum) {
	if (regNum >= CALLER_START && regNum <= CALLER_END)
		return true;
	else if (regNum >= CALLEE_START && regNum <= CALLEE_END)
		return false;
	else {
		std::cout<<"ERROR: register number out of range!!! reg:"<<regNum<<std::endl;
		return false;
	}
}

void RegManager::releaseReg(int regNum, bool isFloat) {
	bool isCaller;

	isCaller = RegManager::isCallerSR(regNum);

	if (isCaller && isFloat) {
		inUseCallerSFR_.remove(regNum);
		freeCallerSFR_.push_back(regNum);
	} else if (isCaller && !isFloat) {
		inUseCallerSIR_.remove(regNum);
		freeCallerSIR_.push_back(regNum);
	} else if (!isCaller && isFloat) {
		inUseCalleeSFR_.remove(regNum);
		freeCalleeSFR_.push_back(regNum);
	} else {
		inUseCalleeSIR_.remove(regNum);
		freeCalleeSIR_.push_back(regNum);
	}
}
