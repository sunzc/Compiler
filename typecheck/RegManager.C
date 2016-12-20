#include "RegManager.h"

// define 
int REG_RT = 900;
int REG_BP = 901;
int REG_SP = 902;

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
	} else if (!isCallerSave && !isFloat) {
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
	} else if (!isCaller && !isFloat) {
		inUseCalleeSIR_.remove(regNum);
		freeCalleeSIR_.push_back(regNum);
	}
}
