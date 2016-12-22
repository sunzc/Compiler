#include "Instruction.h"
#include "ParserUtil.h"

ArithIns::ArithIns(ArithInsType at, const Operand* arg1, const Operand* arg2, const Operand* result)
        :Instruction(Instruction::InstructionType::ARITH){
		at_ = at;
		arg1_ = arg1;
		arg2_ = arg2;
		dest_ = result;
}

std::string ArithIns::toString(){
	string inst;

	switch(at_) {
		case ArithInsType::ADD:
			inst = "ADD ";
			break;
		case ArithInsType::SUB:
			inst = "SUB ";
			break;
		case ArithInsType::DIV:
			inst = "DIV ";
			break;
		case ArithInsType::MUL:
			inst = "MUL ";
			break;
		case ArithInsType::MOD:
			inst = "MOD ";
			break;
		case ArithInsType::NEG:
			inst = "NEG ";
			break;
		case ArithInsType::AND:
			inst = "AND ";
			break;
		case ArithInsType::OR:
			inst = "OR ";
			break;
		case ArithInsType::XOR:
			inst = "XOR ";
			break;
		default:
			std::cout<<"Unknown ArithInsType!"<<std::endl;
			break;
	}

	if (arg2_ != NULL) // not NEG
		inst += arg1_->toString() + " " + arg2_->toString() + " " + dest_->toString()+"\n";
	else
		inst += arg1_->toString() + " " + dest_->toString()+"\n";

	return inst;
}
            
FloatArithIns::FloatArithIns(FloatArithInsType fat, const Operand* arg1, const Operand* arg2, const Operand* result)
        :Instruction(Instruction::InstructionType::FLOAT){
		fat_ = fat;
		arg1_ = arg1;
		arg2_ = arg2;
		dest_ = result;
}

std::string FloatArithIns::toString(){
	string inst;

	switch(fat_) {
		case FloatArithInsType::FADD:
			inst = "FADD ";
			break;
		case FloatArithInsType::FSUB:
			inst = "FSUB ";
			break;
		case FloatArithInsType::FDIV:
			inst = "FDIV ";
			break;
		case FloatArithInsType::FMUL:
			inst = "FMUL ";
			break;
		case FloatArithInsType::FNEG:
			inst = "FNEG ";
			break;
		default:
			std::cout<<"Unknown FloatArithInsType!"<<std::endl;
			break;
	}

	if (arg2_ != NULL) // not NEG
		inst += arg1_->toString() + " " + arg2_->toString() + " " + dest_->toString()+"\n";
	else
		inst += arg1_->toString() + " " + dest_->toString()+"\n";

	return inst;
}
            
RelOpIns::RelOpIns(RelOpInsType relop, const Operand* arg1, const Operand* arg2)
        :Instruction(Instruction::InstructionType::RELOP){
		relop_ = relop;
		arg1_ = arg1;
		arg2_ = arg2;
}

std::string RelOpIns::toString(){
	string inst;

	switch(relop_) {
		case RelOpInsType::GT:
			inst = "GT ";
			break;
		case RelOpInsType::GE:
			inst = "GE ";
			break;
		case RelOpInsType::UGT:
			inst = "UGT ";
			break;
		case RelOpInsType::UGE:
			inst = "UGE ";
			break;
		case RelOpInsType::EQ:
			inst = "EQ ";
			break;
		case RelOpInsType::NE:
			inst = "NE ";
			break;
		default:
			std::cout<<"Unknown RelOpIns!"<<std::endl;
			break;
	}

	inst += arg1_->toString() + " " + arg2_->toString() + "\n";

	return inst;
}
            
FloatRelOpIns::FloatRelOpIns(FloatRelOpInsType frelop, const Operand* arg1, const Operand* arg2)
        :Instruction(Instruction::InstructionType::FLOATRELOP){
		frelop_ = frelop;
		arg1_ = arg1;
		arg2_ = arg2;
}

std::string FloatRelOpIns::toString(){
	string inst;

	switch(frelop_) {
		case FloatRelOpInsType::FGT:
			inst = "FGT ";
			break;
		case FloatRelOpInsType::FGE:
			inst = "FGE ";
			break;
		case FloatRelOpInsType::FEQ:
			inst = "FEQ ";
			break;
		case FloatRelOpInsType::FNE:
			inst = "FNE ";
			break;
		default:
			std::cout<<"Unknown RelOpInsType!"<<std::endl;
			break;
	}

	inst += arg1_->toString() + " " + arg2_->toString() + "\n";

	return inst;
}

PrintIns::PrintIns(PrintInsType pit, const Operand* arg1)
        :Instruction(Instruction::InstructionType::PRINT){
		pit_ = pit;
		arg1_ = arg1;
}

std::string PrintIns::toString(){
	string inst;

	switch(pit_) {
		case PrintInsType::PRTI:
			inst = "PRTI ";
			break;
		case PrintInsType::PRTS:
			inst = "PRTS ";
			break;
		case PrintInsType::PRTF:
			inst = "PRTF ";
			break;
		default:
			std::cout<<"Unknown PrintInsType!"<<std::endl;
			break;
	}

	if (pit_ == PrintInsType::PRTS && arg1_->type() == Instruction::Operand::OperandType::STR_CONST)
		inst += "\"" + arg1_->toString() + "\"" + "\n";
	else
		inst += arg1_->toString() + "\n";

	return inst;
}

MovIns::MovIns(MovInsType mit, const Operand* arg1, const Operand* arg2)
        :Instruction(Instruction::InstructionType::DATAMOV){
		mit_ = mit;
		arg1_ = arg1;
		arg2_ = arg2;
}

std::string MovIns::toString(){
	string inst;

	switch(mit_) {
		case MovInsType::MOVL:
			inst = "MOVL ";
			break;
		case MovInsType::MOVS:
			inst = "MOVS ";
			break;
		case MovInsType::MOVI:
			inst = "MOVI ";
			break;
		case MovInsType::MOVF:
			inst = "MOVF ";
			break;
		case MovInsType::MOVIF:
			inst = "MOVIF ";
			break;
		case MovInsType::LDI:
			inst = "LDI ";
			break;
		case MovInsType::LDF:
			inst = "LDF ";
			break;
		case MovInsType::STI:
			inst = "STI ";
			break;
		case MovInsType::STF:
			inst = "STF ";
			break;
		default:
			std::cout<<"Unknown MovInsType!"<<std::endl;
			break;
	}

	if (mit_ == MovInsType::MOVS)
		inst += "\"" + arg1_->toString() + "\"" + " " + arg2_->toString() + "\n";
	else
		inst += arg1_->toString() + " " + arg2_->toString() + "\n";

	return inst;
}

InputIns:: InputIns(InputInsType iit, Operand *arg1)
	:Instruction(Instruction::InstructionType::INPUT){
		iit_ = iit;
		arg1_ = arg1;
}

std::string InputIns::toString(){
	string inst;

	switch(iit_) {
		case InputInsType::IN:
			inst = "IN " + arg1_->toString() + "\n";
			break;
		case InputInsType::INI:
			inst = "INI " + arg1_->toString() + "\n";
			break;
		case InputInsType::INF:
			inst = "INF " + arg1_->toString() + "\n";
			break;
		default:
			std::cout<<"Unknown InputInsType!"<<std::endl;
			break;
	}

	return inst;
}

JumpIns::JumpIns(JumpInsType jit, Instruction *cond, Operand *arg1)
	:Instruction(Instruction::InstructionType::JMP){
		jit_ = jit;
		cond_ = cond;
		arg1_ = arg1;
}

std::string JumpIns::toString(){
	string inst;
	string cond;

	switch(jit_) {
		case JumpInsType::JMP:
			inst = "JMP " + arg1_->toString() + "\n";
			break;
		case JumpInsType::JMPI:
			inst = "JMPI " + arg1_->toString() + "\n";
			break;
		case JumpInsType::JMPC:
			cond = cond_->toString();
			cond.erase(cond.find_last_not_of("\n")+1);
			inst = "JMPC " + cond + " " + arg1_->toString() + "\n";
			break;
		case JumpInsType::JMPCI:
			cond = cond_->toString();
			cond.erase(cond.find_last_not_of("\n")+1);
			inst = "JMPCI " + cond + " " + arg1_->toString() + "\n";
			break;
		default:
			std::cout<<"Unknown JumpInsType!"<<std::endl;
			break;
	}

	return inst;
}
/* TODO: OTHER methods to be implemented */
