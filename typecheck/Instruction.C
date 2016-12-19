#include "Instruction.h"
#include "ParserUtil.h"

ArithIns::ArithIns(ArithInsType at, const Operand* arg1, const Operand* arg2, const Operand* result)
        :Instruction(Instruction::InstructionType::ARITH){
		at_ = at;
		arg1_ = arg1;
		arg2_ = arg2;
		dest_ = result;
}
            
FloatArithIns::FloatArithIns(FloatArithInsType fat, const Operand* arg1, const Operand* arg2, const Operand* result)
        :Instruction(Instruction::InstructionType::FLOAT){
		fat_ = fat;
		arg1_ = arg1;
		arg2_ = arg2;
		dest_ = result;
}
            
RelOpIns::RelOpIns(RelOpInsType relop, const Operand* arg1, const Operand* arg2)
        :Instruction(Instruction::InstructionType::RELOP){
		relop_ = relop;
		arg1_ = arg1;
		arg2_ = arg2;
}
            
FloatRelOpIns::FloatRelOpIns(FloatRelOpInsType frelop, const Operand* arg1, const Operand* arg2)
        :Instruction(Instruction::InstructionType::FLOATRELOP){
		frelop_ = frelop;
		arg1_ = arg1;
		arg2_ = arg2;
}
            
PrintIns::PrintIns(PrintInsType pit, const Operand* arg1)
        :Instruction(Instruction::InstructionType::PRINT){
		pit_ = pit;
		arg1_ = arg1;
}
            
MovIns::MovIns(MovInsType mit, const Operand* arg1, const Operand* arg2)
        :Instruction(Instruction::InstructionType::DATAMOV){
		mit_ = mit;
		arg1_ = arg1;
		arg2_ = arg2;
}
            
JumpIns::JumpIns(JumpInsType jit, Instruction *cond, Operand *arg1)
	:Instruction(Instruction::InstructionType::JMP){
		jit_ = jit;
		cond_ = cond;
		arg1_ = arg1;
}
/* TODO: OTHER methods to be implemented */
