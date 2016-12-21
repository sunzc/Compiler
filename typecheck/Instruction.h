#ifndef INSTRUCTION_H
#define INSTRUCTION_H

/********************* class definition of an instruction and what it contains ****************************/

#include <iostream>
#include <string>

class Instruction {
  public:
	enum class InstructionType {
		ARITH,
		FLOAT,
		RELOP,
		FLOATRELOP,
		PRINT,
		JMP,
		DATAMOV,
		INPUT
			
	}; // all possible instruction types //
	Instruction(InstructionType it) { it_ = it;};
	InstructionType instType() { return it_;};
	/* An address can be a name, constant or a compiler-generated temporary */
	
	static std::string getLabel();
	virtual std::string toString() { return NULL;};
	
	/* Operand class to be defined here */
	
	class Operand{
	  public:
		enum class OperandType {
			INT_REG,
			FLOAT_REG,
			INT_CONST,
			FLOAT_CONST,
			STR_CONST
		};

	        Operand(OperandType ot, int i_val) { ot_ = ot; i_val_ = i_val;};
	        Operand(OperandType ot, float f_val) { ot_ = ot; f_val_ = f_val;};
	        Operand(OperandType ot, std::string s_val) { ot_ = ot; s_val_ = s_val;};

		int i_val() const { return i_val_;};
		float f_val() const { return f_val_;};
		std::string s_val() const { return s_val_;};
		std::string reg_name(int i) const{
			std::string name = NULL;

			if (i>=0 && i<=9)
				name = "00" + std::to_string(i);
			else if (i>9 && i<=99)
				name = "0" + std::to_string(i);
			else if (i>99 && i<=999)
				name = std::to_string(i);
			else
				std::cout<<"Illeagal register number:"<<i<<std::endl;

			return name;
		};

		// return a corresponding operand name like 'R001','F003',"L2:","test output"
		std::string toString() const{
			std::string op = NULL;
			switch (ot_) {
				case OperandType::INT_REG:
					op = "R" + reg_name(i_val_);
					break;
				case OperandType::FLOAT_REG:
					op = "F" + reg_name(i_val_);
					break;
				case OperandType::INT_CONST:
					op = std::to_string(i_val_);
					break;
				case OperandType::FLOAT_CONST:
					op = std::to_string(f_val_);
					break;
				case OperandType::STR_CONST:
					op = s_val();
					break;
				default:
					std::cout<<"Unsupport Operand Type:"<<(int)ot_<<std::endl;
			}

			return op;
		};
        
        	~Operand();
	  private:
		OperandType ot_;
		int i_val_;
		float f_val_;
		std::string s_val_;
        };

	private:
	InstructionType it_;
};

// The instruction types can have subclasses based on its type //
class ArithIns: public Instruction{
	public:
		enum class ArithInsType {
			ADD, SUB, DIV, MUL, 
			MOD, NEG, AND, OR, XOR 
			};
	public:
		ArithIns(ArithInsType at, const Operand* arg1, const Operand* arg2, const Operand* result);
		std::string toString();
	private:
        ArithInsType at_;
	const Operand* arg1_;
	const Operand* arg2_;
	const Operand* dest_;
};

class FloatArithIns: public Instruction{
	public:
		enum class FloatArithInsType {
			FADD, FSUB, FDIV, FMUL, FNEG
		};
	public: /* TODO constructor */
		FloatArithIns(FloatArithInsType fat, const Operand* arg1, const Operand* arg2, const Operand* result);
		std::string toString();
	private:
        FloatArithInsType fat_;
	const Operand* arg1_;
	const Operand* arg2_;
	const Operand* dest_;

};

class RelOpIns: public Instruction{
	public:
		enum class RelOpInsType {
			GT, GE, UGT, UGE, EQ, NE
		};
	public: /* TODO constructor */
		RelOpIns(RelOpInsType relop, const Operand* arg1, const Operand* arg2); /* No destination register */
		std::string toString();
	private:
        RelOpInsType relop_;
        const Operand* arg1_;
        const Operand* arg2_;
};

class FloatRelOpIns: public Instruction{
	public:
		enum class FloatRelOpInsType {
			FGT, FGE, FEQ, FNE
		};
	public: /*TODO constructor */
		FloatRelOpIns(FloatRelOpInsType frelop, const Operand* arg1, const Operand* arg2);
		std::string toString();
	private:
        FloatRelOpInsType frelop_;
        const Operand* arg1_;
        const Operand* arg2_;
        
};

class PrintIns: public Instruction{
	public:
		enum class PrintInsType {
			PRTI, PRTS, PRTF
		};
	public: /* TODO constructor */
		PrintIns(PrintInsType pit, const Operand* arg1); /* Only one operand */
		std::string toString();
	private:
        PrintInsType pit_;
        const Operand* arg1_;
};

class JumpIns: public Instruction{
	public:
		enum class JumpInsType {
			JMP, JMPC, JMPI, JMPCI
		};
	public: /* TODO constructor ---> how many operators */
		JumpIns(JumpInsType jit, Instruction *cond, Operand* arg1);
		std::string toString();
	private:
        JumpInsType jit_;
	Instruction *cond_; /* cond is a RelOpIns or FloatRelOpIns, required by JMPC/JMPCI */
        const Operand* arg1_;
};

class MovIns: public Instruction{
	public:
		enum class MovInsType {
			MOVL, MOVS, MOVI, MOVF,
			MOVIF, LDI, LDF, STI,
			STF
		};
	public: /* TODO constructor */
		MovIns(MovInsType mit, const Operand* arg1, const Operand* arg2);  /* mov instructions have 2 operands */
		std::string toString();
	private:
        MovInsType mit_;
        const Operand* arg1_;
        const Operand* arg2_;
};	

class InputIns: public Instruction{
	public:
		enum class InputInsType{
			IN, INI, INF
		};
	public:
		InputIns(InputInsType iit, Operand* arg1);
		std::string toString();
	private:
        InputInsType iit_;
        const Operand* arg1_;
};
#endif
