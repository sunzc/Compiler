#include "STEClasses.h"
#include "Value.h"
#include "ParserUtil.h"
#include "Instruction.h"

// Global Data Memory Allocation: record current offset for global data
int currentOffset = DATA_START;

extern int REG_BP;
extern int REG_SP;
extern int REG_RV;
extern int REG_RA;
extern int REG_RL;

string GlobalEntry::stackInit(RegManager *rm, int stackStart) {
	string code = "";
	Instruction::Operand *arg1, *arg2;
	MovIns *mi;

	// inst: MOVI STACK_START REG_SP
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, stackStart);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
	mi = new MovIns(MovIns::MovInsType::MOVI, arg1, arg2);
	code += mi->toString();

	// inst: MOVI REG_SP REG_BP
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_BP);
	mi = new MovIns(MovIns::MovInsType::MOVI, arg1, arg2);
	code += mi->toString();

	return code;
}

/**
 * workflow of mainLoop:
 * loop_start:
 *	1. IN R001
 *	2. ADD R001 EVENT_MAP_START R001	# get label stored at R001 + EVENT_MAP_START
 *	3. LDI R001 R002
 *	5. MOVL ret_addr R904			# R904 dedicated for ret_addr here
 *	4. JMPI R002
 * ret_addr:
 *	5. JMP loop_start
 */
string GlobalEntry::mainLoop(RegManager *rm) {
	string code;
	string loopStart = AstNode::getLabel();
	string retLabel = AstNode::getLabel();
	Instruction::Operand *arg1, *arg2, *dest;
	ArithIns *ai;
	MovIns *mi;
	JumpIns *ji;
	InputIns *ii;
	int tmpReg1, tmpReg2;

	code = loopStart + ": ";

	// inst: IN tmpReg
	// alloc a caller-save, interger reg
	tmpReg1 = rm->getReg(true, false);
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg1);
	ii = new InputIns(InputIns::InputInsType::IN, arg1);
	code += ii->toString();

	// inst: ADD R001 EVENT_MAP_START
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg1);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, EVENT_MAP_START);
	dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg1);
	ai = new ArithIns(ArithIns::ArithInsType::ADD, arg1, arg2, dest);
	code += ai->toString();

	// alloc a caller-save, interger reg
	tmpReg2 = rm->getReg(true, false);
	// inst: LDI tmpReg1 tmpReg2
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg1);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
	mi = new MovIns(MovIns::MovInsType::LDI, arg1, arg2);
	code += mi->toString();

	// inst: MOVL ret_label REG_RL
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::STR_CONST, retLabel);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_RL);
	mi = new MovIns(MovIns::MovInsType::MOVL, arg1, arg2);
	code += mi->toString();

	// inst: JMPI tmpReg2
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
	ji = new JumpIns(JumpIns::JumpInsType::JMPI, NULL, arg1);
	code += ji->toString();

	code += retLabel + ": ";

	// inst: JMP loopStart
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::STR_CONST, loopStart);
	ji = new JumpIns(JumpIns::JumpInsType::JMP, NULL, arg1);
	code += ji->toString();

	rm->releaseReg(tmpReg1, false);
	rm->releaseReg(tmpReg2, false);

	return code;
}

string GlobalEntry::codeGen(RegManager *rm) {
	const SymTab *st = this->symTab();
	Type::TypeTag tag;
	int offset;
	// tempReg: reg name like "R001", "F009"
	int tmpReg1, tmpReg2;
	string helpInfo = "// ABI:\n// REG_RA(func ret addr) : R900\n// REG_BP: R901\n// REG_SP: R902\n// REG_RV(func ret value): R903 or F903\n// REG_RL(main loop ret label): R904\n";
	string globalVarInitCode = "//global var init code\n";
	string funcCode = "// func code\n";
	string ruleCode = "// rule code\n";
	string ruleInitCode = "// rule init code\n";
	string stackInitCode = "// stack init code\n";
	string mainLoopCode = "// main loop code\n";
	ExprNode *initVal;
	bool isFloat;
	Instruction::Operand *arg1, *arg2;
	MovIns *mi;

	std::cout<<"Debug: before stackInitCode"<<std::endl;
	stackInitCode += this->stackInit(rm, STACK_START);

	std::cout<<"Debug: before mainLoop"<<std::endl;
	mainLoopCode += this->mainLoop(rm);

	if (st != NULL) {
		auto it = st->begin();
		while(it != st->end()) {
			if ((*it)->name().compare("any") == 0) {
				++it;
				continue;
			}

			std::cout<<"Debug: inwhile loop before var gencode ,var:"<<(*it)->name()<<std::endl;
			if ((*it)->kind() == SymTabEntry::Kind::VARIABLE_KIND) {
				assert(((VariableEntry *)(*it))->varKind() == VariableEntry::VarKind::GLOBAL_VAR);

				tag = (*it)->type()->tag();
				if(tag >= Type::TypeTag::VOID && tag <= Type::TypeTag::CLASS){
					initVal = ((VariableEntry *)(*it))->initVal();
					if (initVal == NULL) {
						++it;
						continue;
					}

					globalVarInitCode += initVal->codeGen(rm);

					// generate initialization code for global var
					// inst1: MOVI offset tmpReg1
					offset = ((VariableEntry *)(*it))->offSet();
					// alloc a caller-save, interger reg
					tmpReg1 = rm->getReg(true, false);
					arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, offset);
					arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg1);
					mi = new MovIns(MovIns::MovInsType::MOVI, arg1, arg2);
					globalVarInitCode += mi->toString();
					
					// inst2: STI/F tmpReg2 tmpReg1
					tmpReg2 =  initVal->getTmpReg();
					isFloat = initVal->isFloat();
					if (isFloat) {
						arg1 = new Instruction::Operand(Instruction::Operand::OperandType::FLOAT_REG, tmpReg2);
						mi = new MovIns(MovIns::MovInsType::STF, arg1, arg2);
						rm->releaseReg(tmpReg2, true);
					} else {
						arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
						mi = new MovIns(MovIns::MovInsType::STI, arg1, arg2);
						rm->releaseReg(tmpReg2, false);
					}

					rm->releaseReg(tmpReg1, false);
					globalVarInitCode += mi->toString();

				} else {
					// unexpected error, all global var should follow into one primitive type
					// TODO
					errMsg("Type Error: unknown type for global variable", this->line(), this->column(), this->file().c_str());
				}
			} else if ((*it)->kind() == SymTabEntry::Kind::FUNCTION_KIND) {
				((FunctionEntry *)(*it))->regManager(new RegManager());
				funcCode += ((FunctionEntry *)(*it))->codeGen(((FunctionEntry *)(*it))->regManager());
			}

			++it;
		}

		int i = 0;
		int size = this->rules().size();

		const RuleNode *rn;
		while (i < size){
			rn = this->rule(i);
			ruleCode += ((RuleNode *)rn)->codeGen(rm);
			ruleInitCode += ((RuleNode *)rn)->ruleInitCode(rm);
			i++;
		}
	}

	return helpInfo + stackInitCode + globalVarInitCode + ruleInitCode + mainLoopCode + funcCode + ruleCode;
}

void GlobalEntry::memAlloc() {
	const SymTab *st = this->symTab();
	Type::TypeTag tag;
	if (st != NULL) {
		auto it = st->begin();
		while(it != st->end()) {
			if ((*it)->name().compare("any") == 0) {
				++it;
				continue;
			}

			if ((*it)->kind() == SymTabEntry::Kind::VARIABLE_KIND) {
				assert(((VariableEntry *)(*it))->varKind() == VariableEntry::VarKind::GLOBAL_VAR);

				tag = (*it)->type()->tag();
				if(tag >= Type::TypeTag::VOID && tag <= Type::TypeTag::CLASS){
					((VariableEntry *)(*it))->offSet(currentOffset);
					// For simplicity, memory is addressed by word, so we treat BOOL, BYTE just
					// the same as INTEGER and sizeof(float) == sizeof(int)
					currentOffset += 1;
				} else {
					// unexpected error, all global var should follow into one primitive type
					// TODO
					errMsg("Type Error: unknown type for global variable", this->line(), this->column(), this->file().c_str());
				}
			} else if ((*it)->kind() == SymTabEntry::Kind::FUNCTION_KIND) {
				// call function entry's own memAlloc()
				((FunctionEntry *)(*it))->memAlloc();
			}

			++it;
		}
	}
}

const Type* GlobalEntry::typeCheck() {
	const SymTab *st = this->symTab();
	if (st != NULL) {
		auto it = st->begin();
		while(it != st->end()) {
			if ((*it)->name().compare("any") == 0) {
				++it;
				continue;
			}

			//std::cout<<(*it)->name()<<endl;
			((SymTabEntry *)(*it))->typeCheck();

			++it;
		}

		int i = 0;
		int size = this->rules().size();

		RuleNode *rn;
		while (i < size){
			rn = this->rule(i);
			rn->typeCheck();
			i++;
		}
	}

	return NULL;
}

void GlobalEntry::typePrint(ostream& out, int indent) const
{
	// TODO
	const SymTab *st = this->symTab();
	if (st != NULL) {
		out << endl;
		auto it = st->begin();
		while(it != st->end()) {
			if ((*it)->name().compare("any") == 0) {
				++it;
				continue;
			}

			if ((*it)->kind() == SymTabEntry::Kind::VARIABLE_KIND) {
				prtSpace(out, indent);
				(*it)->typePrint(out, indent);
				out << ";" << endl;
			} else {
				(*it)->typePrint(out, indent);
			}

			++it;
		}

		out<<endl;

		int i = 0;
		int size = this->rules().size();

		const RuleNode *rn;
		while (i < size){
			rn = this->rule(i);
			rn->typePrint(out, indent);
			i++;
		}
	}

	prtSpace(out, indent);
	out<<endl;
}

void GlobalEntry::print(ostream& out, int indent) const
{
	// Add your code
	const SymTab *st = this->symTab();
	if (st != NULL) {
		out << endl;
		auto it = st->begin();
		while(it != st->end()) {
			if ((*it)->name().compare("any") == 0) {
				++it;
				continue;
			}

			if ((*it)->kind() == SymTabEntry::Kind::VARIABLE_KIND) {
				prtSpace(out, indent);
				(*it)->print(out, indent);
				out << ";" << endl;
			} else {
				(*it)->print(out, indent);
			}

			++it;
		}

		out<<endl;

		int i = 0;
		int size = this->rules().size();

		const RuleNode *rn;
		while (i < size){
			rn = this->rule(i);
			rn->print(out, indent);
			i++;
		}
	}

	prtSpace(out, indent);
	out<<endl;
}

const Type* EventEntry::typeCheck() {
	// TODO

	return NULL;
}

void EventEntry::typePrint(ostream& out, int indent) const {
	// TODO
	prtSpace(out, indent);
	out << "event";
	out << step;
	out << this->name();
	if (this->symTab() != NULL) {
		out << "(";
		auto it = this->symTab()->begin();
		for(; it != this->symTab()->end(); ++it) {
			(*it)->typePrint(out, 0);
			if ((*it)->next() != NULL)
				out<<", ";
		}
		out << ")";
	}
	out << ";";
	out <<endl;
}
void EventEntry::print(ostream& out, int indent) const
{
	// Add your code
	// out << type()->fullName() <<endl;
	prtSpace(out, indent);
	out << "event";
	out << step;
	out << this->name();
	if (this->symTab() != NULL) {
		out << "(";
		auto it = this->symTab()->begin();
		for(; it != this->symTab()->end(); ++it) {
			(*it)->print(out, 0);
			if ((*it)->next() != NULL)
				out<<", ";
		}
		out << ")";
	}
	out << ";";
	out <<endl;
}

VariableEntry::VariableEntry(const VariableEntry &v):
    SymTabEntry(*(new string(v.name())), SymTabEntry::Kind::VARIABLE_KIND, v.line(), v.column(), v.file(), (Type *)v.type()) {
	vkind_ = v.varKind();
	initVal_ = (ExprNode *)v.initVal();
}

void VariableEntry::typePrint(ostream& out, int indent) const
{
	// TODO
	this->type()->print(out, indent);
	out << step;
	out << (this->name());
	if (this->initVal() != NULL) {
		out << " = ";
		this->initVal()->typePrint(out, indent);
	}
}

void VariableEntry::print(ostream& out, int indent) const
{
	// Add your code
	this->type()->print(out, indent);
	out << step;
	out << (this->name());
	if (this->initVal() != NULL) {
		out << " = ";
		this->initVal()->print(out, indent);
	}
}

const Type* VariableEntry::typeCheck() {
	const Type *type;
	const Type* initValType;
	ExprNode *exp;

	type = this->type();
	exp = this->initVal();
	if (exp != NULL) {
		initValType = exp->typeCheck();
		//std::cout<<"before print initValType"<<endl;
		//std::cout<<initValType->name()<<endl;
		//std::cout<<"after print initValType"<<endl;
		if (type == NULL || initValType == NULL) {
			// TODO handle type error here! However, do not propagate type error, return type.
			errMsg("Assignment between incompatible types", this->line(), this->column(), this->file().c_str());
		} else if (!(type->isSubType(initValType))) {
			errMsg("Assignment between incompatible types", this->line(), this->column(), this->file().c_str());
		}
	}

	return type;
}

void BlockEntry::print(ostream& os, int indent) const{
	// Add your code
}

void BlockEntry::typePrint(ostream& os, int indent) const{
	// Add your code
	// TODO
}

const Type* ClassEntry::typeCheck() {
	// TODO
	return NULL;
}

void ClassEntry::typePrint(ostream& os, int indent) const{
	// TODO
	prtSpace(os, indent);
	os << "class";
	os << step;
	os << this->name();
	os << ";";
	os << endl;
}

void ClassEntry::print(ostream& os, int indent) const{
	// Add your code
	prtSpace(os, indent);
	os << "class";
	os << step;
	os << this->name();
	os << ";";
	os << endl;
}

/**
 * After enter function, the following things needs to be done in order
 *	1. push bp into stack
 *	2. move sp to bp
 *	3. sub rsp to allocate space for local vars rsp -= #local_vars
 *	4. push callee save registers(we only allocate callee save registers for func params)
 *	5. read param_n -> param_1 into regs
 * So when return, we should do the following:
 *	0. calcualte ret_val
 *	1. mov retval REG_RV
 *	2. pop param_1 -> param_n(callee save)
 *	3. restore rsp += #local_vars
 *	4. pop bp
 *	5. read REG_RA
 *	6. jmp REG_RA
 */
string FunctionEntry::codeGen(RegManager *rm) {
	string code;
	int tmpReg1, tmpReg2;
	int localVarNum;
	int i, p_num;
	bool isFloat = false;
	Instruction::Operand *arg1, *arg2, *dest;
	MovIns *mi;
	ArithIns *ai;
	vector<int> *paramRegList = new vector<int>();
	vector<bool> *paramRegAtrList = new vector<bool>();

	cout<<"Debug in Func::codeGen, func name:"<<this->name()<<endl;

	// handle funcLabel
	string funcLabel = this->getFuncLabel();
	code += funcLabel + ": "; 

	// initialize param reg list, will be used when return
	this->paramRegList(paramRegList);
	this->paramRegAtrList(paramRegAtrList);

	//1. push bp into stack
	// inst1: STI REG_BP REG_SP
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_BP);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
	mi = new MovIns(MovIns::MovInsType::STI, arg1, arg2);
	code += mi->toString();

	// inst2: SUB REG_SP 1 REG_SP
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, 1);
	dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
	ai = new ArithIns(ArithIns::ArithInsType::SUB, arg1, arg2, dest);
	code += ai->toString();

	// 2. move sp to bp
	// inst1: MOVI REG_SP REG_BP 
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_BP);
	mi = new MovIns(MovIns::MovInsType::MOVI, arg1, arg2);
	code += mi->toString();

	// 3. sub rsp to allocate space for local vars rsp -= #local_vars
	localVarNum = this->localVarNum();
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, localVarNum);
	dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
	ai = new ArithIns(ArithIns::ArithInsType::SUB, arg1, arg2, dest);
	code += ai->toString();

	cout<<"Debug in Func::codeGen, before traverse symTab "<<endl;

	// 4. push callee save registers(we only allocate callee save registers for func params and REG_RA)
	if (this->symTab() != NULL) {
		auto it = this->symTab()->begin();
		p_num = this->type()->arity();
		i = 0;

		// parameters
		for(; (it != this->symTab()->end()) && (i < p_num); ++it, ++i) {
			cout<<"Debug in Func::codeGen, inside for-loop, handle parameters"<<endl;

			if ((*it)->type()->tag() != Type::TypeTag::DOUBLE)
				isFloat = false;
			else 
				isFloat = true;

			// record callee-save regs alloced for param vars
			paramRegAtrList->push_back(isFloat);
			// alloc a callee-save, int/float reg
			tmpReg1 = rm->getReg(false, isFloat);
			paramRegList->push_back(tmpReg1);

			// set param var tmpReg tmpReg1
			((VariableEntry *)(*it))->setTmpReg(tmpReg1);
			((VariableEntry *)(*it))->setIsFloat(isFloat);

			// push allocated reg onto stack to protect their original value

			// inst1: STI tmpReg1 REG_SP
			if (isFloat) {
				arg1 = new Instruction::Operand(Instruction::Operand::OperandType::FLOAT_REG, tmpReg1);
				arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
				mi = new MovIns(MovIns::MovInsType::STF, arg1, arg2);
			} else {
				arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg1);
				arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
				mi = new MovIns(MovIns::MovInsType::STI, arg1, arg2);
			}
			code += mi->toString();

			// inst2: SUB REG_SP 1 REG_SP
			arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
			arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, 1);
			dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_SP);
			ai = new ArithIns(ArithIns::ArithInsType::SUB, arg1, arg2, dest);
			code += ai->toString();
		}
	}

	// 5. read param_n -> param_1 into regs

	// alloc a caller-save, int reg
	tmpReg2 = rm->getReg(true, false);

	// inst: MOVI REG_BP tmpReg2
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, REG_BP);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
	mi = new MovIns(MovIns::MovInsType::MOVI, arg1, arg2);
	code += mi->toString();

	// point tmpReg2 --> param1
	// inst: ADD tmpReg2 3 tmpReg2, skip blank, n params, BP, ret_addr
	arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
	arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, 3 + p_num);
	dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
	ai = new ArithIns(ArithIns::ArithInsType::ADD, arg1, arg2, dest);
	code += ai->toString();

	if (paramRegList->size() > 0) {
		// load params into registers
		auto pit = paramRegList->begin();
		auto pait = paramRegAtrList->begin();
		while (pit != paramRegList->end() && pait != paramRegAtrList->end()) {
			isFloat = (*pait);
			tmpReg1 = (*pit);

			if (isFloat) {
				// inst: LDF tmpReg2 param_i
				arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
				arg2 = new Instruction::Operand(Instruction::Operand::OperandType::FLOAT_REG, tmpReg1);
				mi = new MovIns(MovIns::MovInsType::LDF, arg1, arg2);
			} else {
				// inst: LDI tmpReg2 param_i
				arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
				arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg1);
				mi = new MovIns(MovIns::MovInsType::LDI, arg1, arg2);
			}
			code += mi->toString();

			// inst: ADD tmpReg2 1 tmpReg2 
			arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
			arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, 1);
			dest = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg2);
			ai = new ArithIns(ArithIns::ArithInsType::ADD, arg1, arg2, dest);
			code += ai->toString();

			pit++;
			pait++;
		}
	}

	rm->releaseReg(tmpReg2, false);

	cout<<"Debug in Func::codeGen, before generate code for body"<<endl;

	// add function body code
	if (this->body() != NULL)
		code += this->body()->codeGen(rm);

	cout<<"Debug in Func::codeGen, after generate code for body"<<endl;
	return code;
}

// Stack Layout
//  +------------+
//  | param3     |
//  | param2     |
//  | param1     |
//  | retaddr    |
//  | BP         | <--BP
//  | local var1 |
//  | local var2 |
//  | local var3 |
//  +------------+

void FunctionEntry::memAlloc() {
	Type::TypeTag tag;
	int offsetOnStack = 1;
	int localVarNum = 0;

	if (this->symTab() != NULL) {
		auto it = this->symTab()->begin();
		int p_num = this->type()->arity();
		int i = 0;

		// skip parameters
		for(; (it != this->symTab()->end()) && (i < p_num); ++it, ++i);

		// start allocate memory on stack for local vars
		for(; (it != this->symTab()->end()) && (i >= p_num); ++it, ++i) {
			if ((*it)->kind() == SymTabEntry::Kind::VARIABLE_KIND) {
				assert(((VariableEntry *)(*it))->varKind() == VariableEntry::VarKind::LOCAL_VAR);

				tag = (*it)->type()->tag();
				if(tag >= Type::TypeTag::VOID && tag <= Type::TypeTag::CLASS){
					((VariableEntry *)(*it))->offSet(offsetOnStack);
					// For simplicity, memory is addressed by word, so we treat BOOL, BYTE just
					// the same as INTEGER and sizeof(float) == sizeof(int)
					offsetOnStack += 1;
					localVarNum++;
				} else {
					// unexpected error, all global var should follow into one primitive type
					// TODO
					errMsg("Type Error: unknown type for local variable", this->line(), this->column(), this->file().c_str());
				}
			}
		}
	}

	// set local var number
	this->localVarNum(localVarNum);
}

const Type* FunctionEntry::typeCheck() {
	// TODO
	if (this->body() != NULL)
		this->body()->typeCheck();
	return NULL;
}

void FunctionEntry::typePrint(ostream& os, int indent) const{
	// TODO
	prtSpace(os, indent);
	this->type()->retType()->print(os, indent);
	os << step;
	os << this->name();
	os << "(";
	if (this->symTab() != NULL) {
		auto it = this->symTab()->begin();
		int p_num = this->type()->arity();
		int i = 0;
		for(; (it != this->symTab()->end()) && (i < p_num); ++it, ++i) {
			(*it)->typePrint(os, 0);
			if (i < p_num - 1)
				os<<", ";
		}
	}
	os << ")";
	if (this->body() != NULL) {
		os << "{"<<endl;
		auto it = this->symTab()->begin();
		int p_num = this->type()->arity();
		int i = 0;
		// skip formal parameters
		for (; i < p_num; ++i, ++it){}
		// print var decl in function
		for(; (it != this->symTab()->end()); ++it) {
			prtSpace(os, indent + 2);
			(*it)->typePrint(os, indent + 2);
			os << ";" << endl;
		}

		this->body()->typePrintWithoutBraces(os, indent + 2);

		prtSpace(os, indent);
		os << "}";
	}
	os << ";";
	os <<endl;
}

void FunctionEntry::print(ostream& os, int indent) const{
	// Add your code
	prtSpace(os, indent);
	this->type()->retType()->print(os, indent);
	os << step;
	os << this->name();
	os << "(";
	if (this->symTab() != NULL) {
		auto it = this->symTab()->begin();
		int p_num = this->type()->arity();
		int i = 0;
		for(; (it != this->symTab()->end()) && (i < p_num); ++it, ++i) {
			(*it)->print(os, 0);
			if (i < p_num - 1)
				os<<", ";
		}
	}
	os << ")";
	if (this->body() != NULL) {
		os << "{"<<endl;
		auto it = this->symTab()->begin();
		int p_num = this->type()->arity();
		int i = 0;
		// skip formal parameters
		for (; i < p_num; ++i, ++it){}
		// print var decl in function
		for(; (it != this->symTab()->end()); ++it) {
			prtSpace(os, indent + 2);
			(*it)->print(os, indent + 2);
			os << ";" << endl;
		}

		this->body()->printWithoutBraces(os, indent + 2);

		prtSpace(os, indent);
		os << "}";
	}
	os << ";";
	os <<endl;
}
