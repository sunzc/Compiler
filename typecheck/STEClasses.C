#include "STEClasses.h"
#include "Value.h"
#include "ParserUtil.h"
#include "Instruction.h"

// Global Data Memory Allocation: record current offset for global data
int currentOffset = 0;

string GlobalEntry::codeGen(RegManager *rm) {
	const SymTab *st = this->symTab();
	Type::TypeTag tag;
	int offset;
	// tempReg: reg name like "R001", "F009"
	int tmpReg1, tmpReg2;
	string initCode = "";
	string funcCode = "";
	string ruleCode = "";
	ExprNode *initVal;
	bool isFloat;
	Instruction::Operand *arg1, *arg2;
	MovIns *mi;

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
					initVal = ((VariableEntry *)(*it))->initVal();
					if (initVal == NULL)
						continue;

					initCode += initVal->codeGen(rm);

					// generate initialization code for global var
					// inst1: MOVI offset tmpReg1
					offset = ((VariableEntry *)(*it))->offSet();
					// alloc a caller-save, interger reg
					tmpReg1 = rm->getReg(true, false);
					arg1 = new Instruction::Operand(Instruction::Operand::OperandType::INT_CONST, offset);
					arg2 = new Instruction::Operand(Instruction::Operand::OperandType::INT_REG, tmpReg1);
					mi = new MovIns(MovIns::MovInsType::MOVI, arg1, arg2);
					initCode += mi->toString();
					
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
					initCode += mi->toString();

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
			i++;
		}
	}

	return initCode + funcCode + ruleCode;
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

string FunctionEntry::codeGen(RegManager *rm) {
	// TODO
	string code;
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
				} else {
					// unexpected error, all global var should follow into one primitive type
					// TODO
					errMsg("Type Error: unknown type for local variable", this->line(), this->column(), this->file().c_str());
				}
			}
		}
	}
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
