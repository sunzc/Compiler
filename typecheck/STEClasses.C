#include "STEClasses.h"
#include "Value.h"
#include "ParserUtil.h"

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
