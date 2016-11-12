#include "Ast.h"					
#include "ParserUtil.h"					
#define MAXBUF	256

// indicate we are in the scope of rule, to allievate the check of assignments
// where only assignments to global variable is allowed when in rule scope
bool inRuleScope = false;

AstNode::AstNode(NodeType nt, int line, int column, string file):
  ProgramElem(NULL, line, column, file) {
	// Add your code here
	nodeType_ = nt;
}

AstNode::AstNode(const AstNode& ast): ProgramElem(ast) {
	// Add your code here
	nodeType_ = ast.nodeType();
}

/****************************************************************/

ExprNode::ExprNode(ExprNodeType et, const Value* val, int line, int column, 
				   string file):
	AstNode(AstNode::NodeType::EXPR_NODE, line, column, file)
{
	// Add your code here
	exprType_ = et;
	val_ = val;
}


ExprNode::ExprNode(const ExprNode& e) : AstNode(e)
{
	// Add your code here
	exprType_ = e.exprNodeType();
	val_ = e.value();
}
/****************************************************************/
extern const OpNode::OpInfo opInfo[] = {
  // print name, arity, paren_flag, fixity, arg types, out type, constraints
  //
  // Paren_flag -- opnode->print() outputs is surrounded by parenthesis if 
  // this flag is set. As set below, the expression may not print correctly
  // in some rare cases, e.g., ~(b * c) will get printed as ~b * c,
  // which actually corresponds to (~b)*c. To ensure that things get printed
  // correctly all the time, more paren_flags should be set to 1, but this
  // will lead to more clutter in printed output. Basically, what we have done
  // here is to look are expressions by type -- arithmetic, relational, 
  // boolean, bit operations, etc. Within each type, the highest priority 
  // operator is printed without paren. This will work correctly, as long
  // as the language doesn't permit mixing of different types of expressions.
  // But this assumption doesn't always hold, as in the example above. Also,
  // there is an exception to this general approach in the case of unary minus
  // and * -- since (-a)*b and -(a*b) have the same meaning, we can exclude
  // paren for * without an error.
  //
  // Codes for constraints:
  // first character:
  //    N: No additional constraint over what is given by argTypes
  //    I: all arguments must have identical type
  //    S: one of the arguments must have a type that is a supertype of
  //        of all other arguments. All other arguments require a coercion 
  //        operation to be introduced so as to convert their type to S.
  //    s: one of the arguments must have a type that is a subtype of
  //        of all other arguments. 
  //    L: all list arguments (and list output) must have same type. In 
  //        addition, all non-list arguments (and output) must have same 
  //        type as that of elements in these lists
  //    T: all tuple arguments to the function must have same type.
  //    A: (assignment). Type of second argument must be a subtype of
  //       the first argument
  //
  // second character:
  //    O: output type is the same as out type. (In the following cases,
  //        the output type need not be equal to out type, but a subtype
  //        of it.) Since a TypeTag provides complete type information only
  //        for primitive types, `O' is applicable only in this case.
  //    digit: output type is the same as that of the digit'th argument
  //       In this case, a third character may be used, the code for
  //       which is as follows:
  //         'e' denotes that the output is of type alpha, where
  //             the type of digit'th argument is list(alpha)
  //         'l' denotes that the output is of type list(alpha), where
  //             alpha is the type of the digit'th argument.
  //    S: The output type is the same as that of the argument with the
  //        most general type. (Typically used with first character 'S')
  //    s: The output type is the same as that of the argument with the
  //        least general type. (Typically used with first character 'S')
  //    P: The output type is the product of the types of all arguments
  //    p: The output type is a component of the input tuple type. The
  //        following character specifies the component. A digit k specifies
  //        that the component number as k. The character 'a' indicates that
  //        the component number is given by an integer argument to the
  //        operator. The argument number is given by the following digit.
  //        'p' can be used only in conjunction with first character 'P'.
  //    L: Output type is the same as type of list arguments. Can be used
  //        only in conjunction with first character L.
  //    e: Output type is the same as type of element of list arguments. 
  //        Can be used only in conjunction with first character L.
  //
  {OpNode::OpCode::UMINUS, "-",  1, 0, OpNode::OpPrintType::PREFIX, {Type::SIGNED}, Type::SIGNED, "N1"},
  {OpNode::OpCode::PLUS, "+",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS"},
  {OpNode::OpCode::MINUS, "-",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS"},
  {OpNode::OpCode::MULT, "*",  2, 0, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS"},
  {OpNode::OpCode::DIV, "/",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS"},
  {OpNode::OpCode::MOD, "%",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "S2"},
  {OpNode::OpCode::EQ, "==", 2, 0, OpNode::OpPrintType::INFIX, {Type::PRIMITIVE, Type::PRIMITIVE}, Type::BOOL, "SO"},
  {OpNode::OpCode::NE, "!=", 2, 0, OpNode::OpPrintType::INFIX, {Type::PRIMITIVE, Type::PRIMITIVE}, Type::BOOL, "SO"},
  {OpNode::OpCode::GT, ">",  2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO"},
  {OpNode::OpCode::LT, "<",  2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO"},
  {OpNode::OpCode::GE, ">=", 2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO"},
  {OpNode::OpCode::LE, "<=", 2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO"},
  {OpNode::OpCode::AND, "&&",  2, 1, OpNode::OpPrintType::INFIX, {Type::BOOL, Type::BOOL}, Type::BOOL, "NO"},
  {OpNode::OpCode::OR, "||",  2, 1, OpNode::OpPrintType::INFIX, {Type::BOOL, Type::BOOL}, Type::BOOL, "NO"},
  {OpNode::OpCode::NOT, "!",  1, 0, OpNode::OpPrintType::PREFIX, {Type::BOOL}, Type::BOOL, "NO"}, 
  {OpNode::OpCode::BITNOT, "~",  1, 0, OpNode::OpPrintType::PREFIX, {Type::INTEGRAL}, Type::INTEGRAL, "N1"},
  {OpNode::OpCode::BITAND, "&",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "Ss"},
  {OpNode::OpCode::BITOR, "|",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "SS"},
  {OpNode::OpCode::BITXOR, "^",  2, 0, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "SS"},
  {OpNode::OpCode::SHL, "<<", 2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "N1"},
  {OpNode::OpCode::SHR, ">>", 2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "N1"},
  {OpNode::OpCode::ASSIGN, "=",  2, 0, OpNode::OpPrintType::INFIX, {Type::NATIVE, Type::NATIVE}, Type::VOID, "AO"},
  {OpNode::OpCode::PRINT, "print", OpNode::VARIABLE, 1, OpNode::OpPrintType::PREFIX, {Type::NATIVE}, Type::VOID, "NO"},
  {OpNode::OpCode::INVALID, "invalid",            0, 0, OpNode::OpPrintType::PREFIX, {}, Type::ERROR, "NO"}
};

OpNode::OpNode(OpCode op, ExprNode* a1, ExprNode* a2, 
			   int ln, int col, string file):
  ExprNode(ExprNode::ExprNodeType::OP_NODE, NULL, ln,col,file) {
  opCode_ = op;
  if (a1 != NULL) {
	arity_ = 1;
	arg_.push_back(a1);
	if (a2 != NULL) {
	  arity_++;
	  arg_.push_back(a2);
	}
  }
}

OpNode::OpNode(const OpNode &other):
  ExprNode(other) {
  arity_ = other.arity();
  opCode_ = other.opCode();
  for (unsigned int i=0; (i < other.arity()); i++) {
    if (other.arg_[i]) {
      arg_.push_back((other.arg_[i])->clone());
    } 
	else {
      arg_.push_back(NULL);
    }
  }
}

const Type* OpNode::typeCheck() {
	const Type *type, *ltype, *rtype;
	char msg[MAXBUF];

	OpCode opCode = this->opCode();
	ltype = this->arg(0)->typeCheck();
	if (this->arg(1) != NULL)
		rtype = this->arg(1)->typeCheck();
	int argIdx = 0;

	switch(opCode) {
		case OpNode::OpCode::UMINUS:
			if(!ltype->isNumeric(ltype->tag())) {
				// TODO handle type error here
				sprintf(msg,"Incompatible type for argument 1 for operator `%s'",opInfo[(int)opCode].name_);
				errMsg(msg, this->line(), this->column(), this->file().c_str());
			}

			if (ltype->isIntegral(ltype->tag()))
				type = new Type::Type(Type::TypeTag::INT);
			else
				type = ltype;
			break;
		case OpNode::OpCode::PLUS:
		case OpNode::OpCode::MINUS:
		case OpNode::OpCode::MULT:
		case OpNode::OpCode::DIV:
			if (!ltype->isNumeric(ltype->tag()) || !rtype->isNumeric(rtype->tag())) {
				// TODO handle type error here
				if (!ltype->isNumeric(ltype->tag()))
					argIdx = 1;
				else
					argIdx = 2;
				sprintf(msg,"Incompatible type for argument %d for operator `%s'",argIdx,opInfo[(int)opCode].name_);
				errMsg(msg, this->line(), this->column(), this->file().c_str());
				// TODO should we create a numeric type to avoid propagate of type errors?
				// maybe we can choose the numeric type and return
				if (ltype->isNumeric(ltype->tag()))
					type = ltype;
				else if (rtype->isNumeric(rtype->tag()))
					type = rtype;
				else
					type = new Type(Type::TypeTag::INT);
					//type = NULL;

			} else if (ltype->tag() == rtype->tag()) {
				type = ltype;
			} else if (ltype->isSubType(rtype) || rtype->isSubType(ltype)) {
				// type not match, set coerced type here
				if (ltype->tag() > rtype->tag()) {
					type = ltype;
					this->arg(1)->coercedType(type);
				} else {
					type = rtype;
					this->arg(0)->coercedType(type);
				}

			} else {
				// TODO
				errMsg("type error: unexpected type error");
				// type = new Type(Type::TypeTag::INT);
				type = NULL;
			}
			break;
		case OpNode::OpCode::BITAND:
		case OpNode::OpCode::BITOR:
		case OpNode::OpCode::BITXOR:
		case OpNode::OpCode::SHL:
		case OpNode::OpCode::SHR:
		case OpNode::OpCode::MOD:
			if ((! ltype->isIntegral(ltype->tag())) || (! rtype->isIntegral(rtype->tag()))) {
				// TODO handle type error here
				if (!ltype->isIntegral(ltype->tag()))
					argIdx = 1;
				else
					argIdx = 2;
				sprintf(msg,"Incompatible type for argument %d for operator `%s'",argIdx, opInfo[(int)opCode].name_);
				errMsg(msg, this->line(), this->column(), this->file().c_str());
				// TODO should we create a numeric type to avoid propagate of type errors?
				// maybe we can choose the numeric type and return
				if (ltype->isIntegral(ltype->tag()))
					type = ltype;
				else if (rtype->isIntegral(rtype->tag()))
					type = rtype;
				else
					// type = new Type(Type::TypeTag::INT);
					type = NULL;
			} else if (ltype->tag() == rtype->tag()) {
				type = ltype;
			} else {
				// type not match, set coerced type here
				if (ltype->tag() > rtype->tag()) {
					type = ltype;
					this->arg(1)->coercedType(type);
				} else {
					type = rtype;
					this->arg(0)->coercedType(type);
				}
			}
			break;

		case OpNode::OpCode::BITNOT:
			if (! ltype->isIntegral(ltype->tag())) {
				// TODO handle type error here
				sprintf(msg,"Incompatible type for argument 1 for operator `%s'",opInfo[(int)opCode].name_);
				errMsg(msg, this->line(), this->column(), this->file().c_str());
				//type = NULL;
				type = new Type(Type::TypeTag::INT);
			} else {
				type = ltype;
			}
			break;

		case OpNode::OpCode::EQ:
		case OpNode::OpCode::NE:
		case OpNode::OpCode::GT:
		case OpNode::OpCode::LT:
		case OpNode::OpCode::GE:
		case OpNode::OpCode::LE:
			if (!ltype->isNumeric(ltype->tag()) || !rtype->isNumeric(rtype->tag())) {
				// TODO handle type error here
				if (!ltype->isIntegral(ltype->tag()))
					argIdx = 1;
				else
					argIdx = 2;
				sprintf(msg,"Incompatible type for argument %d for operator `%s'",argIdx,opInfo[(int)opCode].name_);
				errMsg(msg, this->line(), this->column(), this->file().c_str());

			} else if (ltype->tag() == rtype->tag()) {
				// Do nothing
			} else if (ltype->isSubType(rtype) || rtype->isSubType(ltype)) {
				// type not match, set coerced type here
				if (ltype->tag() > rtype->tag()) {
					this->arg(1)->coercedType(type);
				} else {
					this->arg(0)->coercedType(type);
				}
			} else {
				// TODO
				errMsg("type error: unexpected type error");
			}

			// return bool type
			type = new Type(Type::TypeTag::BOOL);
			break;
		case OpNode::OpCode::NOT:
			if (! ltype->isBool(ltype->tag())) {
				// TODO handle type error here
				sprintf(msg,"Incompatible type for argument 1 for operator `%s'",opInfo[(int)opCode].name_);
				errMsg(msg, this->line(), this->column(), this->file().c_str());
			}
			type = new Type(Type::TypeTag::BOOL);
			break;
		case OpNode::OpCode::AND:
		case OpNode::OpCode::OR:
			if (!ltype->isBool(ltype->tag()) || !rtype->isBool(rtype->tag())) {
				// TODO handle type error here
				if (!ltype->isBool(ltype->tag()))
					argIdx = 1;
				else
					argIdx = 2;
				sprintf(msg,"Incompatible type for argument %d for operator `%s'",argIdx,opInfo[(int)opCode].name_);
				errMsg(msg, this->line(), this->column(), this->file().c_str());
			}
			type = new Type(Type::TypeTag::BOOL);
			break;
		case OpNode::OpCode::ASSIGN:
			if (ltype->tag() == rtype->tag()) {
				// Do nothing
			} else if (ltype->isSubType(rtype)) {
					this->arg(1)->coercedType(ltype);
			} else {
				// TODO
				errMsg("Assigned expression must be a subtype of target", this->line(), this->column(), this->file().c_str());
			}

			// extra check for assignment if in rule scope
			if (inRuleScope) {
				SymTabEntry *ste = stm.lookUp(((RefExprNode *)this->arg(0))->ext());
				// because we are in global scope now, so if we can find symtabentry, it's ok
				if (ste == NULL)
					errMsg("type error: assignment inside rule can only assign to global variables", this->line(), this->column(), this->file().c_str());
			}

			// return bool type, TODO for assignment, we are required to return true!
			type = new Type(Type::TypeTag::BOOL);
			break;
		default:
			errMsg("Unsupported OpCode: type check not support here, default action: return ltype");
			type = ltype;
			break;
	}

	return type;
}

void 
OpNode::print(ostream& os, int indent) const {
	int iopcode = static_cast<int>(opCode_);
  if (opInfo[iopcode].prtType_ == OpNode::OpPrintType::PREFIX) {
	os << opInfo[iopcode].name_;
	if (arity_ > 0) {
	  if (opInfo[iopcode].needParen_) 
		os << '(';
	  for (unsigned i=0; i < arity_-1; i++) {
		if (arg_[i])
		  arg_[i]->print(os, indent);
	    else os << "NULL";
		os << ", ";
	  }
      if (arg_[arity_-1])
		arg_[arity_-1]->print(os, indent);
	  else os << "NULL";
	  if (opInfo[iopcode].needParen_) 
		os << ") ";
	}
  }
  else if ((opInfo[iopcode].prtType_ == OpNode::OpPrintType::INFIX) && (arity_ == 2)) {
	if (opInfo[iopcode].needParen_) 
	  os << "(";
	if(arg_[0])
	  arg_[0]->print(os, indent);
	else os << "NULL";
	os << opInfo[iopcode].name_; 
	if(arg_[1])
	  arg_[1]->print(os, indent);
	else os << "NULL";
	if (opInfo[iopcode].needParen_) 
	  os << ")";
  }
  else internalErr("Unhandled case in OpNode::print");
}

void 
OpNode::typePrint(ostream& os, int indent) const {
	int iopcode = static_cast<int>(opCode_);
  if (opInfo[iopcode].prtType_ == OpNode::OpPrintType::PREFIX) {
	os << opInfo[iopcode].name_;
	if (arity_ > 0) {
	  if (opInfo[iopcode].needParen_) 
		os << '(';
	  for (unsigned i=0; i < arity_-1; i++) {
		if (arg_[i])
		  arg_[i]->typePrint(os, indent);
	    else os << "NULL";
		os << ", ";
	  }
      if (arg_[arity_-1])
		arg_[arity_-1]->typePrint(os, indent);
	  else os << "NULL";
	  if (opInfo[iopcode].needParen_) 
		os << ") ";
	}
  }
  else if ((opInfo[iopcode].prtType_ == OpNode::OpPrintType::INFIX) && (arity_ == 2)) {
	if (opInfo[iopcode].needParen_) 
	  os << "(";
	if(arg_[0])
	  arg_[0]->typePrint(os, indent);
	else os << "NULL";
	os << opInfo[iopcode].name_; 
	if(arg_[1])
	  arg_[1]->typePrint(os, indent);
	else os << "NULL";
	if (opInfo[iopcode].needParen_) 
	  os << ")";
  }
  else internalErr("Unhandled case in OpNode::print");
}

PrimitivePatNode::PrimitivePatNode(EventEntry* ee, vector<VariableEntry*>* params,
				   ExprNode* c,
				   int line, int column, string file):
	BasePatNode(BasePatNode::PatNodeKind::PRIMITIVE, line, column, file) {
	ee_ = ee;
	params_ = params;
	// TODO we only consider cond_ here. condition_ seems useless so far.
	cond_ = c;
	condition_ = nullptr;
}

bool PrimitivePatNode::hasStar() const{
	return false;
}

bool PrimitivePatNode::hasSeqOps() const{
	return false;
}

bool PrimitivePatNode::hasNeg() const{
	return false;
}

bool PrimitivePatNode::hasAnyOrOther() const{
	// TODO what does this function mean? Any means "any" event ???
	if (ee_->name().compare(string("any")) == 0)
		return true;
	else
		return false;
}

const Type* PrimitivePatNode::typeCheck() {
	// TODO
	// Acutal parameters to an event will have the types given by the declaration for that event
	// we don't need to check it now, they will only be read in during runtime.
	// first check event paramters numbers
	EventEntry *ee = this->ee_;
	const Type *type = ee->EventEntry::type();
	const vector<const Type*>* argTypes = type->Type::argTypes();
	vector<VariableEntry*>* params = this->params();
	char msg[MAXBUF];

	if (argTypes == NULL) {
		if (params == NULL) {
			// good thing
		} else {
			// bad thing
			sprintf(msg,"Event %s require %d arguments", ee->SymTabEntry::name().c_str(), 0);
			errMsg(msg, this->line(), this->column(), this->file().c_str());
		}
	} else {
		if (params == NULL) {
			// bad thing
			sprintf(msg,"Event %s require %d arguments", ee->SymTabEntry::name().c_str(), argTypes->size());
			errMsg(msg, this->line(), this->column(), this->file().c_str());
		} else if(params->size() != argTypes->size()){
			// bad thing
			sprintf(msg,"Event %s require %d arguments", ee->SymTabEntry::name().c_str(), argTypes->size());
			errMsg(msg, this->line(), this->column(), this->file().c_str());
		}
	} 
	return NULL;
}

void PrimitivePatNode::typePrint(ostream& os, int indent) const{
	// TODO
	os << "(";
	cout << this->event()->name();
	if (this->event()->name().compare("any") != 0) {
		os << "(";
		if (this->params() != NULL) {
			auto it = this->params()->begin();
			int i, size;
			size = this->params()->size();
			for(i = 0;it != this->params()->end(); ++it, ++i) {
				(*it)->typePrint(os, 0);
				if (i < size - 1)
					os<<", ";
			}
		}
		os << ")";
	}

	if (this->cond() != NULL) {
		os << "|";
		this->cond()->typePrint(os, indent);
	}

	os << ")";

}

void PrimitivePatNode::print(ostream& os, int indent) const{
	// Add your code
	os << "(";
	cout << this->event()->name();
	if (this->event()->name().compare("any") != 0) {
		os << "(";
		if (this->params() != NULL) {
			auto it = this->params()->begin();
			int i, size;
			size = this->params()->size();
			for(i = 0;it != this->params()->end(); ++it, ++i) {
				(*it)->print(os, 0);
				if (i < size - 1)
					os<<", ";
			}
		}
		os << ")";
	}

	if (this->cond() != NULL) {
		os << "|";
		this->cond()->print(os, indent);
	}

	os << ")";

}

PatNode::PatNode(PatNodeKind pk, BasePatNode *p1, BasePatNode *p2, int line, int column, string file):
	BasePatNode(pk, line, column, file) {

	pat1_ = p1;
	pat2_ = p2;
}

bool PatNode::hasNeg() const{
	bool p1 = false, p2 = false;
	if (kind() == BasePatNode::PatNodeKind::NEG)
		return true;
	else {
		if (pat1() != nullptr)
			p1 = pat1()->hasNeg();
		if (pat2() != nullptr)
			p2 = pat2()->hasNeg();

		return p1 | p2;
	}
}

bool PatNode::hasStar() const{
	bool p1 = false, p2 = false;

	if (kind() == BasePatNode::PatNodeKind::STAR)
		return true;
	else {
		if (pat1() != nullptr)
			p1 = pat1()->hasSeqOps();
		if (pat2() != nullptr)
			p2 = pat2()->hasSeqOps();

		return p1 | p2;
	}
}

bool PatNode::hasSeqOps() const{
	bool p1 = false, p2 = false;

	if (kind() == BasePatNode::PatNodeKind::SEQ)
		return true;
	else {
		if (pat1() != nullptr)
			p1 = pat1()->hasSeqOps();
		if (pat2() != nullptr)
			p2 = pat2()->hasSeqOps();

		return p1 | p2;
	}
}

// TODO don't understand hasAnyOrOther() means? Any event and sth like STAR ?
// Only consider any here.
bool PatNode::hasAnyOrOther() const{
	bool p1 = false, p2 = false;

	if (pat1() != nullptr)
		p1 = pat1()->hasAnyOrOther();
	if (pat2() != nullptr)
		p2 = pat2()->hasAnyOrOther();

	return p1 | p2;
}

const Type* PatNode::typeCheck() {
	BasePatNode::PatNodeKind pnk = this->kind();
	if (pnk == BasePatNode::PatNodeKind::NEG) {
		this->pat1()->typeCheck();
		if (this->pat1()->hasSeqOps() || this->pat1()->hasStar())
			errMsg("Only simple patterns without `.', `*', and `!' operatorscan be negated", this->line(), this->column(), this->file().c_str());
	} else {
		if (this->pat1() != NULL)
			this->pat1()->typeCheck();
		if (this->pat2() != NULL)
			this->pat2()->typeCheck();
	}

	return NULL;
}

void PatNode::typePrint(ostream& os, int indent) const{
	// TODO
	// Add your code
	os << "(";
	switch(this->kind()) {
		case BasePatNode::PatNodeKind::NEG :
			os << "!";
			this->pat1()->typePrint(os, indent);
			break;
		case BasePatNode::PatNodeKind::STAR :
			this->pat1()->typePrint(os, indent);
			os << " **";
			break;
		case BasePatNode::PatNodeKind::OR :
			this->pat1()->typePrint(os, indent);
			os << " \\/ ";
			this->pat2()->typePrint(os, indent);
			break;
		case BasePatNode::PatNodeKind::SEQ :
			this->pat1()->typePrint(os, indent);
			os << " : ";
			this->pat2()->typePrint(os, indent);
			break;
		default:
			os << "ERROR: undefined PatNodeKind for PatNode"<<endl;
			break;
	}
	os << ")";
}

void PatNode::print(ostream& os, int indent) const{
	// Add your code
	os << "(";
	switch(this->kind()) {
		case BasePatNode::PatNodeKind::NEG :
			os << "!";
			this->pat1()->print(os, indent);
			break;
		case BasePatNode::PatNodeKind::STAR :
			this->pat1()->print(os, indent);
			os << " **";
			break;
		case BasePatNode::PatNodeKind::OR :
			this->pat1()->print(os, indent);
			os << " \\/ ";
			this->pat2()->print(os, indent);
			break;
		case BasePatNode::PatNodeKind::SEQ :
			this->pat1()->print(os, indent);
			os << " : ";
			this->pat2()->print(os, indent);
			break;
		default:
			os << "ERROR: undefined PatNodeKind for PatNode"<<endl;
			break;
	}
	os << ")";
}

const Type* ValueNode::typeCheck() {
	const Value* val = this->value();
	if (val != NULL) {
		//std::cout << "val != NULL" << endl;
		//this->print(std::cout, 0);
		//std::cout<<endl;
		return this->type();
	 } else {
		//std::cout << "val == NULL" << endl;
		return new Type::Type(Type::TypeTag::VOID);
	}
}

void ValueNode::print(ostream& os, int indent) const{
	// Add your code
	this->value()->print(os, indent);
}

void ValueNode::typePrint(ostream& os, int indent) const{
	// TODO
	const Type *ctype = this->coercedType();
	if (ctype != NULL)
		os << "(" << Type::name(ctype->tag()) << ")" <<" ";
	this->value()->typePrint(os, indent);
}

RefExprNode::RefExprNode(string ext, const SymTabEntry* ste,
	      int line, int column, string file):
	ExprNode(ExprNode::ExprNodeType::REF_EXPR_NODE, NULL, line, column, file){
	// TODO
	ext_ = ext;
	sym_ = ste;
}

void RefExprNode::print(ostream& os, int indent) const{
	// Add your code
	os << this->ext();
}

const Type* RefExprNode::typeCheck() {
	// TODO
	return this->symTabEntry()->type();
}

void RefExprNode::typePrint(ostream& os, int indent) const{
	// TODO
	//os << this->ext();
	const Type *ctype = this->coercedType();
	if (ctype != NULL)
		os << "(" << Type::name(ctype->tag()) << ")" <<" ";
	os << Type::name(this->symTabEntry()->type()->tag());
}

// TODO don't know what kind of copy here it is, should we copy ext_???
RefExprNode::RefExprNode(const RefExprNode& ren):
	ExprNode(ExprNode::ExprNodeType::REF_EXPR_NODE){
	ext_ = ren.ext();
	sym_ = ren.symTabEntry();
}

InvocationNode::InvocationNode(const SymTabEntry *ste, vector<ExprNode*>* param,
		 int line, int column, string file):
	ExprNode(ExprNode::ExprNodeType::INV_NODE) {
	ste_ = ste;
	params_ = param;
}

InvocationNode::InvocationNode(const InvocationNode& in):
	ExprNode(ExprNode::ExprNodeType::INV_NODE) {
	ste_ = in.symTabEntry();
	params_ = (vector <ExprNode*> *)in.params();
}

void InvocationNode::print(ostream& os, int indent) const{
	// Add your code
	os << this->symTabEntry()->name();
	os << "(";
	if (this->params() != NULL) {
		auto it = this->params()->begin();
		for(;it != this->params()->end(); ++it) {
			(*it)->print(os, 0);
			if ((it + 1) != this->params()->end())
				os<<", ";
		}
	}
	os << ")";
}

const Type* InvocationNode::typeCheck() {
	// TODO check parametere
	const SymTabEntry *ste = this->symTabEntry();
	const Type *ftype = ste->type();
	const Type *formalType,*actualType;
	const vector<const Type*>* argTypes = ftype->argTypes();
	vector<ExprNode*>* params = this->params();
	char msg[MAXBUF];

	int i = 0, expSize = 0, actSize = 0;
	if (argTypes != NULL)
		expSize = argTypes->size();
	if (params != NULL)
		actSize = params->size();

	if (actSize != expSize) {
		sprintf(msg,"%d arguments expected for %s", expSize, ste->SymTabEntry::name().c_str());
		errMsg(msg, this->line(), this->column(), this->file().c_str());

		return ftype->retType();
	} else if (actSize == 0) {
		return ftype->retType();
	}

	auto it = argTypes->begin();
	auto pa = params->begin();
	for (; (it != argTypes->end()) && (pa != params->end()); ++it, ++pa) {
		++i;
		formalType = (*it);
		actualType = (*pa)->typeCheck();
		if (formalType == NULL || actualType == NULL) {
			sprintf(msg,"Type mismatch for argument %d to %s", i, ste->SymTabEntry::name().c_str());
			errMsg(msg, this->line(), this->column(), this->file().c_str());
		} else if (formalType->tag() == actualType->tag()) {
			// type match exactly, do nothing
			if (formalType->tag() == Type::TypeTag::CLASS) {
				const SymTabEntry *lste, *rste;
				lste = formalType->typeDesc();
				rste = actualType->typeDesc();
				if (lste != rste) {
					sprintf(msg,"Type mismatch for argument %d to %s", i, ste->SymTabEntry::name().c_str());
					errMsg(msg, this->line(), this->column(), this->file().c_str());
				}
			}
		} else {
			if (formalType->isSubType(actualType)) {
				// coerced type needed
				(*pa)->coercedType(formalType);
			} else {
				sprintf(msg,"Type mismatch for argument %d to %s", i, ste->SymTabEntry::name().c_str());
				errMsg(msg, this->line(), this->column(), this->file().c_str());
				//errMsg("type error: function parameter type not match");
			}
		}
	}

	return ftype->retType();
}

void InvocationNode::typePrint(ostream& os, int indent) const{
	// TODO
	os << this->symTabEntry()->name();
	os << "(";
	if (this->params() != NULL) {
		auto it = this->params()->begin();
		for(;it != this->params()->end(); ++it) {
			(*it)->typePrint(os, 0);
			if ((it + 1) != this->params()->end())
				os<<", ";
		}
	}
	os << ")";
}

IfNode::IfNode(ExprNode* cond, StmtNode* thenStmt, 
		 StmtNode* elseStmt, int line, int column, string file):
	StmtNode(StmtNode::StmtNodeKind::IF, line, column, file) {
	cond_ = cond;
	then_ = thenStmt;
	else_ = elseStmt;
}

const Type* IfNode::typeCheck() {
	const Type *type;
	ExprNode *en = this->cond();

	type = en->typeCheck();
	if (! (type->isBool(type->tag()))) {
		errMsg("Boolean argument expected", en->line(), en->column(), en->file().c_str());
	}

	if (then_ != NULL)
		then_->typeCheck();
	if (else_ != NULL)
		else_->typeCheck();

	// nobody care about the return type here.
	return NULL;
}

void IfNode::typePrint(ostream& os, int indent) const{
	// TODO
	prtSpace(os, indent);
	os << "if";
	os << "(";
	this->cond()->typePrint(os, indent);
	os << ")";
	if (this->thenStmt() == NULL) {
		// do nothing
	} else if ((this->thenStmt()->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND)) { 
		prtSpace(os, indent + 2);
		this->thenStmt()->typePrint(os, indent + 2);
		os << ";";
		os << endl;
	} else {
		
		(this->thenStmt())->typePrint(os, indent + 2);
	}

	if (this->elseStmt() != NULL) {
		prtSpace(os, indent);
		os << "else "; 

		if ((this->elseStmt()->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND)){
			prtSpace(os, indent + 2);
			this->elseStmt()->typePrint(os, indent + 2);
			os << ";";
			os << endl;
		} else {
			(this->elseStmt())->typePrint(os, indent + 2);
		}
	}
}

void IfNode::print(ostream& os, int indent) const{
	// Add your code
	prtSpace(os, indent);
	os << "if";
	os << "(";
	this->cond()->print(os, indent);
	os << ")";
	if (this->thenStmt() == NULL) {
		// do nothing
	} else if ((this->thenStmt()->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND)) { 
		prtSpace(os, indent + 2);
		this->thenStmt()->print(os, indent + 2);
		os << ";";
		os << endl;
	} else {
		
		(this->thenStmt())->print(os, indent + 2);
/*
		os << step;
		os << "{";
		os << endl; 
		((CompoundStmtNode *)(this->thenStmt()))->printWithoutBraces(os, indent + 2);
		prtSpace(os, indent);
		os << "}";
		os << endl; 
*/	}

	if (this->elseStmt() != NULL) {
		prtSpace(os, indent);
		os << "else "; 

		if ((this->elseStmt()->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND)){
			prtSpace(os, indent + 2);
			this->elseStmt()->print(os, indent + 2);
			os << ";";
			os << endl;
		} else {
			(this->elseStmt())->print(os, indent + 2);
		}
	}
}

RuleNode::RuleNode(BlockEntry *re, BasePatNode* pat, StmtNode* reaction, 
	int line, int column, string file):
	AstNode(AstNode::NodeType::RULE_NODE, line, column, file) {
	rste_ = re;
	pat_ = pat;
	reaction_ = reaction;
}

const Type* RuleNode::typeCheck() {
	// TODO we use a global variable to indicate we are current in the scope of rule
	inRuleScope = true;

	// pat check
	pat_->typeCheck();

	// statement type check
	reaction_->typeCheck();

	inRuleScope = false;

	return NULL;
}

void RuleNode::typePrint(ostream& os, int indent) const{
	// TODO
	prtSpace(os, indent);
	this->pat()->typePrint(os, indent);
	os << "-->";
	this->reaction()->typePrint(os, indent);
	prtSpace(os, indent);
	os << ";;" << endl;
}

void RuleNode::print(ostream& os, int indent) const{
	// Add your code
	prtSpace(os, indent);
	this->pat()->print(os, indent);
	os << "-->";
	this->reaction()->print(os, indent);
	prtSpace(os, indent);
	os << ";;" << endl;
}

void  CompoundStmtNode::typePrintWithoutBraces(ostream& os, int indent) const{
	// Add your code
	if (this->stmts() != NULL) {
		auto it = this->stmts()->begin();
		for (; it != this->stmts()->end(); ++it) {
			if (((*it)->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND) ||
				((*it)->stmtNodeKind() != StmtNode::StmtNodeKind::IF)) {
				prtSpace(os, indent);
				(*it)->typePrint(os, indent);
				os << ";" << endl;
			} else
				(*it)->typePrint(os, indent);
		}
	}
}

void  CompoundStmtNode::printWithoutBraces(ostream& os, int indent) const{
	// Add your code
	if (this->stmts() != NULL) {
		auto it = this->stmts()->begin();
		for (; it != this->stmts()->end(); ++it) {
			if (((*it)->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND) ||
				((*it)->stmtNodeKind() != StmtNode::StmtNodeKind::IF)) {
				prtSpace(os, indent);
				(*it)->print(os, indent);
				os << ";" << endl;
			} else
				(*it)->print(os, indent);
		}
	}
}

void  CompoundStmtNode::typePrint(ostream& os, int indent) const{
	// TODO
	if (this->stmts()->size() > 0) {
		prtSpace(os, indent);
		os << "{" << endl;
		auto it = this->stmts()->begin();
		for (; it != this->stmts()->end(); ++it) {
			if (((*it)->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND) && 
				((*it)->stmtNodeKind() != StmtNode::StmtNodeKind::IF)) {
				prtSpace(os, indent + 2);
				(*it)->typePrint(os, indent + 2);
				os << ";" << endl;
			} else
				(*it)->typePrint(os, indent + 2);
		}
		prtSpace(os, indent);
		os << "};" << endl;
	} else {
		prtSpace(os, indent);
		os << "{};" << endl;
	}
}

const Type* CompoundStmtNode::typeCheck() {
	list<StmtNode*>* stmts = this->stmts();
	if (stmts == NULL)
		return NULL;

	auto it = stmts->begin();

	for (;it != stmts->end(); ++it) {
		if ((*it) != NULL)
			(*it)->typeCheck();
	}

	return NULL;
}

void  CompoundStmtNode::print(ostream& os, int indent) const{
	// Add your code
	if (this->stmts()->size() > 0) {
		prtSpace(os, indent);
		os << "{" << endl;
		auto it = this->stmts()->begin();
		for (; it != this->stmts()->end(); ++it) {
			if (((*it)->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND) && 
				((*it)->stmtNodeKind() != StmtNode::StmtNodeKind::IF)) {
				prtSpace(os, indent + 2);
				(*it)->print(os, indent + 2);
				os << ";" << endl;
			} else
				(*it)->print(os, indent + 2);
		}
		prtSpace(os, indent);
		os << "};" << endl;
	} else {
		prtSpace(os, indent);
		os << "{};" << endl;
	}
}

const Type* ReturnStmtNode::typeCheck() {
	const Type * funType = fun_->type();
	const Type * retType = funType->retType();
	const Type * retValType;
	if (expr_ != NULL)
		retValType = expr_->typeCheck();

	if ((retType->Type::tag() == Type::TypeTag::VOID) && (expr_ != NULL)) {
		errMsg("No return value expected for a void function", this->line(), this->column(), this->file().c_str());
	} else if (! retType->isSubType(retValType)) {
		errMsg("Return value incompatible with current function's type", this->line(), this->column(), this->file().c_str());
	}

	return retType;
}
