#include "Ast.h"					
#include "ParserUtil.h"					


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
	// TODO
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

void PrimitivePatNode::typePrint(ostream& os, int indent) const{
	// TODO
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

void PatNode::typePrint(ostream& os, int indent) const{
	// TODO
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

void ValueNode::print(ostream& os, int indent) const{
	// Add your code
	this->value()->print(os, indent);
}

void ValueNode::typePrint(ostream& os, int indent) const{
	// TODO
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

void RefExprNode::typePrint(ostream& os, int indent) const{
	// TODO
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

void InvocationNode::typePrint(ostream& os, int indent) const{
	// TODO
}

IfNode::IfNode(ExprNode* cond, StmtNode* thenStmt, 
		 StmtNode* elseStmt, int line, int column, string file):
	StmtNode(StmtNode::StmtNodeKind::IF, line, column, file) {
	cond_ = cond;
	then_ = thenStmt;
	else_ = elseStmt;
}

void IfNode::typePrint(ostream& os, int indent) const{
	// TODO
}
void IfNode::print(ostream& os, int indent) const{
	// Add your code
	prtSpace(os, indent);
	os << "if";
	os << "(";
	this->cond()->print(os, indent);
	os << ")";
	if ((this->thenStmt()->stmtNodeKind() != StmtNode::StmtNodeKind::COMPOUND)) { 
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

void RuleNode::typePrint(ostream& os, int indent) const{
	// TODO
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
