#ifndef AST_H
#define AST_H

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

class AST; // Abstract Syntax Tree
class MSM; // list of statements for main
class Statement; // Top-level statement
class Expression; // Binary operation of numbers and identifiers
class AssignStatement; // a = 2;
class DecStatement; // int a = 2;
class IfStatement; 
class ElifStatement;
class ElseStatement;
class Condition; // expression == expression
class LoopStatement;
class Conditions; // "and" and "or" for condition
class Final; // identifiers or numbers

class ASTVisitor
{
public:
    // Virtual visit functions for each AST node type
    virtual void visit(AST &) {}
    virtual void visit(Expression &) {}
    virtual void visit(MSM &) = 0;
    virtual void visit(Statement &) = 0;
    virtual void visit(DecStatement &) = 0;
    virtual void visit(AssignStatement &) = 0;
    virtual void visit(IfStatement &) = 0;
    virtual void visit(ElifStatement &) = 0;
    virtual void visit(ElseStatement &) = 0;
    virtual void visit(Condition &) = 0;
    virtual void visit(LoopStatement &) = 0;
    virtual void visit(Conditions &) = 0;
    virtual void visit(Final &) = 0;
};

class AST
{
public:
    virtual ~AST() {}
    virtual void accept(ASTVisitor &V) = 0;
};

class MSM : public AST
{
private:
    llvm::SmallVector<Statement *> statements; // Stores the list of expressions

public:
    MSM(llvm::SmallVector<Statement *> Statements) : statements(Statements) {}
    llvm::SmallVector<Statement *> getStatements() { return statements; }

    llvm::SmallVector<Statement *>::const_iterator begin() { return statements.begin(); }

    llvm::SmallVector<Statement *>::const_iterator end() { return statements.end(); }
    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class Statement : public AST
{
public:
    enum StatementType
    {
        Declaration,
        Assignment,
        If,
        Loop
    };

private:
    StatementType Type;

public:
    StatementType getKind()
    {
        return Type;
    }


    Statement(StatementType type) : Type(type) {}
    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class IfStatement : public Statement
{

private:
    Conditions *condition;
    llvm::SmallVector<AssignStatement *> Assignments;
    llvm::SmallVector<ElifStatement *> Elifs;
    ElseStatement *Else;

public:
    IfStatement(Conditions *condition, llvm::SmallVector<AssignStatement *> Assignments,llvm::SmallVector<ElifStatement *> Elifs,ElseStatement *Else) : 
    condition(condition), Assignments(Assignments), Statement(Statement::StatementType::If), Elifs(Elifs), Else(Else) {}
    IfStatement(): Statement(Statement::StatementType::If) {}

    Conditions *getCondition()
    {
        return condition;
    }

    llvm::SmallVector<AssignStatement *> getAssignments()
    {
        return Assignments;
    }

    llvm::SmallVector<ElifStatement *> getElifs()
    {
        return Elifs;
    }

    ElseStatement *getElse()
    {
        return Else;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class ElifStatement : public IfStatement
{
    
private:
    Conditions *condition;
    llvm::SmallVector<AssignStatement *> Assignments;

public:
    ElifStatement(Conditions *condition, llvm::SmallVector<AssignStatement *> Assignments) :
     condition(condition), Assignments(Assignments), IfStatement() {}

    Conditions *getCondition()
    {
        return condition;
    }

    llvm::SmallVector<AssignStatement *> getStatements()
    {
        return Assignments;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class ElseStatement : public IfStatement
{

private:
    llvm::SmallVector<AssignStatement *> Assignments;

public:
    ElseStatement(llvm::SmallVector<AssignStatement *> Assignments) : 
    Assignments(Assignments), IfStatement() {}

    llvm::SmallVector<AssignStatement *> getAssignments()
    {
        return Assignments;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class LoopStatement : public Statement
{

private:
    Conditions *condition;
    llvm::SmallVector<AssignStatement *> Assignments;

public:
    LoopStatement(Conditions *condition, llvm::SmallVector<AssignStatement *> Assignments) : 
    condition(condition), Assignments(Assignments), Statement(Statement::StatementType::If) {}

    Conditions *getCondition()
    {
        return condition;
    }

    llvm::SmallVector<AssignStatement *> getAssignments()
    {
        return Assignments;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class DecStatement : public Statement
{
private:
    llvm::SmallVector<llvm::StringRef, 8> vars;
    llvm::SmallVector<Expression *> exprs;

public:
    DecStatement(llvm::SmallVector<llvm::StringRef, 8> vars, llvm::SmallVector<Expression *> exprs) :
        vars(vars), exprs(exprs), Statement(Statement::StatementType::Declaration) {}

    llvm::SmallVector<llvm::StringRef, 8> getVars()
    {
        return vars;
    }

    llvm::SmallVector<Expression *> getExprs()
    {
        return exprs;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class AssignStatement : public Statement
{
public:
    enum AssOp
    {
        PlusAssign,
        MinusAssign,
        MulAssign,
        DivAssign,
        ModAssign,
        Assign
    };

private:
    Final *lvalue;
    AssOp AssignmentOp;
    Expression *rvalue;

public:
    AssignStatement(Final *lvalue, AssOp AssignmentOp, Expression *rvalue) :
     lvalue(lvalue), AssignmentOp(AssignmentOp), rvalue(rvalue), Statement(StatementType::Assignment) {}
    Final *getLValue()
    {
        return lvalue;
    }

    Expression *getRValue()
    {
        return rvalue;
    }

    AssOp getAssignmentOP()
    {
        return AssignmentOp;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class Expression : public AST
{
public:
    enum Operator
    {
        Plus,
        Minus,
        Mul,
        Div,
        Mod,
        Pow
    };

private:
    Expression *Left; // Left-hand side expression
    Expression *Right; // Right-hand side expression
    Operator Op;      // Operator of the binary operation

public:
    Expression(Operator Op, Expression *L, Expression *R) : 
    Op(Op), Left(L), Right(R) {}
    Expression() {}

    Expression *getLeft() { return Left; }

    Expression *getRight() { return Right; }

    Operator getOperator() { return Op; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class Conditions : public AST
{
public:
    enum AndOr
    {
        And,
        Or
    };
private:
    Conditions *Left;
    AndOr Sign;
    Conditions *Right;

public:
    Conditions(Conditions *left, AndOr sign, Conditions *right) : 
    Left(left), Sign(sign), Right(right) {}
    Conditions() {}

    Conditions *getLeft() { return Left; }

    AndOr getSign() { return Sign; }

    Conditions *getRight() { return Right; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class Condition : public Conditions
{
public:
    enum Operator
    {
        LessEqual,
        Less,
        Greater,
        GreaterEqual,
        Equal,
        NotEqual
    };

private:
    Expression *Left; // Left-hand side expression
    Expression *Right; // Right-hand side expression
    Operator Op;      // Operator of the boolean operation

public:
    Condition(Expression *left, Operator Op, Expression *right) : 
    Left(left), Op(Op), Right(right), Conditions() {}

    Expression *getLeft() { return Left; }

    Operator getSign() { return Op; }

    Expression *getRight() { return Right; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class Final : public Expression
{
public:
  enum ValueKind
  {
    Ident,
    Number
  };

private:
  ValueKind Kind;                            
  llvm::StringRef Val;                       

public:
  Final(ValueKind Kind, llvm::StringRef Val) : Kind(Kind), Val(Val), Expression() {}

  ValueKind getKind() { return Kind; }

  llvm::StringRef getVal() { return Val; }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

#endif
