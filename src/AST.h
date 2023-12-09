#ifndef AST_H
#define AST_H

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

class AST; // Abstract Syntax Tree
class Base; // Top-level program
class Statement; // Top-level statement
class Expression; // Binary operation of numbers and identifiers
class AssignStatement; // Assignment statement like a = 3;
class DecStatement; // Declaration statement like int a;
class IfStatement;
class ElifStatement;
class ElseStatement;
class Condition;
class LoopStatement;

class ASTVisitor
{
public:
    // Virtual visit functions for each AST node type
    virtual void visit(AST &) {}
    virtual void visit(Expression &) {}
    virtual void visit(Base &) = 0;
    virtual void visit(Statement &) = 0;
    virtual void visit(DecStatement &) = 0;
    virtual void visit(AssignStatement &) = 0;
    virtual void visit(IfStatement &) = 0;
    virtual void visit(ElifStatement &) = 0;
    virtual void visit(ElseStatement &) = 0;
    virtual void visit(Condition &) = 0;
    virtual void visit(LoopStatement &) = 0;
};

class AST
{
public:
    virtual ~AST() {}
    virtual void accept(ASTVisitor &V) = 0;
};

// Base Node that contains all the syntax nodes
class Base : public AST
{
private:
    llvm::SmallVector<Statement *> statements; // Stores the list of expressions

public:
    Base(llvm::SmallVector<Statement *> Statements) : statements(Statements) {}
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
    enum StateMentType
    {
        Declaration,
        Assignment,
        If,
        Loop
    };

private:
    StateMentType Type;

public:
    StateMentType getKind()
    {
        return Type;
    }


    Statement(StateMentType type) : Type(type) {}
    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class IfStatement : public Statement
{

private:
    Conditions *Condition;
    llvm::SmallVector<Statement *> Statements;
    llvm::SmallVector<ElifStatement *> Elifs;
    ElseStatement *Else;

public:
    IfStatement(Conditions *condition, llvm::SmallVector<Statement *> statements,llvm::SmallVector<ElifStatement *> Elifs,ElseStatement *Else, StateMentType type) : 
    Condition(condition), Statements(statements), Statement(type),Elifs(Elifs) Else(Else) {}

    Conditions *getCondition()
    {
        return Condition;
    }

    llvm::SmallVector<Statement *> getStatements()
    {
        return Statements;
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

class ElifStatement : public Statement
{
    
private:
    Conditions *Condition;
    llvm::SmallVector<Statement *> Statements;

public:
    ElifStatement(Conditions *condition, llvm::SmallVector<Statement *> statements, StateMentType type) :
     Condition(condition), Statements(statements), Statement(type) {}

    Conditions *getCondition()
    {
        return Condition;
    }

    llvm::SmallVector<Statement *> getStatements()
    {
        return Statements;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class ElseStatement : public Statement
{

private:
    llvm::SmallVector<Statement *> Statements;

public:
    ElseStatement(llvm::SmallVector<Statement *> statements, StateMentType type) : 
    Statements(statements), Statement(type) {}

    llvm::SmallVector<Statement *> getStatements()
    {
        return Statements;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class LoopStatement : public Statement
{

private:
    Conditions *Condition;
    llvm::SmallVector<Statement *> Statements;

public:
    LoopStatement(Conditions *condition, llvm::SmallVector<Statement *> statements, StateMentType type) : 
    Condition(condition), Statements(statements), Statement(type) {}

    Conditions *getCondition()
    {
        return Condition;
    }

    llvm::SmallVector<Statement *> getStatements()
    {
        return Statements;
    }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class DecStatement : public Statement
{
private:
    using VarVector = llvm::SmallVector<llvm::StringRef, 8>;
    VarVector Vars;
    using ExprVector = llvm::SmallVector<Expression *>;
    ExprVector Exprs;

public:
    DecStatement(VarVector *Vars, ExprVector *Exprs) :
     Vars(Vars), Exprs(Exprs), Statement(StateMentType::Declaration) {}
    Expression *getVars()
    {
        return lvalue;
    }

    Expression *getExprs()
    {
        return rvalue;
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
    Expression *lvalue;
    AssOp AssignmentOp;
    Expression *rvalue;

public:
    AssignStatement(Expression *lvalue, AssOp AssignmentOp, Expression *rvalue) :
     lvalue(lvalue), AssignmentOp(AssignmentOp), rvalue(rvalue), Statement(StateMentType::Assignment) {}
    Expression *getLValue()
    {
        return lvalue;
    }

    Expression *getRValue()
    {
        return rvalue;
    }

    AssOP *getAssignmentOP()
    {
        return rvalue;
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
    Expression(Operator Op, Expression *L, Expression *R) : Op(Op), Left(L), Right(R), Expression(ExpressionType::ExpressionType) {}

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
    Condition *Left;
    AndOr *Sign;
    Condition *Right;

public:
    Conditions(Condition *left, AndOr *sign, Condition *right) : Left(left), Sign(sign), Right(right) {}

    Condition *getLeft() { return Left; }

    AndOr *getSign() { return Sign; }

    Condition *getRight() { return Right; }

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
    Condition(Expression *left, Operator Op, Expression *right) : Left(left), Op(Op), Right(right) {}

    Expression *getLeft() { return Left; }

    Operator getSign() { return Sign; }

    Expression *getRight() { return Right; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

#endif
