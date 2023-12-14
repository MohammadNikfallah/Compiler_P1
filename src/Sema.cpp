#include "Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

namespace {
class InputCheck : public ASTVisitor {
  llvm::StringSet<> Scope; // StringSet to store declared variables
  bool HasError; // Flag to indicate if an error occurred

  enum ErrorType { Twice, Not }; // Enum to represent error types: Twice - variable declared twice, Not - variable not declared

  void error(ErrorType ET, llvm::StringRef V) {
    // Function to report errors
    llvm::errs() << "Variable " << V << " is "
                 << (ET == Twice ? "already" : "not")
                 << " declared\n";
    HasError = true; // Set error flag to true
  }

public:
  InputCheck() : HasError(false) {} // Constructor

  bool hasError() { return HasError; } // Function to check if an error occurred

  // Visit function for GSM nodes
  virtual void visit(MSM &Node) override { 
    for (auto I = Node.begin(), E = Node.end(); I != E; ++I)
    {
      (*I)->accept(*this); // Visit each child node
    }
  };

  // Visit function for Factor nodes
  virtual void visit(Final &Node) override {
    if (Node.getKind() == Final::Ident) {
      // Check if identifier is in the scope
      if (Scope.find(Node.getVal()) == Scope.end())
        error(Not, Node.getVal());
    }
  };
  

  // Visit function for BinaryOp nodes
  virtual void visit(Expression &Node) override {
    if (Node.getLeft())
      Node.getLeft()->accept(*this);
    else
      HasError = true;

    auto right = Node.getRight();
    if (right)
      right->accept(*this);
    else
      HasError = true;

    if (Node.getOperator() == Expression::Operator::Div && right) {
      Final * f = (Final *)right;
      
      if (right && f->getKind() == Final::ValueKind::Number) {
        int intval;
        f->getVal().getAsInteger(10, intval);

        if (intval == 0) {
          llvm::errs() << "Division by zero is not allowed." << "\n";
          HasError = true;
        }
      }
    }
  };

  // Visit function for Assignment nodes
  // virtual void visit(Assig &Node) override {
  //   Factor *dest = Node.getLeft();

  //   dest->accept(*this);

  //   if (dest->getKind() == Factor::Number) {
  //       llvm::errs() << "Assignment destination must be an identifier.";
  //       HasError = true;
  //   }

  //   if (dest->getKind() == Factor::Ident) {
  //     // Check if the identifier is in the scope
  //     if (Scope.find(dest->getVal()) == Scope.end())
  //       error(Not, dest->getVal());
  //   }

  //   if (Node.getRight())
  //     Node.getRight()->accept(*this);
  // };
      
  virtual void visit(Statement &Node) override
    {
      if (Node.getKind()==Statement::StatementType::Assignment)
      {
        AssignStatement* assignStatement=(AssignStatement*)&Node;
        assignStatement->accept(*this);
      }
      else if (Node.getKind()==Statement::StatementType::If)
      {
        IfStatement* ifStatement=(IfStatement*)&Node;
        ifStatement->accept(*this);
      }
      else if(Node.getKind()==Statement::StatementType::Loop)
      {
        LoopStatement* loopStatement=(LoopStatement*)&Node;
        loopStatement->accept(*this);
      }
      else 
      {
        DecStatement* dec = (DecStatement*)&Node;
        dec->accept(*this);

      }
    };
    
  virtual void visit(AssignStatement &Node)override{
    if (!Scope.count(Node.getLValue()->getVal()))
        error(Not, Node.getLValue()->getVal());
    Expression* right = Node.getRValue();
    if (right)
      right->accept(*this);
    else
      HasError = true;

    if (Node.getAssignmentOP() == AssignStatement::AssOp::DivAssign && right) {
      Final * f = (Final *)right;
      
      if (right && f->getKind() == Final::ValueKind::Number) {
        int intval;
        f->getVal().getAsInteger(10, intval);


        if (intval == 0) {
          llvm::errs() << "Division by zero is not allowed." << "\n";
          HasError = true;
        }
      }
    }
    
    
  }
  virtual void visit(IfStatement &Node) override{
    Node.getCondition()->accept(*this);
    llvm::SmallVector<AssignStatement* > assignments = Node.getAssignments();
    for (auto I = assignments.begin(), E = assignments.end(); I != E; ++I)
      {
        (*I)->accept(*this);
      }
    llvm::SmallVector<ElifStatement* > elifs = Node.getElifs();
    for (auto I = elifs.begin(), E = elifs.end(); I != E; ++I)
      {
        (*I)->accept(*this);
      }
    auto els = Node.getElse();
    if(els)
      Node.getElse()->accept(*this);
  }
  virtual void visit(ElifStatement &Node) override{
    Node.getCondition()->accept(*this);
    llvm::SmallVector<AssignStatement* > assignments = Node.getAssignments();
    for (auto I = assignments.begin(), E = assignments.end(); I != E; ++I)
      {
          (*I)->accept(*this);
      }
  }
  virtual void visit(ElseStatement &Node) override{
    llvm::SmallVector<AssignStatement* > assignments = Node.getAssignments();
    for (auto I = assignments.begin(), E = assignments.end(); I != E; ++I)
            {
                (*I)->accept(*this);
            }
  }
  virtual void visit(Condition &Node) override{
    Node.getLeft()->accept(*this);
    Node.getRight()->accept(*this);
  }
  virtual void visit(LoopStatement &Node) override{
    Node.getCondition()->accept(*this);

    llvm::SmallVector<AssignStatement* > assignments = Node.getAssignments();
    for (auto I = assignments.begin(), E = assignments.end(); I != E; ++I)
            {
                (*I)->accept(*this);
            }
  }
  virtual void visit(Conditions &Node) override{
    Node.getLeft()->accept(*this);
    Node.getRight()->accept(*this);
  }
  
  virtual void visit(DecStatement &Node) override {
    for (auto I = Node.getVars().begin(), E = Node.getVars().end(); I != E;
         ++I) {
      if (!Scope.insert(*I).second)
        error(Twice, *I); // If the insertion fails (element already exists in Scope), report a "Twice" error
    }
    if (Node.getExprs().size() > 0 ){
      for (auto i=Node.getExprs().begin(),j=Node.getExprs().end(); j!=i ; ++i)
      {
        (*i)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
      }
    }
  };
};
}

bool Sema::semantic(AST *Tree) {
  if (!Tree)
    return false; // If the input AST is not valid, return false indicating no errors

  InputCheck Check; // Create an instance of the InputCheck class for semantic analysis
  Tree->accept(Check); // Initiate the semantic analysis by traversing the AST using the accept function

  return Check.hasError(); // Return the result of Check.hasError() indicating if any errors were detected during the analysis
}
