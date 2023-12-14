#include "CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

// Define a visitor class for generating LLVM IR from the AST.
namespace 
{
  class ToIRVisitor : public ASTVisitor
  {
    
    Module *M;
    IRBuilder<> Builder;
    Type *VoidTy;
    Type *Int32Ty;
    Type *Int8PtrTy;
    Type *Int8PtrPtrTy;
    Constant *Int32Zero;
    Constant *Int32One;
    Function *CalcWriteFn;
    FunctionType *CalcWriteFnTy;
    
    Value *V;
    StringMap<AllocaInst *> nameMap;

    llvm::FunctionType* MainFty;
    llvm::Function* MainFn;

  public:
    // Constructor for the visitor class.
    ToIRVisitor(Module *M) : M(M), Builder(M->getContext())
    {
      // Initialize LLVM types and constants.
      VoidTy = Type::getVoidTy(M->getContext());
      Int32Ty = Type::getInt32Ty(M->getContext());
      Int8PtrTy = Type::getInt8PtrTy(M->getContext());
      Int8PtrPtrTy = Int8PtrTy->getPointerTo();
      Int32Zero = ConstantInt::get(Int32Ty, 0, true);
      Int32One = ConstantInt::get(Int32Ty, 1, true);
      CalcWriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      CalcWriteFn = Function::Create(CalcWriteFnTy, GlobalValue::ExternalLinkage, "gsm_write", M);
    }

    // Entry point for generating LLVM IR from the AST.
    void run(AST *Tree)
    {
      // Create the main function with the appropriate function type.
      MainFty = FunctionType::get(Int32Ty, {Int32Ty, Int8PtrPtrTy}, false);
      MainFn = Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);

      // Create a basic block for the entry point of the main function.
      BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
      Builder.SetInsertPoint(BB);

      // Visit the root node of the AST to generate IR.
      Tree->accept(*this);

      // Create a return instruction at the end of the main function.
      Builder.CreateRet(Int32Zero);
    }

    // Visit function for the GSM node in the AST.
    virtual void visit(MSM &Node) override
    {
      // Iterate over the children of the GSM node and visit each child.
      for (auto I = Node.begin(), E = Node.end(); I != E; ++I)
      {
        (*I)->accept(*this);//check for error in I!=E
      }
    };

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
    
    virtual void visit(Final &Node) override
    {
      if (Node.getKind() == Final::Ident)
      {
        // If the factor is an identifier, load its value from memory.
        V = Builder.CreateLoad(Int32Ty, nameMap[Node.getVal()]);
      }
      else
      {
        // If the factor is a literal, convert it to an integer and create a constant.
        int number;
        llvm::StringRef strVal = Node.getVal(); 
        strVal.getAsInteger(10,number);
        V = ConstantInt::get(Int32Ty, number, true);
      }
    };

    virtual void visit(Expression &Node) override
    {
      // Visit the left-hand side of the binary operation and get its value.
      Node.getLeft()->accept(*this);
      Value *Left = V;

      // Visit the right-hand side of the binary operation and get its value.
      Node.getRight()->accept(*this);
      Value *Right = V;


      // Perform the binary operation based on the operator type and create the corresponding instruction.
      switch (Node.getOperator())
      {
      case Expression::Operator::Plus:
        V = Builder.CreateNSWAdd(Left, Right);
        break;
      case Expression::Operator::Minus:
        V = Builder.CreateNSWSub(Left, Right);
        break;
      case Expression::Operator::Mul:
        V = Builder.CreateNSWMul(Left, Right);
        break;
      case Expression::Operator::Div:
        V = Builder.CreateSDiv(Left, Right);
        break;
      case Expression::Operator::Mod:
      {
        Value* div = Builder.CreateSDiv(Left, Right);
        Value* mult = Builder.CreateNSWMul(div, Right);
        V = Builder.CreateNSWSub(Left, mult);
        break;
      }
        
      
      case Expression::Operator::Pow:
      {
        int RightValue=0;
        Final * ff = (Final *) Node.getRight();
        ff->getVal().getAsInteger(10,RightValue);
        
        Value* LeftVal= Left;
      
        if (RightValue == 0) {
          V = ConstantInt::get(Int32Ty, 1, true);
          break;
        }
        else {
          for (int i = 1; i < RightValue; i++)
          {
           Left = Builder.CreateNSWMul(Left, LeftVal);
          }
          V = Left;  
          break;
        }
      }

      }
    };
    virtual void visit(Condition& Node) override
    {
      Node.getLeft()->accept(*this);
      Value* Left = V;
      Node.getRight()->accept(*this);
      Value* Right = V;
      switch (Node.getSign())
      {
      case Condition::Operator::Equal:
        V = Builder.CreateICmpEQ(Left, Right);
        break;
      case Condition::Operator::Less:
        V = Builder.CreateICmpSLT(Left, Right);
        break;
      case Condition::Operator::LessEqual:
        V = Builder.CreateICmpSLE(Left, Right);
        break;
      case Condition::Operator::GreaterEqual:
        V = Builder.CreateICmpSGE(Left, Right);
        break;
      case Condition::Operator::Greater:
        V = Builder.CreateICmpSGT(Left, Right);
        break;
      case Condition::Operator::NotEqual:
        V = Builder.CreateICmpNE(Left, Right);
      }
    }
    virtual void visit(DecStatement &Node) override
    {
      llvm::SmallVector<llvm::StringRef, 8> vars=Node.getVars();
      llvm::SmallVector<Expression *> Exprs=Node.getExprs();
      auto expr=Exprs.begin();
      Value* val;
      // Iterate over the variables declared in the declaration statement.
      for (auto var = vars.begin(), var_end = vars.end(); var != var_end; ++var )
      {
        llvm::StringRef Var = *var;
        Expression*  Expr = *expr;
        if (expr != Exprs.end())
        {
          // If there is an expression provided, visit it and get its value.
          Expr->accept(*this);
          val = V;
          ++expr;
          
          nameMap[Var] = Builder.CreateAlloca(Int32Ty);

        // Store the initial value (if any) in the variable's memory location.
          if (val != nullptr)
          {
            Builder.CreateStore(val, nameMap[Var]);
          }
        }
        else //set zero
        {
        nameMap[Var] = Builder.CreateAlloca(Int32Ty);

        // Store the initial value (if any) in the variable's memory location.
        if (val != nullptr)
        {
          Value* zero = ConstantInt::get(Type::getInt32Ty(M->getContext()), 0);
          Builder.CreateStore(zero, nameMap[Var]);
        }
        }
        // Create an alloca instruction to allocate memory for the variable.
        
      }
    };

    virtual void visit(IfStatement& Node) override{
      llvm::BasicBlock* IfCondBB = llvm::BasicBlock::Create(M->getContext(), "if.cond", MainFn);
      llvm::BasicBlock* IfBodyBB = llvm::BasicBlock::Create(M->getContext(), "if.body", MainFn);
      llvm::BasicBlock* AfterIfBB = llvm::BasicBlock::Create(M->getContext(), "after.if", MainFn);

      Builder.CreateBr(IfCondBB);
      Builder.SetInsertPoint(IfCondBB);
      Node.getCondition()->accept(*this);
      llvm::Value* IfCondVal = V;

      Builder.SetInsertPoint(IfBodyBB);

      llvm::SmallVector<AssignStatement* > assignStatements = Node.getAssignments();
      for (auto I = assignStatements.begin(), E = assignStatements.end(); I != E; ++I)
      {
        (*I)->accept(*this);
      }
      Builder.CreateBr(AfterIfBB);


      llvm::BasicBlock* PrevCondBB = IfCondBB;
      llvm::BasicBlock* PrevBodyBB = IfBodyBB;
      llvm::Value* PrevCondVal = IfCondVal;

      for (auto& Elif : Node.getElifs()) {
        llvm::BasicBlock* ElifCondBB = llvm::BasicBlock::Create(M->getContext(), "elif.cond", MainFn);
        llvm::BasicBlock* ElifBodyBB = llvm::BasicBlock::Create(M->getContext(), "elif.body", MainFn);

        Builder.SetInsertPoint(PrevCondBB);
        Builder.CreateCondBr(PrevCondVal, PrevBodyBB, ElifCondBB);

        Builder.SetInsertPoint(ElifCondBB);
        Elif->getCondition()->accept(*this);
        llvm::Value* ElifCondVal = V;
        // Builder.CreateCondBr(ElifCondVal, ElifBodyBB, nullptr);

        Builder.SetInsertPoint(ElifBodyBB);
        Elif->accept(*this);
        Builder.CreateBr(AfterIfBB);

        PrevCondBB = ElifCondBB;
        PrevCondVal = ElifCondVal;
        PrevBodyBB = ElifBodyBB;
      }

      llvm::BasicBlock* ElseBB = nullptr;
      ElseStatement* els = Node.getElse();
      if (els) {
        ElseBB = llvm::BasicBlock::Create(M->getContext(), "else.body", MainFn);
        Builder.SetInsertPoint(ElseBB);
        Node.getElse()->accept(*this);
        Builder.CreateBr(AfterIfBB);

        Builder.SetInsertPoint(PrevCondBB);
        Builder.CreateCondBr(PrevCondVal, PrevBodyBB, ElseBB);
      } else {
        Builder.SetInsertPoint(PrevCondBB);
        Builder.CreateCondBr(IfCondVal, PrevBodyBB, AfterIfBB);
      }

      Builder.SetInsertPoint(AfterIfBB);
    }

    virtual void visit(ElifStatement& Node) override{
      llvm::SmallVector<AssignStatement* > assignStatements = Node.getStatements();
      for (auto I = assignStatements.begin(), E = assignStatements.end(); I != E; ++I){
        (*I)->accept(*this);
      }
    }

    virtual void visit(ElseStatement& Node) override{
      llvm::SmallVector<AssignStatement* > assignStatements = Node.getAssignments();
      for (auto I = assignStatements.begin(), E = assignStatements.end(); I != E; ++I){
        (*I)->accept(*this);
      }
    }

    virtual void visit(Conditions& Node) override{
      Node.getLeft()->accept(*this);
      Value* Left = V;
      Node.getRight()->accept(*this);
      Value* Right = V;
      switch (Node.getSign())
      {
      case Conditions::AndOr::And:
        V = Builder.CreateAnd(Left, Right);
        break;
      case Conditions::AndOr::Or:
        V = Builder.CreateOr(Left, Right);
        break;
      }
    }

    virtual void visit(AssignStatement& Node) override
    {
      // Visit the right-hand side of the assignment and get its value.
    
      Node.getRValue()->accept(*this);
      Value* val = V;


      // Get the name of the variable being assigned.
      auto varName = Node.getLValue()->getVal();
      auto op=Node.getAssignmentOP();
      
      Value* tempVal;
      switch (Node.getAssignmentOP())
      {
        case AssignStatement::AssOp::Assign:
          Builder.CreateStore(val, nameMap[varName]);
          break;
        case AssignStatement::AssOp::DivAssign:{
          tempVal = Builder.CreateLoad(Int32Ty, nameMap[varName]);
          Builder.CreateStore(Builder.CreateSDiv(tempVal, val), nameMap[varName]);
          break;
        }
        case AssignStatement::AssOp::ModAssign:{
          tempVal = Builder.CreateLoad(Int32Ty, nameMap[varName]);
          Value* div = Builder.CreateSDiv(tempVal, val);
          Value* mult = Builder.CreateNSWMul(div, val);
          Builder.CreateStore(Builder.CreateNSWSub(tempVal, mult), nameMap[varName]);
          break;
        }
        case AssignStatement::AssOp::MulAssign:{
          tempVal = Builder.CreateLoad(Int32Ty, nameMap[varName]);
          Builder.CreateStore(Builder.CreateNSWMul(tempVal, val), nameMap[varName]);
          break;
        }
        case AssignStatement::AssOp::PlusAssign:{
          tempVal = Builder.CreateLoad(Int32Ty, nameMap[varName]);
          Builder.CreateStore(Builder.CreateNSWAdd(tempVal, val), nameMap[varName]);
          break;
        }
        case AssignStatement::AssOp::MinusAssign:{
          tempVal = Builder.CreateLoad(Int32Ty, nameMap[varName]);
          Builder.CreateStore(Builder.CreateNSWSub(tempVal, val), nameMap[varName]);
          break;
        }
          
      }
      CallInst *Call = Builder.CreateCall(CalcWriteFnTy, CalcWriteFn, {val});
    }

        virtual void visit(LoopStatement& Node) override
        {
          llvm::BasicBlock* WhileCondBB = llvm::BasicBlock::Create(M->getContext(), "loopc.cond", MainFn);
          llvm::BasicBlock* WhileBodyBB = llvm::BasicBlock::Create(M->getContext(), "loopc.body", MainFn);
          llvm::BasicBlock* AfterWhileBB = llvm::BasicBlock::Create(M->getContext(), "after.loopc", MainFn);

          Builder.CreateBr(WhileCondBB);
          Builder.SetInsertPoint(WhileCondBB);
          Node.getCondition()->accept(*this);
          Value* val=V;
          Builder.CreateCondBr(val, WhileBodyBB, AfterWhileBB);
          Builder.SetInsertPoint(WhileBodyBB);
          llvm::SmallVector<AssignStatement* > assignStatements = Node.getAssignments();
          for (auto I = assignStatements.begin(), E = assignStatements.end(); I != E; ++I)
          {
            (*I)->accept(*this);
          }
          Builder.CreateBr(WhileCondBB);

          Builder.SetInsertPoint(AfterWhileBB);
        }

  };
};

void CodeGen::compile(AST *Tree)
{
  // Create an LLVM context and a module.
  LLVMContext Ctx;
  Module *M = new Module("GSM.expr", Ctx);

  // Create an instance of the ToIRVisitor and run it on the AST to generate LLVM IR.
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);

  // Print the generated module to the standard output.
  M->print(outs(), nullptr);
}
