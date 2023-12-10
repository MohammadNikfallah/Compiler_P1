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

    Value *V;
    StringMap<AllocaInst *> nameMap;

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
    }

    // Entry point for generating LLVM IR from the AST.
    void run(AST *Tree)
    {
      // Create the main function with the appropriate function type.
      FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, Int8PtrPtrTy}, false);
      Function *MainFn = Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);

      // Create a basic block for the entry point of the main function.
      BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
      Builder.SetInsertPoint(BB);

      // Visit the root node of the AST to generate IR.
      Tree->accept(*this);

      // Create a return instruction at the end of the main function.
      Builder.CreateRet(Int32Zero);
    }

    // Visit function for the GSM node in the AST.
    virtual void visit(Root &Node) override
    {
      // Iterate over the children of the GSM node and visit each child.
      for (auto I = Node.begin(), E = Node.end(); I != E; ++I)
      {
        (*I)->accept(*this);
      }
    };

    virtual void visit(Statement &Node) override
    {
      if (Node.getKind()==Statement::StatementType::Assignment)
      {
        AssignStatement* assignStatement=(AssignStatement*)&Node;
        assignStatement->accept(*this);
      }
      else if (Node.getKind==Statement::StatementType::If)
      {
        IfStatement* ifStatement=(IfStatement*)&Node;
        ifStatement->accept(*this);
      }
      else if(Node.getKind==Statement::StatementType::Loop)
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
        llvm::StringRe strVal = Node.getVal(); 
        strVal.getAsInteger(10,number)
        V = ConstantInt::get(Int32Ty, number, true);
      }
    };

    virtual void visit(BinaryOp &Node) override
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
      case BinaryOp::Plus:
        V = Builder.CreateNSWAdd(Left, Right);
        break;
      case BinaryOp::Minus:
        V = Builder.CreateNSWSub(Left, Right);
        break;
      case BinaryOp::Mul:
        V = Builder.CreateNSWMul(Left, Right);
        break;
      case BinaryOp::Div:
        V = Builder.CreateSDiv(Left, Right);
        break;
      }
    };
    virtual void visit(Condition& Node) override
        {
            Node.getLeft()->accept(*this);
            Value* Left = V;
            Node.getRight()->accept(*this);
            Value* Right = V;
            switch (Node.getOperator())
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
            case Condition::Operator::And:
                V = Builder.CreateAnd(Left, Right);
                break;
            case Condition::Operator::Or:
                V = Builder.CreateOr(Left, Right);
                break;
            case Condition::Operator::Greater:
                V = Builder.CreateICmpSGT(Left, Right);
                break;
            
            }
        }
    virtual void visit(DecStatement &Node) override
    {
      Value *val = nullptr;

      if (Node.getExpr())
      {
        // If there is an expression provided, visit it and get its value.
        Node.getExpr()->accept(*this);
        val = V;
      }

      // Iterate over the variables declared in the declaration statement.
      for (auto I = Node.begin(), E = Node.end(); I != E; ++I)
      {
        StringRef Var = *I;

        // Create an alloca instruction to allocate memory for the variable.
        nameMap[Var] = Builder.CreateAlloca(Int32Ty);

        // Store the initial value (if any) in the variable's memory location.
        if (val != nullptr)
        {
          Builder.CreateStore(val, nameMap[Var]);
        }
      }
    };
  };
}; // namespace

void CodeGen::compile(AST *Tree)
{
  // Create an LLVM context and a module.
  LLVMContext Ctx;
  Module *M = new Module("calc.expr", Ctx);

  // Create an instance of the ToIRVisitor and run it on the AST to generate LLVM IR.
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);

  // Print the generated module to the standard output.
  M->print(outs(), nullptr);
}
