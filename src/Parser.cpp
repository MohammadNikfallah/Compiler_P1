#include "Parser.h"

AST *Parser::parse()
{
    AST *Res = parseMSM();
    return Res;
}

MSM *Parser::parseMSM()
{
    llvm::SmallVector<Statement *> statement;
    bool flag = false;
    while (!Tok.is(Token::eoi))
    {
        switch (Tok.getKind())
        {
        case Token::KW_int:
            DecStatement *d;
            d = parseDec();
            if (d)
                statement.push_back(d);
            else{
                error();
                flag = true;
                break;
            }
            break;
        case Token::ident:
            AssignStatement *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                flag = true;
                break;
            }
            if (a)
                statement.push_back(a);
            else{
                error();
                flag = true;
                break;
            }
            break;
        case Token::KW_if:
            IfStatement *i;
            i = parseIf();
            if (i)
                statement.push_back(i);
            else{
                flag = true;
                break;
                break;
            }
            break;
        case Token::KW_loopc:
            LoopStatement *l;
            l = parseLoop();
            if (l)
                statement.push_back(l);
            
            else{
                error();
                flag = true;
                break;
            }
            break;
        default:
            error();
            flag = true;
            break; 
        }
        advance();
    }
    if(flag){
        while (Tok.getKind() != Token::eoi)
        advance();
        return nullptr;
    }
    
    return new MSM(statement);
}

DecStatement *Parser::parseDec()
{
    Expression *e;
    llvm::SmallVector<llvm::StringRef, 8> vars;
    llvm::SmallVector<Expression *> exprs;

    if (expect(Token::KW_int))
        goto _error;

    advance();

    if (expect(Token::ident))
        goto _error;
    vars.push_back(Tok.getText());
    advance();

    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident))
            goto _error;
        vars.push_back(Tok.getText());
        advance();
    }


    if (Tok.is(Token::equal))
    {
        advance();
        e = parseExpr();
        if(e)
            exprs.push_back(e);
        else
            goto _error;
        while (Tok.is(Token::comma))
        {
            advance();
            e = parseExpr();
            if(e)
                exprs.push_back(e);
            else
                goto _error;
            if(Tok.getKind() == Token::semicolon)
                break;
        }
    }

    if (vars.size() < exprs.size() || expect(Token::semicolon)){
        goto _error;
    }

    return new DecStatement(vars, exprs);
_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

AssignStatement *Parser::parseAssign()
{   
    Expression *e;
    Final *f;
    f = (Final *)(parseFactor());

    if (!Tok.isOneOf(Token::equal, Token::plus_equal, Token::star_equal
    , Token::minus_equal, Token::slash_equal, Token::precent_equal))
    {
        error();
        return nullptr;
    }

    AssignStatement::AssOp a;

    switch (Tok.getKind())
    {
    case Token::equal:
        a = AssignStatement::AssOp::Assign;
        break;
    case Token::plus_equal:
        a = AssignStatement::AssOp::PlusAssign;
        break;
    case Token::star_equal:
        a = AssignStatement::AssOp::MulAssign;
        break;
    case Token::minus_equal:
        a = AssignStatement::AssOp::MinusAssign;
        break;
    case Token::slash_equal:
        a = AssignStatement::AssOp::DivAssign;
        break;
    case Token::precent_equal:
        a = AssignStatement::AssOp::ModAssign;
        break;
    default:
        goto _error3;
        break;
    }

    advance();
    e = parseExpr();
    
    return (new AssignStatement(f, a, e));

_error3:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expression *Parser::parseExpr()
{
    Expression *Left = parseTerm();
    while (Tok.isOneOf(Token::plus, Token::minus))
    {
        Expression::Operator Op =
            Tok.is(Token::plus) ? Expression::Plus : Expression::Minus;
        advance();
        Expression *Right = parseTerm();
        Left = new Expression(Op, Left, Right);
    }
    return Left;
}

Expression *Parser::parseTerm()
{
    Expression *Left = parseFactor();
    while (Tok.isOneOf(Token::star, Token::slash))
    {
        Expression::Operator Op =
            Tok.is(Token::star) ? Expression::Mul : Expression::Div;
        advance();
        Expression *Right = parseFactor();
        Left = new Expression(Op, Left, Right);
    }
    return Left;
}

Expression *Parser::parseFactor()
{
    Expression *Left = parseFinal();
    while (Tok.is(Token::power))
    {
        Expression::Operator Op = Expression::Operator::Pow;
        advance();
        Expression *Right = parseFinal();
        Left = new Expression(Op, Left, Right);
    }
    return Left;
}

Expression *Parser::parseFinal()
{
    Expression *Res = nullptr;
    
    switch (Tok.getKind())
    {
    case Token::number:
        Res = new Final(Final::ValueKind::Number, Tok.getText());
        advance();
        break;
    case Token::ident:
        Res = new Final(Final::ValueKind::Ident, Tok.getText());
        advance();
        break;
    case Token::l_paren:
        advance();
        Res = parseExpr();
        if (!consume(Token::r_paren)){
            break;
        }
    default:
        if (!Res)
            error();
        while (!Tok.isOneOf(Token::r_paren, Token::star, Token::plus, Token::minus, Token::slash, Token::eoi))// what the fuck is this
            advance();
        break;
    }
    return Res;
}

IfStatement *Parser::parseIf()
{
    llvm::SmallVector<ElifStatement *> elifs;

    if (!Tok.is(Token::KW_if))
    {
        error();
        return nullptr;
    }
    advance();

    Conditions *conditions = parseConditions();

    if (!Tok.is(Token::colon))
    {
        error();
        return nullptr;
    }
    advance();


    if (!Tok.is(Token::begin))
    {
        error();
        return nullptr;
    }
    advance();
    
    
    llvm::SmallVector<AssignStatement *> statement;
    bool flag = false;
    while (!Tok.is(Token::end))
    {

        if(Tok.is(Token::ident)){
            AssignStatement *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                flag = true;
                break;
            }
            if (a)
                statement.push_back(a);
            advance();
        }
    }
    advance();
    while(Tok.is(Token::KW_elif) && !flag){
        ElifStatement *e;
        e = parseElif();
        if(e)
            elifs.push_back(e);
        else {
            error();
            flag = true;
            break;
        }
        advance();
    }
    if(flag){
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
    }
    ElseStatement *el = nullptr;
    if(Tok.is(Token::KW_else)){
        el = parseElse();
    } 
    return new IfStatement(conditions, statement, elifs, el);
}

ElifStatement *Parser::parseElif()
{
    ElifStatement *Res = nullptr;

    if (!Tok.is(Token::KW_elif))
    {
        error();
        return nullptr;
    }
    advance();

    Conditions *conditions = parseConditions();

    if (!Tok.is(Token::colon))
    {
        error();
        return nullptr;
    }
    advance();

    if (!Tok.is(Token::begin))
    {
        error();
        return nullptr;
    }
    advance();
    llvm::SmallVector<AssignStatement *> statement;

    while (!Tok.is(Token::end))
    {

        if(Tok.is(Token::ident)){
            AssignStatement *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _errorelif;
            }
            if (a)
                statement.push_back(a);
            advance();
        }
    }
    Res = new ElifStatement(conditions, statement);
    return Res;
_errorelif:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

ElseStatement *Parser::parseElse()
{
    ElseStatement *Res = nullptr;

    if (!Tok.is(Token::KW_else))
    {
        error();
        return nullptr;
    }
    advance();

    if (!Tok.is(Token::colon))
    {
        error();
        return nullptr;
    }
    advance();

    if (!Tok.is(Token::begin))
    {
        error();
        return nullptr;
    }
    advance();
    llvm::SmallVector<AssignStatement *> statement;
    int flag = 0;

    while (!Tok.is(Token::end))
    {

        if(Tok.is(Token::ident)){
            AssignStatement *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                flag = 1;
                break;
            }
            if (a)
                statement.push_back(a);
            advance();
        }
    }
    if(flag)
        goto _errorelif;
    return new ElseStatement(statement);
_errorelif:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

LoopStatement *Parser::parseLoop()
{
    LoopStatement *Res = nullptr;


    if (!Tok.is(Token::KW_loopc))
    {
        error();
        return nullptr;
    }
    advance();

    Conditions *conditions = parseConditions();



    if (!Tok.is(Token::colon))
    {
        error();
        return nullptr;
    }
    advance();

    if (!Tok.is(Token::begin))
    {
        error();
        return nullptr;
    }
    advance();
    llvm::SmallVector<AssignStatement *> statement;

    while (!Tok.is(Token::end))
    {
        if(Tok.is(Token::ident)){
            AssignStatement *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _errorloop;
            }
            if (a)
                statement.push_back(a);
            advance();
        }
    }
    Res = new LoopStatement(conditions, statement);
    return Res;
_errorloop:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Conditions *Parser::parseConditions()
{
    Conditions *Left = parseCondition();
    while (Tok.isOneOf(Token::KW_and, Token::KW_or))
    {
        Conditions::AndOr Op =
            Tok.is(Token::KW_and) ? Conditions::AndOr::And : Conditions::AndOr::Or;
        advance();
        Conditions *Right = parseCondition();
        Left = new Conditions(Left, Op, Right);
    }
    return Left;
}

Condition *Parser::parseCondition()
{
    Expression *Left = parseExpr();
    
    Condition::Operator Op;
    switch (Tok.getKind())
    {
    case Token::bigger:
        Op = Condition::Operator::Greater;
        break;
    case Token::bigger_equal:
        Op = Condition::Operator::GreaterEqual;
        break;
    case Token::less:
        Op = Condition::Operator::Less;
        break;
    case Token::less_equal:
        Op = Condition::Operator::LessEqual;
        break;
    case Token::equal_equal:
        Op = Condition::Operator::Equal;
        break;
    case Token::not_equal:
        Op = Condition::Operator::NotEqual;
        break;
    
    default:
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
        break;
    }
    
    advance();
    Expression *Right = parseExpr();
        
    return new Condition(Left, Op, Right);
}