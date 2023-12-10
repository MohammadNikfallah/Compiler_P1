#include "Parser.h"

// main point is that the whole input has been consumed
AST *Parser::parse()
{
    AST *Res = parseMSM();
    return Res;
}

MSM *Parser::parseMSM()
{
    llvm::SmallVector<Statement *> statement;
    while (!Tok.is(Token::eoi))
    {
        switch (Tok.getKind())
        {
        case Token::KW_int:
            DecStatement *d;
            d = parseDec();
            if (d)
                statement.push_back(d);
            else
                goto _error2;
            break;
        case Token::ident:
            AssignStatement *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _error2;
            }
            if (a)
                statement.push_back(a);
            else
                goto _error2;
            break;
        case Token::KW_if:
            IfStatement *d;
            d = parseIf();
            if (d)
                statement.push_back(d);
            else
                goto _error2;
            break;
        case Token::KW_loopc:
            LoopStatement *d;
            d = parseLoop();
            if (d)
                statement.push_back(d);
            else
                goto _error2;
            break;
        default:
            goto _error2;
            break;
        }
        advance();
    }
    return new MSM(statement);
_error2:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
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
    Vars.push_back(Tok.getText());
    advance();

    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident))
            goto _error;
        Vars.push_back(Tok.getText());
        advance();
    }

    if (Tok.is(Token::equal))
    {
        advance();
        e = parseExpr();
        if(e)
            exprs.push_back(E);
        else
            goto _error
        while (Tok.is(Token::comma))
        {
            advance();
            e = parseExpr();
            if(e)
                exprs.push_back(E);
            else
                goto _error
            advance();
        }
    }

    if (vars.size() < exprs.size() || expect(Token::semicolon))
        goto _error;

    return new DecStatement(vars, exprs);
_error: // TODO: Check this later in case of error :)
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

AssignStatement *Parser::parseAssign()
{   
    Expression *e;
    Final *f;
    f = (Final *)(parseFactor());

    advance();

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
    case Token::star_equal:
        a = AssignStatement::AssOp::MulAssign;
    case Token::minus_equal:
        a = AssignStatement::AssOp::MinusAssign;
    case Token::slash_equal:
        a = AssignStatement::AssOp::DivAssign;
    case Token::precent_equal:
        a = AssignStatement::AssOp::ModAssign;
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
        Expression::Operator Op = Expression::Power;
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
        if (!consume(Token::r_paren))
            break;
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

    Conditions *conditions = ParseConditions();
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

    while (!Tok.is(Token::end))
    {

        if(Tok.is(Token::ident)){
            AssignStatement *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _error2;
            }
            if (a)
                statement.push_back(a);
        }
    }
    advance();
    while(Tok.is(Token::KW_elif)){
        ElifStatement *e;
        e = parseElif();
        if(e)
            Elifs.push_back(e);
        else
            goto _errorif;
    }
    advance();
    if(Tok.is(Token::KW_else)){
        ElseStatement *el;
        el = parseElse();
    } 
    return new IfStatement(conditions, statement, elifs, el);
_errorif: // TODO: Check this later in case of error :)
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
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

    Conditions *conditions = ParseConditions();
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

    while (!Tok.is(Token::end))
    {
        llvm::SmallVector<AssignStatement *> statement;

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

    while (!Tok.is(Token::end))
    {
        llvm::SmallVector<AssignStatement *> statement;

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
        }
    }
    Res = new ElseStatement();
    return Res;
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

    Conditions *conditions = ParseConditions();
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

    while (!Tok.is(Token::end))
    {
        llvm::SmallVector<AssignStatement *> statement;

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
        Left = new Conditions(Op, Left, Right);
    }
    return Left;
}

Condition *Parser::parseCondition()
{
    Condition *Left = parseExpr();
    while (Tok.isOneOf(Token::bigger, Token::bigger_equal, Token::less, Token::less_equal, Token::not_equal, Token::equal_equal))
    {
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
            goto _errorcondition;
            break;
        }
        
        advance();
        Expression *Right = parseExpr();
        Left = new Condition(Op, Left, Right);
        
    }
    return Left;
_errorcondition:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}