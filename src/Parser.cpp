#include "Parser.h"

// main point is that the whole input has been consumed
AST *Parser::parse()
{
    AST *Res = parseGSM();
    return Res;
}

AST *Parser::parseGSM()
{
    llvm::SmallVector<Statement *> Statement;
    while (!Tok.is(Token::eoi))
    {
        switch (Tok.getKind())
        {
        case Token::KW_int:
            Expr *d;
            d = parseDec();
            if (d)
                exprs.push_back(d);
            else
                goto _error2;
            break;
        case Token::ident:
            Expr *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _error2;
            }
            if (a)
                exprs.push_back(a);
            else
                goto _error2;
            break;
        case Token::KW_if:
             *d;
            d = parseIf();
            if (d)
                exprs.push_back(d);
            else
                goto _error2;
            break;
        case Token::KW_loopc:
             *d;
            d = parseLoop();
            if (d)
                exprs.push_back(d);
            else
                goto _error2;
            break;
        default:
            goto _error2;
            break;
        }
        advance(); // TODO: watch this part
    }
    return new GSM(exprs);
_error2:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseDec()
{
    Expr *E;
    llvm::SmallVector<llvm::StringRef, 8> Vars;

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
        E = parseExpr();
    }

    if (expect(Token::semicolon))
        goto _error;

    return new Declaration(Vars, E);
_error: // TODO: Check this later in case of error :)
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseAssign()
{   
    Expr *E;
    Factor *F;
    F = (Factor *)(parseFactor());

    if (!Tok.is(Token::equal))
    {
        error();
        return nullptr;
    }

    advance();
    E = parseExpr();
    //TODO Do AssignKind 
    return new Assignment(F, E);
}

Expr *Parser::parseExpr()
{
    Expr *Left = parseTerm();
    while (Tok.isOneOf(Token::plus, Token::minus))
    {
        BinaryOp::Operator Op =
            Tok.is(Token::plus) ? BinaryOp::Plus : BinaryOp::Minus;
        advance();
        Expr *Right = parseTerm();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseTerm()
{
    Expr *Left = parseFactor();
    while (Tok.isOneOf(Token::star, Token::slash))
    {
        BinaryOp::Operator Op =
            Tok.is(Token::star) ? BinaryOp::Mul : BinaryOp::Div;
        advance();
        Expr *Right = parseFactor();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseFactor()
{
    Expr *Left = parseFinal();
    while (Tok.is(Token::power))
    {
        BinaryOp::Operator Op = BinaryOp::Power;
        advance();
        Expr *Right = parseFinal();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseFinal()
{
    Expr *Res = nullptr;
    switch (Tok.getKind())
    {
    case Token::number:
        Res = new Factor(Factor::Number, Tok.getText());
        advance();
        break;
    case Token::ident:
        Res = new Factor(Factor::Ident, Tok.getText());
        advance();
        break;
    case Token::l_paren:
        advance();
        Res = parseExpr();
        if (!consume(Token::r_paren))
            break;
    default: // error handling
        if (!Res)
            error();
        while (!Tok.isOneOf(Token::r_paren, Token::star, Token::plus, Token::minus, Token::slash, Token::eoi))
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

    while (!Tok.is(Token::end))
    {
        llvm::SmallVector<AssignStatement *> statement;

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
    while(Tok.is(Token::KW_elif)){
        ElifStatement *e;
        e = parseElif();
        if(e)
            Elifs.push_back(e);
        else
            goto _errorif;
    }
    if(Tok.is(Token::KW_else)){
        ElseStatement *el;
        el = parseElse();
    } 
    return new IfStatement(conditions, statement, elifs, el);
_errorif: // TODO: Check this later in case of error :)
    while (Tok.getKind() != Token::end)
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
        llvm::SmallVector<Statement *> statement;
        llvm::SmallVector<ElifStatement *> Elifs;

        switch (Tok.getKind())
        {
        case Token::ident:
            Expr *a;
            a = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _error2;
            }
            if (a)
                statement.push_back(a);
            else
                goto _errorif;
            break;
        case Token::KW_if:
            IfStatement *d;
            d = parseIf();
            if (d)
                Elifs.push_back(d);
            else
                goto _errorif;
            break;
        case Token::KW_loopc:
            LoopStatement *d;
            d = parseLoop();
            if (d)
                statement.push_back(d);
            else
                goto _errorif;
            break;

        default:
            goto _errorif;
            break;
        }
    Res = new IfStatement(conditions, statement, Elifs);
    return Res;
_errorif: // TODO: Check this later in case of error :)
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
        advance();
    }
    return Res;
}
