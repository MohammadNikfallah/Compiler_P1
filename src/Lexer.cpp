#include "Lexer.h"

// classifying characters
namespace charinfo
{
    // ignore whitespaces
    LLVM_READNONE inline bool isWhitespace(char c)
    {
        return c == ' ' || c == '\t' || c == '\f' || c == '\v' ||
               c == '\r' || c == '\n';
    }

    LLVM_READNONE inline bool isDigit(char c)
    {
        return c >= '0' && c <= '9';
    }

    LLVM_READNONE inline bool isLetter(char c)
    {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }
}

void Lexer::next(Token &token)
{
    while (*BufferPtr && charinfo::isWhitespace(*BufferPtr))
    {
        ++BufferPtr;
    }
    // make sure we didn't reach the end of input
    if (!*BufferPtr)
    {
        token.Kind = Token::eoi;
        return;
    }
    // collect characters and check for keywords or ident
    if (charinfo::isLetter(*BufferPtr))
    {
        const char *end = BufferPtr + 1;
        while (charinfo::isDigit(*end) || charinfo::isLetter(*end) )
            ++end;
        llvm::StringRef Name(BufferPtr, end - BufferPtr);
        Token::TokenKind kind;

        if (Name == "int")
            kind = Token::KW_int;
        else if (Name=="if")
            kind=Token::KW_if;
        else if (Name=="else")
            kind=Token::KW_else;
        else if (Name=="elif")
            kind=Token::KW_elif;
        else if (Name=="and")
            kind=Token::KW_and;
        else if (Name=="or")
            kind=Token::KW_or;
        else if (Name=="true")
            kind=Token::KW_true;
        else if (Name=="false")
            kind=Token::KW_false;
        else if (Name=="end")
            kind=Token::end;
        else if (Name=="begin")
            kind=Token::begin;
        else if (Name=="loopc")
            kind=Token::KW_loopc;
        else
            kind = Token::ident;
        // generate the token
        formToken(token, end, kind);
        return;
    }
    // check for numbers
    else if (charinfo::isDigit(*BufferPtr))
    {
        const char *end = BufferPtr + 1;
        while (charinfo::isDigit(*end))
            ++end;
        formToken(token, end, Token::number);
        return;
    }
    else
    {
        char now=*BufferPtr;
        char next=*(BufferPtr+1);
        if (now=='+' && next=='=')
            formToken(token, BufferPtr + 2, Token::plus_equal);
        else if(now=='-' && next=='=')
            formToken(token, BufferPtr + 2, Token::minus_equal);
        else if (now =='*' && next=='=')
            formToken(token, BufferPtr + 2, Token::star_equal);
        else if (now=='/' && next=='=')
            formToken(token, BufferPtr + 2, Token::slash_equal);
        else if (now=='%' && next=='=')
            formToken(token, BufferPtr + 2, Token::precent_equal);
        else if (now=='=' && next=='=')
            formToken(token, BufferPtr + 2, Token::equal_equal);
        else if (now=='!' && next=='=')
            formToken(token, BufferPtr + 2, Token::not_equal);
        else if (now=='>' && next=='=')
            formToken(token, BufferPtr + 2, Token::bigger_equal);
        else if (now=='<' && next=='=')
            formToken(token, BufferPtr + 2, Token::less_equal);
        
        else{
        switch (*BufferPtr)
        {
#define CASE(ch, tok)                         \
    case ch:                                  \
        formToken(token, BufferPtr + 1, tok); \
        break
            CASE('+', Token::plus);
            CASE('-', Token::minus);
            CASE('*', Token::star);
            CASE('/', Token::slash);
            CASE('%', Token::percent);
            CASE('^', Token::power);
            CASE('(', Token::l_paren);
            CASE(')', Token::r_paren);
            CASE(';', Token::semicolon);
            CASE(',', Token::comma);
            CASE('=', Token::equal);
            CASE('>', Token::bigger);
            CASE('<', Token::less);
            CASE(':', Token::colon);
#undef CASE
        default:
            formToken(token, BufferPtr + 1, Token::unknown);
        }
        return;
        }
    }
}

void Lexer::formToken(Token &Tok, const char *TokEnd,
                      Token::TokenKind Kind)
{
    Tok.Kind = Kind;
    Tok.Text = llvm::StringRef(BufferPtr, TokEnd - BufferPtr);
    BufferPtr = TokEnd;
}
