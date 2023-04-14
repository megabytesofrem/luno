use logos::Logos;

#[derive(Debug, Clone, PartialEq, Eq, Logos)]
#[logos(skip r"[\t\n\f ]+")]
pub enum TokenKind {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("=")]
    Equal,
    #[token("==")]
    Eq,
    #[token("~=")]
    NotEq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("=>")]
    FatArrow,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    // Keywords
    #[token("import")]
    Import,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("fn")]
    Fn,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("do")]
    Do,
    #[token("of")]
    Of,
    #[token("var")]
    Var,
    #[token("while")]
    While,
    #[token("then")]
    Then,
    #[token("end")]
    End,
    #[token("ret")]
    Ret,

    // Literals
    #[regex(r"[0-9]+")]
    Integer,
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    #[regex(r"#[^\n]*", logos::skip)]
    Comment,

    Literal,
    EOF,
}

pub type Location = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Location,
}

pub fn lex_tokens(input: &str) -> impl Iterator<Item = Token> + '_ {
    TokenKind::lexer(input).spanned().map(|(kind, span)| Token {
        kind: kind.unwrap_or(TokenKind::EOF),
        span,
    })
}
