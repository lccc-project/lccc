use core::fmt;

use crate::{
    ast,
    interning::Symbol,
    span::{Pos, Span, Speekable, Speekerator},
};

use unicode_xid::UnicodeXID;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum GroupType {
    Parens,
    Brackets,
    Braces,
}

impl GroupType {
    #[allow(dead_code)]
    pub fn start_char(&self) -> char {
        match self {
            Self::Parens => '(',
            Self::Brackets => '[',
            Self::Braces => '{',
        }
    }

    pub fn end_char(&self) -> char {
        match self {
            Self::Parens => ')',
            Self::Brackets => ']',
            Self::Braces => '}',
        }
    }

    pub fn from_start_char(start: char) -> Self {
        match start {
            '(' => Self::Parens,
            '[' => Self::Brackets,
            '{' => Self::Braces,
            _ => panic!("invalid start char"),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum IdentifierType {
    Default,
    Keyword,
    Raw,
    Reserved,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum StringType {
    Default,
    Raw(u8), // number of #s
    Byte,
    RawByte(u8), // see above
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum CharType {
    Default,
    Byte,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum TokenType {
    Character(CharType),
    CommentMulti,
    CommentSingle,
    Identifier(IdentifierType),
    Lifetime,
    Number,
    Punctuation,
    String(StringType),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Group {
    pub ty: GroupType,
    pub body: Vec<Lexeme>,
}

impl Group {
    #[allow(dead_code)] // TODO: Evaluate if we care about this
    pub fn new(ty: GroupType, body: Vec<Lexeme>) -> Self {
        Self { ty, body }
    }

    pub fn with_span(self, span: Span) -> Lexeme {
        Lexeme {
            span,
            body: LexemeBody::Group(self),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Token {
    pub ty: TokenType,
    pub body: Symbol,
}

impl Token {
    pub fn new(mut ty: TokenType, body: impl Into<Symbol>) -> Self {
        let body = body.into();
        if matches!(ty, TokenType::Identifier(IdentifierType::Default)) {
            if matches!(
                body.as_str(),
                "as" | "break"
                    | "const"
                    | "continue"
                    | "crate"
                    | "else"
                    | "enum"
                    | "extern"
                    | "false"
                    | "fn"
                    | "for"
                    | "if"
                    | "impl"
                    | "in"
                    | "let"
                    | "loop"
                    | "match"
                    | "mod"
                    | "move"
                    | "mut"
                    | "pub"
                    | "ref"
                    | "return"
                    | "self"
                    | "Self"
                    | "static"
                    | "struct"
                    | "super"
                    | "trait"
                    | "true"
                    | "type"
                    | "unsafe"
                    | "use"
                    | "where"
                    | "while"
            ) || matches!(body.as_str(), "async" | "await" | "dyn")
            {
                ty = TokenType::Identifier(IdentifierType::Keyword);
            } else if matches!(
                body.as_str(),
                "abstract"
                    | "become"
                    | "box"
                    | "do"
                    | "final"
                    | "macro"
                    | "override"
                    | "priv"
                    | "typeof"
                    | "unsized"
                    | "virtual"
                    | "yield"
            ) || matches!(body.as_str(), "try")
            {
                ty = TokenType::Identifier(IdentifierType::Reserved);
            }
        }
        Self { ty, body }
    }

    pub fn punct(body: impl Into<Symbol>) -> Self {
        Self::new(TokenType::Punctuation, body)
    }

    pub fn with_span(self, span: Span) -> Lexeme {
        Lexeme {
            span,
            body: LexemeBody::Token(self),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum LexemeBody {
    Group(Group),
    Token(Token),
    AstFrag(Box<AstFrag>),
    Eof,
}

impl fmt::Debug for LexemeBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Group(Group { ty, body }) => {
                f.debug_tuple("Group").field(ty).field(body).finish()
            }
            Self::Token(Token { ty, body }) => write!(f, "Token({:?}, {:?})", ty, body),
            Self::AstFrag(frag) => write!(f, "AstFrag({:?})", frag),
            Self::Eof => write!(f, "Eof"),
        }
    }
}

macro_rules! punctuation {
    {
        $($tok:tt => $name:ident),* $(,)?
    } => {
        #[derive(Clone, Copy, Hash, PartialEq, Eq)]
        pub enum Punctuation {
            $($name),*
        }

        impl Punctuation{
            pub fn from_token(x: &str) -> Self {
                match x {
                    $(::core::stringify!($tok) => Self::$name,)*
                    _ => panic!("Not a punctuation token: {}", x),
                }
            }

            #[allow(dead_code)]
            pub const fn symbol(&self) -> &'static str {
                match self {
                    $(Self::$name => ::core::stringify!($tok)),*
                }
            }
        }

        impl core::fmt::Debug for Punctuation{
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
                match self{
                    $(Self::$name => f.write_str(::core::stringify!($tok))),*
                }
            }
        }

        impl core::fmt::Display for Punctuation{
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
                match self{
                    $(Self::$name => f.write_str(::core::stringify!($tok))),*
                }
            }
        }

        #[macro_export]
        macro_rules! punct {
            $(
                [
                    $tok
                ] => {
                    $crate::lex::LexemeClass::Punctuation($crate::lex::Punctuation::$name)
                };
            )*

            [$tt:tt] => {
                {
                    ::core::compile_error!(::core::concat!("Not a punctuation token: ", ::core::stringify!($tt)))
                }
            };
        }

        pub use punct;
    }
}

macro_rules! keywords{
    {
        $($tok:tt => $name:ident),* $(,)?
    } => {
        #[derive(Clone, Copy, Hash, PartialEq, Eq)]
        pub enum Keyword {
            $($name),*
        }

        impl Keyword {
            pub fn from_token(x: &str) -> Self {
                match x {
                    $(::core::stringify!($tok) => Self::$name,)*
                    _ => panic!("Not a keyword: {}", x),
                }
            }

            pub const fn symbol(&self) -> &'static str {
                match self {
                    $(Self::$name => ::core::stringify!($tok)),*
                }
            }
        }

        impl core::fmt::Debug for Keyword{
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
                match self{
                    $(Self::$name => f.write_str(::core::stringify!($tok))),*
                }
            }
        }

        impl core::fmt::Display for Keyword{
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
                match self{
                    $(Self::$name => f.write_str(::core::stringify!($tok))),*
                }
            }
        }

        #[macro_export]
        macro_rules! keyword {
            $(
                [
                    $tok
                ] => {
                    $crate::lex::LexemeClass::Keyword($crate::lex::Keyword::$name)
                };
            )*

            [$tt:tt] => {
                {
                    ::core::compile_error!(::core::concat!("Not a keyword: ", ::core::stringify!($tt)))
                }
            };
        }

        pub use keyword;
    }
}

punctuation! {
    + => Add,
    += => AddAssign,
    - => Sub,
    -= => SubAssign,
    * => Mul,
    *= => MulAssign,
    / => Div,
    /= => DivAssign,
    % => Rem,
    %= => RemAssign,
    & => BitAnd,
    &= => BitAndAssign,
    | => BitOr,
    |= => BItOrAsign,
    ^ => BitXor,
    ^= => BitXorAssign,
    << => RightShift,
    <<= => RightShiftAssign,
    >> => LeftShift,
    >>= => LeftShiftAssign,
    ! => Not,
    && => BoolAnd,
    || => BoolOr,
    = => Assign,
    == => Equal,
    != => NotEqual,
    <= => LessEqual,
    >= => GreaterEqual,
    < => Less,
    > => Greater,
    $ => Dollar,
    # => Hash,
    : => Colon,
    ; => Semi,
    ? => Question,
    , => Comma,
    . => Dot,
    -> => ThinArrow,
    => => Recursive,
    .. => DotDot,
    ..= => DotDotEquals,
    ... => Cursed,
    ~ => Tilde,
    :: => Path,
    @ => At,
}

keywords! {
    abstract => Abstract,
    as => As,
    async => Async,
    await => Await,
    auto => Auto,
    become => Become,
    box => Box,
    break => Break,
    const => Const,
    continue => Continue,
    crate => Crate,
    do => Do,
    dyn => Dyn,
    else => Else,
    enum => Enum,
    extern => Extern,
    false => False,
    final => Final,
    fn => Fn,
    for => For,
    if => If,
    impl => Impl,
    in => In,
    let => Let,
    loop => Loop,
    macro => Macro,
    match => Match,
    mod => Mod,
    move => Move,
    mut => Mut,
    override => Override,
    priv => Priv,
    pub => Pub,
    ref => Ref,
    return => Return,
    self => SelfPat,
    Self => SelfTy,
    static => Static,
    struct => Struct,
    super => Super,
    trait => Trait,
    true => True,
    try => Try,
    type => Type,
    typeof => Typeof,
    unsafe => Unsafe,
    use => Use,
    virtual => Virtual,
    where => Where,
    while => While,
    yield => Yield,
    // Contextual Keywords
    raw => Raw,
    yeet => Yeet,
    union => Union,
    macro_rules => MacroRules,
    // Special Lifetimes - only used in errors
    'static => StaticLife,
    '_ => WildcardLife,
    // Macro Fragment Specifiers
    block => Block,
    expr => Expr,
    ident => Ident,
    item => Item,
    lifetime => Lifetime,
    literal => Literal,
    meta => Meta,
    pat => Pat,
    pat_param => PatParam,
    path => Path,
    stmt => Stmt,
    tt => Tt,
    ty => Ty,
    vis => Vis,
    _ => Underscore,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum AstFragClass {
    Item,
    Vis,
    Expr,
    Meta,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum LexemeClass {
    Character,
    Eof,
    Group(Option<GroupType>),
    Keyword(Keyword),
    Identifier,
    IdentOrKeyword,
    Lifetime,
    Number,
    Punctuation(Punctuation),
    String,
    AstFrag(Option<AstFragClass>),
}

impl LexemeClass {
    pub fn of(lexeme: Option<&Lexeme>) -> Self {
        match lexeme {
            None
            | Some(Lexeme {
                body: LexemeBody::Eof,
                ..
            }) => Self::Eof,
            Some(Lexeme {
                body: LexemeBody::Group(Group { ty, .. }),
                ..
            }) => Self::Group(Some(*ty)),
            Some(Lexeme {
                body: LexemeBody::Token(token),
                ..
            }) => match token.ty {
                TokenType::Character(_) => Self::Character,
                TokenType::Identifier(ty) => match ty {
                    IdentifierType::Keyword => Self::Keyword(Keyword::from_token(&token.body)),
                    _ => Self::Identifier,
                },
                TokenType::Lifetime => Self::Lifetime,
                TokenType::Number => Self::Number,
                TokenType::Punctuation => Self::Punctuation(Punctuation::from_token(&token.body)),
                TokenType::String(_) => Self::String,
                _ => unreachable!(), // Comments should be removed by now
            },
            Some(Lexeme {
                body: LexemeBody::AstFrag(frag),
                ..
            }) => match &**frag {
                AstFrag::Expr(_) => Self::AstFrag(Some(AstFragClass::Expr)),
                AstFrag::Item(_) => Self::AstFrag(Some(AstFragClass::Item)),
                AstFrag::Vis(_) => Self::AstFrag(Some(AstFragClass::Vis)),
                AstFrag::Meta(_) => Self::AstFrag(Some(AstFragClass::Meta)),
            },
        }
    }

    pub fn matches(&self, tok: Option<&Lexeme>) -> bool {
        match (self, tok) {
            (
                Self::Eof,
                None
                | Some(Lexeme {
                    body: LexemeBody::Eof,
                    ..
                }),
            ) => true,
            (
                Self::Group(None),
                Some(Lexeme {
                    body: LexemeBody::Group(_),
                    ..
                }),
            ) => true,
            (
                Self::Group(Some(gty)),
                Some(Lexeme {
                    body: LexemeBody::Group(g),
                    ..
                }),
            ) => gty == &g.ty,
            (
                cl,
                Some(Lexeme {
                    body: LexemeBody::Token(token),
                    ..
                }),
            ) => match (cl, token.ty) {
                (Self::Character, TokenType::Character(_)) => true,
                (Self::Lifetime, TokenType::Lifetime) => true,
                (Self::Number, TokenType::Number) => true,
                (Self::String, TokenType::String(_)) => true,
                (Self::Punctuation(punct), TokenType::Punctuation) => {
                    Punctuation::from_token(&token.body) == *punct
                }
                (
                    Self::Keyword(kw),
                    TokenType::Identifier(IdentifierType::Default | IdentifierType::Keyword),
                ) => &token.body == kw.symbol(),
                (
                    Self::Identifier,
                    TokenType::Identifier(IdentifierType::Default | IdentifierType::Raw),
                ) => true,
                (Self::IdentOrKeyword, TokenType::Identifier(_)) => true,
                _ => false,
            },
            (
                LexemeClass::AstFrag(cl),
                Some(Lexeme {
                    body: LexemeBody::AstFrag(frag),
                    ..
                }),
            ) => match (cl, &**frag) {
                (None, _) => true,
                (Some(AstFragClass::Expr), AstFrag::Expr(_)) => true,
                (Some(AstFragClass::Item), AstFrag::Item(_)) => true,
                (Some(AstFragClass::Meta), AstFrag::Meta(_)) => true,
                (Some(AstFragClass::Vis), AstFrag::Vis(_)) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is(&self, other: &Self) -> bool {
        self == other
            || (matches!(other, Self::Group(None)) && matches!(self, Self::Group(_)))
            || (matches!(other, Self::AstFrag(None)) && matches!(self, Self::AstFrag(_)))
            || (matches!(other, Self::IdentOrKeyword)
                && matches!(self, Self::Identifier | Self::Keyword(_)))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Lexeme {
    pub span: Span,
    pub body: LexemeBody,
}

impl Lexeme {
    pub fn text(&self) -> Option<&Symbol> {
        match self.body {
            LexemeBody::Token(Token { ref body, .. }) => Some(body),
            _ => None,
        }
    }

    pub fn into_text(self) -> Option<Symbol> {
        match self.body {
            LexemeBody::Token(Token { body, .. }) => Some(body),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof(Pos),
    UnrecognizedChar(char, Pos),
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AstFrag {
    Vis(ast::Visibility),
    Expr(ast::Expr),
    Item(ast::Item),
    Meta(ast::Attr),
}

pub type Result<T> = core::result::Result<T, Error>;

pub trait IsEof {
    fn is_eof(&self) -> bool;
}

impl IsEof for Lexeme {
    fn is_eof(&self) -> bool {
        matches!(self.body, LexemeBody::Eof)
    }
}

impl IsEof for Option<Lexeme> {
    fn is_eof(&self) -> bool {
        matches!(
            self,
            Some(Lexeme {
                body: LexemeBody::Eof,
                ..
            })
        )
    }
}

impl IsEof for Option<&Lexeme> {
    fn is_eof(&self) -> bool {
        matches!(
            self,
            Some(Lexeme {
                body: LexemeBody::Eof,
                ..
            })
        )
    }
}

fn do_char(file: &mut Speekable<impl Iterator<Item = char>>, start: Pos) -> Result<Lexeme> {
    match file.snext() {
        Some((pos, '\\')) => {
            let mut token = String::from("'\\");
            let mut end = pos;
            loop {
                match file.snext() {
                    Some((pos, '\'')) => {
                        token.push('\'');
                        end = pos;
                        break;
                    }
                    Some((pos, '\n')) => Err(Error::UnrecognizedChar('\n', pos))?,
                    Some((pos, x)) => {
                        token.push(x);
                        end = pos;
                    }
                    None => Err(Error::UnexpectedEof(end))?,
                }
            }
            Ok(Token::new(TokenType::Character(CharType::Default), token)
                .with_span(Span::new_simple(start, end, file.file_name())))
        }
        Some((pos, x)) => {
            let mut token = String::from("'");
            token.push(x);
            match file.speek() {
                Some(&(end, '\'')) => {
                    file.next();
                    token.push('\'');
                    Ok(Token::new(TokenType::Character(CharType::Default), token)
                        .with_span(Span::new_simple(start, end, file.file_name())))
                }
                Some((_, x)) if !x.is_xid_continue() => Ok(Token::new(TokenType::Lifetime, token)
                    .with_span(Span::new_simple(start, pos, file.file_name()))),
                None => Ok(
                    Token::new(TokenType::Lifetime, token).with_span(Span::new_simple(
                        start,
                        pos,
                        file.file_name(),
                    )),
                ),
                Some(&(pos, x)) => {
                    file.next();
                    token.push(x);
                    let mut end = pos;
                    while let Some(&(pos, x)) = file.speek() {
                        if !x.is_xid_continue() {
                            break;
                        }
                        file.next();
                        end = pos;
                        token.push(x);
                    }
                    Ok(
                        Token::new(TokenType::Lifetime, token).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )),
                    )
                }
            }
        }
        None => Err(Error::UnexpectedEof(start))?,
    }
}

fn do_str(file: &mut Speekable<impl Iterator<Item = char>>) -> Result<(String, Pos)> {
    let mut str = String::from('"');
    let end = loop {
        if let Some((pos, c)) = file.snext() {
            str.push(c);
            match c {
                '"' => break pos,
                '\\' => match file.next() {
                    Some(c @ ('n' | 'r' | 't' | '\\' | '0' | '"')) => str.push(c),
                    x => todo!("{} {:?}", str, x),
                },
                '\n' => todo!(), // error
                _ => {}
            }
        } else {
            Err(Error::UnexpectedEof(file.last_pos()))?
        }
    };
    Ok((str, end))
}

fn do_lexeme(file: &mut Speekable<impl Iterator<Item = char>>) -> Result<Lexeme> {
    loop {
        match file.snext() {
            Some((start, c)) => {
                match c {
                    ' ' | '\n' => {}
                    '(' | '[' | '{' => {
                        let ty = GroupType::from_start_char(c);
                        let (body, end) = do_group(file, Some(ty.end_char()))?;
                        break Ok(Group { ty, body }.with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '"' => {
                        let (str, end) = do_str(file)?;
                        break Ok(Token::new(TokenType::String(StringType::Default), str)
                            .with_span(Span::new_simple(start, end, file.file_name())));
                    }
                    '0'..='9' => {
                        let mut id = String::from(c);
                        let mut end = start;
                        while let Some(&(pos, c)) = file.speek() {
                            if !c.is_xid_continue() {
                                break;
                            } else {
                                id.push(c);
                                end = pos;
                                file.next();
                            }
                        }
                        break Ok(
                            Token::new(TokenType::Number, id).with_span(Span::new_simple(
                                start,
                                end,
                                file.file_name(),
                            )),
                        );
                    }
                    x if x.is_xid_start() || x == '_' => {
                        let mut id = String::from(x);
                        let mut end = start;
                        let mut ty = TokenType::Identifier(IdentifierType::Default);
                        while let Some(&(pos, c)) = file.speek() {
                            if !c.is_xid_continue() {
                                break;
                            } else {
                                id.push(c);
                                end = pos;
                                file.next();
                            }
                        }
                        if id == "r" || id == "rb" {
                            match file.peek() {
                                Some('#') => {
                                    id.push('#');
                                    file.next();
                                    while let Some(&(pos, c)) = file.speek() {
                                        if !c.is_xid_continue() {
                                            break;
                                        } else {
                                            id.push(c);
                                            end = pos;
                                            file.next();
                                        }
                                    }
                                    ty = TokenType::Identifier(IdentifierType::Raw);
                                }
                                Some('"') => todo!(),
                                _ => {}
                            }
                        } else if id == "b" {
                            if file.peek() == Some(&'"') {
                                file.next();
                                let (str, end) = do_str(file)?;
                                break Ok(Token::new(
                                    TokenType::String(StringType::Byte),
                                    id + &str,
                                )
                                .with_span(Span::new_simple(start, end, file.file_name())));
                            } else if file.peek() == Some(&'\'') {
                                file.next();
                                let mut result = do_char(file, start)?;
                                let span = result.span;
                                match &mut result.body {
                                    LexemeBody::Token(Token {
                                        ty: TokenType::Character(_),
                                        body,
                                    }) => {
                                        break Ok(Token::new(
                                            TokenType::Character(CharType::Byte),
                                            id + &body,
                                        )
                                        .with_span(span));
                                    }
                                    LexemeBody::Token(_) => {
                                        todo!("validation error for \"byte lifetimes\"");
                                    }
                                    _ => unreachable!("do_char returns a Token always"),
                                }
                            }
                        }
                        break Ok(Token::new(ty, id).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '/' => {
                        let (tok, end, ty) = match file.speek() {
                            Some(&(pos, '/')) => {
                                file.next();
                                let mut tok = String::from("//");
                                let mut end = pos;
                                while let Some((pos, c)) = file.snext() {
                                    if c == '\n' {
                                        break;
                                    }
                                    tok.push(c);
                                    end = pos;
                                }
                                (tok, end, TokenType::CommentSingle)
                            }
                            Some(&(pos, '*')) => {
                                file.next();
                                let mut tok = String::from("/*");
                                let mut end = pos;
                                let mut star = false;
                                while let Some((pos, c)) = file.snext() {
                                    tok.push(c);
                                    end = pos;
                                    if c == '/' && star {
                                        break;
                                    }
                                    star = c == '*';
                                }
                                (tok, end, TokenType::CommentMulti)
                            }
                            Some(&(end, '=')) => {
                                file.next();
                                ("/=".into(), end, TokenType::Punctuation)
                            }
                            _ => ("/".into(), start, TokenType::Punctuation),
                        };
                        break Ok(Token::new(ty, tok).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '\'' => break do_char(file, start),
                    '.' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, '.')) => {
                                file.next();
                                match file.speek() {
                                    Some(&(end, '.')) => {
                                        file.next();
                                        ("...", end)
                                    }
                                    Some(&(end, '=')) => {
                                        file.next();
                                        ("..=", end)
                                    }
                                    _ => ("..", end),
                                }
                            }
                            _ => (".", start),
                        };
                        break Ok(Token::punct(punct).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '<' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, '<')) => {
                                file.next();
                                match file.speek() {
                                    Some(&(end, '=')) => {
                                        file.next();
                                        ("<<=", end)
                                    }
                                    _ => ("<<", end),
                                }
                            }
                            Some(&(end, '=')) => {
                                file.next();
                                ("<=", end)
                            }
                            _ => ("<", start),
                        };
                        break Ok(Token::punct(punct).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '>' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, '>')) => {
                                file.next();
                                match file.speek() {
                                    Some(&(end, '=')) => {
                                        file.next();
                                        (">>=", end)
                                    }
                                    _ => (">>", end),
                                }
                            }
                            Some(&(end, '=')) => {
                                file.next();
                                (">=", end)
                            }
                            _ => (">", start),
                        };
                        break Ok(Token::punct(punct).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '|' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, '|')) => ("||", end),
                            Some(&(end, '=')) => {
                                file.next();
                                ("|=", end)
                            }
                            _ => ("|", start),
                        };
                        break Ok(Token::punct(punct).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    ':' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, ':')) => {
                                file.next();
                                ("::", end)
                            }
                            _ => (":", start),
                        };
                        break Ok(Token::punct(punct).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '=' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, '=')) => {
                                file.next();
                                ("==", end)
                            }
                            Some(&(end, '>')) => {
                                file.next();
                                ("=>", end)
                            }
                            _ => ("=", start),
                        };
                        break Ok(Token::punct(punct).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '&' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, '=')) => {
                                file.next();
                                ("&=", end)
                            }
                            Some(&(end, '&')) => {
                                file.next();
                                ("&&", end)
                            }
                            _ => ("&", start),
                        };
                        break Ok(Token::punct(punct).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    '+' | '*' | '!' | '%' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, '=')) => {
                                file.next();
                                ("=", end)
                            }
                            _ => ("", start),
                        };
                        break Ok(Token::punct(String::from(c) + punct)
                            .with_span(Span::new_simple(start, end, file.file_name())));
                    }
                    '-' => {
                        let (punct, end) = match file.speek() {
                            Some(&(end, '>')) => {
                                file.next();
                                ("->", end)
                            }
                            Some(&(end, '=')) => {
                                file.next();
                                ("-=", end)
                            }
                            _ => ("-", start),
                        };
                        break Ok(Token::punct(punct).with_span(Span::new_simple(
                            start,
                            end,
                            file.file_name(),
                        )));
                    }
                    ';' | '#' | ',' | '@' => {
                        break Ok(Token::punct(String::from(c)).with_span(Span::new_simple(
                            start,
                            start,
                            file.file_name(),
                        )));
                    }
                    x => Err(Error::UnrecognizedChar(x, start))?,
                }
            }
            None => Err(Error::UnexpectedEof(file.last_pos()))?,
        }
    }
}

#[allow(dead_code)]
/// Assumes that the token is legal. If it is not, the result is unspecified.
pub fn do_lexeme_str(token: &str, span: Span) -> Result<Lexeme> {
    let mut iter = token.chars();
    let ty = match iter.next() {
        Some('"') => TokenType::String(StringType::Default),
        Some('0'..='9') => TokenType::Number,
        Some('\'') => {
            // Lifetime or char
            match iter.next() {
                Some(x) if x.is_xid_start() || x == '_' => match iter.next() {
                    Some('\'') => TokenType::Character(CharType::Default),
                    _ => TokenType::Lifetime,
                },
                _ => TokenType::Character(CharType::Default),
            }
        }
        Some(x) if x.is_xid_start() || x.is_xid_continue() => {
            let hash_pos = token.find('#');
            let quote_pos = token.find('"');
            match (hash_pos, quote_pos) {
                (None, None) => TokenType::Identifier(IdentifierType::Default),
                (None, Some(_)) => {
                    if x == 'b' {
                        TokenType::String(StringType::Byte)
                    } else if x == 'r' && iter.next() == Some('b') {
                        TokenType::String(StringType::RawByte(0))
                    } else {
                        // Could have an if, but again, we are assuming a legal token
                        TokenType::String(StringType::Raw(0))
                    }
                }
                (Some(_), None) => TokenType::Identifier(IdentifierType::Raw),
                (Some(a), Some(b)) if a > b => {
                    if x == 'b' {
                        TokenType::String(StringType::Byte)
                    } else if x == 'r' && iter.next() == Some('b') {
                        TokenType::String(StringType::RawByte(0))
                    } else {
                        // Could have an if, but again, we are assuming a legal token
                        TokenType::String(StringType::Raw(0))
                    }
                }
                (Some(_), Some(_)) => todo!(),
            }
        }
        Some(x) => Err(Error::UnrecognizedChar(x, Pos::default()))?, // invalid pos b/c we have no idea
        None => Err(Error::UnexpectedEof(Pos::default()))?,
    };
    Ok(Token::new(ty, token).with_span(span))
}

pub fn do_group(
    file: &mut Speekable<impl Iterator<Item = char>>,
    end_char: Option<char>,
) -> Result<(Vec<Lexeme>, Pos)> {
    let mut result = Vec::new();
    let end = loop {
        let lexeme = do_lexeme(file);
        if let Ok(lexeme) = lexeme {
            result.push(lexeme);
        } else if let Some(c) = end_char {
            if let Err(Error::UnrecognizedChar(x, end)) = lexeme {
                if x == c {
                    break end;
                } else {
                    lexeme?;
                }
            } else {
                lexeme?;
            }
        } else if let Err(Error::UnexpectedEof(end)) = lexeme {
            break end;
        } else {
            lexeme?;
        }
    };
    result.push(Lexeme {
        span: Span::new_simple(end, end, file.file_name()),
        body: LexemeBody::Eof,
    });
    Ok((result, end))
}

pub fn lex(
    file: &mut impl Iterator<Item = char>,
    file_name: impl Into<Symbol>,
) -> Result<Vec<Lexeme>> {
    do_group(&mut file.speekable(file_name), None).map(|x| x.0)
}

pub fn filter_comments(tree: &mut Vec<Lexeme>) {
    tree.retain_mut(|lexeme| match &mut lexeme.body {
        LexemeBody::Group(group) => {
            filter_comments(&mut group.body);
            true
        }
        LexemeBody::Token(Token {
            ty: TokenType::CommentMulti | TokenType::CommentSingle,
            ..
        }) => false, // TODO: Doc comments
        _ => true,
    })
}
