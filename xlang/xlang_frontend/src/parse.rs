use std::convert::Infallible;

use xlang::abi::ops::{ControlFlow, Try};

use crate::{
    diagnostics::{DiagPosition, NoLocation},
    iter::PeekMoreIterator,
    lexer::{DefaultTy, Group, LGroup, LToken, Lexeme, Lexer, TokenType},
    span::{NoHygiene, Spanned},
};

pub fn take_left<R>(left: R, _: R) -> R {
    left
}

pub fn take_right<R>(_: R, right: R) -> R {
    right
}

pub fn do_alternation<C, I: Iterator, F: IntoIterator, R, S>(
    parse: &mut PeekMoreIterator<I>,
    alternates: F,
    mut combiner: R,
    state: &S,
) -> C
where
    C: Try + core::fmt::Debug,
    F::Item: FnOnce(&mut PeekMoreIterator<I>, &S) -> C,
    R: FnMut(C::Residual, C::Residual) -> C::Residual,
{
    crate::iter::with_rewinder_accept_on_continue(parse, move |tree| {
        let mut last_res = None;
        for alt in alternates {
            match alt(tree, state).branch() {
                ControlFlow::Continue(val) => return C::from_output(val),
                ControlFlow::Break(res) => {
                    match &mut last_res {
                        Some(last_res) => {
                            // Manual replace_with impl
                            struct AbortGuard;
                            impl Drop for AbortGuard {
                                #[allow(unconditional_recursion)] // Will only run twice due to the panic
                                fn drop(&mut self) {
                                    let _x = AbortGuard;
                                    panic!("AbortGuard dropped");
                                }
                            }
                            let abort_guard = AbortGuard;
                            let val = unsafe { core::ptr::read(last_res) };
                            let res = combiner(val, res);
                            unsafe {
                                core::ptr::write(last_res, res);
                            }
                            core::mem::forget(abort_guard)
                        }
                        x @ None => {
                            *x = Some(res);
                        }
                    }
                }
            }
        }

        C::from_residual(last_res.expect("alternates must not be empty"))
    })
}

pub trait TokenDiscriminator<L: Lexer>: Sized {
    fn matches(&self, lex: &LToken<L>) -> bool;
}

impl<L: Lexer> TokenDiscriminator<L> for Infallible {
    fn matches(&self, _: &LToken<L>) -> bool {
        match *self {}
    }
}

pub trait LexemeDiscriminator<L: Lexer>: Sized {
    fn matches(&self, lex: &Lexeme<L>) -> bool;
}

impl<L: Lexer, T: TokenDiscriminator<L>> LexemeDiscriminator<L> for T {
    fn matches(&self, lex: &Lexeme<L>) -> bool {
        match lex {
            Lexeme::Token(lex) => <T as TokenDiscriminator<L>>::matches(self, lex),
            _ => false,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LexemeClass<K, P, I = DefaultTy, GTy = Infallible, O = Infallible> {
    Ident(Option<I>),
    Keyword(K),
    Punct(P),
    Group(Option<GTy>),
    String,
    Char,
    Integer,
    Float,
    Other(O),
    Eof,
}
impl<K, P, I, GTy, O> LexemeClass<K, P, I, GTy, O> {
    pub fn matches<L: Lexer>(&self, lex: &Lexeme<L>) -> bool
    where
        K: TokenDiscriminator<L>,
        P: TokenDiscriminator<L>,
        I: PartialEq<L::IdentTy>,
        GTy: PartialEq<L::GroupType>,
        O: LexemeDiscriminator<L>,
    {
        match (lex, self) {
            (Lexeme::Token(tok), cl) => match (&tok.ty, cl) {
                (TokenType::Identifier(_), LexemeClass::Keyword(kw)) if kw.matches(tok) => true,
                (TokenType::Identifier(_), LexemeClass::Ident(None)) => true,
                (TokenType::Identifier(ty), LexemeClass::Ident(Some(cl_ty))) if cl_ty == ty => true,
                (TokenType::Eof, LexemeClass::Eof) => true,
                (TokenType::Punct, LexemeClass::Punct(p)) => p.matches(tok),
                (TokenType::String(_), LexemeClass::String) => true,
                (TokenType::Char(_), LexemeClass::Char) => true,
                (TokenType::Integer, LexemeClass::Integer) => true,
                (TokenType::Float, LexemeClass::Float) => true,
                (TokenType::Other(_), LexemeClass::Other(o)) => o.matches(lex),
                _ => false,
            },
            (Lexeme::Group(_), LexemeClass::Group(None)) => true,
            (Lexeme::Group(g), LexemeClass::Group(Some(cl_gty))) => cl_gty == &g.ty,
            (Lexeme::Other(_), LexemeClass::Other(o)) => o.matches(lex),
            _ => false,
        }
    }
}

pub trait Parser {
    type Loc;
    type Lexer: Lexer<Hygiene = Self::Hygiene>;
    type Keyword: TokenDiscriminator<Self::Lexer>;
    type Punct: TokenDiscriminator<Self::Lexer>;
    type Other: LexemeDiscriminator<Self::Lexer>;
    type Hygiene;

    fn build_loc(&self, tok: &Spanned<Lexeme<Self::Lexer>, Self::Hygiene>) -> Self::Loc;
}

pub type PLexemeClass<P> = LexemeClass<
    <P as Parser>::Keyword,
    <P as Parser>::Punct,
    <<P as Parser>::Lexer as Lexer>::IdentTy,
    <<P as Parser>::Lexer as Lexer>::GroupType,
    <P as Parser>::Other,
>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub struct Error<Cl, Lex, R = NoLocation, H = NoHygiene> {
    pub expected: Vec<Cl>,
    pub got: Lex,
    pub at: DiagPosition<R, H>,
    pub next: Option<Box<Self>>,
}

fn combine_errors_non_residual<Cl, Lex, R, H>(
    mut a: Error<Cl, Lex, R, H>,
    b: Error<Cl, Lex, R, H>,
) -> Error<Cl, Lex, R, H>
where
    DiagPosition<R, H>: PartialEq,
{
    if a.at == b.at {
        a.expected.extend(b.expected)
    } else {
        if let Some(mut next) = { a.next } {
            *next = combine_errors_non_residual(*next, b);
            a.next = Some(next);
        } else {
            a.next = Some(Box::new(b))
        }
    }

    a
}

pub fn combine_errors<Cl, Lex, R, H>(
    a: Result<Infallible, Error<Cl, Lex, R, H>>,
    b: Result<Infallible, Error<Cl, Lex, R, H>>,
) -> Result<Infallible, Error<Cl, Lex, R, H>>
where
    DiagPosition<R, H>: PartialEq,
{
    match (a, b) {
        (Err(a), Err(b)) => Err(combine_errors_non_residual(a, b)),
        (Ok(a), _) | (_, Ok(a)) => match a {},
    }
}

pub type PToken<P> = LToken<<P as Parser>::Lexer>;
pub type PGroup<P> = LGroup<<P as Parser>::Lexer>;
pub type PLexeme<P> = Lexeme<<P as Parser>::Lexer>;

pub type PError<P> = Error<PLexemeClass<P>, PLexeme<P>, <P as Parser>::Loc, <P as Parser>::Hygiene>;

pub type PResult<T, P> = Result<Spanned<T, <P as Parser>::Hygiene>, PError<P>>;

pub type PClassResult<T, P> =
    Result<(PLexemeClass<P>, Spanned<T, <P as Parser>::Hygiene>), PError<P>>;

pub fn do_lexeme_class<P: Parser, I: Iterator<Item = Spanned<PLexeme<P>, <P as Parser>::Hygiene>>>(
    parse: &P,
    it: &mut PeekMoreIterator<I>,
    cl: PLexemeClass<P>,
) -> PResult<PLexeme<P>, P>
where
    PLexeme<P>: Clone,
    P::Hygiene: Clone,
    <P::Lexer as Lexer>::IdentTy: PartialEq,
    <P::Lexer as Lexer>::GroupType: PartialEq,
{
    let tok = it.peek().cloned().unwrap();

    if cl.matches(tok.body()) {
        it.peek_next();
        Ok(tok)
    } else {
        let pos = parse.build_loc(&tok);
        let span = tok.span().clone();
        Err(Error {
            expected: vec![cl],
            got: tok.into_inner(),
            at: DiagPosition { span, extra: pos },
            next: None,
        })
    }
}

pub fn do_lexeme_classes<
    P: Parser,
    I: Iterator<Item = Spanned<PLexeme<P>, <P as Parser>::Hygiene>>,
    C: IntoIterator,
>(
    parse: &P,
    it: &mut PeekMoreIterator<I>,
    cl: C,
) -> PClassResult<PLexeme<P>, P>
where
    C::Item: AsRef<PLexemeClass<P>>,
    PLexeme<P>: Clone,
    P::Hygiene: Clone,
    <P::Lexer as Lexer>::IdentTy: PartialEq,
    <P::Lexer as Lexer>::GroupType: PartialEq,
    PLexemeClass<P>: Clone,
{
    let tok = it.peek().cloned().unwrap();
    let mut expected = vec![];
    for cl in cl {
        let class = cl.as_ref();
        if class.matches(tok.body()) {
            return Ok((class.clone(), tok));
        } else {
            expected.push(class.clone())
        }
    }

    let pos = parse.build_loc(&tok);
    let span = tok.span().clone();
    Err(Error {
        expected,
        got: tok.into_inner(),
        at: DiagPosition { span, extra: pos },
        next: None,
    })
}

pub fn do_lexeme_group<P: Parser, I: Iterator<Item = Spanned<PLexeme<P>, <P as Parser>::Hygiene>>>(
    parse: &P,
    it: &mut PeekMoreIterator<I>,
    ty: Option<<P::Lexer as Lexer>::GroupType>,
) -> PResult<PGroup<P>, P>
where
    PLexeme<P>: Clone,
    P::Hygiene: Clone,
    <P::Lexer as Lexer>::IdentTy: PartialEq,
    <P::Lexer as Lexer>::GroupType: PartialEq,
{
    do_lexeme_class(parse, it, LexemeClass::Group(ty)).map(|lex| {
        lex.map(|lex| match lex {
            Lexeme::Group(g) => g,
            _ => unreachable!(),
        })
    })
}

pub fn do_token_class<P: Parser, I: Iterator<Item = Spanned<PLexeme<P>, <P as Parser>::Hygiene>>>(
    parse: &P,
    it: &mut PeekMoreIterator<I>,
    cl: PLexemeClass<P>,
) -> PResult<PToken<P>, P>
where
    PLexeme<P>: Clone,
    P::Hygiene: Clone,
    <P::Lexer as Lexer>::IdentTy: PartialEq,
    <P::Lexer as Lexer>::GroupType: PartialEq,
{
    do_lexeme_class(parse, it, cl).map(|lex| {
        lex.map(|lex| match lex {
            Lexeme::Token(tok) => tok,
            _ => panic!("do_token_class is not valid for non-token Lexemes"),
        })
    })
}

pub fn do_token_classes<
    P: Parser,
    I: Iterator<Item = Spanned<PLexeme<P>, <P as Parser>::Hygiene>>,
    C: IntoIterator,
>(
    parse: &P,
    it: &mut PeekMoreIterator<I>,
    cl: C,
) -> PClassResult<PToken<P>, P>
where
    C::Item: AsRef<PLexemeClass<P>>,
    PLexeme<P>: Clone,
    P::Hygiene: Clone,
    <P::Lexer as Lexer>::IdentTy: PartialEq,
    <P::Lexer as Lexer>::GroupType: PartialEq,
    PLexemeClass<P>: Clone,
{
    do_lexeme_classes(parse, it, cl).map(|(cl, lex)| {
        (
            cl,
            lex.map(|lex| match lex {
                Lexeme::Token(tok) => tok,
                _ => panic!("do_token_class is not valid for non-token Lexemes"),
            }),
        )
    })
}

#[macro_export]
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

        impl<L: $crate::lexer::Lexer> $crate::parse::TokenDiscriminator<L> for Punctuation{
            fn matches(&self, ty: &$crate::lexer::LToken<L>) -> bool{
                match &ty.ty{
                    $crate::lexer::TokenType::Punct => &ty.body == self.symbol(),
                    _ => false
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
                    $crate::parse::LexemeClass::Punctuation($crate::lex::Punctuation::$name)
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

#[macro_export]
macro_rules! keywords{
    {
        type $pat:pat in $ident_ty:ty;
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

        impl<L: $crate::lexer::Lexer<IdentTy = $ident_ty>> $crate::parse::TokenDiscriminator<L>  for Keyword{
            fn matches(&self, ty: &$crate::lexer::LToken<L>) -> bool{
                match &ty.ty{
                    $crate::lexer::TokenType::Identifier($($pat)|+) => &ty.body == self.symbol(),
                    _ => false
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
                    $crate::parse::LexemeClass::Keyword($crate::lex::Keyword::$name)
                };
            )*

            [$tt:tt] => {
                {
                    ::core::compile_error!(::core::concat!("Not a keyword: ", ::core::stringify!($tt)))
                }
            };
        }

        pub use keyword;
    };
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

        impl<L: $crate::lexer::Lexer> $crate::parse::TokenDiscriminator<L>  for Keyword{
            fn matches(&self, ty: &$crate::lexer::LToken<L>) -> bool{
                match &ty.ty{
                    $crate::lexer::TokenType::Identifier(_) => &ty.body == self.symbol(),
                    _ => false
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
                    $crate::parse::LexemeClass::Keyword($crate::lex::Keyword::$name)
                };
            )*

            [$tt:tt] => {
                {
                    ::core::compile_error!(::core::concat!("Not a keyword: ", ::core::stringify!($tt)))
                }
            };
        }

        pub use keyword;
    };
}
