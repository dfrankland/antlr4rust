use std::borrow::{Borrow, BorrowMut, Cow};
use std::borrow::Cow::{Borrowed, Owned};
use std::cell::Cell;
use std::fmt::Debug;
use std::marker::{PhantomData, Unsize};
use std::ops::{CoerceUnsized, Deref};
use std::sync::atomic::AtomicIsize;

use typed_arena::Arena;

use crate::char_stream::{CharStream, InputData};
use crate::token::{CommonToken, OwningToken, TOKEN_INVALID_TYPE};
use crate::token::Token;

lazy_static! {
    pub static ref CommonTokenFactoryDEFAULT: Box<CommonTokenFactory> =
        Box::new(CommonTokenFactory {});
    pub static ref INVALID_OWNING: Box<OwningToken> = Box::new(OwningToken {
        token_type: TOKEN_INVALID_TYPE,
        channel: 0,
        start: -1,
        stop: -1,
        token_index: AtomicIsize::new(-1),
        line: -1,
        column: -1,
        text: "<invalid>".to_owned(),
        read_only: true,
    });
    pub static ref INVALID_COMMON: Box<CommonToken<'static>> = Box::new(CommonToken {
        token_type: TOKEN_INVALID_TYPE,
        channel: 0,
        start: -1,
        stop: -1,
        token_index: AtomicIsize::new(-1),
        line: -1,
        column: -1,
        text: Borrowed("<invalid>"),
        read_only: true,
    });
}

// todo remove redundant allocation for arenas

/// Trait for creating tokens
pub trait TokenFactory<'a>: Sized {
    /// type of tokens emitted by this factory
    type Inner: Token<Data=Self::Data> + ?Sized + 'a;
    /// ownership of the emitted tokens
    type Tok: Borrow<Self::Inner> + Clone + 'a + Debug;
    /// type of the underlying storage
    type Data: InputData + ?Sized;
    /// type of the reference to `Self::Data` that factory needs for producing tokens
    type From: Borrow<Self::Data>;

    /// Creates token
    fn create<T>(
        &'a self,
        source: Option<&mut T>,
        ttype: isize,
        text: Option<String>,
        channel: isize,
        start: isize,
        stop: isize,
        line: isize,
        column: isize,
    ) -> Self::Tok
        where
            T: CharStream<Self::From> + ?Sized;

    /// Creates invalid token
    /// Invalid tokens must have `TOKEN_INVALID_TYPE` token type.
    fn create_invalid() -> Self::Tok;
}

#[derive(Default)]
pub struct CommonTokenFactory;

impl Default for &'_ CommonTokenFactory {
    fn default() -> Self { &**CommonTokenFactoryDEFAULT }
}

impl<'a> TokenFactory<'a> for CommonTokenFactory {
    type Inner = CommonToken<'a>;
    type Tok = Box<Self::Inner>;
    type Data = str;
    type From = &'a str;

    #[inline]
    fn create<T>(
        &'a self,
        source: Option<&mut T>,
        ttype: isize,
        text: Option<String>,
        channel: isize,
        start: isize,
        stop: isize,
        line: isize,
        column: isize,
    ) -> Self::Tok
        where
            T: CharStream<Self::From> + ?Sized,
    {
        let text = match (text, source) {
            (Some(t), _) => Owned(t),
            (None, Some(x)) => {
                if stop >= x.size() || start >= x.size() {
                    Borrowed("<EOF>")
                } else {
                    x.get_text(start, stop).into()
                }
            }
            _ => Borrowed(""),
        };
        Box::new(CommonToken {
            token_type: ttype,
            channel,
            start,
            stop,
            token_index: AtomicIsize::new(-1),
            line,
            column,
            text,
            read_only: false,
        })
    }

    fn create_invalid() -> Self::Tok { INVALID_COMMON.clone() }
}

#[derive(Default)]
pub struct OwningTokenFactory;

impl<'a> TokenFactory<'a> for OwningTokenFactory {
    type Inner = OwningToken;
    type Tok = Box<Self::Inner>;
    type Data = str;
    type From = String;

    #[inline]
    fn create<T>(
        &'a self,
        source: Option<&mut T>,
        ttype: isize,
        text: Option<String>,
        channel: isize,
        start: isize,
        stop: isize,
        line: isize,
        column: isize,
    ) -> Self::Tok
        where
            T: CharStream<String> + ?Sized,
    {
        let text = match (text, source) {
            (Some(t), _) => t,
            (None, Some(x)) => {
                if stop >= x.size() || start >= x.size() {
                    "<EOF>".to_owned()
                } else {
                    x.get_text(start, stop)
                }
            }
            _ => String::new(),
        };
        Box::new(OwningToken {
            token_type: ttype,
            channel,
            start,
            stop,
            token_index: AtomicIsize::new(-1),
            line,
            column,
            text,
            read_only: false,
        })
    }

    fn create_invalid() -> Self::Tok { INVALID_OWNING.clone() }
}

// pub struct DynFactory<'input,TF:TokenFactory<'.into()input>>(TF) where TF::Tok:CoerceUnsized<Box<dyn Token+'input>>;
// impl <'input,TF:TokenFactory<'input>> TokenFactory<'input> for DynFactory<'input,TF>
// where TF::Tok:CoerceUnsized<Box<dyn Token+'input>>
// {
//
// }

pub type ArenaOwningFactory<'a> = ArenaFactory<'a, OwningTokenFactory, OwningToken>;
pub type ArenaCommonFactory<'a> = ArenaFactory<'a, CommonTokenFactory, CommonToken<'a>>;

/// This is a wrapper for Token factory that allows to allocate tokens in separate arena.
/// It can allow to significantly improve performance by passing Tokens by references everywhere.
// Box is used here because it is almost always should be used for token factory
pub struct ArenaFactory<
    'input,
    TF: TokenFactory<'input, Tok=Box<T>, Inner=T>,
    T: Token<Data=TF::Data> + Clone + 'input,
> {
    arena: Arena<T>,
    factory: TF,
    pd: PhantomData<&'input str>,
}

impl<
    'input,
    TF: TokenFactory<'input, Tok=Box<T>, Inner=T> + Default,
    T: Token<Data=TF::Data> + Clone + 'input,
> Default for ArenaFactory<'input, TF, T>
{
    fn default() -> Self {
        Self {
            arena: Default::default(),
            factory: Default::default(),
            pd: Default::default(),
        }
    }
}

impl<'input, TF, Tok> TokenFactory<'input> for ArenaFactory<'input, TF, Tok>
    where
        TF: TokenFactory<'input, Tok=Box<Tok>, Inner=Tok>,
        Tok: Token<Data=TF::Data> + Clone + 'input,
        for<'a> &'a Tok: Default,
{
    type Inner = Tok;
    type Tok = &'input Tok;
    type Data = TF::Data;
    type From = TF::From;

    #[inline]
    fn create<T>(
        &'input self,
        source: Option<&mut T>,
        ttype: isize,
        text: Option<String>,
        channel: isize,
        start: isize,
        stop: isize,
        line: isize,
        column: isize,
    ) -> Self::Tok
        where
            T: CharStream<Self::From> + ?Sized,
    {
        let token = self
            .factory
            .create(source, ttype, text, channel, start, stop, line, column);
        self.arena.alloc(*token)
    }

    fn create_invalid() -> &'input Tok { <&Tok as Default>::default() }
}

pub trait TokenAware<'input> {
    type TF: TokenFactory<'input> + 'input;
}
