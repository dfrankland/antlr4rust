use std::cell::{Cell, RefCell};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use antlr_rust::char_stream::CharStream;
use antlr_rust::common_token_factory::TokenFactory;
use antlr_rust::error_listener::{ConsoleErrorListener, ErrorListener};
use antlr_rust::errors::ANTLRError;
use antlr_rust::lexer_atn_simulator::{ILexerATNSimulator, LexerATNSimulator};
use antlr_rust::parser_rule_context::ParserRuleContext;
use antlr_rust::recognizer::{Actions, Recognizer};
use antlr_rust::token::{Token, TOKEN_INVALID_TYPE};
use antlr_rust::token_source::TokenSource;
use antlr_rust::lexer::{
  LexerPosition,
  Lexer,
  LexerRecog,
  LEXER_DEFAULT_MODE,
  LEXER_MORE,
  LEXER_SKIP,
  LEXER_DEFAULT_TOKEN_CHANNEL,
  LEXER_HIDDEN,
  LEXER_MIN_CHAR_VALUE,
  LEXER_MAX_CHAR_VALUE,
};

pub struct MyBaseLexer<T: LexerRecog<Self> + 'static> {
    pub interpreter: Option<LexerATNSimulator>,
    pub input: Option<Box<dyn CharStream>>,
    recog: Box<T>,

    factory: &'static dyn TokenFactory,

    error_listeners: RefCell<Vec<Box<dyn ErrorListener>>>,

    pub token_start_char_index: isize,
    pub token_start_line: isize,
    pub token_start_column: isize,
    current_pos: Rc<LexerPosition>,
    pub token_type: isize,
    pub token: Option<Box<dyn Token>>,
    hit_eof: bool,
    pub channel: isize,
    mode_stack: Vec<usize>,
    pub mode: usize,
    pub text: String,
}

impl<T: LexerRecog<Self> + 'static> Deref for MyBaseLexer<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.recog
    }
}

impl<T: LexerRecog<Self> + 'static> DerefMut for MyBaseLexer<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.recog
    }
}

impl<T: LexerRecog<Self> + 'static> Recognizer for MyBaseLexer<T> {
    fn sempred(&mut self, _localctx: &dyn ParserRuleContext, rule_index: isize, action_index: isize) -> bool {
        <T as Actions>::sempred(_localctx, rule_index, action_index, self)
    }

    fn action(&mut self, _localctx: &dyn ParserRuleContext, rule_index: isize, action_index: isize) {
        <T as Actions>::action(_localctx, rule_index, action_index, self)
    }
}

impl<T: LexerRecog<Self> + 'static> MyBaseLexer<T> {

    fn emit_token(&mut self, token: Box<dyn Token>) {
        self.token = Some(token);
    }

    fn emit(&mut self) {
        <T as LexerRecog<Self>>::before_emit(self);
        let stop = self.get_char_index() - 1;
        let token = self.factory.create(
            Some(self.input.as_mut().unwrap().as_mut()),
            self.token_type,
            self.channel,
            self.token_start_char_index,
            stop,
            self.token_start_line,
            self.token_start_column,

        );
        self.emit_token(token);
    }

    fn emit_eof(&mut self) {
        let token = self.factory.create(
            None,
            antlr_rust::int_stream::EOF,
            LEXER_DEFAULT_TOKEN_CHANNEL,
            self.get_char_index(),
            self.get_char_index() - 1,
            self.get_line(),
            self.get_char_position_in_line(),
        );
        self.emit_token(token)
    }

    pub fn get_type(&self) -> isize {
        self.token_type
    }

    pub fn get_char_index(&self) -> isize {
        self.input.as_ref().unwrap().index()
    }

    pub fn get_text(&self) -> String {
        self.input.as_ref().unwrap().get_text(self.token_start_char_index, self.get_char_index() - 1)
    }

    pub fn set_text(&self, _text: String) {
        unimplemented!()
    }

    fn get_all_tokens(&self) -> Vec<Box<dyn Token>> {
        unimplemented!()
    }

    fn get_error_display_for_char(&self, _c: char) -> String {
        unimplemented!()
    }

    fn get_char_error_display(&self, _c: char) -> String {
        unimplemented!()
    }

    /// Add error listener
    pub fn add_error_listener(&mut self, listener: Box<dyn ErrorListener>) {
        self.error_listeners.borrow_mut().push(listener);
    }

    pub fn remove_error_listeners(&mut self) {
        self.error_listeners.borrow_mut().clear();
    }

    pub fn new_base_lexer(
        input: Box<dyn CharStream>,
        interpreter: LexerATNSimulator,
        recog: Box<T>,
    ) -> MyBaseLexer<T> {
        let mut lexer = MyBaseLexer {
            interpreter: Some(interpreter),
            input: Some(input),
            recog,
            factory: antlr_rust::common_token_factory::CommonTokenFactoryDEFAULT.as_ref(),
            error_listeners: RefCell::new(vec![Box::new(ConsoleErrorListener {})]),
            token_start_char_index: 0,
            token_start_line: 0,
            token_start_column: 0,
            current_pos: Rc::new(LexerPosition { line: Cell::new(1), char_position_in_line: Cell::new(0) }),
            token_type: antlr_rust::token::TOKEN_INVALID_TYPE,
            text: "".into(),
            token: None,
            hit_eof: false,
            channel: antlr_rust::token::TOKEN_DEFAULT_CHANNEL,
            //            token_factory_source_pair: None,
            mode_stack: Vec::new(),
            mode: self::LEXER_DEFAULT_MODE,
        };
        let pos = lexer.current_pos.clone();
        lexer.interpreter.as_mut().unwrap().current_pos = pos;
        lexer
    }
}

impl<T: LexerRecog<Self> + 'static> TokenSource for MyBaseLexer<T> {
    #[allow(unused_labels)]
    fn next_token(&mut self) -> Box<dyn Token> {
        assert!(self.input.is_some());

        let _marker = self.input.as_mut().unwrap().mark();
        'outer: loop {
            if self.hit_eof {
                self.emit_eof();
                break;
            }
            self.token = None;
            self.channel = LEXER_DEFAULT_TOKEN_CHANNEL;
            self.token_start_column = self.interpreter.as_ref().unwrap().get_char_position_in_line();
            self.token_start_line = self.interpreter.as_ref().unwrap().get_line();
            self.text = String::new();
            let index = self.get_input_stream().index();
            self.token_start_char_index = index;

            'inner: loop {
                let ttype;
                self.token_type = TOKEN_INVALID_TYPE;
                {
                    // detach from self, to allow self to be passed deeper
                    let mut interpreter = self.interpreter.take().unwrap();
//                    let mut input = self.input.take().unwrap();
                    let result = interpreter
                        .match_token(self.mode, self);
                    self.interpreter = Some(interpreter);

                    ttype = match result {
                        Ok(ttype) => {
//                            println!("new mode {}",self.mode);
                            ttype
                        },
                        Err(err) => {
//                            println!("error, recovering");
                            notify_listeners(&mut self.error_listeners.borrow_mut(), &err, self);
                            self.interpreter.as_mut().unwrap().recover(err, self.input.as_mut().unwrap().deref_mut());
                            LEXER_SKIP
                        }
                    };
//                    self.input = Some(input)
                }
                if self.get_input_stream().la(1) == antlr_rust::int_stream::EOF {
                    self.hit_eof = true;
                }

                if self.token_type == TOKEN_INVALID_TYPE {
                    self.token_type = ttype;
                }

                if self.token_type == LEXER_SKIP {
                    continue 'outer;
                }

                if self.token_type != LEXER_MORE {
                    break;
                }
            }

            if self.token.is_none() {
                self.emit();
                break;
            }
        }
        self.input.as_mut().unwrap().release(_marker);
        self.token.take().unwrap()
    }

    fn get_line(&self) -> isize {
        self.current_pos.line.get()
    }

    fn get_char_position_in_line(&self) -> isize {
        self.current_pos.char_position_in_line.get()
    }

    fn get_input_stream(&mut self) -> &mut dyn CharStream {
        self.input.as_mut().unwrap().deref_mut()
    }

    fn get_source_name(&self) -> String {
        self.input.as_deref().map(|it| it.get_source_name()).unwrap_or("<none>".to_string())
    }

//    fn set_token_factory<'c: 'b>(&mut self, f: &'c TokenFactory) {
//        self.factory = f;
//    }

    fn get_token_factory(&self) -> &dyn TokenFactory {
        self.factory
    }
}

fn notify_listeners<T: LexerRecog<MyBaseLexer<T>> + 'static>(_liseners: &mut Vec<Box<dyn ErrorListener>>, e: &ANTLRError, lexer: &MyBaseLexer<T>) {
    let text = format!("token recognition error at: '{}'", lexer.input.as_ref().unwrap().get_text(lexer.token_start_char_index, lexer.get_char_index()));
    for listener in _liseners.iter_mut() {
        listener.syntax_error(lexer, None, lexer.token_start_line, lexer.token_start_column, &text, Some(e))
    }
}


impl<T: LexerRecog<Self> + 'static> Lexer for MyBaseLexer<T> {
    fn set_channel(&mut self, v: isize) {
        self.channel = v;
    }

    fn push_mode(&mut self, _m: usize) {
        self.mode_stack.push(self.mode);
        self.mode = _m;
    }

    fn pop_mode(&mut self) -> Option<usize> {
        self.mode_stack.pop().map(|mode| {
            self.mode = mode;
            mode
        })
    }

    fn set_type(&mut self, t: isize) {
        self.token_type = t;
    }

    fn set_mode(&mut self, m: usize) {
        self.mode = m;
    }

    fn more(&mut self) {
        self.set_type(LEXER_MORE)
    }

    fn skip(&mut self) {
        self.set_type(LEXER_SKIP)
    }

    fn reset(&mut self) {
        unimplemented!()
    }

    fn get_interpreter(&self) -> Option<&LexerATNSimulator> { self.interpreter.as_ref() }
}
