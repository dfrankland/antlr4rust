use crate::atn::ATN;
use crate::common_token_factory::{TokenAware, TokenFactory};
use crate::parser_rule_context::ParserRuleContext;
use crate::vocabulary::Vocabulary;

/// Major version of this runtime.
/// Used by generated parser to verify that it is compatible with current version of runtime
pub const VERSION_MAJOR: &'static str = env!("CARGO_PKG_VERSION_MAJOR");
/// Major version of this runtime.
/// Used by generated parser to verify that it is compatible with current version of runtime
pub const VERSION_MINOR: &'static str = env!("CARGO_PKG_VERSION_MINOR");

// todo move to compile time check when it will be possible to compare strings in constants
/// Used by generated parser to verify that it is compatible with current version of runtime
pub fn check_version(major: &str, minor: &str) {
    assert!(major == VERSION_MAJOR && minor == VERSION_MINOR,
            "parser is not compatible with current runtime version, please generate parser with the latest version of ANTLR")
}
//todo just a reminder to update version to be inserted in generated parser,
//const _:[();0-!(VERSION_MAJOR == "0" && VERSION_MINOR == "1") as usize] = [];

/// **! Usually generated by ANTLR !**
pub trait Recognizer<'input>: TokenAware<'input> {
    fn sempred(&mut self, localctx: &(dyn ParserRuleContext<'input, TF=Self::TF> + 'input), rule_index: isize, action_index: isize) -> bool
        where Self: Sized
    { true }
    fn action(&mut self, localctx: &(dyn ParserRuleContext<'input, TF=Self::TF> + 'input), rule_index: isize, action_index: isize)
        where Self: Sized
    {}

    /// Returns array of rule names.
    /// Used for debugging and error reporting
    fn get_rule_names(&self) -> &[&str] {
        &[]
    }
    fn get_vocabulary(&self) -> &dyn Vocabulary { unimplemented!() }

    /// Name of the file this recognizer was generated from
    fn get_grammar_file_name(&self) -> &str { "" }
    fn get_atn(&self) -> &ATN { unimplemented!() }
}

/// **! Usually generated by ANTLR !**
///
/// Used to make user predicates and actions callable by parser
/// Generated by ANTLR tool from actions and predicated added in grammar file
pub trait Actions<'a, TF: TokenFactory<'a>, T> {
    fn sempred(_localctx: &dyn ParserRuleContext<'a, TF=TF>, _rule_index: isize, _action_index: isize,
               _recog: &mut T,
    ) -> bool {
        true
    }

    fn action(_localctx: &dyn ParserRuleContext<'a, TF=TF>, _rule_index: isize, _action_index: isize,
              _recog: &mut T,
    ) {}
}

//impl Recognizer for BaseRecognizer {
//    fn get_state(&self) -> isize {
//        self.state
//    }
//
//    fn set_state(&mut self, _v: isize) {
//        self.state = _v;
//    }
//
//    fn add_error_listener(&mut self, _listener: Box<ErrorListener>) {
//        self.listeners.push(_listener)
//    }
//
//    fn remove_error_listeners(&self) {
//        unimplemented!()
//    }
//
//    fn get_error_listener_dispatch(&self) -> Box<ErrorListener> {
//        unimplemented!()
//    }
//}
//
//pub struct BaseRecognizer {
//    pub listeners: Vec<Box<ErrorListener>>,
//    pub state: isize, //    rule_names: Vec<String>,
//    //    literal_names: Vec<String>,
//    //    symbolic_names: Vec<String>,
//    //    grammar_file_name: String
//}
//
//impl BaseRecognizer {
//    pub fn new_base_recognizer() -> BaseRecognizer {
//        BaseRecognizer {
//            listeners: Vec::new(),
//            state: -1,
//        }
//    }
//
//    fn check_version(&self, _toolVersion: String) {
//        unimplemented!()
//    }
//
//    fn get_token_names(&self) -> Vec<String> {
//        unimplemented!()
//    }
//
//    fn get_rule_index_map(&self) -> Map<isize, String> {
//        unimplemented!()
//    }
//
//    fn get_token_type(&self, _tokenName: String) -> isize {
//        unimplemented!()
//    }
//
//    fn get_error_header(&self, _e: ANTLRError) -> String {
//        unimplemented!()
//    }
//
//    fn get_token_error_display(&self, _t: &Token) -> String {
//        unimplemented!()
//    }
//}
