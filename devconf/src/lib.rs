//! Postludium: Device Configuration Processor

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate parsetools;
extern crate itertools;
extern crate interlude;

#[macro_use] mod items;
pub use items::*;
mod hobjects;
pub use hobjects::*;
mod resolver;

mod syntree;
mod parser;
pub use parser::*;
mod instantiate;
pub use instantiate::*;
