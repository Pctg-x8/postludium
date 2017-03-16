//! Postludium: Device Configuration Processor

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate parsetools;
extern crate interlude;
#[cfg(test)] extern crate itertools;

#[macro_use] mod items;
pub use items::*;
mod hobjects;
pub use hobjects::*;
mod resolver;

mod parser;
pub use parser::*;
