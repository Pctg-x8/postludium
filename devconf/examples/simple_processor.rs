//! Postludium Device Configuration Processor: Simple Processor

extern crate devconf;
use std::io::prelude::*;

fn main()
{
	let arg = std::env::args().nth(1).map(std::path::PathBuf::from).expect("Require file path to be processed");
	let base_path = arg.parent().unwrap();
	devconf::load_configurations(std::borrow::Cow::Borrowed(&arg), |name|
		std::fs::File::open(base_path.join(name)).and_then(|mut fp| { let mut s = String::new(); fp.read_to_string(&mut s).map(|_| s) }));
}
