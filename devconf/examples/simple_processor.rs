//! Postludium Device Configuration Processor: Simple Processor

extern crate devconf; extern crate colored;
use colored::*;
use std::io::prelude::*;

struct ColoredReporter;
impl devconf::ErrorReporter for ColoredReporter
{
	fn report_fmt(&mut self, args: std::fmt::Arguments, loc: Option<&devconf::ErrorLocation>)
	{
		if let Some(l) = loc
		{
			std::io::stderr().write_fmt(format_args!("{}({}): {}\n", "Error".red().bold(), l, args))
		}
		else
		{
			std::io::stderr().write_fmt(format_args!("{}: {}\n", "Error".red().bold(), args))
		}.unwrap();
	}
}

fn main()
{
	let arg = std::env::args().nth(1).map(std::path::PathBuf::from).expect("Require file path to be processed");
	let base_path = arg.parent().unwrap();
	let mut pdc = devconf::load_configurations(std::borrow::Cow::Borrowed(&arg), |name|
		std::fs::File::open(base_path.join(name)).and_then(|mut fp| { let mut s = String::new(); fp.read_to_string(&mut s).map(|_| s) }), &mut ColoredReporter).unwrap();
	let pdc = devconf::ir::DeviceConfigurations::resolve_all(&mut pdc, &mut ColoredReporter);
	println!("{:?}", pdc);
}
