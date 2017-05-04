//! Postludium: Device Configuration Processor

#![feature(associated_consts, box_syntax)]

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate parsetools;
extern crate itertools;
extern crate interlude;

#[macro_use] mod items;
pub use items::*;
mod hobjects;
pub use hobjects::*;
mod resolver;

pub mod syntree;
mod parser;
pub use parser::*;
mod assetloader;
pub use assetloader::LoadedAssets;
mod instantiate;
pub use instantiate::*;

pub trait ErrorReporter
{
	fn report_fmt(&mut self, args: std::fmt::Arguments);

	fn report(&mut self, msg: &str) { self.report_fmt(format_args!("{}\n", msg)); }
}
pub struct StdErrReporter;
impl ErrorReporter for StdErrReporter
{
	fn report(&mut self, msg: &str) { std::io::stderr().write(msg.as_ref()).unwrap(); }
	fn report_fmt(&mut self, args: std::fmt::Arguments) { std::io::stderr().write_fmt(args).unwrap(); }
}

// Entry Function //
use std::path::{Path, PathBuf};
use std::collections::HashSet;
use std::io::Write;
use std::borrow::Cow;
use syntree::{ LocationPacked, ParsedDeviceResources };
pub fn load_configurations<F>(entername: Cow<Path>, mut source_provider: F, reporter: &mut ErrorReporter) -> ParsedDeviceResources
	where F: FnMut(&Path) -> std::io::Result<String>
{
	fn load_impl<F>(source: String, source_provider: &mut F, sink: &mut ParsedDeviceResources, loaded: &mut HashSet<PathBuf>, reporter: &mut ErrorReporter)
		-> Result<(), ()> where F: FnMut(&Path) -> std::io::Result<String>
	{
		let mut includes = Vec::new();
		let chars = source.chars().collect::<Vec<_>>();
		match parse_device_resources(sink, &mut includes, &mut parsetools::LazyLines::new(&chars))
		{
			Ok(()) => { for LocationPacked(loc, include_path) in includes.into_iter()
			{
				if !loaded.contains(&include_path)
				{
					match source_provider(&include_path)
					{
						Ok(s) =>
						{
							loaded.insert(include_path.clone());
							if load_impl(s, source_provider, sink, loaded, reporter).is_err()
							{
								reporter.report_fmt(format_args!("-- In parsing {:?}", include_path));
								Err(())
							}
							else { Ok(()) }
						},
						Err(e) =>
						{
							reporter.report_fmt(format_args!("Failed to load {}(included at {}): {}", include_path.to_string_lossy(), loc, e));
							loaded.insert(include_path);
							Err(())
						}
					}
				}
				else { Ok(()) }?
			} Ok(()) },
			Err(e) => { reporter.report_fmt(format_args!("{:?}", e)); Err(()) }
		}
	}
	let mut loaded_sources = HashSet::new();
	let mut sink = ParsedDeviceResources::empty();
	match source_provider(&entername)
	{
		Ok(s) =>
		{
			loaded_sources.insert(entername.clone().into_owned());
			if load_impl(s, &mut source_provider, &mut sink, &mut loaded_sources, reporter).is_err()
			{
				reporter.report_fmt(format_args!("-- In parsing {:?}", entername));
			}
		},
		Err(e) => panic!("Failed to load {}: {}", entername.to_string_lossy(), e)
	}
	sink
}
