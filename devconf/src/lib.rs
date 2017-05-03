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

// Entry Function //
use std::path::{Path, PathBuf};
use std::collections::HashSet;
use std::io::Write;
use std::borrow::Cow;
use syntree::{ LocationPacked, ParsedDeviceResources };
pub fn load_configurations<F>(entername: Cow<Path>, mut source_provider: F) -> ParsedDeviceResources where F: FnMut(&Path) -> std::io::Result<String>
{
	fn load_impl<F>(source: String, source_provider: &mut F, sink: &mut ParsedDeviceResources, loaded: &mut HashSet<PathBuf>)
		where F: FnMut(&Path) -> std::io::Result<String>
	{
		let mut includes = Vec::new();
		let chars = source.chars().collect::<Vec<_>>();
		parse_device_resources(sink, &mut includes, &mut parsetools::LazyLines::new(&chars));
		for LocationPacked(loc, include_path) in includes.into_iter()
		{
			if !loaded.contains(&include_path)
			{
				match source_provider(&include_path)
				{
					Ok(s) =>
					{
						loaded.insert(include_path);
						load_impl(s, source_provider, sink, loaded);
					},
					Err(e) =>
					{
						writeln!(std::io::stderr(), "Failed to load {}(included at {}): {}", include_path.to_string_lossy(), loc, e).unwrap();
						loaded.insert(include_path);
					}
				}
			}
		}
	}
	let mut loaded_sources = HashSet::new();
	let mut sink = ParsedDeviceResources::empty();
	match source_provider(&entername)
	{
		Ok(s) =>
		{
			loaded_sources.insert(entername.into_owned());
			load_impl(s, &mut source_provider, &mut sink, &mut loaded_sources);
		},
		Err(e) => panic!("Failed to load {}: {}", entername.to_string_lossy(), e)
	}
	sink
}
