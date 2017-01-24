// Peeking with Cache

use std;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use super::parsetools::ParseTools;

pub struct LazyLines<'a>
{
	source: &'a [char], current: usize, cache: Option<(usize, &'a [char])>
}
impl<'a> LazyLines<'a>
{
	pub fn new(source: &'a [char]) -> Self
	{
		LazyLines { source: source, current: 0, cache: None }
	}

	pub fn next(&mut self) -> Option<(usize, &'a [char])>
	{
		if self.cache.is_none()
		{
			if self.source.is_empty() { None }
			else
			{
				let (l, s) = self.source.take_until(|c| c == '\n');
				self.source = if s.is_empty() { s } else { s.drop(1) };
				self.current += 1;
				self.cache = Some((self.current, l));
				self.cache.clone()
			}
		}
		else { self.cache.clone() }
	}
	pub fn pop(&mut self) -> Option<(usize, &'a [char])>
	{
		if self.cache.is_none()
		{
			if self.source.is_empty() { None }
			else
			{
				let (l, s) = self.source.take_until(|c| c == '\n');
				self.source = if s.is_empty() { s } else { s.drop(1) };
				self.current += 1;
				Some((self.current, l))
			}
		}
		else { self.cache.take() }
	}
	pub fn drop_line(&mut self)
	{
		if self.cache.is_none()
		{
			if !self.source.is_empty()
			{
				let (l, s) = self.source.take_until(|c| c == '\n');
				self.source = if s.is_empty() { s } else { s.drop(1) };
				self.current += 1;
			}
		}
		else { self.cache = None; }
	}
}
