// Peeking with Cache

use std;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use super::parsetools::ParseTools;

pub trait LazyLines
{
	fn next(&mut self) -> Option<&(usize, String)>;
	fn pop(&mut self) -> Option<(usize, String)>;
}
pub trait LazyLinesRef<'a>
{
	fn next(&mut self) -> Option<&(usize, &'a str)>;
	fn pop(&mut self) -> Option<(usize, &'a str)>;
}
#[allow(dead_code)]
pub struct LazyLinesStr<'a>
{
	iter: std::iter::Enumerate<std::str::Lines<'a>>, cache: Option<(usize, String)>
}
impl<'a> LazyLinesStr<'a>
{
	#[cfg(test)]
	pub fn new(source: &'a String) -> Self
	{
		LazyLinesStr { iter: source.lines().enumerate(), cache: None }
	}
}
impl<'a> LazyLines for LazyLinesStr<'a>
{
	fn next(&mut self) -> Option<&(usize, String)>
	{
		if self.cache.is_none() { self.cache = self.iter.next().map(|(u, s)| (u + 1, s.to_owned())); }
		self.cache.as_ref()
	}
	fn pop(&mut self) -> Option<(usize, String)>
	{
		if self.cache.is_none() { self.iter.next().map(|(u, s)| (u + 1, s.to_owned())) }
		else { std::mem::replace(&mut self.cache, None) }
	}
}
pub struct LazyLinesChars<'a>
{
	left: &'a [char], line_count: usize, cache: Option<(usize, &'a [char])>
}
impl<'a> LazyLinesChars<'a>
{
	pub fn new(source: &'a [char]) -> Self
	{
		LazyLinesChars { left: source, line_count: 1, cache: None }
	}
	pub fn next(&mut self) -> Option<&(usize, &'a [char])>
	{
		if self.cache.is_none()
		{
			self.cache = if self.left.is_empty() { None } else
			{
				let (cache_ref, rest) = self.left.take_while(|c| c != '\n');
				self.left = rest.drop(1);
				self.line_count += 1;
				Some((self.line_count - 1, cache_ref))
			};
		}
		self.cache.as_ref()
	}
	pub fn pop(&mut self) -> Option<(usize, &'a [char])>
	{
		if self.cache.is_none()
		{
			if self.left.is_empty() { None } else
			{
				let (cache_ref, rest) = self.left.take_while(|c| c != '\n');
				self.left = rest.drop(1);
				self.line_count += 1;
				Some((self.line_count - 1, cache_ref))
			}
		}
		else { std::mem::replace(&mut self.cache, None) }
	}
}
pub struct LazyLinesBR
{
	iter: std::iter::Enumerate<std::io::Lines<BufReader<File>>>, cache: Option<(usize, String)>
}
impl LazyLinesBR
{
	pub fn new(reader: BufReader<File>) -> Self { LazyLinesBR { iter: reader.lines().enumerate(), cache: None } }
}
impl LazyLines for LazyLinesBR
{
	fn next(&mut self) -> Option<&(usize, String)>
	{
		if self.cache.is_none() { self.cache = self.iter.next().map(|(u, s)| (u + 1, s.unwrap())); }
		self.cache.as_ref()
	}
	fn pop(&mut self) -> Option<(usize, String)>
	{
		if self.cache.is_none() { self.iter.next().map(|(u, s)| (u + 1, s.unwrap())) }
		else { std::mem::replace(&mut self.cache, None) }
	}
}
