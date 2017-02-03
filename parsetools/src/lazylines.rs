// Peeking with Cache

use CharSliceSafetyExt;
use std::cmp::min;

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

	fn fetch_line(&mut self) -> &'a [char]
	{
		let mut counter = 0;
		while let Some(c) = self.source.peek(counter)
		{
			if c == '\n' { break; }
			counter += 1;
		}
		let l = &self.source[..counter];
		self.source = &self.source[min(self.source.len(), counter + 1)..];
		l
	}

	pub fn next(&mut self) -> Option<(usize, &'a [char])>
	{
		if self.cache.is_none()
		{
			if self.source.is_empty() { None }
			else
			{
				let l = self.fetch_line();
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
				let l = self.fetch_line();
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
				self.fetch_line();
				self.current += 1;
			}
		}
		else { self.cache = None; }
	}
}

#[cfg(test)] mod tests
{
	use super::*;
	use itertools::Itertools;

	#[test] fn drop_line()
	{
		let case = "a\nb\n".chars().collect_vec();
		let mut ll = LazyLines::new(&case);
		ll.drop_line();
		assert_eq!(ll.next(), Some((2, &['b'][..])));
	}
}
