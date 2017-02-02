
mod lazylines;
pub use lazylines::LazyLines;

pub trait CharSliceSafetyExt
{
	fn peek(&self, offset: usize) -> Option<char>;
	fn front(&self) -> Option<char> { self.peek(0) }

	fn starts_with(&self, target: &[char]) -> bool;
	fn starts_with_trailing_opt<F>(&self, target: &[char], trailing: F) -> bool where F: FnOnce(char) -> bool
	{
		self.starts_with(target) && self.peek(target.len()).map(trailing).unwrap_or(true)
	}
}
impl CharSliceSafetyExt for [char]
{
	fn peek(&self, offset: usize) -> Option<char> { if offset >= self.len() { None } else { Some(self[offset]) } }
	fn starts_with(&self, target: &[char]) -> bool { target.len() <= self.len() && &self[..target.len()] == target }
}

#[test] fn char_slice_safety_ext()
{
	assert_eq!("test".chars().collect::<Vec<_>>().front(), Some('t'));
	assert_eq!("test".chars().collect::<Vec<_>>().peek(0), Some('t'));
	assert_eq!("test".chars().collect::<Vec<_>>().peek(1), Some('e'));
	assert!("Hello, World!".chars().collect::<Vec<_>>().starts_with(&"Hello".chars().collect::<Vec<_>>()));
	assert!(!"Hello, World!".chars().collect::<Vec<_>>().starts_with(&"World".chars().collect::<Vec<_>>()));
	assert!("Hello, World!".chars().collect::<Vec<_>>().starts_with_trailing_opt(&"Hello".chars().collect::<Vec<_>>(), |c| c == ','));
	assert!(!"Hello World!".chars().collect::<Vec<_>>().starts_with_trailing_opt(&"Hello".chars().collect::<Vec<_>>(), |c| c == ','));
	assert!("Hello".chars().collect::<Vec<_>>().starts_with_trailing_opt(&"Hello".chars().collect::<Vec<_>>(), |c| c == ','));
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParseLine<'s>(pub &'s [char], pub usize);
impl<'s> PartialEq<[char]> for ParseLine<'s>
{
	fn eq(&self, right: &[char]) -> bool { self.0 == right }
}
impl<'s> ParseLine<'s>
{
	pub fn is_empty(&self) -> bool { self.0.is_empty() }
	pub fn current(&self) -> usize { self.1 }
	pub fn len(&self) -> usize { self.0.len() }
	pub fn chars(&self) -> &'s [char] { self.0 }

	pub fn peek(&self, offset: usize) -> Option<char> { if self.len() <= offset { None } else { Some(self.0[offset]) } }
	pub fn front(&self) -> Option<char> { self.peek(0) }
	pub fn drop_one(&mut self) -> &mut Self
	{
		self.1 = if self.front() == Some('\t') { (((self.1 - 1) / 4) + 1) * 4 + 1 } else { self.1 + 1 };
		self.0 = &self.0[1..];
		self
	}
	pub fn drop_opt(&mut self, count: usize) -> &mut Self
	{
		self.0 = &self.0[count..];
		self.1 += count;
		self
	}
	pub fn drop_while<F>(&mut self, pred: F) -> &mut Self where F: Fn(char) -> bool
	{
		if pred('\t')
		{
			// unoptimized
			while self.front().map(&pred).unwrap_or(false) { self.drop_one(); }
			self
		}
		else
		{
			let mut counter = 0;
			while self.peek(counter).map(&pred).unwrap_or(false) { counter += 1; }
			self.drop_opt(counter)
		}
	}
	pub fn take_while<F>(&mut self, pred: F) -> Self where F: Fn(char) -> bool
	{
		if pred('\t')
		{
			// unoptimized
			let start = self.clone();
			let mut counter = 0;
			while self.front().map(&pred).unwrap_or(false) { counter += 1; self.drop_one(); }
			ParseLine(&start.0[..counter], start.1)
		}
		else
		{
			let mut counter = 0;
			while self.peek(counter).map(&pred).unwrap_or(false) { counter += 1; }
			let r = ParseLine(&self.0[..counter], self.1);
			self.drop_opt(counter);
			r
		}
	}
	pub fn drop_until<F>(&mut self, pred: F) -> &mut Self where F: Fn(char) -> bool { self.drop_while(|c| !pred(c)) }
	pub fn take_until<F>(&mut self, pred: F) -> Self where F: Fn(char) -> bool { self.take_while(|c| !pred(c)) }

	pub fn clone_as_string(&self) -> String { self.0.iter().cloned().collect() }
	pub fn clone_as_string_flatmapping<F, I>(&self, map: F) -> String where F: Fn(&char) -> I, I: Iterator<Item = char>
	{
		self.0.iter().flat_map(map).collect()
	}

	pub fn starts_with(&self, target: &[char]) -> bool { self.0.starts_with(target) }
	pub fn starts_with_trailing_opt<F>(&self, target: &[char], trailing: F) -> bool where F: FnOnce(char) -> bool
	{
		self.0.starts_with_trailing_opt(target, trailing)
	}
	pub fn trackbacking<F, T>(&mut self, act: F) -> Option<T> where F: FnOnce(&mut Self) -> Option<T>
	{
		let saved = self.clone();
		match act(self)
		{
			Some(v) => Some(v),
			None => { *self = saved; None }
		}
	}
}

#[macro_export]
macro_rules! PartialEqualityMatchMap
{
	($src: expr; { $($target: expr => $st: expr),* }) =>
	{
		$(if $src == $target { Some($st) })else* else { None }
	};
	($src: expr; { $($target: expr => $st: expr),*; _ => $est: expr }) =>
	{
		$(if $src == $target { $st })else* else { $est }
	}
}
#[macro_export]
macro_rules! PartialEqualityMatch
{
	($src: expr; { $($target: expr => $st: stmt),*; _ => $est: stmt }) =>
	{
		$(if $src == $target { $st })else* else { $est }
	}
}
