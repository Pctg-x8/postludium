// ParseTools for reference to slice of char

use std;

pub trait ParseTools : std::marker::Sized
{
	fn peek(&self, offset: usize) -> Option<char>;
	fn skip_while<F>(self, pred: F) -> Self where F: Fn(char) -> bool;
	fn take_while<F>(self, pred: F) -> (Self, Self) where F: Fn(char) -> bool;
	fn drop(self, count: usize) -> Self;
	fn clone_as_string_mapping<F>(self, map: F) -> String where F: Fn(&char) -> char;
	fn clone_as_string_flatmapping<F, I>(self, map: F) -> String where F: Fn(&char) -> I, I: Iterator<Item = char>;
	fn starts_with(&self, target: &[char]) -> bool;

	fn front(&self) -> Option<char> { self.peek(0) }
	fn is_front_of(&self, t: char) -> bool { self.is_front(move |c| c == t) }
	fn is_front<F>(&self, pred: F) -> bool where F: FnOnce(char) -> bool { self.front().map(pred).unwrap_or(false) }
	fn take_until<F>(self, pred: F) -> (Self, Self) where F: Fn(char) -> bool { self.take_while(move |c| !pred(c)) }
	fn skip_until<F>(self, pred: F) -> Self where F: Fn(char) -> bool { self.skip_while(move |c| !pred(c)) }
	fn clone_as_string(self) -> String { self.clone_as_string_mapping(char::clone) }
	fn starts_with_trailing_opt<F>(&self, target: &[char], trailing: F) -> bool where F: Fn(char) -> bool
	{
		self.starts_with(target) && self.peek(target.len()).map(trailing).unwrap_or(true)
	}
}
impl<'a> ParseTools for &'a [char]
{
	fn peek(&self, offset: usize) -> Option<char> { if self.len() <= offset { None } else { Some(self[offset]) } }
	fn skip_while<F>(self, pred: F) -> Self where F: Fn(char) -> bool
	{
		if !self.is_empty() && pred(self[0]) { Self::skip_while(&self[1..], pred) } else { self }
	}
	fn take_while<F>(self, pred: F) -> (Self, Self) where F: Fn(char) -> bool
	{
		fn _impl<F>(input: &[char], counter: usize, pred: F) -> usize where F: Fn(char) -> bool
		{
			if !input.is_empty() && pred(input[0]) { _impl(&input[1..], counter + 1, pred) } else { counter }
		}
		let len = _impl(self, 0, pred);
		(&self[..len], &self[len..])
	}
	fn drop(self, count: usize) -> Self { &self[std::cmp::min(count, self.len())..] }
	fn clone_as_string_mapping<F>(self, map: F) -> String where F: Fn(&char) -> char { self.into_iter().map(map).collect() }
	fn clone_as_string_flatmapping<F, I>(self, map: F) -> String where F: Fn(&char) -> I, I: Iterator<Item = char> { self.into_iter().flat_map(map).collect() }
	fn starts_with(&self, target: &[char]) -> bool { self.len() >= target.len() && &self[..target.len()] == target }
}

#[derive(Clone)] #[cfg_attr(test, derive(Debug, PartialEq))]
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

	pub fn peek(&self, offset: usize) -> Option<char> { if self.len() <= offset { None } else { Some(self.0[offset]) } }
	pub fn front(&self) -> Option<char> { self.peek(0) }
	pub fn drop_one(self) -> Self
	{
		if self.front() == Some('\t') { ParseLine(&self.0[1..], (((self.1 - 1) / 4) + 1 * 4) + 1) }
		else { ParseLine(&self.0[1..], self.1 + 1) }
	}
	pub fn drop_opt(self, count: usize) -> Self
	{
		ParseLine(&self.0[count..], self.1 + count)
	}
	pub fn drop_while<F>(mut self, pred: F) -> Self where F: Fn(char) -> bool
	{
		if pred('\t')
		{
			// unoptimized
			while self.front().map(&pred).unwrap_or(false) { self = self.drop_one(); }
			self
		}
		else
		{
			let mut counter = 0;
			while self.peek(counter).map(&pred).unwrap_or(false) { counter += 1; }
			self.drop_opt(counter)
		}
	}
	pub fn take_while<F>(self, pred: F) -> (Self, Self) where F: Fn(char) -> bool
	{
		if pred('\t')
		{
			// unoptimized
			let mut rest = self.clone();
			let mut counter = 0;
			while rest.front().map(&pred).unwrap_or(false) { counter += 1; rest = rest.drop_one(); }
			(ParseLine(&self.0[..counter], self.1), rest)
		}
		else
		{
			let mut counter = 0;
			while self.peek(counter).map(&pred).unwrap_or(false) { counter += 1; }
			(ParseLine(&self.0[..counter], self.1), ParseLine(&self.0[counter..], self.1 + counter))
		}
	}
	pub fn drop_until<F>(self, pred: F) -> Self where F: Fn(char) -> bool { self.drop_while(|c| !pred(c)) }
	pub fn take_until<F>(self, pred: F) -> (Self, Self) where F: Fn(char) -> bool { self.take_while(|c| !pred(c)) }

	pub fn clone_as_string(&self) -> String { self.0.iter().cloned().collect() }
	pub fn clone_as_string_flatmapping<F, I>(&self, map: F) -> String where F: Fn(&char) -> I, I: Iterator<Item = char>
	{
		self.0.iter().flat_map(map).collect()
	}

	pub fn starts_with(&self, target: &[char]) -> bool
	{
		self.0.len() >= target.len() && &self.0[..target.len()] == target
	}
	pub fn starts_with_trailing_opt<F>(&self, target: &[char], trailing: F) -> bool where F: FnOnce(char) -> bool
	{
		self.starts_with(target) && self.peek(target.len()).map(trailing).unwrap_or(true)
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
macro_rules! PartialEqualityMatch
{
	($src: expr; { $($target: expr => $st: stmt),*; _ => $est: stmt }) =>
	{
		$(if $src == $target { $st })else* else { $est }
	}
}
