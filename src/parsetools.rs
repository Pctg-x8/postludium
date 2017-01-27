// ParseTools for reference to slice of char

use std;

pub trait ParseTools : std::marker::Sized
{
	fn front(&self) -> Option<char>;
	fn peek(&self, offset: usize) -> Option<char>;
	fn skip_while<F>(self, pred: F) -> Self where F: Fn(char) -> bool;
	fn take_while<F>(self, pred: F) -> (Self, Self) where F: Fn(char) -> bool;
	fn drop(self, count: usize) -> Self;
	fn clone_as_string_mapping<F>(self, map: F) -> String where F: Fn(&char) -> char;
	fn clone_as_string_flatmapping<F, I>(self, map: F) -> String where F: Fn(&char) -> I, I: Iterator<Item = char>;
	fn starts_with(&self, target: &[char]) -> bool;

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
	fn front(&self) -> Option<char> { if self.is_empty() { None } else { Some(self[0]) } }
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
