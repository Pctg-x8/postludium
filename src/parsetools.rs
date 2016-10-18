// ParseTools for reference to slice of char

#![allow(unsized_in_tuple)]
use std;

pub trait ParseTools
{
	type Item;
	fn skip_while<F>(self, pred: F) -> Self where F: Fn(Self::Item) -> bool;
	fn take_while<F>(self, pred: F) -> (Self, Self) where F: Fn(Self::Item) -> bool;
	fn is_front_of(&self, t: Self::Item) -> bool;
	fn is_front<F>(&self, pred: F) -> bool where F: FnOnce(&Self::Item) -> bool;
	fn drop(self, count: usize) -> Self;
	fn clone_as_string(self) -> String;

	fn take_until<F>(self, pred: F) -> (Self, Self) where F: Fn(Self::Item) -> bool;
	fn skip_until<F>(self, pred: F) -> Self where F: Fn(Self::Item) -> bool;
}
impl<'a> ParseTools for &'a [char]
{
	type Item = char;
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
	fn is_front_of(&self, t: char) -> bool { !self.is_empty() && self[0] == t }
	fn is_front<F>(&self, pred: F) -> bool where F: FnOnce(&Self::Item) -> bool { !self.is_empty() && pred(&self[0]) }
	fn clone_as_string(self) -> String { self.into_iter().cloned().collect() }

	fn take_until<F>(self, pred: F) -> (Self, Self) where F: Fn(Self::Item) -> bool { self.take_while(|x| !pred(x)) }
	fn skip_until<F>(self, pred: F) -> Self where F: Fn(Self::Item) -> bool { self.skip_while(|x| !pred(x)) }
}

macro_rules! PartialEqualityMatchMap
{
	($src: expr; { $($target: expr => $st: expr),* }) =>
	{
		$(if $src == $target { Some($st) })else* else { None }
	};
	($src: expr; { $ftarget: expr => $fst: expr, $($target: expr => $st: expr),*; _ => $est: expr }) =>
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
