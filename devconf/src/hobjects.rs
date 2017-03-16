// High-ordered objects

use parsetools::*;
use parser::{ident_break, ignore_chars, ParseError, from_token};
use syntree::*;

impl<T> Transition<T>
{
	pub fn parse<'s, F>(input: &mut ParseLine<'s>, childparser: F) -> Result<Self, ParseError>
		where F: Fn(&mut ParseLine<'s>) -> Result<T, ParseError>
	{
		childparser(input).and_then(|a|
		{
			input.drop_while(ignore_chars);
			if input.starts_with_trailing_opt(&['T', 'o'], ident_break) || input.starts_with(&['-', '>'])
			{
				childparser(input.drop_opt(2).drop_while(ignore_chars)).map(|b| Transition { from: a, to: b })
			}
			else if from_token(input) { childparser(input.drop_while(ignore_chars)).map(|b| Transition { from: b, to: a }) }
			else { Err(ParseError::DirectionRequired(input.current())) }
		})
	}
}
impl<T: Clone> Transition<T>
{
	pub fn parse_opt<'s, F>(input: &mut ParseLine<'s>, childparser: F) -> Result<Self, ParseError> where F: Fn(&mut ParseLine<'s>) -> Result<T, ParseError>
	{
		childparser(input).and_then(|a|
		{
			input.drop_while(ignore_chars);
			if input.starts_with_trailing_opt(&['T', 'o'], ident_break) || input.starts_with(&['-', '>'])
			{
				childparser(input.drop_opt(2).drop_while(ignore_chars)).map(|b| Transition { from: a, to: b })
			}
			else if from_token(input)
			{
				childparser(input.drop_while(ignore_chars)).map(|b| Transition { to: a, from: b })
			}
			else { Ok(Transition { to: a.clone(), from: a }) }
		})
	}
}

#[cfg(test)] mod tests
{
	use itertools::Itertools;
	use super::*;
	use items::*;
	use interlude::ffi::*;

	#[test] fn parse_transition()
	{
		Testing!
		{
			PartialApply1!(Transition::parse; parse_shader_stage_bits); "Vertex -> Fragment"
				=> Ok(Transition { from: LocationPacked(1, 0, VK_SHADER_STAGE_VERTEX_BIT), to: LocationPacked(1, 10, VK_SHADER_STAGE_FRAGMENT_BIT) }),
			PartialApply1!(Transition::parse; parse_shader_stage_bits); "Vertex"
				=> Err(ParseError::DirectionRequired(6)),
			PartialApply1!(Transition::parse_opt; parse_shader_stage_bits); "Vertex"
				=> Ok(Transition { from: LocationPacked(1, 0, VK_SHADER_STAGE_VERTEX_BIT), to: LocationPacked(1, 0, VK_SHADER_STAGE_VERTEX_BIT) })
		}
	}
}
