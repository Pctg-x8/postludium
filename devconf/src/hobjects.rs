// High-ordered objects

#[cfg(test)] use itertools::Itertools;
use parsetools::*;
use super::{ident_break, ignore_chars, ParseError, from_token};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Transition<T> { pub from: T, pub to: T }
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
impl<T> Transition<T> where T: Copy
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
			else { Ok(Transition { to: a, from: a }) }
		})
	}
}
#[test] fn parse_transition()
{
	use super::items::*;
	use vk::*;

	assert_eq!(Transition::parse(&mut ParseLine(&"Vertex -> Fragment".chars().collect_vec(), 0), parse_shader_stage_bits),
		Ok(Transition { from: VK_SHADER_STAGE_VERTEX_BIT, to: VK_SHADER_STAGE_FRAGMENT_BIT }));
	assert_eq!(Transition::parse(&mut ParseLine(&"Vertex".chars().collect_vec(), 0), parse_shader_stage_bits), Err(ParseError::DirectionRequired(6)));
	assert_eq!(Transition::parse_opt(&mut ParseLine(&"Vertex".chars().collect_vec(), 0), parse_shader_stage_bits),
		Ok(Transition { from: VK_SHADER_STAGE_VERTEX_BIT, to: VK_SHADER_STAGE_VERTEX_BIT }));
}
