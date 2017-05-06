// High-ordered objects

use parsetools::*;
use parser::{ident_break, ignore_chars, from_token};
use error::*;
use syntree::Transition;

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
			else { Err(ParseError::Required(RequestType::Direction, input.current())) }
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
	use interlude; use parsetools::*;
	use parser::FromSource;
	use syntree::*; use error::*;

	#[test] fn parse_transition()
	{
		Testing!
		{
			PartialApply1!(Transition::parse; interlude::ShaderStage::parse); "Vertex -> Fragment"
				=> Ok(Transition { from: interlude::ShaderStage::Vertex, to: interlude::ShaderStage::Fragment }),
			PartialApply1!(Transition::parse; interlude::ShaderStage::parse); "Vertex"
				=> Err(ParseError::Required(RequestType::Direction, 6)),
			PartialApply1!(Transition::parse_opt; interlude::ShaderStage::parse); "Vertex"
				=> Ok(Transition { from: interlude::ShaderStage::Vertex, to: interlude::ShaderStage::Vertex })
		}
	}
}
