
use super::ParseError;
use parsetools::*;
use interlude::ffi::*;
use super::{ignore_chars, ident_break, PartialResult};
use super::PartialResult::*;

#[cfg(test)] use itertools::Itertools;
#[cfg(test)]
macro_rules! Testing
{
	{$f: path: $t: expr => $e: expr , $($r: tt)+} =>
	{
		Testing! { $f: $t => $e }
		Testing! { $($r)* }
	};
	{$f: path: $t: expr => $e: expr ,} =>
	{
		Testing! { $f: $t => $e }
	};
	{$f: path: $t: expr => $e: expr} =>
	{
		assert_eq!($f(ParseLine(&$t.chars().collect_vec(), 0)), $e);
	}
}

#[derive(Debug, PartialEq)]
// Integer Literal or $~~
pub enum ConfigInt { Value(u32), Ref(String) }
impl ConfigInt
{
	pub fn parse(input: ParseLine) -> PartialResult<Self>
	{
		match input.front()
		{
			Some('$') =>
			{
				let (refn, rest) = input.drop_opt(1).take_until(ident_break);
				if refn.is_empty() { Failed(ParseError::NameRequired(refn.current())) } else { Success(ConfigInt::Ref(refn.clone_as_string()), rest) }
			},
			Some('0' ... '9') =>
			{
				let (refn, rest) = input.take_while(|c| '0' <= c && c <= '9');
				assert!(!refn.is_empty());
				match refn.clone_as_string().parse::<u32>()
				{
					Ok(v) => Success(ConfigInt::Value(v), rest),
					Err(e) => Failed(ParseError::NumericParseError(e, refn.current()))
				}
			},
			_ => Failed(ParseError::IntValueRequired(input.current()))
		}
	}
	pub fn parse_array(input: ParseLine) -> PartialResult<Vec<Self>>
	{
		if input.front() == Some('[')
		{
			fn recursive<'s>(input: ParseLine<'s>, sink: &mut Vec<ConfigInt>) -> PartialResult<'s, ()>
			{
				ConfigInt::parse(input).and_then(|v, r|
				{
					sink.push(v);
					let r = r.drop_while(ignore_chars);
					match r.front()
					{
						Some(',') =>
						{
							let r = r.drop_while(|c| c == ',' || ignore_chars(c));
							if r.front() == Some(']') { Success((), r.drop_opt(1)) } else { recursive(r, sink) }
						},
						Some(']') => { Success((), r.drop_opt(1)) },
						_ => Failed(ParseError::DelimiterRequired(r.current()))
					}
				})
			}
			let mut v = Vec::new();
			recursive(input.drop_opt(1), &mut v).vmap(|_| v)
		}
		else { Self::parse(input).vmap(|v| vec![v]) }
	}
}
#[test] fn parse_config_int()
{
	Testing!
	{
		ConfigInt::parse: "10" => Success(ConfigInt::Value(10), ParseLine(&[], 2)),
		ConfigInt::parse: "$TA," => Success(ConfigInt::Ref("TA".into()), ParseLine(&[','], 3)),
		ConfigInt::parse: "$ " => Failed(ParseError::NameRequired(1)),
		ConfigInt::parse: "T" => Failed(ParseError::IntValueRequired(0)),
		ConfigInt::parse_array: "10" => Success(vec![ConfigInt::Value(10)], ParseLine(&[], 2)),
		ConfigInt::parse_array: "[1, 2]~" => Success(vec![ConfigInt::Value(1), ConfigInt::Value(2)], ParseLine(&['~'], 6)),
		ConfigInt::parse_array: "[1,,]" => Success(vec![ConfigInt::Value(1)], ParseLine(&[], 5)),
		ConfigInt::parse_array: "[1 2]" => Failed(ParseError::DelimiterRequired(3)),
		ConfigInt::parse_array: "[1," => Failed(ParseError::IntValueRequired(3))
	}
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum PixelFormat { Ref(String), Value(VkFormat) }
impl PixelFormat
{
	pub fn parse(source: ParseLine) -> PartialResult<Self>
	{
		if source.front() == Some('$')
		{
			let (s, rest) = source.drop_opt(1).take_until(ident_break);
			if s.is_empty() { Failed(ParseError::NameRequired(s.current())) }
			else { Success(PixelFormat::Ref(s.clone_as_string()), rest) }
		}
		else
		{
			let (bits, rest) = source.take_until(ident_break);
			let (form, rest) = rest.drop_while(ignore_chars).take_until(ident_break);
			match (bits.clone_as_string_flatmapping(|&c| c.to_uppercase()).as_ref(), form.clone_as_string_flatmapping(|&c| c.to_uppercase()).as_ref())
			{
				("R8", "UNORM") => Success(PixelFormat::Value(VkFormat::R8_UNORM), rest),
				("R8", "SNORM") => Success(PixelFormat::Value(VkFormat::R8_SNORM), rest),
				("R8G8", "UNORM") => Success(PixelFormat::Value(VkFormat::R8G8_UNORM), rest),
				("R8G8", "SNORM") => Success(PixelFormat::Value(VkFormat::R8G8_SNORM), rest),
				("R8G8B8", "UNORM") => Success(PixelFormat::Value(VkFormat::R8G8B8_UNORM), rest),
				("R8G8B8", "SNORM") => Success(PixelFormat::Value(VkFormat::R8G8B8_SNORM), rest),
				("R8G8B8A8", "UNORM") => Success(PixelFormat::Value(VkFormat::R8G8B8A8_UNORM), rest),
				("R8G8B8A8", "SNORM") => Success(PixelFormat::Value(VkFormat::R8G8B8A8_SNORM), rest),
				("R32", "SFLOAT") => Success(PixelFormat::Value(VkFormat::R32_SFLOAT), rest),
				("R16G16B16A16", "UNORM") => Success(PixelFormat::Value(VkFormat::R16G16B16A16_UNORM), rest),
				("R16G16B16A16", "SNORM") => Success(PixelFormat::Value(VkFormat::R16G16B16A16_SNORM), rest),
				("R16G16B16A16", "SFLOAT") => Success(PixelFormat::Value(VkFormat::R16G16B16A16_SFLOAT), rest),
				_ => Failed(ParseError::UnknownFormat(bits.current()))
			}
		}
	}
}
#[test] fn parse_pixel_format()
{
	Testing!
	{
		PixelFormat::parse: "R8G8B8A8 UNORM" => Success(PixelFormat::Value(VkFormat::R8G8B8A8_UNORM), ParseLine(&[], 14)),
		PixelFormat::parse: "R8G8b8A8 Unorm" => Success(PixelFormat::Value(VkFormat::R8G8B8A8_UNORM), ParseLine(&[], 14)),
		PixelFormat::parse: "$ScreenFormat" => Success(PixelFormat::Ref("ScreenFormat".into()), ParseLine(&[], 13)),
		PixelFormat::parse: "R8G8B8A8 SF" => Failed(ParseError::UnknownFormat(0))
	}
}

lazy_static!
{
	static ref CAO: Vec<char> = "ColorAttachmentOptimal".chars().collect();
	static ref SROO: Vec<char> = "ShaderReadOnlyOptimal".chars().collect();
}
pub fn parse_image_layout(source: ParseLine) -> PartialResult<VkImageLayout>
{
	let (name, rest) = source.take_until(ident_break);
	if name == CAO[..] { Success(VkImageLayout::ColorAttachmentOptimal, rest) }
	else if name == SROO[..] { Success(VkImageLayout::ShaderReadOnlyOptimal, rest) }
	else { Failed(ParseError::UnknownImageLayout(name.current())) }
}
#[test] fn image_layouts()
{
	Testing!
	{
		parse_image_layout: "ColorAttachmentOptimal:" => Success(VkImageLayout::ColorAttachmentOptimal, ParseLine(&[':'], 22)),
		parse_image_layout: "Shaders" => Failed(ParseError::UnknownImageLayout(0))
	}
}

lazy_static!
{
	static ref TOP: Vec<char> = "TopOfPipe".chars().collect();
	static ref FSS: Vec<char> = "FragmentShaderStage".chars().collect();
	static ref BOP: Vec<char> = "BottomOfPipe".chars().collect();
}
pub fn parse_pipeline_stage_bits(source: ParseLine) -> PartialResult<VkPipelineStageFlags>
{
	fn determine_const(input: &ParseLine) -> Option<VkPipelineStageFlags>
	{
		if *input == TOP[..] { Some(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT) }
		else if *input == FSS[..] { Some(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) }
		else if *input == BOP[..] { Some(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT) }
		else { None }
	}
	fn recursive<'s>(input: ParseLine<'s>, mut accum: VkPipelineStageFlags) -> PartialResult<'s, VkPipelineStageFlags>
	{
		if input.is_empty() { Success(accum, input) }
		else
		{
			let (flag, rest) = input.take_until(ident_break);
			PartialResult::from_opt(determine_const(&flag), rest, ParseError::UnknownPipelineStageFlag(flag.current())).and_then(|f, r|
			{
				accum |= f;
				let r = r.drop_while(ignore_chars);
				if r.front() == Some('/') { recursive(r.drop_opt(1).drop_while(ignore_chars), accum) } else { Success(accum, r) }
			})
		}
	}
	recursive(source, 0)
}
lazy_static!
{
	static ref CAW: Vec<char> = "ColorAttachmentWrite".chars().collect();
	static ref SR: Vec<char> = "ShaderRead".chars().collect();
}
pub fn parse_access_mask(source: ParseLine) -> PartialResult<VkAccessFlags>
{
	fn determine_const(input: &ParseLine) -> Option<VkAccessFlags>
	{
		if *input == CAW[..] { Some(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) }
		else if *input == SR[..] { Some(VK_ACCESS_SHADER_READ_BIT) }
		else { None }
	}
	fn recursive<'s>(input: ParseLine<'s>, mut accum: VkAccessFlags) -> PartialResult<'s, VkAccessFlags>
	{
		if input.is_empty() { Success(accum, input) }
		else
		{
			let (flag, rest) = input.take_until(ident_break);
			PartialResult::from_opt(determine_const(&flag), rest, ParseError::UnknownAccessFlag(flag.current())).and_then(|f, r|
			{
				accum |= f;
				let r = r.drop_while(ignore_chars);
				if r.front() == Some('/') { recursive(r.drop_opt(1).drop_while(ignore_chars), accum) } else { Success(accum, r) }
			})
		}
	}
	recursive(source, 0)
}
pub fn parse_string_literal(source: ParseLine) -> PartialResult<String>
{
	fn recursive_char<'s>(input: ParseLine<'s>, sink: &mut String) -> PartialResult<'s, ()>
	{
		match input.front()
		{
			Some('\\') => recursive_escape(input.drop_opt(1), sink),
			Some('"') => Success((), input.drop_opt(1)),
			Some(c) => { sink.push(c); recursive_char(input.drop_one(), sink) },
			None => Failed(ParseError::ClosingRequired(input.current()))
		}
	}
	fn recursive_escape<'s>(input: ParseLine<'s>, sink: &mut String) -> PartialResult<'s, ()>
	{
		match input.front()
		{
			Some('t') => { sink.push('\t'); recursive_char(input.drop_opt(1), sink) },
			Some('n') => { sink.push('\n'); recursive_char(input.drop_opt(1), sink) },
			Some('r') => { sink.push('\r'); recursive_char(input.drop_opt(1), sink) },
			Some(c) => { sink.push(c); recursive_char(input.drop_opt(1), sink) },
			None => Failed(ParseError::ClosingRequired(input.current()))
		}
	}

	if source.front() == Some('"')
	{
		let mut buf = String::new();
		recursive_char(source.drop_opt(1), &mut buf).vmap(|_| buf)
	}
	else { Failed(ParseError::Expected("String Literal", source.current())) }
}

#[test] fn pipeline_stage_bits()
{
	Testing!
	{
		parse_pipeline_stage_bits: "FragmentShaderStage" => Success(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT, ParseLine(&[], 19)),
		parse_pipeline_stage_bits: "TopOfPipe / BottomOfPipe" => Success(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT | VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, ParseLine(&[], 24)),
		parse_pipeline_stage_bits: "a" => Failed(ParseError::UnknownPipelineStageFlag(0))
	}
}
#[test] fn access_mask()
{
	Testing!
	{
		parse_access_mask: "ColorAttachmentWrite" => Success(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, ParseLine(&[], 20)),
		parse_access_mask: "ColorAttachmentWrite / ShaderRead" => Success(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT | VK_ACCESS_SHADER_READ_BIT, ParseLine(&[], 33)),
		parse_access_mask: "a" => Failed(ParseError::UnknownAccessFlag(0))
	}
}
