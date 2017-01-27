
use super::ParseError;
use parsetools::ParseTools;
use interlude::ffi::*;
use super::{ignore_chars, ident_break};

#[derive(Debug, PartialEq)]
// Integer Literal or $~~
pub enum ConfigInt { Value(u32), Ref(String) }
impl ConfigInt
{
	pub fn parse(input: &[char]) -> Result<(Self, &[char]), ParseError>
	{
		use super::ParseError::*;

		match input.front()
		{
			Some('$') =>
			{
				let (refn, rest) = input.drop(1).take_until(ident_break);
				if refn.is_empty() { Err(NameRequired) } else { Ok((ConfigInt::Ref(refn.iter().cloned().collect()), rest)) }
			},
			Some('0' ... '9') =>
			{
				let (refn, rest) = input.take_while(|c| '0' <= c && c <= '9');
				assert!(!refn.is_empty());
				refn.iter().cloned().collect::<String>().parse::<u32>().map_err(NumericParseError).map(|n| (ConfigInt::Value(n), rest))
			},
			_ => Err(IntValueRequired)
		}
	}
	pub fn parse_array(input: &[char]) -> Result<(Vec<Self>, &[char]), ParseError>
	{
		use super::ParseError::*;
		
		if input.front() == Some('[')
		{
			fn recursive<'s>(input: &'s [char], sink: &mut Vec<ConfigInt>) -> Result<&'s [char], ParseError>
			{
				ConfigInt::parse(input).and_then(|(v, r)|
				{
					sink.push(v);
					let r = r.skip_while(ignore_chars);
					if r.front() == Some(',')
					{
						let r = r.skip_while(|c| c == ',' || ignore_chars(c));
						if r.front() == Some(']') { Ok(r.drop(1)) } else { recursive(r, sink) }
					}
					else if r.front() == Some(']') { Ok(r.drop(1)) }
					else { Err(DelimiterRequired) }
				})
			}
			let mut v = Vec::new();
			recursive(input.drop(1), &mut v).map(|r| (v, r))
		}
		else
		{
			Self::parse(input).map(|(v, r)| (vec![v], r))
		}
	}
}
#[test] fn parse_config_int()
{
	assert_eq!(ConfigInt::parse(&['1', '0']), Ok((ConfigInt::Value(10), &[][..])));
	assert_eq!(ConfigInt::parse(&['$', 'T', 'A', ',']), Ok((ConfigInt::Ref("TA".into()), &[','][..])));
	assert_eq!(ConfigInt::parse(&['$', ' ']), Err(ParseError::NameRequired));
	assert_eq!(ConfigInt::parse(&['T']), Err(ParseError::IntValueRequired));
	assert_eq!(ConfigInt::parse_array(&['1', '0']), Ok((vec![ConfigInt::Value(10)], &[][..])));
	assert_eq!(ConfigInt::parse_array(&['[', '1', ',', ' ', '2', ']', '~']), Ok((vec![ConfigInt::Value(1), ConfigInt::Value(2)], &['~'][..])));
	assert_eq!(ConfigInt::parse_array(&['[', '1', ',', ',', ']']), Ok((vec![ConfigInt::Value(1)], &[][..])));
	assert_eq!(ConfigInt::parse_array(&['[', '1', ' ', '2', ']']), Err(ParseError::DelimiterRequired));
	assert_eq!(ConfigInt::parse_array(&['[', '1', ',']), Err(ParseError::IntValueRequired));
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum PixelFormat { Ref(String), User(VkFormat) }
pub fn parse_pixel_format(source: &[char]) -> Result<(PixelFormat, &[char]), ParseError>
{
	use self::PixelFormat::*;

	if source.front() == Some('$')
	{
		let (s, r) = source.drop(1).take_until(ident_break);
		Ok((Ref(s.clone_as_string()), r))
	}
	else
	{
		let (bits, rest) = source.take_until(ident_break);
		let (form, rest) = rest.skip_while(ignore_chars).take_until(ident_break);
		match (bits.clone_as_string_flatmapping(|&c| c.to_uppercase()).as_ref(), form.clone_as_string_flatmapping(|&c| c.to_uppercase()).as_ref())
		{
			("R8", "UNORM") => Ok(User(VkFormat::R8_UNORM)),
			("R8", "SNORM") => Ok(User(VkFormat::R8_SNORM)),
			("R8G8", "UNORM") => Ok(User(VkFormat::R8G8_UNORM)),
			("R8G8", "SNORM") => Ok(User(VkFormat::R8G8_SNORM)),
			("R8G8B8", "UNORM") => Ok(User(VkFormat::R8G8B8_UNORM)),
			("R8G8B8", "SNORM") => Ok(User(VkFormat::R8G8B8_SNORM)),
			("R8G8B8A8", "UNORM") => Ok(User(VkFormat::R8G8B8A8_UNORM)),
			("R8G8B8A8", "SNORM") => Ok(User(VkFormat::R8G8B8A8_SNORM)),
			("R32", "SFLOAT") => Ok(User(VkFormat::R32_SFLOAT)),
			("R16G16B16A16", "UNORM") => Ok(User(VkFormat::R16G16B16A16_UNORM)),
			("R16G16B16A16", "SNORM") => Ok(User(VkFormat::R16G16B16A16_SNORM)),
			("R16G16B16A16", "SFLOAT") => Ok(User(VkFormat::R16G16B16A16_SFLOAT)),
			_ => Err(ParseError::UnknownFormat)
		}.map(|f| (f, rest))
	}
}
lazy_static!
{
	static ref IL_COLORATTACHMENTOPTIMAL: Vec<char> = "ColorAttachmentOptimal".chars().collect();
	static ref IL_SHADERREADONLYOPTIMAL: Vec<char> = "ShaderReadOnlyOptimal".chars().collect();
}
pub fn parse_image_layout(source: &[char]) -> Result<(VkImageLayout, &[char]), ParseError>
{
	if source.starts_with_trailing_opt(&IL_COLORATTACHMENTOPTIMAL, ident_break)
	{
		Ok((VkImageLayout::ColorAttachmentOptimal, source.drop(IL_COLORATTACHMENTOPTIMAL.len())))
	}
	else if source.starts_with_trailing_opt(&IL_SHADERREADONLYOPTIMAL, ident_break)
	{
		Ok((VkImageLayout::ShaderReadOnlyOptimal, source.drop(IL_SHADERREADONLYOPTIMAL.len())))
	}
	else { Err(ParseError::UnknownImageLayout) }
}

pub fn parse_pipeline_stage_bits(source: &[char]) -> Result<(VkPipelineStageFlags, &[char]), ParseError>
{
	fn recursive<'s>(input: &'s [char], accum: &mut VkPipelineStageFlags) -> Result<&'s [char], ParseError>
	{
		if input.is_empty() { Ok(input) }
		else
		{
			let (flag, r) = input.take_until(ident_break);
			let f = match flag.clone_as_string().as_ref()
			{
				"TopOfPipe" => Ok(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
				"FragmentShaderStage" => Ok(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
				"BottomOfPipe" => Ok(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
				_ => Err(ParseError::UnknownPipelineStageFlag)
			};
			f.and_then(|f|
			{
				*accum |= f;
				let r = r.skip_while(ignore_chars);
				if r.front() == Some('/') { recursive(r.drop(1).skip_while(ignore_chars), accum) } else { Ok(r) }
			})
		}
	}
	let mut accum = 0;
	recursive(source, &mut accum).map(|r| (accum, r))
}
pub fn parse_access_mask(source: &[char]) -> Result<(VkAccessFlags, &[char]), ParseError>
{
	fn recursive<'s>(input: &'s [char], accum: &mut VkAccessFlags) -> Result<&'s [char], ParseError>
	{
		if input.is_empty() { Ok(input) }
		else
		{
			let (flag, r) = input.take_until(ident_break);
			let f = match flag.clone_as_string().as_ref()
			{
				"ColorAttachmentWrite" => Ok(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
				"ShaderRead" => Ok(VK_ACCESS_SHADER_READ_BIT),
				_ => Err(ParseError::UnknownAccessFlag)
			};
			f.and_then(|f|
			{
				*accum |= f;
				let r = r.skip_while(ignore_chars);
				if r.front() == Some('/') { recursive(r.drop(1).skip_while(ignore_chars), accum) } else { Ok(r) }
			})
		}
	}
	let mut accum = 0;
	recursive(source, &mut accum).map(|r| (accum, r))
}
pub fn parse_string_literal(source: &[char]) -> Result<(String, &[char]), ParseError>
{
	fn recursive_char<'s>(input: &'s [char], sink: &mut String) -> Result<&'s [char], ParseError>
	{
		match input.front()
		{
			Some('\\') => recursive_escape(input.drop(1), sink),
			Some('"') => Ok(input.drop(1)),
			Some(c) => { sink.push(c); recursive_char(input.drop(1), sink) },
			None => Err(ParseError::ClosingRequired)
		}
	}
	fn recursive_escape<'s>(input: &'s [char], sink: &mut String) -> Result<&'s [char], ParseError>
	{
		match input.front()
		{
			Some('t') => { sink.push('\t'); recursive_char(input.drop(1), sink) },
			Some('n') => { sink.push('\n'); recursive_char(input.drop(1), sink) },
			Some('r') => { sink.push('\r'); recursive_char(input.drop(1), sink) },
			Some(c) => { sink.push(c); recursive_char(input.drop(1), sink) },
			None => Err(ParseError::ClosingRequired)
		}
	}

	if source.front() == Some('"')
	{
		let mut buf = String::new();
		recursive_char(source.drop(1), &mut buf).map(|r| (buf, r))
	}
	else { Err(ParseError::Expected("String Literal")) }
}

#[cfg(test)] use itertools::Itertools;
#[test] fn pixel_format()
{
	assert_eq!(parse_pixel_format(&"R8G8B8A8 UNORM".chars().collect_vec()), Ok((PixelFormat::User(VkFormat::R8G8B8A8_UNORM), &[][..])));
	assert_eq!(parse_pixel_format(&"R8g8B8a8 Unorm".chars().collect_vec()), Ok((PixelFormat::User(VkFormat::R8G8B8A8_UNORM), &[][..])));
	assert_eq!(parse_pixel_format(&"R8G8B8A8 UNORM,".chars().collect_vec()), Ok((PixelFormat::User(VkFormat::R8G8B8A8_UNORM), &[','][..])));
	assert_eq!(parse_pixel_format(&"$ScreenFormat,".chars().collect_vec()), Ok((PixelFormat::Ref("ScreenFormat".into()), &[','][..])));
	assert_eq!(parse_pixel_format(&"R8G8B8A8 SF".chars().collect_vec()), Err(ParseError::UnknownFormat));
}
#[test] fn image_layout()
{
	assert_eq!(parse_image_layout(&"ColorAttachmentOptimal".chars().collect_vec()), Ok((VkImageLayout::ColorAttachmentOptimal, &[][..])));
	assert_eq!(parse_image_layout(&"ShaderReadOnlyOptimal :".chars().collect_vec()), Ok((VkImageLayout::ShaderReadOnlyOptimal, &[' ', ':'][..])));
	assert_eq!(parse_image_layout(&['A']), Err(ParseError::UnknownImageLayout));
}
#[test] fn pipeline_stage_bits()
{
	assert_eq!(parse_pipeline_stage_bits(&"FragmentShaderStage".chars().collect_vec()), Ok((VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT, &[][..])));
	assert_eq!(parse_pipeline_stage_bits(&"TopOfPipe / BottomOfPipe".chars().collect_vec()), Ok((VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT | VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, &[][..])));
	assert_eq!(parse_pipeline_stage_bits(&"a".chars().collect_vec()), Err(ParseError::UnknownPipelineStageFlag));
}
#[test] fn access_mask()
{
	assert_eq!(parse_access_mask(&"ColorAttachmentWrite".chars().collect_vec()), Ok((VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, &[][..])));
	assert_eq!(parse_access_mask(&"ColorAttachmentWrite / ShaderRead".chars().collect_vec()), Ok((VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT | VK_ACCESS_SHADER_READ_BIT, &[][..])));
	assert_eq!(parse_access_mask(&"a".chars().collect_vec()), Err(ParseError::UnknownAccessFlag));
}
