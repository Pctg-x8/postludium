
use super::ParseError;
use parsetools::*;
use vk::*;
use super::{ignore_chars, ident_break};
use std::ops::Range;

macro_rules! PartialApply1
{
	($f: expr; $p: expr) => (|x| $f(x, $p))
}

// Integer Literal or $~~
#[derive(Debug, PartialEq)]
pub enum ConfigInt { Value(u32), Ref(String) }
impl ConfigInt
{
	pub fn parse(input: &mut ParseLine) -> Result<Self, ParseError>
	{
		match input.front()
		{
			Some('$') =>
			{
				let refn = input.drop_opt(1).take_until(ident_break);
				if refn.is_empty() { Err(ParseError::NameRequired(refn.current())) } else { Ok(ConfigInt::Ref(refn.clone_as_string())) }
			},
			Some('0' ... '9') =>
			{
				let refn = input.take_while(|c| '0' <= c && c <= '9');
				assert!(!refn.is_empty());
				match refn.clone_as_string().parse::<u32>()
				{
					Ok(v) => Ok(ConfigInt::Value(v)),
					Err(e) => Err(ParseError::NumericParseError(e, refn.current()))
				}
			},
			_ => Err(ParseError::IntValueRequired(input.current()))
		}
	}
	pub fn parse_array(input: &mut ParseLine) -> Result<Vec<Self>, ParseError>
	{
		if input.front() == Some('[')
		{
			fn recursive<'s>(input: &mut ParseLine<'s>, sink: &mut Vec<ConfigInt>) -> Result<(), ParseError>
			{
				ConfigInt::parse(input).and_then(|v|
				{
					sink.push(v);
					match input.drop_while(ignore_chars).front()
					{
						Some(',') =>
						{
							if input.drop_while(|c| c == ',' || ignore_chars(c)).front() == Some(']')
							{
								input.drop_opt(1); Ok(())
							}
							else { recursive(input, sink) }
						},
						Some(']') => { input.drop_opt(1); Ok(()) },
						_ => Err(ParseError::DelimiterRequired(input.current()))
					}
				})
			}
			let mut v = Vec::new();
			recursive(input.drop_opt(1), &mut v).map(|_| v)
		}
		else { Self::parse(input).map(|v| vec![v]) }
	}
}
#[derive(Debug, PartialEq)]
pub enum NumericLiteral { Integer(i64), Floating(f64), Floating32(f32) }
impl NumericLiteral
{
	pub fn parse(input: &mut ParseLine, default32: bool) -> Result<Self, ParseError>
	{
		let s =
		{
			let ipart = input.take_while(|c| c.is_digit(10));
			if ipart.is_empty() { Err(ParseError::Expected("Numerical Value", ipart.current())) }
			else if input.front() == Some('.')
			{
				let fpart = input.drop_opt(1).take_while(|c| c.is_digit(10));
				Ok((ipart.current(), true, ipart.chars().iter().chain(&['.']).chain(fpart.chars().iter()).cloned().collect::<String>()))
			}
			else { Ok((ipart.current(), false, ipart.clone_as_string())) }
		};
		s.and_then(|(l, f, s)| if input.starts_with(&['f', '3', '2'])
		{
			input.drop_opt(3); s.parse::<f32>().map_err(|e| ParseError::FloatingParseError(e, l)).map(NumericLiteral::Floating32)
		}
		else if input.starts_with(&['f', '6', '4'])
		{
			input.drop_opt(3); s.parse::<f64>().map_err(|e| ParseError::FloatingParseError(e, l)).map(NumericLiteral::Floating)
		}
		else if input.starts_with(&['f'])
		{
			input.drop_opt(1);
			if default32 { s.parse::<f32>().map_err(|e| ParseError::FloatingParseError(e, l)).map(NumericLiteral::Floating32) }
			else { s.parse::<f64>().map_err(|e| ParseError::FloatingParseError(e, l)).map(NumericLiteral::Floating) }
		}
		else if f
		{
			if default32 { s.parse::<f32>().map_err(|e| ParseError::FloatingParseError(e, l)).map(NumericLiteral::Floating32) }
			else { s.parse::<f64>().map_err(|e| ParseError::FloatingParseError(e, l)).map(NumericLiteral::Floating) }
		}
		else
		{
			s.parse::<i64>().map_err(|e| ParseError::NumericParseError(e, l)).map(NumericLiteral::Integer)
		})
	}
}
#[derive(Debug, PartialEq)]
pub enum AssetResource { IntRef(ConfigInt), PathRef(Vec<String>) }
impl AssetResource
{
	pub fn parse(input: &mut ParseLine) -> Result<Self, ParseError>
	{
		if input.front() == Some('!')
		{
			input.drop_opt(1);
			let mut nv = Vec::new();
			loop
			{
				let s = input.take_until(ident_break);
				if s.is_empty() { break; } else
				{
					nv.push(s.clone_as_string()); if input.front() == Some('.') { input.drop_opt(1); } else { break; }
				}
			}
			if nv.is_empty() { Err(ParseError::Expected("Asset Path", input.current())) } else { Ok(AssetResource::PathRef(nv)) }
		}
		else { ConfigInt::parse(input).map(AssetResource::IntRef) }
	}
}

// Bytesize Range
pub fn parse_usize_range(source: &mut ParseLine) -> Result<Range<usize>, ParseError>
{
	let s = source.take_while(|c| c.is_digit(10));
	if s.is_empty() { Err(ParseError::BytesizeRequired(s.current())) }
	else if source.drop_while(ignore_chars).starts_with(&['.', '.'])
	{
		let e = source.drop_opt(2).drop_while(ignore_chars).take_while(|c| c.is_digit(10));
		if e.is_empty() { Err(ParseError::BytesizeRequired(e.current())) }
		else
		{
			let (sn, en) = (s.clone_as_string().parse::<usize>(), e.clone_as_string().parse::<usize>());
			sn.map_err(|se| ParseError::NumericParseError(se, s.current())).and_then(|s|
				en.map_err(|ee| ParseError::NumericParseError(ee, e.current())).map(|e| s .. e))
		}
	}
	else { Err(ParseError::Expected("Bytesize Range", source.current())) }
}

#[derive(Debug, PartialEq)]
pub enum PixelFormat { Ref(String), Value(VkFormat) }
impl PixelFormat
{
	pub fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		if source.front() == Some('$')
		{
			let s = source.drop_opt(1).take_until(ident_break);
			if s.is_empty() { Err(ParseError::NameRequired(s.current())) }
			else { Ok(PixelFormat::Ref(s.clone_as_string())) }
		}
		else
		{
			let bits = source.take_until(ident_break);
			let form = source.drop_while(ignore_chars).take_until(ident_break).clone_as_string_flatmapping(|&c| c.to_uppercase());
			match (bits.clone_as_string_flatmapping(|&c| c.to_uppercase()).as_ref(), form.as_ref())
			{
				("R8", "UNORM") => Ok(VkFormat::R8_UNORM),
				("R8", "SNORM") => Ok(VkFormat::R8_SNORM),
				("R8G8", "UNORM") => Ok(VkFormat::R8G8_UNORM),
				("R8G8", "SNORM") => Ok(VkFormat::R8G8_SNORM),
				("R8G8B8", "UNORM") => Ok(VkFormat::R8G8B8_UNORM),
				("R8G8B8", "SNORM") => Ok(VkFormat::R8G8B8_SNORM),
				("R8G8B8A8", "UNORM") => Ok(VkFormat::R8G8B8A8_UNORM),
				("R8G8B8A8", "SNORM") => Ok(VkFormat::R8G8B8A8_SNORM),
				("R32", "SFLOAT") => Ok(VkFormat::R32_SFLOAT),
				("R16G16B16A16", "UNORM") => Ok(VkFormat::R16G16B16A16_UNORM),
				("R16G16B16A16", "SNORM") => Ok(VkFormat::R16G16B16A16_SNORM),
				("R16G16B16A16", "SFLOAT") => Ok(VkFormat::R16G16B16A16_SFLOAT),
				("BLOCKCOMPRESSION4", "UNORM") | ("BC4", "UNORM") => Ok(VkFormat::BC4_UNORM_BLOCK),
				("BLOCKCOMPRESSION4", "SNORM") | ("BC4", "SNORM") => Ok(VkFormat::BC4_SNORM_BLOCK),
				("BLOCKCOMPRESSION5", "UNORM") | ("BC5", "UNORM") => Ok(VkFormat::BC5_UNORM_BLOCK),
				("BLOCKCOMPRESSION5", "SNORM") | ("BC5", "SNORM") => Ok(VkFormat::BC5_SNORM_BLOCK),
				_ => Err(ParseError::UnknownFormat(bits.current()))
			}.map(PixelFormat::Value)
		}
	}
}

lazy_static!
{
	static ref CAO: Vec<char> = "ColorAttachmentOptimal".chars().collect();
	static ref SROO: Vec<char> = "ShaderReadOnlyOptimal".chars().collect();
}
pub fn parse_image_layout(source: &mut ParseLine) -> Result<VkImageLayout, ParseError>
{
	let name = source.take_until(ident_break);
	if name == CAO[..] { Ok(VkImageLayout::ColorAttachmentOptimal) }
	else if name == SROO[..] { Ok(VkImageLayout::ShaderReadOnlyOptimal) }
	else { Err(ParseError::UnknownImageLayout(name.current())) }
}

lazy_static!
{
	static ref TOP: Vec<char> = "TopOfPipe".chars().collect();
	static ref FSS: Vec<char> = "FragmentShaderStage".chars().collect();
	static ref BOP: Vec<char> = "BottomOfPipe".chars().collect();
}
pub fn parse_pipeline_stage_bits(source: &mut ParseLine) -> Result<VkPipelineStageFlags, ParseError>
{
	fn determine_const(input: &ParseLine) -> Option<VkPipelineStageFlags>
	{
		if *input == TOP[..] { Some(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT) }
		else if *input == FSS[..] { Some(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) }
		else if *input == BOP[..] { Some(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT) }
		else { None }
	}
	fn recursive<'s>(input: &mut ParseLine<'s>, mut accum: VkPipelineStageFlags) -> Result<VkPipelineStageFlags, ParseError>
	{
		if input.is_empty() { Ok(accum) }
		else
		{
			let flag = input.take_until(ident_break);
			determine_const(&flag).ok_or(ParseError::UnknownPipelineStageFlag(flag.current())).and_then(|f|
			{
				accum |= f;
				if input.drop_while(ignore_chars).front() == Some('/')
				{
					recursive(input.drop_opt(1).drop_while(ignore_chars), accum)
				}
				else { Ok(accum) }
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
pub fn parse_access_mask(source: &mut ParseLine) -> Result<VkAccessFlags, ParseError>
{
	fn determine_const(input: &ParseLine) -> Option<VkAccessFlags>
	{
		if *input == CAW[..] { Some(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) }
		else if *input == SR[..] { Some(VK_ACCESS_SHADER_READ_BIT) }
		else { None }
	}
	fn recursive<'s>(input: &mut ParseLine<'s>, mut accum: VkAccessFlags) -> Result<VkAccessFlags, ParseError>
	{
		if input.is_empty() { Ok(accum) }
		else
		{
			let flag = input.take_until(ident_break);
			determine_const(&flag).ok_or(ParseError::UnknownAccessFlag(flag.current())).and_then(|f|
			{
				accum |= f;
				if input.drop_while(ignore_chars).front() == Some('/')
				{
					recursive(input.drop_opt(1).drop_while(ignore_chars), accum)
				}
				else { Ok(accum) }
			})
		}
	}
	recursive(source, 0)
}
lazy_static!
{
	static ref VERTEX: Vec<char> = "Vertex".chars().collect();
	static ref GEOMETRY: Vec<char> = "Geometry".chars().collect();
	static ref TESSCONTROL: Vec<char> = "TessControl".chars().collect();
	static ref TESSEVALUATION: Vec<char> = "TessEvaluation".chars().collect();
	static ref FRAGMENT: Vec<char> = "Fragment".chars().collect();
}
pub fn parse_shader_stage_bits(source: &mut ParseLine) -> Result<VkShaderStageFlags, ParseError>
{
	fn determine_flag(input: &ParseLine) -> Option<VkShaderStageFlags>
	{
		PartialEqualityMatchMap!(*input;
		{
			VERTEX[..] => Some(VK_SHADER_STAGE_VERTEX_BIT),
			GEOMETRY[..] => Some(VK_SHADER_STAGE_GEOMETRY_BIT),
			TESSCONTROL[..] => Some(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT),
			TESSEVALUATION[..] => Some(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT),
			FRAGMENT[..] => Some(VK_SHADER_STAGE_FRAGMENT_BIT);
			_ => None
		})
	}
	fn recursive(source: &mut ParseLine, mut flags: VkShaderStageFlags) -> Result<VkShaderStageFlags, ParseError>
	{
		let flag = source.take_until(ident_break);
		determine_flag(&flag).ok_or(ParseError::UnknownShaderStageFlag(flag.current())).and_then(|f|
		{
			flags |= f;
			if source.drop_while(ignore_chars).front() == Some('/')
			{
				recursive(source.drop_opt(1).drop_while(ignore_chars), flags)
			}
			else { Ok(flags) }
		})
	}
	recursive(source, 0)
}
pub fn parse_string_literal(source: &mut ParseLine) -> Result<String, ParseError>
{
	fn recursive_char<'s>(input: &mut ParseLine<'s>, sink: &mut String) -> Result<(), ParseError>
	{
		match input.front()
		{
			Some('\\') => recursive_escape(input.drop_opt(1), sink),
			Some('"') => { input.drop_opt(1); Ok(()) },
			Some(c) => { sink.push(c); recursive_char(input.drop_one(), sink) },
			None => Err(ParseError::ClosingRequired(input.current()))
		}
	}
	fn recursive_escape<'s>(input: &mut ParseLine<'s>, sink: &mut String) -> Result<(), ParseError>
	{
		match input.front()
		{
			Some('t') => { sink.push('\t'); recursive_char(input.drop_opt(1), sink) },
			Some('n') => { sink.push('\n'); recursive_char(input.drop_opt(1), sink) },
			Some('r') => { sink.push('\r'); recursive_char(input.drop_opt(1), sink) },
			Some(c) => { sink.push(c); recursive_char(input.drop_opt(1), sink) },
			None => Err(ParseError::ClosingRequired(input.current()))
		}
	}

	if source.front() == Some('"')
	{
		let mut buf = String::new();
		recursive_char(source.drop_opt(1), &mut buf).map(|_| buf)
	}
	else { Err(ParseError::Expected("String Literal", source.current())) }
}

#[cfg(test)] #[macro_use] mod tests
{
	use super::*;
	use itertools::Itertools;

	macro_rules! Testing
	{
		{$f: expr; $t: expr => $e: expr , $($r: tt)+} =>
		{
			Testing! { $f; $t => $e }
			Testing! { $($r)* }
		};
		{$f: expr; $t: expr => $e: expr ,} =>
		{
			Testing! { $f; $t => $e }
		};
		{$f: expr; $t: expr => $e: expr} =>
		{
			assert_eq!($f(&mut ParseLine(&$t.chars().collect_vec(), 0)), $e);
		}
	}
	#[test] fn parse_config_int()
	{
		Testing!
		{
			ConfigInt::parse; "10" => Ok(ConfigInt::Value(10)),
			ConfigInt::parse; "$TA," => Ok(ConfigInt::Ref("TA".into())),
			ConfigInt::parse; "$ " => Err(ParseError::NameRequired(1)),
			ConfigInt::parse; "T" => Err(ParseError::IntValueRequired(0)),
			ConfigInt::parse_array; "10" => Ok(vec![ConfigInt::Value(10)]),
			ConfigInt::parse_array; "[1, 2]~" => Ok(vec![ConfigInt::Value(1), ConfigInt::Value(2)]),
			ConfigInt::parse_array; "[1,,]" => Ok(vec![ConfigInt::Value(1)]),
			ConfigInt::parse_array; "[1 2]" => Err(ParseError::DelimiterRequired(3)),
			ConfigInt::parse_array; "[1," => Err(ParseError::IntValueRequired(3))
		}
	}
	#[test] fn parse_numeric()
	{
		Testing!
		{
			PartialApply1!(NumericLiteral::parse; false); "10" => Ok(NumericLiteral::Integer(10)),
			PartialApply1!(NumericLiteral::parse; false); "10.0" => Ok(NumericLiteral::Floating(10.0)),
			PartialApply1!(NumericLiteral::parse; true); "10.0" => Ok(NumericLiteral::Floating32(10.0)),
			PartialApply1!(NumericLiteral::parse; false); "10f" => Ok(NumericLiteral::Floating(10.0)),
			PartialApply1!(NumericLiteral::parse; true); "10f" => Ok(NumericLiteral::Floating32(10.0)),
			PartialApply1!(NumericLiteral::parse; false); "10f32" => Ok(NumericLiteral::Floating32(10.0)),
			PartialApply1!(NumericLiteral::parse; true); "10f64" => Ok(NumericLiteral::Floating(10.0)),
			PartialApply1!(NumericLiteral::parse; false); "" => Err(ParseError::Expected("Numerical Value", 0))
		}
	}
	#[test] fn parse_asset_resource()
	{
		Testing!
		{
			AssetResource::parse; "!shaders.PureF" => Ok(AssetResource::PathRef(vec!["shaders".into(), "PureF".into()])),
			AssetResource::parse; "$en" => Ok(AssetResource::IntRef(ConfigInt::Ref("en".into()))),
			AssetResource::parse; "!" => Err(ParseError::Expected("Asset Path", 1)),
			AssetResource::parse; "~" => Err(ParseError::IntValueRequired(0))
		}
	}
	#[test] fn usize_range()
	{
		Testing!
		{
			parse_usize_range; "0 .. 16" => Ok(0usize .. 16usize),
			parse_usize_range; "n .. m" => Err(ParseError::BytesizeRequired(0)),
			parse_usize_range; "4" => Err(ParseError::Expected("Bytesize Range", 1)),
			parse_usize_range; "4 ..n" => Err(ParseError::BytesizeRequired(4))
		}
	}
	#[test] fn parse_pixel_format()
	{
		Testing!
		{
			PixelFormat::parse; "R8G8B8A8 UNORM" => Ok(PixelFormat::Value(VkFormat::R8G8B8A8_UNORM)),
			PixelFormat::parse; "R8G8b8A8 Unorm" => Ok(PixelFormat::Value(VkFormat::R8G8B8A8_UNORM)),
			PixelFormat::parse; "BlockCompression5 Unorm" => Ok(PixelFormat::Value(VkFormat::BC5_UNORM_BLOCK)),
			PixelFormat::parse; "$ScreenFormat" => Ok(PixelFormat::Ref("ScreenFormat".into())),
			PixelFormat::parse; "R8G8B8A8 SF" => Err(ParseError::UnknownFormat(0))
		}
	}
	#[test] fn image_layouts()
	{
		Testing!
		{
			parse_image_layout; "ColorAttachmentOptimal:" => Ok(VkImageLayout::ColorAttachmentOptimal),
			parse_image_layout; "Shaders" => Err(ParseError::UnknownImageLayout(0))
		}
	}
	#[test] fn shader_stage_bits()
	{
		Testing!
		{
			parse_shader_stage_bits; "Vertex / TessEvaluation" => Ok(VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT),
			parse_shader_stage_bits; "Geometry" => Ok(VK_SHADER_STAGE_GEOMETRY_BIT),
			parse_shader_stage_bits; "GEOMETRY" => Err(ParseError::UnknownShaderStageFlag(0)),
			parse_shader_stage_bits; "Vertex/" => Err(ParseError::UnknownShaderStageFlag(7))
		}
	}
	#[test] fn pipeline_stage_bits()
	{
		Testing!
		{
			parse_pipeline_stage_bits; "FragmentShaderStage" => Ok(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
			parse_pipeline_stage_bits; "TopOfPipe / BottomOfPipe" => Ok(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT | VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
			parse_pipeline_stage_bits; "a" => Err(ParseError::UnknownPipelineStageFlag(0))
		}
	}
	#[test] fn access_mask()
	{
		Testing!
		{
			parse_access_mask; "ColorAttachmentWrite" => Ok(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
			parse_access_mask; "ColorAttachmentWrite / ShaderRead" => Ok(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT | VK_ACCESS_SHADER_READ_BIT),
			parse_access_mask; "a" => Err(ParseError::UnknownAccessFlag(0))
		}
	}
	#[test] fn string_literal()
	{
		Testing!
		{
			parse_string_literal; "\"HogeResource\"" => Ok("HogeResource".into()),
			parse_string_literal; "\"HogeResource" => Err(ParseError::ClosingRequired(13)),
			parse_string_literal; "A" => Err(ParseError::Expected("String Literal", 0))
		}
	}
}
