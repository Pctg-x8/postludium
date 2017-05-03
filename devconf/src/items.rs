
use super::ParseError;
use parsetools::*;
use interlude::ffi::*;
use super::{ignore_chars, ident_break};
use std::ops::Range;
use std::fmt::Debug;
use syntree::*;
use parser::{ParserExt, CombinedParser, FromSource, FromSourceArray};
use std::borrow::Cow;

macro_rules! PartialApply1
{
	($f: expr; $p: expr) => (|x| $f(x, $p))
}
macro_rules! CombineFunc
{
	($f: expr; $g: expr) => (|x| $f($g(x)))
}

pub type LocatedParseResult<T> = Result<LocationPacked<T>, ParseError>;
/// The structure that can be constructed by source with its location, returns error when it is found.
pub trait FromSourceLocated : FromSource + PartialEq + Debug
{
	fn parse_with_location(source: &mut ParseLine) -> LocatedParseResult<Self>
	{
		let inloc = source.current();
		<Self as FromSource>::parse(source).map(|x| LocationPacked(Location(source.line(), inloc), x))
	}
}
/// The array that can be constructed with some structures, returns error when it is found.
pub trait FromSourceArrayLocated : FromSourceLocated
{
	fn array_name() -> Cow<'static, str> { format!("Array of {}", Self::object_name()).into() }
	fn parse_array_with_location(source: &mut ParseLine) -> Result<Vec<LocationPacked<Self>>, ParseError>
	{
		// '[' self,* ']'
		if source.front() != Some('[') { Err(ParseError::Expected(Self::array_name(), source.current())) }
		else
		{
			if source.drop_opt(1).drop_while(ignore_chars).front() == Some(']') { Ok(Vec::new()) }
			else
			{
				let mut sink = Vec::new();
				while try!(Self::parse_with_location(source).and_then(|abs|
				{
					sink.push(abs);
					match source.drop_while(ignore_chars).front()
					{
						Some(',') => { source.drop_opt(1).drop_while(ignore_chars); Ok(true) },
						Some(']') => { source.drop_opt(1); Ok(false) },
						_ => Err(ParseError::ClosingRequired(source.current()))
					}
				})) {}
				Ok(sink)
			}
		}
	}
}

impl FromSource for ConfigInt
{
	fn object_name() -> Cow<'static, str> { "ConfigInt".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		match source.front()
		{
			Some('$') => source.drop_opt(1).take_until(ident_break).require_content(|s| ParseError::NameRequired(s.current()))
				.map(|x| ConfigInt::Ref(x.clone_as_string())),
			Some('0' ... '9') =>
			{
				let refn = source.take_while(|c| '0' <= c && c <= '9');
				refn.clone_as_string().parse::<u32>()
					.map_err(|e| ParseError::NumericParseError(e, refn.current())).map(ConfigInt::Value)
			},
			_ => Err(ParseError::IntValueRequired(source.current()))
		}
	}
}
impl FromSourceLocated for ConfigInt {}
impl FromSourceArray for ConfigInt {}
impl FromSourceArrayLocated for ConfigInt
{
	fn parse_array_with_location(source: &mut ParseLine) -> Result<Vec<LocationPacked<Self>>, ParseError>
	{
		if source.front() == Some('[')
		{
			// '[' self,* ']'
			if source.drop_opt(1).drop_while(ignore_chars).front() == Some(']') { Ok(Vec::new()) }
			else
			{
				let mut sink = Vec::new();
				fn parse_loop(source: &mut ParseLine, sink: &mut Vec<LocationPacked<ConfigInt>>) -> Result<(), ParseError>
				{
					match source.drop_while(|x| ignore_chars(x) || x == ',').front()
					{
						Some(']') => { source.drop_opt(1); Ok(()) },
						Some(_) => source.parse_loc::<ConfigInt>().and_then(|abs|
						{
							sink.push(abs);
							match source.drop_while(ignore_chars).front()
							{
								Some(',') => parse_loop(source.drop_opt(1), sink),
								Some(']') => { source.drop_opt(1); Ok(()) },
								_ => Err(ParseError::DelimiterRequired(source.current()))
							}
						}),
						None => Err(ParseError::ClosingRequired(source.current()))
					}
				}
				parse_loop(source, &mut sink).map(|_| sink)
			}
		}
		else { Self::parse_with_location(source).map(|x| vec![x]) }
	}
}

impl NumericLiteral
{
	pub fn parse(input: &mut ParseLine, default32: bool) -> LocatedParseResult<Self>
	{
		let startloc = input.current();
		let s =
		{
			let ipart = input.take_while(|c| c.is_digit(10));
			if ipart.is_empty() { Err(ParseError::Expected("Numerical Value".into(), ipart.current())) }
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
		}).map(|v| LocationPacked(Location(input.line(), startloc), v))
	}
}
impl FromSource for AssetResource
{
	fn object_name() -> Cow<'static, str> { "Asset Resource".into() }
	fn parse(source: &mut ParseLine) -> Result<AssetResource, ParseError>
	{
		if source.front() == Some('!')
		{
			// following path
			source.drop_opt(1);
			let mut nv = Vec::new();
			loop
			{
				let s = source.take_until(ident_break);
				if s.is_empty() { break; } else
				{
					nv.push(s.clone_as_string());
					if source.front() == Some('.') { source.drop_opt(1); } else { break; }
				}
			}
			if nv.is_empty() { Err(ParseError::Expected("Asset Path".into(), source.current())) }
			else { Ok(AssetResource::PathRef(nv)) }
		}
		else { source.parse::<ConfigInt>().map(AssetResource::IntRef) }
	}
}
impl FromSourceLocated for AssetResource {}

// Bytesize Range
pub fn parse_usize_range(source: &mut ParseLine) -> LocatedParseResult<Range<usize>>
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
			sn.map_err(|se| ParseError::NumericParseError(se, s.current())).and_then(|sn|
				en.map_err(|ee| ParseError::NumericParseError(ee, e.current())).map(|en| LocationPacked(s.location(), sn .. en)))
		}
	}
	else { Err(ParseError::Expected("Bytesize Range".into(), source.current())) }
}

impl FromSource for Format
{
	fn object_name() -> Cow<'static, str> { "Format".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		if source.front() == Some('$')
		{
			let s = source.drop_opt(1).take_until(ident_break);
			if s.is_empty() { Err(ParseError::NameRequired(s.current())) }
			else { Ok(Format::Ref(s.clone_as_string())) }
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
			}.map(Format::Value)
		}
	}
}
impl FromSourceLocated for Format {}
/*
impl FromSourceLocated for Format
{
	fn parse(source: &mut ParseLine) -> LocatedParseResult<Self>
	{
		let inloc = source.current();
		<Self as FromSource>::parse(source).map(|f| LocationPacked(source.line(), inloc, f))
	}
}
*/

lazy_static!
{
	static ref CAO: Vec<char> = "ColorAttachmentOptimal".chars().collect();
	static ref SROO: Vec<char> = "ShaderReadOnlyOptimal".chars().collect();
}
pub fn parse_image_layout(source: &mut ParseLine) -> LocatedParseResult<VkImageLayout>
{
	let name = source.take_until(ident_break);
	if name == CAO[..] { Ok(LocationPacked(name.location(), VkImageLayout::ColorAttachmentOptimal)) }
	else if name == SROO[..] { Ok(LocationPacked(name.location(), VkImageLayout::ShaderReadOnlyOptimal)) }
	else { Err(ParseError::UnknownImageLayout(name.current())) }
}

lazy_static!
{
	static ref TOP: Vec<char> = "TopOfPipe".chars().collect();
	static ref FSS: Vec<char> = "FragmentShaderStage".chars().collect();
	static ref BOP: Vec<char> = "BottomOfPipe".chars().collect();
}
pub fn parse_pipeline_stage_bits(source: &mut ParseLine) -> LocatedParseResult<VkPipelineStageFlags>
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
	let startloc = source.current();
	recursive(source, 0).map(|f| LocationPacked(Location(source.line(), startloc), f))
}
lazy_static!
{
	static ref CAW: Vec<char> = "ColorAttachmentWrite".chars().collect();
	static ref SR: Vec<char> = "ShaderRead".chars().collect();
}
impl FromSource for AccessFlags
{
	fn object_name() -> Cow<'static, str> { "Access Flags".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		fn determine_const(input: &ParseLine) -> Option<VkAccessFlags>
		{
			if *input == CAW[..] { Some(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) }
			else if *input == SR[..] { Some(VK_ACCESS_SHADER_READ_BIT) }
			else { None }
		}
		fn recursive<'s>(input: &mut ParseLine<'s>, mut accum: AccessFlags) -> Result<AccessFlags, ParseError>
		{
			if input.is_empty() { Ok(accum) }
			else
			{
				let flag = input.take_until(ident_break);
				determine_const(&flag).ok_or(ParseError::UnknownAccessFlag(flag.current())).and_then(|f|
				{
					accum.0 |= f;
					if input.drop_while(ignore_chars).front() == Some('/')
					{
						recursive(input.drop_opt(1).drop_while(ignore_chars), accum)
					}
					else { Ok(accum) }
				})
			}
		}
		recursive(source, AccessFlags(0))
	}
}
impl FromSourceLocated for AccessFlags {}
lazy_static!
{
	static ref VERTEX: Vec<char> = "Vertex".chars().collect();
	static ref GEOMETRY: Vec<char> = "Geometry".chars().collect();
	static ref TESSCONTROL: Vec<char> = "TessControl".chars().collect();
	static ref TESSEVALUATION: Vec<char> = "TessEvaluation".chars().collect();
	static ref FRAGMENT: Vec<char> = "Fragment".chars().collect();
}
impl FromSource for ShaderStageFlags
{
	fn object_name() -> Cow<'static, str> { "Shader Stage Flags".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
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
		fn recursive(source: &mut ParseLine, mut flags: ShaderStageFlags) -> Result<ShaderStageFlags, ParseError>
		{
			let flag = source.take_until(ident_break);
			determine_flag(&flag).ok_or(ParseError::UnknownShaderStageFlag(flag.current())).and_then(|f|
			{
				flags.0 |= f;
				if source.drop_while(ignore_chars).front() == Some('/')
				{
					recursive(source.drop_opt(1).drop_while(ignore_chars), flags)
				}
				else { Ok(flags) }
			})
		}
		recursive(source, ShaderStageFlags(0))
	}
}
impl FromSourceLocated for ShaderStageFlags {}

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
			assert_eq!($f(&mut ParseLine::wrap(&$t.chars().collect_vec(), 0, 1)), $e);
		}
	}
	#[test] fn parse_config_int()
	{
		Testing!
		{
			ConfigInt::parse_with_location; "10" => Ok(LocationPacked(Location(1, 0), ConfigInt::Value(10))),
			ConfigInt::parse_with_location; "$TA," => Ok(LocationPacked(Location(1, 0), ConfigInt::Ref("TA".into()))),
			ConfigInt::parse_with_location; "$ " => Err(ParseError::NameRequired(1)),
			ConfigInt::parse_with_location; "T" => Err(ParseError::IntValueRequired(0)),
			ConfigInt::parse_array_with_location; "10" => Ok(vec![LocationPacked(Location(1, 0), ConfigInt::Value(10))]),
			ConfigInt::parse_array_with_location; "[1, 2]~" => Ok(vec![
				LocationPacked(Location(1, 1), ConfigInt::Value(1)), LocationPacked(Location(1, 4), ConfigInt::Value(2))
			]),
			ConfigInt::parse_array_with_location; "[1,,]" => Ok(vec![LocationPacked(Location(1, 1), ConfigInt::Value(1))]),
			ConfigInt::parse_array_with_location; "[1 2]" => Err(ParseError::DelimiterRequired(3)),
			ConfigInt::parse_array_with_location; "[1," => Err(ParseError::ClosingRequired(3))
		}
	}
	#[test] fn parse_numeric()
	{
		Testing!
		{
			PartialApply1!(NumericLiteral::parse; false); "10" => Ok(LocationPacked(Location(1, 0), NumericLiteral::Integer(10))),
			PartialApply1!(NumericLiteral::parse; false); "10.0" => Ok(LocationPacked(Location(1, 0), NumericLiteral::Floating(10.0))),
			PartialApply1!(NumericLiteral::parse; true); "10.0" => Ok(LocationPacked(Location(1, 0), NumericLiteral::Floating32(10.0))),
			PartialApply1!(NumericLiteral::parse; false); "10f" => Ok(LocationPacked(Location(1, 0), NumericLiteral::Floating(10.0))),
			PartialApply1!(NumericLiteral::parse; true); "10f" => Ok(LocationPacked(Location(1, 0), NumericLiteral::Floating32(10.0))),
			PartialApply1!(NumericLiteral::parse; false); "10f32" => Ok(LocationPacked(Location(1, 0), NumericLiteral::Floating32(10.0))),
			PartialApply1!(NumericLiteral::parse; true); "10f64" => Ok(LocationPacked(Location(1, 0), NumericLiteral::Floating(10.0))),
			PartialApply1!(NumericLiteral::parse; false); "" => Err(ParseError::Expected("Numerical Value".into(), 0))
		}
	}
	#[test] fn parse_asset_resource()
	{
		Testing!
		{
			AssetResource::parse; "!shaders.PureF" => Ok(AssetResource::PathRef(vec!["shaders".into(), "PureF".into()])),
			AssetResource::parse; "$en" => Ok(AssetResource::IntRef(ConfigInt::Ref("en".into()))),
			AssetResource::parse; "!" => Err(ParseError::Expected("Asset Path".into(), 1)),
			AssetResource::parse; "~" => Err(ParseError::IntValueRequired(0))
		}
	}
	#[test] fn usize_range()
	{
		Testing!
		{
			parse_usize_range; "0 .. 16" => Ok(LocationPacked(Location(1, 0), 0usize .. 16usize)),
			parse_usize_range; "n .. m" => Err(ParseError::BytesizeRequired(0)),
			parse_usize_range; "4" => Err(ParseError::Expected("Bytesize Range".into(), 1)),
			parse_usize_range; "4 ..n" => Err(ParseError::BytesizeRequired(4))
		}
	}
	#[test] fn parse_pixel_format()
	{
		Testing!
		{
			Format::parse; "R8G8B8A8 UNORM" => Ok(Format::Value(VkFormat::R8G8B8A8_UNORM)),
			Format::parse; "R8G8b8A8 Unorm" => Ok(Format::Value(VkFormat::R8G8B8A8_UNORM)),
			Format::parse; "BlockCompression5 Unorm" => Ok(Format::Value(VkFormat::BC5_UNORM_BLOCK)),
			Format::parse; "$ScreenFormat" => Ok(Format::Ref("ScreenFormat".into())),
			Format::parse; "R8G8B8A8 SF" => Err(ParseError::UnknownFormat(0))
		}
	}
	#[test] fn image_layouts()
	{
		Testing!
		{
			parse_image_layout; "ColorAttachmentOptimal:" => Ok(LocationPacked(Location(1, 0), VkImageLayout::ColorAttachmentOptimal)),
			parse_image_layout; "Shaders" => Err(ParseError::UnknownImageLayout(0))
		}
	}
	#[test] fn shader_stage_bits()
	{
		Testing!
		{
			ShaderStageFlags::parse; "Vertex / TessEvaluation" => Ok(ShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT)),
			ShaderStageFlags::parse; "Geometry" => Ok(ShaderStageFlags(VK_SHADER_STAGE_GEOMETRY_BIT)),
			ShaderStageFlags::parse; "GEOMETRY" => Err(ParseError::UnknownShaderStageFlag(0)),
			ShaderStageFlags::parse; "Vertex/" => Err(ParseError::UnknownShaderStageFlag(7))
		}
	}
	#[test] fn pipeline_stage_bits()
	{
		Testing!
		{
			parse_pipeline_stage_bits; "FragmentShaderStage" => Ok(LocationPacked(Location(1, 0), VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)),
			parse_pipeline_stage_bits; "TopOfPipe / BottomOfPipe" => Ok(LocationPacked(Location(1, 0), VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT | VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)),
			parse_pipeline_stage_bits; "a" => Err(ParseError::UnknownPipelineStageFlag(0))
		}
	}
	#[test] fn access_mask()
	{
		Testing!
		{
			AccessFlags::parse; "ColorAttachmentWrite" => Ok(AccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)),
			AccessFlags::parse; "ColorAttachmentWrite / ShaderRead" => Ok(AccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT | VK_ACCESS_SHADER_READ_BIT)),
			AccessFlags::parse; "a" => Err(ParseError::UnknownAccessFlag(0))
		}
	}
}
