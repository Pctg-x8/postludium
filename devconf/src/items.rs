
use {std, interlude};
use error::*;
use parsetools::*;
use interlude::ffi::*;
use super::{ignore_chars, ident_break};
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
		let inloc = source.location();
		<Self as FromSource>::parse(source).map(|x| LocationPacked(inloc, x))
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
						_ => Err(ParseError::Required(RequestType::Closing, source.current()))
					}
				})) {}
				Ok(sink)
			}
		}
	}
}

impl<N: FromSource> FromSource for ConfigInt<N>
{
	fn object_name() -> Cow<'static, str> { "ConfigInt".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		match source.front()
		{
			Some('$') => source.drop_opt(1).take_until(ident_break).require_content(|s| ParseError::Required(RequestType::Name, s.current()))
				.map(|x| ConfigInt::Ref(x.clone_as_string())),
			Some(_) => source.parse::<N>().map(ConfigInt::Value).map_err(|e| e.xc_require_type(RequestType::ConfigInt)),
			_ => Err(ParseError::Required(RequestType::ConfigInt, source.current()))
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
								_ => Err(ParseError::Required(RequestType::Delimiter, source.current()))
							}
						}),
						None => Err(ParseError::Required(RequestType::Closing, source.current()))
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
		let inloc = input.location();
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
		}).map(|v| LocationPacked(inloc, v))
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

impl FromSource for Format
{
	fn object_name() -> Cow<'static, str> { "Format".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		if source.front() == Some('$')
		{
			let s = source.drop_opt(1).take_until(ident_break);
			if s.is_empty() { Err(ParseError::Required(RequestType::Name, s.current())) }
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
				("R32G32B32A32", "SFLOAT") => Ok(VkFormat::R32G32B32A32_SFLOAT),
				("BLOCKCOMPRESSION4", "UNORM") | ("BC4", "UNORM") => Ok(VkFormat::BC4_UNORM_BLOCK),
				("BLOCKCOMPRESSION4", "SNORM") | ("BC4", "SNORM") => Ok(VkFormat::BC4_SNORM_BLOCK),
				("BLOCKCOMPRESSION5", "UNORM") | ("BC5", "UNORM") => Ok(VkFormat::BC5_UNORM_BLOCK),
				("BLOCKCOMPRESSION5", "SNORM") | ("BC5", "SNORM") => Ok(VkFormat::BC5_SNORM_BLOCK),
				_ => Err(ParseError::UnknownEnumValue(EnumType::Format, bits.current()))
			}.map(Format::Value)
		}
	}
}
impl FromSourceLocated for Format {}

pub fn parse_image_layout(source: &mut ParseLine) -> LocatedParseResult<VkImageLayout>
{
	let name = source.take_until(ident_break);
	match name.chars()
	{
		&['C', 'o', 'l', 'o', 'r', 'A', 't', 't', 'a', 'c', 'h', 'm', 'e', 'n', 't', 'O', 'p', 't', 'i', 'm', 'a', 'l'] =>
			Ok(LocationPacked(name.location(), VkImageLayout::ColorAttachmentOptimal)),
		&['S', 'h', 'a', 'd', 'e', 'r', 'R', 'e', 'a', 'd', 'O', 'n', 'l', 'y', 'O', 'p', 't', 'i', 'm', 'a', 'l'] =>
			Ok(LocationPacked(name.location(), VkImageLayout::ShaderReadOnlyOptimal)),
		_ => Err(ParseError::UnknownEnumValue(EnumType::ImageLayout, name.current()))
	}
}

/// The trait for flag types provides empty flag value
pub trait EmptyBits { fn empty_bits() -> Self; }
impl EmptyBits for VkPipelineStageFlags { fn empty_bits() -> Self { 0 } }
impl EmptyBits for interlude::AccessFlags { fn empty_bits() -> Self { unsafe { std::mem::transmute(0) } } }
impl EmptyBits for interlude::ShaderStage { fn empty_bits() -> Self { unsafe { std::mem::transmute(0u8) } } }

/// F/*
fn parse_flags<F: EmptyBits + std::ops::BitOrAssign, DF>(source: &mut ParseLine, identifier: DF) -> Result<F, ParseError>
	where DF: Fn(&ParseLine) -> Result<F, ParseError>
{
	fn recursive<F: EmptyBits + std::ops::BitOrAssign, DF>(input: &mut ParseLine, mut accum: F, identifier: DF) -> Result<F, ParseError>
		where DF: Fn(&ParseLine) -> Result<F, ParseError>
	{
		if input.is_empty() { Ok(accum) }
		else
		{
			accum |= identifier(&input.take_until(ident_break))?;
			if input.drop_while(ignore_chars).front() == Some('/') { recursive(input.drop_opt(1).drop_while(ignore_chars), accum, identifier) }
			else { Ok(accum) }
		}
	}
	recursive(source, F::empty_bits(), identifier)
}

pub fn parse_pipeline_stage_bits(source: &mut ParseLine) -> LocatedParseResult<VkPipelineStageFlags>
{
	let inloc = source.location();
	parse_flags(source, |input| match input.chars()
	{
		&['T', 'o', 'p', 'O', 'f', 'P', 'i', 'p', 'e'] => Ok(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
		&['F', 'r', 'a', 'g', 'm', 'e', 'n', 't', 'S', 'h', 'a', 'd', 'e', 'r', 'S', 't', 'a', 'g', 'e'] => Ok(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
		&['B', 'o', 't', 't', 'o', 'm', 'O', 'f', 'P', 'i', 'p', 'e'] => Ok(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
		_ => Err(ParseError::UnknownEnumValue(EnumType::PipelineStageFlag, input.current()))
	}).map(|x| LocationPacked(inloc, x))
}
impl FromSource for interlude::AccessFlags
{
	fn object_name() -> Cow<'static, str> { "Access Flags".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		use interlude::AccessFlags::*;
		parse_flags(source, |input| match input.chars()
		{
			&['C', 'o', 'l', 'o', 'r', 'A', 't', 't', 'a', 'c', 'h', 'm', 'e', 'n', 't', 'W', 'r', 'i', 't', 'e'] => Ok(ColorAttachmentWrite),
			&['S', 'h', 'a', 'd', 'e', 'r', 'R', 'e', 'a', 'd'] => Ok(ShaderRead),
			_ => Err(ParseError::UnknownEnumValue(EnumType::AccessFlag, input.current()))
		})
	}
}
impl FromSourceLocated for interlude::AccessFlags {}
impl FromSource for interlude::ShaderStage
{
	fn object_name() -> Cow<'static, str> { "Shader Stage".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		use interlude::ShaderStage::*;
		parse_flags(source, |input| match input.chars()
		{
			&['V', 'e', 'r', 't', 'e', 'x'] => Ok(Vertex),
			&['T', 'e', 's', 's', 'C', 'o', 'n', 't', 'r', 'o', 'l'] => Ok(TessControl),
			&['T', 'e', 's', 's', 'E', 'v', 'a', 'l', 'u', 'a', 't', 'i', 'o', 'n'] => Ok(TessEvaluation),
			&['G', 'e', 'o', 'm', 'e', 't', 'r', 'y'] => Ok(Geometry),
			&['F', 'r', 'a', 'g', 'm', 'e', 'n', 't'] => Ok(Fragment),
			_ => Err(ParseError::UnknownEnumValue(EnumType::ShaderStageFlag, input.current()))
		})
	}
}
impl FromSourceLocated for interlude::ShaderStage {}

#[cfg(test)] #[macro_use] mod tests
{
	use super::*;
	use itertools::Itertools;

	macro_rules! Testing
	{
		{} => ();
		{$f: expr; $t: expr => $e: expr , $($r: tt)*} =>
		{
			Testing! { $f; $t => $e }
			Testing! { $($r)* }
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
			ConfigInt::parse_with_location; "10" => Ok(LocationPacked(Location(1, 1), ConfigInt::Value(10))),
			ConfigInt::parse_with_location; "$TA," => Ok(LocationPacked(Location(1, 1), ConfigInt::Ref("TA".into()))),
			ConfigInt::parse_with_location; "$ " => Err(ParseError::Required(RequestType::Name, 1)),
			ConfigInt::parse_with_location; "T" => Err(ParseError::Required(RequestType::ConfigInt, 0)),
			ConfigInt::parse_array_with_location; "10" => Ok(vec![LocationPacked(Location(1, 1), ConfigInt::Value(10))]),
			ConfigInt::parse_array_with_location; "[1, 2]~" => Ok(vec![
				LocationPacked(Location(1, 2), ConfigInt::Value(1)), LocationPacked(Location(1, 5), ConfigInt::Value(2))
			]),
			ConfigInt::parse_array_with_location; "[1,,]" => Ok(vec![LocationPacked(Location(1, 2), ConfigInt::Value(1))]),
			ConfigInt::parse_array_with_location; "[1 2]" => Err(ParseError::Required(RequestType::Delimiter, 3)),
			ConfigInt::parse_array_with_location; "[1," => Err(ParseError::Required(RequestType::Closing, 3))
		}
	}
	#[test] fn parse_numeric()
	{
		Testing!
		{
			PartialApply1!(NumericLiteral::parse; false); "10"    => Ok(LocationPacked(Location(1, 1), NumericLiteral::Integer(10))),
			PartialApply1!(NumericLiteral::parse; false); "10.0"  => Ok(LocationPacked(Location(1, 1), NumericLiteral::Floating(10.0))),
			PartialApply1!(NumericLiteral::parse; true);  "10.0"  => Ok(LocationPacked(Location(1, 1), NumericLiteral::Floating32(10.0))),
			PartialApply1!(NumericLiteral::parse; false); "10f"   => Ok(LocationPacked(Location(1, 1), NumericLiteral::Floating(10.0))),
			PartialApply1!(NumericLiteral::parse; true);  "10f"   => Ok(LocationPacked(Location(1, 1), NumericLiteral::Floating32(10.0))),
			PartialApply1!(NumericLiteral::parse; false); "10f32" => Ok(LocationPacked(Location(1, 1), NumericLiteral::Floating32(10.0))),
			PartialApply1!(NumericLiteral::parse; true);  "10f64" => Ok(LocationPacked(Location(1, 1), NumericLiteral::Floating(10.0))),
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
			AssetResource::parse; "~" => Err(ParseError::Required(RequestType::ConfigInt, 0))
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
			Format::parse; "R8G8B8A8 SF" => Err(ParseError::UnknownEnumValue(EnumType::Format, 0))
		}
	}
	#[test] fn image_layouts()
	{
		Testing!
		{
			parse_image_layout; "ColorAttachmentOptimal:" => Ok(LocationPacked(Location(1, 1), VkImageLayout::ColorAttachmentOptimal)),
			parse_image_layout; "Shaders" => Err(ParseError::UnknownEnumValue(EnumType::ImageLayout, 0))
		}
	}
	#[test] fn shader_stage_bits()
	{
		Testing!
		{
			interlude::ShaderStage::parse; "Vertex / TessEvaluation" => Ok(interlude::ShaderStage::Vertex | interlude::ShaderStage::TessEvaluation),
			interlude::ShaderStage::parse; "Geometry" => Ok(interlude::ShaderStage::Geometry),
			interlude::ShaderStage::parse; "GEOMETRY" => Err(ParseError::UnknownEnumValue(EnumType::ShaderStageFlag, 0)),
			interlude::ShaderStage::parse; "Vertex/" => Ok(interlude::ShaderStage::Vertex)
		}
	}
	#[test] fn pipeline_stage_bits()
	{
		Testing!
		{
			parse_pipeline_stage_bits; "FragmentShaderStage" => Ok(LocationPacked(Location(1, 1), VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)),
			parse_pipeline_stage_bits; "TopOfPipe / BottomOfPipe" => Ok(LocationPacked(Location(1, 1), VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT | VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)),
			parse_pipeline_stage_bits; "a" => Err(ParseError::UnknownEnumValue(EnumType::PipelineStageFlag, 0))
		}
	}
	#[test] fn access_mask()
	{
		Testing!
		{
			interlude::AccessFlags::parse; "ColorAttachmentWrite" => Ok(interlude::AccessFlags::ColorAttachmentWrite),
			interlude::AccessFlags::parse; "ColorAttachmentWrite / ShaderRead" => Ok(interlude::AccessFlags::ColorAttachmentWrite | interlude::AccessFlags::ShaderRead),
			interlude::AccessFlags::parse; "a" => Err(ParseError::UnknownEnumValue(EnumType::AccessFlag, 0))
		}
	}
}
