//! Postludium: Device Configuration Processor: Parser

use std;
use std::ops::Range;
use std::num::{ParseIntError, ParseFloatError};
use std::collections::BTreeMap;
use parsetools::*;
use std::borrow::Cow;
use interlude::ffi::*;
use std::path::PathBuf;
use interlude::*;

pub use items::*;
pub use hobjects::*;

use syntree::*;
use syntree::PipelineLayout;

// Misc
#[derive(Debug, PartialEq)]
enum FramebufferRenderPassRef { Int(LocationPacked<ConfigInt>), Presented, None }
impl ParsedDeviceResources
{
	pub fn empty() -> Self
	{
		ParsedDeviceResources
		{
			includes: Vec::new(), renderpasses: NamedContents::new(), simple_rps: NamedContents::new(), presented_rps: NamedContents::new(),
			descriptor_set_layouts: NamedContents::new(), push_constant_layouts: NamedContents::new(),
			pipeline_layouts: NamedContents::new(), descriptor_sets: NamedContents::new(), pipeline_states: NamedContents::new(),
			externs: NamedContents::new(), framebuffers: NamedContents::new(), images: NamedContents::new(), ind_shaders: IndependentShaders::new()
		}
	}
}
pub struct Let<T>(T);
impl<T> Let<T>
{
	pub fn _in<F, R>(self, binder: F) -> R where F: FnOnce(T) -> R { binder(self.0) }
}

// --- Parse Error --- //
#[derive(Debug, PartialEq)]
pub enum ParseError
{
	NameRequired(usize),
	UnknownDeviceResource(usize),
	UnknownFormat(usize),
	UnknownImageLayout(usize),
	IntValueRequired(usize),
	UnknownRenderPassAttachmentOptions(usize),
	ImageLayoutRequired(usize),
	DirectionRequired(usize),
	NumericParseError(ParseIntError, usize),
	FloatingParseError(ParseFloatError, usize),
	DelimiterRequired(usize),
	ClosingRequired(usize),
	DefinitionOverrided,
	CorruptedSubpassDesc(usize),
	UnknownPipelineStageFlag(usize),
	UnknownAccessFlag(usize),
	Expected(Cow<'static, str>, usize),
	UnknownConfig(&'static str),
	ConfigRequired(&'static str),
	UnknownExternalResource(usize),
	UnknownClearMode(usize),
	FormatRequired(usize),
	UnknownObjectRef(&'static str, usize),
	UnknownShaderStageFlag(usize),
	UnknownDescriptorKind(usize),
	BytesizeRequired(usize),
	UnknownPrimitiveTopology(bool, usize),
	NameNotAllowed(usize),
	EntryDuplicated(Cow<'static, str>),
	UnknownComponentSwizzle(usize),
	UnknownUsageBits(&'static str, usize)
}
pub struct ParseErrorWithLine(ParseError, usize);
trait WithLine<T, E> { fn with_line(self, line: usize) -> Result<T, E>; }
impl<T> WithLine<T, ParseErrorWithLine> for Result<T, ParseError>
{
	fn with_line(self, line: usize) -> Result<T, ParseErrorWithLine> { self.map_err(|e| ParseErrorWithLine(e, line)) }
}
impl std::fmt::Debug for ParseErrorWithLine
{
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self.0
		{
			&ParseError::NameRequired(p) => write!(fmt, "Name required following $ at line {}, col {}", self.1, p),
			&ParseError::UnknownDeviceResource(p) => write!(fmt, "Unknown Device Resource was found at line {}, col {}", self.1, p),
			&ParseError::UnknownFormat(p) => write!(fmt, "Unknown Image Format was found at line {}, col {}", self.1, p),
			&ParseError::UnknownImageLayout(p) => write!(fmt, "Unknown Image Layout was found at line {}, col {}", self.1, p),
			&ParseError::UnknownRenderPassAttachmentOptions(p) => write!(fmt, "Unknown Options for RenderPass Attachment was found at line {}, col {}", self.1, p),
			&ParseError::ImageLayoutRequired(p) => write!(fmt, "Image Layout required at line {}, col {}", self.1, p),
			&ParseError::DirectionRequired(p) => write!(fmt, "Direction Token(->, <-, To or From) required at line {}, col {}", self.1, p),
			&ParseError::DelimiterRequired(p) => write!(fmt, "Delimiter required at line {}, col {}", self.1, p),
			&ParseError::ClosingRequired(p) => write!(fmt, "Closing required at line {}, col {}", self.1, p),
			&ParseError::BytesizeRequired(p) => write!(fmt, "Bytesize required at line {}, col {}", self.1, p),
			&ParseError::DefinitionOverrided => write!(fmt, "Multiple definitions are found at line {}", self.1),
			&ParseError::CorruptedSubpassDesc(p) => write!(fmt, "Some Error are found parsing SubpassDesc at line {}, col {}", self.1, p),
			&ParseError::IntValueRequired(p) => write!(fmt, "Integer or ConfigRef required at line {}, col {}", self.1, p),
			&ParseError::UnknownPipelineStageFlag(p) => write!(fmt, "Unknown Pipeline Stage Flag was found at line {}, col {}", self.1, p),
			&ParseError::UnknownAccessFlag(p) => write!(fmt, "Unknown Access Mask Flag was found at line {}, col {}", self.1, p),
			&ParseError::UnknownExternalResource(p) => write!(fmt, "Unknown External Resource was found at line {}, col {}", self.1, p),
			&ParseError::UnknownClearMode(p) => write!(fmt, "Unknown Clear Mode was found at line {}, col {}", self.1, p),
			&ParseError::UnknownShaderStageFlag(p) => write!(fmt, "Unknown Shader Stage Flag was found at line {}, col {}", self.1, p),
			&ParseError::UnknownDescriptorKind(p) => write!(fmt, "Unknown Descriptor Kind was found at line {}, col {}", self.1, p),
			&ParseError::UnknownComponentSwizzle(p) => write!(fmt, "Unknown Swizzling Component was found at line {}, col {}", self.1, p),
			&ParseError::UnknownUsageBits(f, p) => write!(fmt, "Unknown Usage Bits for {} was found at line {}, col {}", f, self.1, p),
			&ParseError::FormatRequired(p) => write!(fmt, "Format required for RenderPass Attachment at line {}, col {}", self.1, p),
			&ParseError::UnknownPrimitiveTopology(true, p) => write!(fmt, "Unknown Primitive Topology with Adjacency was found at line {}, col {}", self.1, p),
			&ParseError::UnknownPrimitiveTopology(false, p) => write!(fmt, "Unknown Primitive Topology was found at line {}, col {}", self.1, p),
			&ParseError::UnknownObjectRef(n, p) => write!(fmt, "Unknown {} ref at line {}, col {}", n, self.1, p),
			&ParseError::NumericParseError(ref n, p) => write!(fmt, "NumericParseError: {} at line {}, col {}", n, self.1, p),
			&ParseError::FloatingParseError(ref n, p) => write!(fmt, "FloatingParseError: {} at line {}, col {}", n, self.1, p),
			&ParseError::Expected(ref s, p) => write!(fmt, "Expected {}, but it was not found at line {}, col {}", s, self.1, p),
			&ParseError::UnknownConfig(s) => write!(fmt, "Unknown Config for {} was found at line {}", s, self.1),
			&ParseError::ConfigRequired(c) => write!(fmt, "Configuration \"{}\" required at line {}", c, self.1),
			&ParseError::NameNotAllowed(p) => write!(fmt, "Naming not allowed for the configuration at line {}, col {}", self.1, p),
			&ParseError::EntryDuplicated(ref n) => write!(fmt, "Entry {} is duplicated at line {}", n, self.1)
		}
	}
}

// --- Parsers --- //
/// The structure that can be constructed by source string, returns error when it is found.
pub trait FromSource : Sized
{
	fn object_name() -> Cow<'static, str>;
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>;
}
/// The structure that can be constructed by lines, writes fmt, if there are some errors.
pub trait FromSourceBlock : Sized
{
	fn parse(enterline: &mut ParseLine, lines: &mut LazyLines) -> Result<Self, ParseErrorWithLine>;
}
/// The array that can be constructed with some structures, returns error when it is found.
pub trait FromSourceArray : FromSource
{
	fn array_name() -> Cow<'static, str> { format!("Array of {}", Self::object_name()).into() }
	fn parse_array(source: &mut ParseLine) -> Result<Vec<Self>, ParseError>
	{
		// '[' self,* ']'
		if source.front() != Some('[') { Err(ParseError::Expected(Self::array_name(), source.current())) }
		else
		{
			if source.drop_opt(1).drop_while(ignore_chars).front() == Some(']') { Ok(Vec::new()) }
			else
			{
				let mut sink = Vec::new();
				while try!(FromSource::parse(source).and_then(|abs|
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

/// Parser Extras
pub trait ParserExt: Sized
{
	fn location(&self) -> Location;
	fn consume<CF, E, F, R>(&mut self, consumer: CF, expect_msg: E, cont: F) -> Result<R, ParseError>
		where CF: FnOnce(&mut Self) -> bool, E: FnOnce() -> Cow<'static, str>, F: FnOnce(&mut Self) -> Result<R, ParseError>;
	fn consume_char<E, F, R>(&mut self, c: char, expect_msg: E, cont: F) -> Result<R, ParseError>
		where E: FnOnce() -> Cow<'static, str>, F: FnOnce(&mut Self) -> Result<R, ParseError>;
	fn consume_delimiter<F, R>(&mut self, d: char, cont: F) -> Result<R, ParseError> where F: FnOnce(&mut Self) -> Result<R, ParseError>;
	fn consume_closing<F, R>(&mut self, c: char, cont: F) -> Result<R, ParseError> where F: FnOnce(&mut Self) -> Result<R, ParseError>;
	fn require_content<E>(self, err: E) -> Result<Self, ParseError> where E: FnOnce(Self) -> ParseError;
}
impl<'s> ParserExt for ParseLine<'s>
{
	fn location(&self) -> Location { Location(self.line(), self.current() + 1) }
	fn consume<CF, E, F, R>(&mut self, consumer: CF, expect_msg: E, cont: F) -> Result<R, ParseError>
		where CF: FnOnce(&mut Self) -> bool, E: FnOnce() -> Cow<'static, str>, F: FnOnce(&mut Self) -> Result<R, ParseError>
	{
		if !consumer(self) { Err(ParseError::Expected(expect_msg(), self.current())) } else { cont(self) }
	}
	fn consume_char<E, F, R>(&mut self, c: char, expect_msg: E, cont: F) -> Result<R, ParseError>
		where E: FnOnce() -> Cow<'static, str>, F: FnOnce(&mut Self) -> Result<R, ParseError>
	{
		if self.front() != Some(c) { Err(ParseError::Expected(expect_msg(), self.current())) } else { cont(self.drop_opt(1)) }
	}
	fn consume_delimiter<F, R>(&mut self, d: char, cont: F) -> Result<R, ParseError> where F: FnOnce(&mut Self) -> Result<R, ParseError>
	{
		if self.front() != Some(d) { Err(ParseError::DelimiterRequired(self.current())) } else { cont(self.drop_opt(1)) }
	}
	fn consume_closing<F, R>(&mut self, c: char, cont: F) -> Result<R, ParseError> where F: FnOnce(&mut Self) -> Result<R, ParseError>
	{
		if self.front() != Some(c) { Err(ParseError::ClosingRequired(self.current())) } else { cont(self.drop_opt(1)) }
	}
	fn require_content<E>(self, err: E) -> Result<Self, ParseError> where E: FnOnce(Self) -> ParseError
	{
		if self.is_empty() { Err(err(self)) } else { Ok(self) }
	}
}

/// Combined Parsers
pub trait CombinedParser
{
	/// P
	fn parse<P: FromSource>(&mut self) -> Result<P, ParseError>;
	/// P (location info included)
	fn parse_loc<P: FromSourceLocated>(&mut self) -> LocatedParseResult<P>;
	/// "[" P,* "]"
	fn array<PA: FromSourceArray>(&mut self) -> Result<Vec<PA>, ParseError>;
	/// "[" P,* "]" (location info included in each item)
	fn array_loc<PA: FromSourceArrayLocated>(&mut self) -> Result<Vec<LocationPacked<PA>>, ParseError>;
	/// P "-" P
	fn ranged<P: FromSource>(&mut self) -> Result<Range<P>, ParseError>;
	/// B
	fn block<B: FromSourceBlock>(&mut self, lines: &mut LazyLines) -> Result<B, ParseErrorWithLine>;
}
impl<'s> CombinedParser for ParseLine<'s>
{
	fn parse<P: FromSource>(&mut self) -> Result<P, ParseError> { P::parse(self) }
	fn parse_loc<P: FromSourceLocated>(&mut self) -> LocatedParseResult<P> { P::parse_with_location(self) }
	fn array<PA: FromSourceArray>(&mut self) -> Result<Vec<PA>, ParseError> { PA::parse_array(self) }
	fn array_loc<PA: FromSourceArrayLocated>(&mut self) -> Result<Vec<LocationPacked<PA>>, ParseError> { PA::parse_array_with_location(self) }
	fn ranged<P: FromSource>(&mut self) -> Result<Range<P>, ParseError>
	{
		// P "-" P
		P::parse(self).and_then(|s| match self.drop_while(ignore_chars).front()
		{
			Some('-') => P::parse(self.drop_opt(1).drop_while(ignore_chars)).map(|e| s .. e),
			_ => Err(ParseError::Expected("\"-\"".into(), self.current()))
		})
	}
	fn block<B: FromSourceBlock>(&mut self, lines: &mut LazyLines) -> Result<B, ParseErrorWithLine>
	{
		B::parse(self, lines)
	}
}

type StringLiteral = LocationPacked<String>;
impl FromSource for StringLiteral
{
	fn object_name() -> Cow<'static, str> { "String Literal".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
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
			let startloc = source.current();
			let mut buf = String::new();
			recursive_char(source.drop_opt(1), &mut buf).map(|_| LocationPacked(Location(source.line(), startloc), buf))
		}
		else { Err(ParseError::Expected(Self::object_name(), source.current())) }
	}
}

impl FromSource for PreciseRenderPassRef
{
	fn object_name() -> Cow<'static, str> { "PreciseRenderPassRef".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		/// ConfigInt "." ConfigInt
		let inloc = source.current();
		let rp = source.parse_loc::<ConfigInt>()?;
		if source.drop_while(ignore_chars).front() == Some('.')
		{
			source.drop_opt(1).drop_while(ignore_chars).parse_loc::<ConfigInt>().map(|s| PreciseRenderPassRef { rp: rp, subpass: s })
		}
		else { Err(ParseError::Expected("PreciseRenderPassRef".into(), inloc)) }
	}
}
lazy_static!
{
	static ref SCREENVIEW: Vec<char> = "ScreenView".chars().collect();
}
impl FromSource for ViewportScissorEntry
{
	fn object_name() -> Cow<'static, str> { "ViewportScissorEntry".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// Size3F "-" Size3F [":" Offset2D "-" Offset2D] / "ScreenView"
		if source.starts_with_trailing_opt(&SCREENVIEW, ident_break) { source.drop_opt(SCREENVIEW.len()); Ok(ViewportScissorEntry::ScreenView) }
		else
		{
			let Range { start: Size3F(vsx, vsy, vsz), end: Size3F(vdx, vdy, vdz) } = source.ranged::<Size3F>()?;
			let scissor = if source.drop_while(ignore_chars).front() == Some(':')
			{
				let Range { start: Offset2(sx, sy), end: Offset2(dx, dy) } = source.drop_opt(1).ranged::<Offset2>()?;
				VkRect2D(VkOffset2D(sx as i32, sy as i32), VkExtent2D((dx - sx) as u32, (dy - sy) as u32))
			}
			else { VkRect2D(VkOffset2D(vsx as i32, vsy as i32), VkExtent2D((vdx - vsx) as u32, (vdy - vsy) as u32)) };
			Ok(ViewportScissorEntry::Custom(VkViewport(vsx, vsy, vdx - vsx, vdy - vsy, vsz, vdz), scissor))
		}
	}
}
lazy_static!
{
	static ref DISABLED: Vec<char> = "Disabled".chars().collect();
	static ref ALPHA: Vec<char> = "Alpha".chars().collect();
	static ref PREMULTIPLIEDALPHA: Vec<char> = "PremultipliedAlpha".chars().collect();
}
impl FromSource for AttachmentBlendState
{
	fn object_name() -> Cow<'static, str> { "AttachmentBlendState Constant".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// "Disabled" / "Alpha" / "PremultipliedAlpha"
		let s = source.take_until(ident_break);
		PartialEqualityMatchMap!(s;
		{
			DISABLED[..] => Ok(AttachmentBlendState::Disabled),
			ALPHA[..] => Ok(AttachmentBlendState::AlphaBlend),
			PREMULTIPLIEDALPHA[..] => Ok(AttachmentBlendState::PremultipliedAlphaBlend);
			_ => Err(ParseError::Expected(Self::object_name(), s.current()))
		})
	}
}
impl FromSourceArray for AttachmentBlendState {}
impl FromSource for f32
{
	fn object_name() -> Cow<'static, str> { "f32".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		let inv = source.front() == Some('-');
		if inv { source.drop_opt(1).drop_while(ignore_chars); }
		let x = source.take_while(|c| c.is_digit(10));
		let s = if source.front() == Some('.')
		{
			let fp = source.drop_opt(1).take_while(|c| c.is_digit(10));
			x.chars().iter().chain(&['.']).chain(fp.chars().iter()).cloned().collect()
		}
		else { x.clone_as_string() };
		s.parse::<f32>().map_err(|e| ParseError::FloatingParseError(e, x.current())).map(|v| if inv { -v } else { v })
	}
}
impl FromSource for i32
{
	fn object_name() -> Cow<'static, str> { "i32".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		let inv = source.front() == Some('-');
		if inv { source.drop_opt(1).drop_while(ignore_chars); }
		let x = source.take_while(|c| c.is_digit(10));
		x.clone_as_string().parse::<i32>().map_err(|e| ParseError::NumericParseError(e, x.current())).map(|e| if inv { -e } else { e })
	}
}
impl FromSource for u32
{
	fn object_name() -> Cow<'static, str> { "u32".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		let x = source.take_while(|c| c.is_digit(10)).require_content(|s| ParseError::IntValueRequired(s.current()))?;
		x.clone_as_string().parse::<u32>().map_err(|e| ParseError::NumericParseError(e, x.current()))
	}
}
impl FromSource for usize
{
	fn object_name() -> Cow<'static, str> { "usize".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		let x = source.take_while(|c| c.is_digit(10)).require_content(|s| ParseError::IntValueRequired(s.current()))?;
		x.clone_as_string().parse::<usize>().map_err(|e| ParseError::NumericParseError(e, x.current()))
	}
}
impl FromSource for Size3F
{
	fn object_name() -> Cow<'static, str> { "Size3F".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// "(" f32 "," f32 "," f32 ")"
		let x = source.consume_char('(', Self::object_name, |source| source.drop_while(ignore_chars).parse::<f32>())?;
		let y = source.drop_while(ignore_chars).consume_delimiter(',', |source| source.drop_while(ignore_chars).parse::<f32>())?;
		let z = source.drop_while(ignore_chars).consume_delimiter(',', |source| source.drop_while(ignore_chars).parse::<f32>())?;
		source.drop_while(ignore_chars).consume_closing(')', |_| Ok(Size3F(x, y, z)))
	}
}
impl FromSource for Offset2
{
	fn object_name() -> Cow<'static, str> { "Offset2".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// "(" i32 "," i32 ")"
		let x = source.consume_char('(', Self::object_name, |source| source.drop_while(ignore_chars).parse::<i32>())?;
		let y = source.drop_while(ignore_chars).consume_delimiter(',', |source| source.drop_while(ignore_chars).parse::<i32>())?;
		source.drop_while(ignore_chars).consume_closing(')', |_| Ok(Offset2(x, y)))
	}
}
pub struct VariadicSize(Vec<ConfigInt<usize>>);
impl FromSource for VariadicSize
{
	fn object_name() -> Cow<'static, str> { "VarSize".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// usize / "(" usize,* ")"
		if source.front() == Some('(')
		{
			fn recurse(source: &mut ParseLine, sink: &mut Vec<ConfigInt<usize>>) -> Result<(), ParseError>
			{
				source.parse::<ConfigInt<usize>>().and_then(|v|
				{
					sink.push(v);
					let do_continue = source.drop_while(ignore_chars).front() == Some(',');
					match source.drop_while(|x| ignore_chars(x) || x == ',').front()
					{
						Some(')') => { source.drop_opt(1); Ok(()) },
						Some('0' ... '9') if do_continue => recurse(source, sink),
						Some('$') if do_continue => recurse(source, sink),
						_ => Err(ParseError::ClosingRequired(source.current()))
					}
				})
			}
			let mut sink = Vec::new();
			recurse(source.drop_opt(1).drop_while(ignore_chars), &mut sink).map(|_| VariadicSize(sink))
		}
		else { source.parse::<ConfigInt<usize>>().map(|x| VariadicSize(vec![x])) }
	}
}

pub fn parse_device_resources(sink: &mut ParsedDeviceResources, includes: &mut Vec<LocationPacked<PathBuf>>, lines: &mut LazyLines)
	-> Result<(), ParseErrorWithLine>
{
	while let Some(mut source) = acquire_line(lines, 0)
	{
		let insource = source.current();
		let NamedConfigLine { name, .. } = NamedConfigLine::parse_noargs(&mut source).with_line(source.line())?;
		let name = name.map(From::from);
		let s = source.drop_while(ignore_chars).take_until(ident_break);
		let sline = source.line();
		fn insert_uniq_auto_or<T>(target: &mut NamedContents<T>, name: Option<Cow<str>>, value: T, eline: usize) -> Result<(), ParseErrorWithLine>
		{
			target.insert_auto(name, value).into_parse_result().with_line(eline)
		}
		fn insert_vuniq_auto_or<T: Eq>(target: &mut NamedContents<T>, name: Option<Cow<str>>, value: T, eline: usize) -> Result<(), ParseErrorWithLine>
		{
			target.insert_auto_vunique(name, value).into_parse_result().with_line(eline)
		}
		match s.clone_as_string().as_ref()
		{
			/// Include <StringLiteral>
			"Include" => source.drop_while(ignore_chars).parse::<StringLiteral>().map(PartialApply1!(LocationPacked::apply; From::from))
				.and_then(|p| if name.is_some() { Err(ParseError::NameNotAllowed(insource)) } else { Ok(includes.push(p)) }).with_line(sline),
			"RenderPass" => source.block::<RenderPassData>(lines)
				.and_then(|p| insert_uniq_auto_or(&mut sink.renderpasses, name, p, sline)),
			"SimpleRenderPass" => source.block::<SimpleRenderPassData>(lines)
				.and_then(|p| insert_uniq_auto_or(&mut sink.simple_rps, name, p, sline)),
			"PresentedRenderPass" => source.block::<PresentedRenderPassData>(lines)
				.and_then(|p| insert_uniq_auto_or(&mut sink.presented_rps, name, p, sline)),
			"DescriptorSetLayout" => source.block::<DescriptorSetLayoutData>(lines)
				.and_then(|p| insert_vuniq_auto_or(&mut sink.descriptor_set_layouts, name, p, sline)),
			"PushConstantLayout" => source.block::<PushConstantLayout>(lines)
				.and_then(|p| insert_vuniq_auto_or(&mut sink.push_constant_layouts, name, p, sline)),
			"PipelineLayout" => source.block::<PipelineLayout>(lines)
				.and_then(|p| insert_uniq_auto_or(&mut sink.pipeline_layouts, name, p, sline)),
			"DescriptorSets" => source.block::<DescriptorSetsInfo>(lines)
				.and_then(|p| insert_uniq_auto_or(&mut sink.descriptor_sets, name, p, sline)),
			"PipelineState" => source.drop_while(ignore_chars).block::<PipelineStateInfo>(lines)
				.and_then(|p| insert_uniq_auto_or(&mut sink.pipeline_states, name, p, sline)),
			"Extern" => source.drop_while(ignore_chars).parse::<ExternalResourceData>().with_line(sline)
				.and_then(|p| insert_uniq_auto_or(&mut sink.externs, name, p, sline)),
			"Framebuffer" => source.drop_while(ignore_chars).block::<FramebufferInfo>(lines)
				.and_then(|p| insert_uniq_auto_or(&mut sink.framebuffers, name, p, sline)),
			"Image" => source.drop_while(ignore_chars).block::<ImageDescription>(lines)
				.and_then(|p| insert_uniq_auto_or(&mut sink.images, name, p, sline)),
			"VertexShader" => VertexShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0)
				.and_then(|vsinfo| insert_uniq_auto_or(&mut sink.ind_shaders.vertex, name, vsinfo, sline)),
			"FragmentShader" => PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0)
				.and_then(|f| insert_uniq_auto_or(&mut sink.ind_shaders.fragment, name, IndependentShaderStageInfo::from_pss(f, VK_SHADER_STAGE_FRAGMENT_BIT), sline)),
			"GeometryShader" => PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0)
				.and_then(|g| insert_uniq_auto_or(&mut sink.ind_shaders.geometry, name, IndependentShaderStageInfo::from_pss(g, VK_SHADER_STAGE_GEOMETRY_BIT), sline)),
			"TessellationControlShader" | "TessControlShader" => PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0)
				.map(|tc| IndependentShaderStageInfo::from_pss(tc, VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT))
				.and_then(|tc| insert_uniq_auto_or(&mut sink.ind_shaders.tesscontrol, name, tc, sline)),
			"TessellationEvaluationShader" | "TessEvaluationShader" => PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0)
				.map(|te| IndependentShaderStageInfo::from_pss(te, VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT))
				.and_then(|te| insert_uniq_auto_or(&mut sink.ind_shaders.tessevaluation, name, te, sline)),
			_ => Err(ParseError::UnknownDeviceResource(s.current())).with_line(sline)
		}?
	}
	Ok(())
}
impl FromSource for ImageDimension
{
	fn object_name() -> Cow<'static, str> { "Image Dimension".into() }
	/// "1D" / "2D" / "3D" / "Linear" / "Planar" / "Cubic"
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		if source.starts_with_trailing_opt(&['1', 'D'], ident_break) { source.drop_opt(2); Ok(ImageDimension::Linear) }
		else if source.starts_with_trailing_opt(&['2', 'D'], ident_break) { source.drop_opt(2); Ok(ImageDimension::Planar) }
		else if source.starts_with_trailing_opt(&['3', 'D'], ident_break) { source.drop_opt(2); Ok(ImageDimension::Cubic) }
		else if source.starts_with_trailing_opt(&['L', 'i', 'n', 'e', 'a', 'r'], ident_break) { source.drop_opt(6); Ok(ImageDimension::Linear) }
		else if source.starts_with_trailing_opt(&['P', 'l', 'a', 'n', 'a', 'r'], ident_break) { source.drop_opt(6); Ok(ImageDimension::Planar) }
		else if source.starts_with_trailing_opt(&['C', 'u', 'b', 'i', 'c'], ident_break) { source.drop_opt(5); Ok(ImageDimension::Cubic) }
		else { Err(ParseError::Expected(Self::object_name(), source.current())) }
	}
}
impl FromSource for ExternalResourceData
{
	fn object_name() -> Cow<'static, str> { "ExternalResourceData".into() }
	/// Extern | "ImageView" ImageDimension StringLiteral / "SwapChainViews"
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		let s = source.take_until(ident_break);
		match s.chars()
		{
			&['I', 'm', 'a', 'g', 'e', 'V', 'i', 'e', 'w'] => source.drop_while(ignore_chars).parse::<ImageDimension>()
				.and_then(|d| source.drop_while(ignore_chars).parse::<StringLiteral>().map(|n| ExternalResourceData::ImageView { dim: d, refname: n })),
			&['S', 'w', 'a', 'p', 'C', 'h', 'a', 'i', 'n', 'V', 'i', 'e', 'w', 's'] => Ok(ExternalResourceData::SwapChainViews),
			_ => Err(ParseError::UnknownExternalResource(s.current()))
		}
	}
}
impl FromSource for ComponentMapping
{
	fn object_name() -> Cow<'static, str> { "Component Mapping".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// (R / G / B / A){4}
		fn component_swizzle(c: char, col: usize) -> Result<ComponentSwizzle, ParseError>
		{
			match c
			{
				'r' | 'R' => Ok(ComponentSwizzle::R),
				'g' | 'G' => Ok(ComponentSwizzle::G),
				'b' | 'B' => Ok(ComponentSwizzle::B),
				'a' | 'A' => Ok(ComponentSwizzle::A),
				_ => Err(ParseError::UnknownComponentSwizzle(col))
			}
		}

		let r = source.front().ok_or(ParseError::Expected(Self::object_name(), source.current())).and_then(|c| component_swizzle(c, source.current()))?;
		let g = source.peek(1).ok_or(ParseError::Expected(Self::object_name(), source.current())).and_then(|c| component_swizzle(c, source.current()))?;
		let b = source.peek(2).ok_or(ParseError::Expected(Self::object_name(), source.current())).and_then(|c| component_swizzle(c, source.current()))?;
		let a = source.peek(3).ok_or(ParseError::Expected(Self::object_name(), source.current())).and_then(|c| component_swizzle(c, source.current()))?;
		source.drop_opt(4); Ok(ComponentMapping(r, g, b, a))
	}
}
impl FromSourceBlock for ImageDescription
{
	/// "Image" | "<" Format ["(" ComponentMap ")"] ">" VariadicSize
	fn parse(enterline: &mut ParseLine, lines: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		let format = enterline.consume_char('<', || "\"<\"".into(), |s| s.drop_while(ignore_chars).parse::<Format>()).with_line(enterline.line())?;
		let component_map = if enterline.drop_while(ignore_chars).front() == Some('(')
		{
			enterline.drop_opt(1).drop_while(ignore_chars).parse::<ComponentMapping>()
				.and_then(|cm| enterline.drop_while(ignore_chars).consume_closing(')', |_| Ok(cm)))
		}
		else { Ok(ComponentMapping::straight()) }.with_line(enterline.line())?;
		let extent = enterline.drop_while(ignore_chars).consume_closing('>', |s| s.drop_while(ignore_chars).parse::<VariadicSize>()).with_line(enterline.line())?;

		let mut obj = ImageDescription { extent: extent.0, format, mapping: component_map, device_local: false, usage: 0 };
		while let Some(mut s) = acquire_line(lines, 1)
		{
			acquire_config_name(&mut s).and_then(|name| match name.chars()
			{
				&['U', 's', 'a', 'g', 'e'] =>
				{
					fn recurse(source: &mut ParseLine, sink: &mut ImageDescription) -> Result<(), ParseError>
					{
						if sampled_token(source) { sink.usage |= VK_IMAGE_USAGE_SAMPLED_BIT; Ok(()) }
						else if color_attachment_token(source) { sink.usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT; Ok(()) }
						else if input_attachment_token(source) { sink.usage |= VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT; Ok(()) }
						else if device_local_token(source) { sink.device_local = true; Ok(()) }
						else { Err(ParseError::UnknownUsageBits("Image", source.current())) }?;

						if source.drop_while(ignore_chars).front() == Some('/') { recurse(source.drop_opt(1).drop_while(ignore_chars), sink) }
						else { Ok(()) }
					}
					recurse(s.drop_while(ignore_chars), &mut obj)
				},
				_ => Err(ParseError::UnknownConfig("Image Description".into()))
			}).with_line(s.line())?;
		}
		Ok(obj)
	}
}

impl FromSourceBlock for RenderPassData
{
	fn parse(_: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		let mut rpd = RenderPassData { attachments: NamedContents::new(), subpasses: NamedContents::new(), deps: Vec::new() };
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).with_line(s.line()).and_then(|name| match name.chars()
			{
				&['A', 't', 't', 'a', 'c', 'h', 'm', 'e', 'n', 't', 's'] => rpd.attachments.sink_check_overriding_online(s.line(), |sink|
				{
					while let Some(mut s) = acquire_line(source, 2)
					{
						let l = NamedConfigLine::parse(&mut s, RPAttachment::parse).with_line(s.line())?;
						sink.insert_auto(l.name.map(From::from), l.config).into_parse_result().with_line(s.line())?;
					}
					Ok(())
				}),
				&['S', 'u', 'b', 'p', 'a', 's', 's', 'e', 's'] => rpd.subpasses.sink_check_overriding_online(s.line(), |sink|
				{
					while let Some(mut s) = acquire_line(source, 2)
					{
						let l = NamedConfigLine::parse(&mut s, RPSubpassDesc::parse).with_line(s.line())?;
						sink.insert_auto(l.name.map(From::from), l.config).into_parse_result().with_line(s.line())?;
					}
					Ok(())
				}),
				&['D', 'e', 'p', 'e', 'n', 'd', 'e', 'n', 'c', 'i', 'e', 's'] => rpd.deps.sink_check_overriding_online(s.line(), |sink|
				{
					while let Some(mut s) = acquire_line(source, 2)
					{
						let dep = s.parse::<RPSubpassDeps>().with_line(s.line())?;
						sink.push(dep);
					}
					Ok(())
				}),
				_ => Err(ParseError::UnknownConfig("RenderPass")).with_line(s.line())
			})?
		}
		Ok(rpd)
	}
}
impl FromSource for RPAttachment
{
	fn object_name() -> Cow<'static, str> { "RenderPass Attachment".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// pixel_format "," transition_opt#image_layout "," option,*
		let format = source.parse_loc::<Format>()?;
		let layouts = source.drop_while(ignore_chars).consume_delimiter(',', |source|
			Transition::parse_opt(source.drop_while(ignore_chars), |x| parse_image_layout(x).map(LocationPacked::unwrap)))?;
		let mut rpa = RPAttachment { format: format, layouts: layouts, clear_on_load: None, preserve_content: false };
		if source.drop_while(ignore_chars).front() != Some(',') { Ok(rpa) } else
		{
			// Parse slice delimitered attachment options
			fn recurse(rpa: &mut RPAttachment, source: &mut ParseLine) -> Result<(), ParseError>
			{
				if source.is_empty() { Ok(()) }
				else
				{
					if clear_on_load_token(source) { rpa.clear_on_load = Some(true); Ok(()) }
					else if load_content_token(source) { rpa.clear_on_load = Some(false); Ok(()) }
					else if preserve_content_token(source) { rpa.preserve_content = true; Ok(()) }
					else { Err(ParseError::UnknownRenderPassAttachmentOptions(source.current())) }?;

					if source.drop_while(ignore_chars).front() == Some('/') { recurse(rpa, source.drop_opt(1).drop_while(ignore_chars)) }
					else { Ok(()) }
				}
			}
			recurse(&mut rpa, source.drop_opt(1).drop_while(ignore_chars)).map(|_| rpa)
		}
	}
}
impl FromSource for RPSubpassDesc
{
	fn object_name() -> Cow<'static, str> { "RenderPass Subpass".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// ("RenderTo" (int/ints) / From (int/ints))*
		fn recurse<'s>(input: &mut ParseLine<'s>, sink: &mut RPSubpassDesc) -> Result<(), ParseError>
		{
			if input.is_empty() { Ok(()) }
			else
			{
				if render_to_token(input) { sink.color_outs.assign_check_overriding(|| input.drop_while(ignore_chars).array_loc::<ConfigInt>()) }
				else if from_token(input) { sink.inputs.assign_check_overriding(|| input.drop_while(ignore_chars).array_loc::<ConfigInt>()) }
				else { Err(ParseError::CorruptedSubpassDesc(input.current())) }?;
				recurse(input.drop_while(ignore_chars), sink)
			}
		}
		let mut rpsd = RPSubpassDesc { color_outs: Vec::new(), inputs: Vec::new() };
		recurse(source, &mut rpsd).map(|_| rpsd)
	}
}
lazy_static! { static ref BY_REGION: Vec<char> = "ByRegion".chars().collect(); }
impl FromSource for RPSubpassDeps
{
	fn object_name() -> Cow<'static, str> { "RenderPass Subpass Dependencies".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// int (From/To) int ":" transition#access_mask At stage_bits ["," ["ByRegion"]]
		let pass_trans = Transition::parse(source, ConfigInt::parse_with_location)?;
		let access_mask_trans = source.drop_while(ignore_chars).consume_delimiter(':',
			|source| Transition::parse(source.drop_while(ignore_chars), AccessFlags::parse_with_location))?;
		let stage_bits = source.drop_while(ignore_chars).consume(at_token, || "\"At\" or \"@\"".into(), |s| parse_pipeline_stage_bits(s.drop_while(ignore_chars)))?;
		let by_region = if source.drop_while(ignore_chars).front() == Some(',')
		{
			source.drop_opt(1).drop_while(ignore_chars).starts_with_trailing_opt(&BY_REGION, ident_break)
		} else { false };

		Ok(RPSubpassDeps { passtrans: pass_trans, access_mask: access_mask_trans, stage_bits, by_region: by_region })
	}
}
fn parse_rp_clear_mode(source: &ParseLine) -> Result<Option<bool>, ParseError>
{
	match source.chars()
	{
		&['N', 'o', 'n', 'e'] => Ok(None),
		&['O', 'n', 'L', 'o', 'a', 'd'] => Ok(Some(true)),
		&['P', 'r', 'e', 's', 'e', 'r', 'v', 'e'] => Ok(Some(false)),
		_ => Err(ParseError::UnknownClearMode(source.current()))
	}
}

impl FromSourceBlock for SimpleRenderPassData
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Result<SimpleRenderPassData, ParseErrorWithLine>
	{
		let (mut fmt, mut clear_mode) = (None, None);
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).and_then(|name| match name.chars()
			{
				&['F', 'o', 'r', 'm', 'a', 't'] => fmt.assign_check_overriding(|| s.drop_while(ignore_chars).parse_loc::<Format>()),
				&['C', 'l', 'e', 'a', 'r', 'M', 'o', 'd', 'e'] => parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; }),
				_ => Err(ParseError::UnknownConfig("SimpleRenderPass"))
			}).with_line(s.line())?;
		}
		fmt.ok_or(ParseError::ConfigRequired("Format")).with_line(enterline.line())
			.map(|fmt| SimpleRenderPassData { format: fmt, clear_on_load: clear_mode })
	}
}
impl FromSourceBlock for PresentedRenderPassData
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		let (mut fmt, mut clear_mode) = (None, None);
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).and_then(|name| match name.chars()
			{
				&['F', 'o', 'r', 'm', 'a', 't'] => fmt.assign_check_overriding(|| s.drop_while(ignore_chars).parse_loc::<Format>()),
				&['C', 'l', 'e', 'a', 'r', 'M', 'o', 'd', 'e'] => parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; }),
				_ => Err(ParseError::UnknownConfig("PresentedRenderPass"))
			}).with_line(s.line())?;
		}
		fmt.ok_or(ParseError::ConfigRequired("Format")).with_line(enterline.line())
			.map(|fmt| PresentedRenderPassData { format: fmt, clear_on_load: clear_mode })
	}
}
lazy_static! { static ref PRESENTED: Vec<char> = "Presented".chars().collect(); }
impl FromSource for FramebufferRenderPassRef
{
	fn object_name() -> Cow<'static, str> { "<FRenderPassRef>".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// "<" ("Presented" / int) ">" / <EMPTY>
		if source.front() == Some('<')
		{
			// Which Parameter: "Presented" / int
			match source.drop_opt(1).drop_while(ignore_chars).front()
			{
				/// int
				Some('0' ... '9') | Some('$') => source.parse_loc::<ConfigInt>().map(FramebufferRenderPassRef::Int),
				/// Presented
				Some('P') if source.starts_with_trailing_opt(&PRESENTED, ident_break) =>
				{
					source.drop_opt(PRESENTED.len()); Ok(FramebufferRenderPassRef::Presented)
				},
				_ => Err(ParseError::UnknownObjectRef("RenderPass", source.current()))
			}.and_then(|p| source.consume_closing('>', |_| Ok(p)))
		}
		else { Ok(FramebufferRenderPassRef::None) }
	}
}
impl FromSourceBlock for FramebufferInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		let arg = enterline.drop_while(ignore_chars).parse::<FramebufferRenderPassRef>().with_line(enterline.line())?;
		let vs = enterline.drop_while(ignore_chars).array_loc::<ConfigInt>().with_line(enterline.line())?;
		
		let mut clear_mode = None;
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(s.drop_while(ignore_chars)).and_then(|name|
				if name.chars() == ['C', 'l', 'e', 'a', 'r', 'M', 'o', 'd', 'e'] { parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; }) }
				else { Err(ParseError::UnknownConfig("Framebuffer")) }
			).with_line(s.line())?;
		}
		let style = match arg
		{
			FramebufferRenderPassRef::Int(rp) => FramebufferStyle::WithRenderPass(rp),
			FramebufferRenderPassRef::Presented => FramebufferStyle::Presented(clear_mode),
			FramebufferRenderPassRef::None => FramebufferStyle::Simple(clear_mode)
		};
		Ok(FramebufferInfo { style: style, views: vs })
	}
}
impl FromSourceBlock for DescriptorSetLayoutData
{
	fn parse(_: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		let mut entries = Vec::new();
		while let Some(mut s) = acquire_line(source, 1)
		{
			entries.push(s.parse::<DescriptorEntry>().with_line(s.line())?);
		}
		Ok(DescriptorSetLayoutData { entries: entries })
	}
}
impl FromSource for DescriptorEntryKind
{
	fn object_name() -> Cow<'static, str> { "Descriptor Entry Kind".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		let kstr = source.take_until(ident_break);
		match kstr.clone_as_string().as_ref()
		{
			"Sampler" => Ok(DescriptorEntryKind::Sampler),
			"CombinedSampler" => Ok(DescriptorEntryKind::CombinedSampler),
			"SampledImage" => Ok(DescriptorEntryKind::SampledImage),
			"StorageImage" => Ok(DescriptorEntryKind::StorageImage),
			"UniformTexelBuffer" => Ok(DescriptorEntryKind::UniformBuffer(BufferDescriptorOption::TexelStore)),
			"StorageTexelBuffer" => Ok(DescriptorEntryKind::StorageBuffer(BufferDescriptorOption::TexelStore)),
			"UniformBuffer" => Ok(DescriptorEntryKind::UniformBuffer(BufferDescriptorOption::None)),
			"StorageBuffer" => Ok(DescriptorEntryKind::StorageBuffer(BufferDescriptorOption::None)),
			"UniformBufferDynamic" => Ok(DescriptorEntryKind::UniformBuffer(BufferDescriptorOption::DynamicOffset)),
			"StorageBufferDynamic" => Ok(DescriptorEntryKind::StorageBuffer(BufferDescriptorOption::DynamicOffset)),
			"InputAttachment" => Ok(DescriptorEntryKind::InputAttachment),
			_ => Err(ParseError::UnknownDescriptorKind(kstr.current()))
		}
	}
}
impl FromSource for DescriptorEntry
{
	fn object_name() -> Cow<'static, str> { "DescriptorSet Entry".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		let count = match source.front()
		{
			/// count type : visibility
			Some('0' ... '9') =>
			{
				let count_str = source.take_while(|c| '0' <= c && c <= '9'); assert!(!count_str.is_empty());
				count_str.clone_as_string().parse::<usize>().map_err(|e| ParseError::NumericParseError(e, count_str.current()))
			},
			_ => Ok(1)
		}?;

		let typename = source.drop_while(ignore_chars).parse::<DescriptorEntryKind>()?;
		let visibility = source.drop_while(ignore_chars).consume_delimiter(':', |source| source.drop_while(ignore_chars).parse::<ShaderStageFlags>())?;
		Ok(DescriptorEntry { count: count, kind: typename, visibility: visibility })
	}
}
impl FromSourceBlock for PushConstantLayout
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		let (mut range, mut vis) = (None, None);
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).and_then(|name| match name.chars()
			{
				&['R', 'a', 'n', 'g', 'e'] => range.assign_check_overriding(|| s.drop_while(ignore_chars).ranged::<usize>()),
				&['V', 'i', 's', 'i', 'b', 'i', 'l', 'i', 't', 'y'] => vis.assign_check_overriding(|| s.drop_while(ignore_chars).parse::<ShaderStageFlags>()),
				_ => Err(ParseError::UnknownConfig("PushConstantLayout"))
			}).with_line(s.line())?;
		}
		let range = range.ok_or(ParseError::ConfigRequired("Range")).with_line(enterline.line())?;
		let visibility = vis.ok_or(ParseError::ConfigRequired("Visibility")).with_line(enterline.line())?;
		Ok(PushConstantLayout { range: range, visibility: visibility })
	}
}
impl FromSourceBlock for PipelineLayout
{
	fn parse(_: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		let (mut desc, mut pcls) = (Vec::new(), Vec::new());
		while let Some(mut s) = acquire_line(source, 1)
		{
			/// - | "Descriptors" ints... / "PushConstantLayouts" ints...
			acquire_config_name(&mut s).and_then(|name| match name.chars()
			{
				&['D', 'e', 's', 'c', 'r', 'i', 'p', 't', 'o', 'r', 's'] =>
					desc.assign_check_overriding(|| s.drop_while(ignore_chars).array_loc::<ConfigInt>()),
				&['P', 'u', 's', 'h', 'C', 'o', 'n', 's', 't', 'a', 'n', 't', 'L', 'a', 'y', 'o', 'u', 't', 's'] =>
					pcls.assign_check_overriding(|| s.drop_while(ignore_chars).array_loc::<ConfigInt>()),
				_ => Err(ParseError::UnknownConfig("PipelineLayout"))
			}).with_line(s.line())?;
		}
		Ok(PipelineLayout { descs: desc, pushconstants: pcls })
	}
}
impl FromSourceBlock for DescriptorSetsInfo
{
	fn parse(_: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		let mut entries = Vec::new();
		while let Some(mut s) = acquire_line(source, 1)
		{
			/// - [$Name :] $Name
			let l = NamedConfigLine::parse(&mut s, ConfigInt::parse_with_location)
				.map(|l| DescriptorSetEntry { name: l.name, layout: l.config }).with_line(s.line())?;
			entries.push(l);
		}
		Ok(DescriptorSetsInfo(entries))
	}
}
impl FromSourceBlock for PipelineStateInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		/// PipelineState | "for" precise_rpref "with" int
		let r = enterline.consume(for_token, || "\"for\"".into(), |s| s.drop_while(ignore_chars).parse::<PreciseRenderPassRef>()).with_line(enterline.line())?;
		let l = enterline.consume(with_token, || "\"with\"".into(), |s| s.drop_while(ignore_chars).parse_loc::<ConfigInt>()).with_line(enterline.line())?;

		struct ShaderInfos
		{
			fragment: Option<PipelineShaderStageInfo>, geometry: Option<PipelineShaderStageInfo>,
			tesscontrol: Option<PipelineShaderStageInfo>, tessevaluation: Option<PipelineShaderStageInfo>
		}
		let mut vsinfo = None;
		let mut shaderinfo = ShaderInfos { fragment: None, geometry: None, tesscontrol: None, tessevaluation: None };
		let (mut primt, mut vpsc, mut blends) = (None, Vec::new(), Vec::new());
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).with_line(s.line()).and_then(|name| match name.clone_as_string().as_ref()
			{
				"VertexShader" =>
					vsinfo.assign_check_overriding_online(s.line(), || s.drop_while(ignore_chars).block::<VertexShaderStageInfo>(source)),
				"FragmentShader" =>
					shaderinfo.fragment.assign_check_overriding_online(s.line(), || s.drop_while(ignore_chars).block::<PipelineShaderStageInfo>(source)),
				"GeometryShader" =>
					shaderinfo.geometry.assign_check_overriding_online(s.line(), || s.drop_while(ignore_chars).block::<PipelineShaderStageInfo>(source)),
				"TessellationControlShader" =>
					shaderinfo.tesscontrol.assign_check_overriding_online(s.line(), || s.drop_while(ignore_chars).block::<PipelineShaderStageInfo>(source)),
				"TessellationEvaluationShader" =>
					shaderinfo.tessevaluation.assign_check_overriding_online(s.line(), || s.drop_while(ignore_chars).block::<PipelineShaderStageInfo>(source)),
				"PrimitiveTopology" => primt.assign_check_overriding(|| s.drop_while(ignore_chars).parse::<VkPrimitiveTopology>()).with_line(s.line()),
				"ViewportScissors" => vpsc.assign_check_overriding_online(s.line(), || parse_viewport_scissors(s.drop_while(ignore_chars), source)),
				"BlendStates" => blends.assign_check_overriding(|| s.drop_while(ignore_chars).array::<AttachmentBlendState>()).with_line(s.line()),
				_ => Err(ParseErrorWithLine(ParseError::UnknownConfig("PipelineState"), s.line()))
			})?;
		}
		let vsinfo = vsinfo.ok_or(ParseError::ConfigRequired("VertexShader")).with_line(enterline.line())?;
		let primt = primt.ok_or(ParseError::ConfigRequired("PrimitiveTopology")).with_line(enterline.line())?;
		Ok(PipelineStateInfo
		{
			renderpass: r, layout_ref: l,
			vertex_shader: vsinfo, fragment_shader: shaderinfo.fragment, geometry_shader: shaderinfo.geometry,
			tesscontrol_shader: shaderinfo.tesscontrol, tessevaluation_shader: shaderinfo.tessevaluation,
			primitive_topology: primt, viewport_scissors: vpsc, blendstates: blends
		})
	}
}
impl FromSourceBlock for PipelineShaderStageInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		Self::parse_baseindent(enterline, source, 1)
	}
}
impl FromSourceBlock for VertexShaderStageInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Result<Self, ParseErrorWithLine>
	{
		Self::parse_baseindent(enterline, source, 1)
	}
}
lazy_static!
{
	static ref PERVERTEX: Vec<char> = "PerVertex".chars().collect();
	static ref PERINSTANCE: Vec<char> = "PerInstance".chars().collect();
}
impl FromSource for StreamBindingDesc
{
	fn object_name() -> Cow<'static, str> { "Stream Binding Descriptor".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		/// "PerVertex" usize / "PerInstance" usize / "v" usize / "i" usize
		let pi = source.front() == Some('i') || source.starts_with_trailing_opt(&PERINSTANCE, ident_break);
		if !pi && source.front() != Some('v') && !source.starts_with_trailing_opt(&PERVERTEX, ident_break)
		{
			Err(ParseError::Expected(Self::object_name(), source.current()))
		}
		else
		{
			source.drop_until(ident_break).drop_while(ignore_chars).parse::<usize>()
				.map(if pi { StreamBindingDesc::PerInstance } else { StreamBindingDesc::PerVertex })
		}
	}
}
impl FromSourceArray for StreamBindingDesc {}
impl VertexShaderStageInfo
{
	fn parse_baseindent(enterline: &mut ParseLine, source: &mut LazyLines, baseindent: usize) -> Result<Self, ParseErrorWithLine>
	{
		/// VertexShader | AssetResource
		let asset = enterline.parse_loc::<AssetResource>().with_line(enterline.line())?;

		let (mut consts, mut bindings, mut inputs) = (BTreeMap::new(), Vec::new(), BTreeMap::new());
		while let Some(mut s) = acquire_line(source, baseindent + 1)
		{
			if constant_token(&mut s)
			{
				// - "Constant" usize ":" value
				let index = s.drop_while(ignore_chars).parse::<usize>().with_line(s.line())?;
				s.drop_while(ignore_chars).consume_char(':', || "\":\"".into(), |s| NumericLiteral::parse(s.drop_while(ignore_chars), true))
					.map(|v| { consts.insert(index, v); })
			}
			else if stream_bindings_token(&mut s)
			{
				// - "StreamBindings" ":" (StreamBindingDesc...)
				bindings.assign_check_overriding(|| s.drop_while(ignore_chars).consume_delimiter(':', |s| s.drop_while(ignore_chars).array::<StreamBindingDesc>()))
			}
			else if input_token(&mut s)
			{
				// - "Input" usize "." usize To usize ":" Format
				let binding = s.drop_while(ignore_chars).parse::<usize>().with_line(s.line())?;
				let offset = s.drop_while(ignore_chars).consume_char('.', || "\".\"".into(), |s| s.drop_while(ignore_chars).parse::<usize>()).with_line(s.line())?;
				let index = s.drop_while(ignore_chars).consume(to_token, || "\"To\" or \"->\"".into(), |s| s.drop_while(ignore_chars).parse::<usize>())
					.with_line(s.line())?;
				s.drop_while(ignore_chars).consume_char(':', || "\":\"".into(), |s| s.drop_while(ignore_chars).parse::<Format>())
					.map(|format| { inputs.insert(index, VertexInputInfo { binding, offset, format }); })
			}
			else { Err(ParseError::UnknownConfig("Vertex Shader Module")) }.with_line(s.line())?;
		}
		Ok(VertexShaderStageInfo { asset: asset, consts: consts, stream_bindings: bindings, inputs: inputs })
	}
}
impl PipelineShaderStageInfo
{
	fn parse_baseindent(enterline: &mut ParseLine, source: &mut LazyLines, baseindent: usize) -> Result<Self, ParseErrorWithLine>
	{
		let asset = enterline.parse_loc::<AssetResource>().with_line(enterline.line())?;

		let mut consts = BTreeMap::new();
		while let Some(mut s) = acquire_line(source, baseindent + 1)
		{
			/// - "Constant" usize : value
			if constant_token(&mut s)
			{
				let index = s.drop_while(ignore_chars).parse::<usize>().with_line(s.line())?;
				s.drop_while(ignore_chars).consume_char(':', || "\":\"".into(), |s| NumericLiteral::parse(s.drop_while(ignore_chars), true))
					.map(|v| { consts.insert(index, v); })
			}
			else { Err(ParseError::UnknownConfig("Shader Module")) }.with_line(s.line())?;
		}
		Ok(PipelineShaderStageInfo { asset: asset, consts: consts })
	}
}
impl FromSource for VkPrimitiveTopology
{
	fn object_name() -> Cow<'static, str> { "Primitive Topology".into() }
	fn parse(source: &mut ParseLine) -> Result<VkPrimitiveTopology, ParseError>
	{
		// ident ["with" "Adjacency"]
		let prim = source.take_until(ident_break).require_content(|s| ParseError::Expected(Self::object_name(), s.current()))?;
		if with_token(source.drop_while(ignore_chars))
		{
			// with adjacency
			source.drop_while(ignore_chars).consume(adjacency_token, || "\"Adjacency\"".into(), |_| match prim.clone_as_string().as_ref()
			{
				"LineList" => Ok(VkPrimitiveTopology::LineListWithAdjacency),
				"LineStrip" => Ok(VkPrimitiveTopology::LineStripWithAdjacency),
				"TriangleList" => Ok(VkPrimitiveTopology::TriangleListWithAdjacency),
				"TriangleStrip" => Ok(VkPrimitiveTopology::TriangleStripWithAdjacency),
				_ => Err(ParseError::UnknownPrimitiveTopology(true, prim.current()))
			})
		}
		else
		{
			match prim.clone_as_string().as_ref()
			{
				"PointList" => Ok(VkPrimitiveTopology::PointList),
				"LineList" => Ok(VkPrimitiveTopology::LineList),
				"LineStrip" => Ok(VkPrimitiveTopology::LineStrip),
				"TriangleList" => Ok(VkPrimitiveTopology::TriangleList),
				"TriangleStrip" => Ok(VkPrimitiveTopology::TriangleStrip),
				"TriangleFan" => Ok(VkPrimitiveTopology::TriangleFan),
				"PatchList" => Ok(VkPrimitiveTopology::PatchList),
				_ => Err(ParseError::UnknownPrimitiveTopology(false, prim.current()))
			}
		}
	}
}
fn parse_viewport_scissors(current: &mut ParseLine, source: &mut LazyLines) -> Result<Vec<ViewportScissorEntry>, ParseErrorWithLine>
{
	let mut v = Vec::new();
	if current.front() == None
	{
		// Descending
		while let Some(mut s) = acquire_line(source, 2)
		{
			v.push(ViewportScissorEntry::parse(&mut s).with_line(s.line())?);
		}
		Ok(v)
	}
	else if current.front() == Some('[')
	{
		// Array Parsing
		fn recursive(current: &mut ParseLine, sink: &mut Vec<ViewportScissorEntry>) -> Result<(), ParseError>
		{
			ViewportScissorEntry::parse(current).and_then(|ent|
			{
				sink.push(ent);
				if current.drop_while(ignore_chars).front() == Some(',')
				{
					recursive(current.drop_opt(1).drop_while(ignore_chars), sink)
				}
				else { Ok(()) }
			})
		}
		match recursive(current.drop_opt(1).drop_while(ignore_chars), &mut v)
		{
			Ok(()) => if current.front() == Some(']') { Ok(v) } else { Err(ParseError::ClosingRequired(current.current())) },
			Err(e) => Err(e)
		}.with_line(current.line())
	}
	else { Err(ParseErrorWithLine(ParseError::Expected("Array or Children, of ViewportScissor".into(), current.current()), current.line())) }
}

/// Short Tokens
pub fn to_token(input: &mut ParseLine) -> bool
{
	// "To" / "->"
	if input.starts_with_trailing_opt(&['T', 'o'], ident_break) || input.starts_with(&['-', '>']) { input.drop_opt(2); true }
	else { false }
}
pub fn from_token(input: &mut ParseLine) -> bool
{
	// "From" / "<-"
	if input.starts_with_trailing_opt(&['F', 'r', 'o', 'm'], ident_break) { input.drop_opt(4); true }
	else if input.starts_with(&['<', '-']) { input.drop_opt(2); true }
	else { false }
}
pub fn at_token(input: &mut ParseLine) -> bool
{
	// "At" / "@"
	if input.starts_with_trailing_opt(&['A', 't'], ident_break) { input.drop_opt(2); true }
	else if input.front() == Some('@') { input.drop_opt(1); true }
	else { false }
}
macro_rules! Token
{
	($name: ident ($chars: expr)) =>
	{
		pub fn $name(input: &mut ParseLine) -> bool
		{
			if input.starts_with_trailing_opt(&$chars, ident_break) { input.drop_opt($chars.len()); true }
			else { false }
		}
	}
}
Token!(for_token				(['f', 'o', 'r']));
Token!(with_token				(['w', 'i', 't', 'h']));
Token!(adjacency_token			(['A', 'd', 'j', 'a', 'c', 'e', 'n', 'c', 'y']));
Token!(constant_token			(['C', 'o', 'n', 's', 't', 'a', 'n', 't']));
Token!(stream_bindings_token	(['S', 't', 'r', 'e', 'a', 'm', 'B', 'i', 'n', 'd', 'i', 'n', 'g', 's']));
Token!(input_token				(['I', 'n', 'p', 'u', 't']));
Token!(clear_on_load_token		(['C', 'l', 'e', 'a', 'r', 'O', 'n', 'L', 'o', 'a', 'd']));
Token!(load_content_token		(['L', 'o', 'a', 'd', 'C', 'o', 'n', 't', 'e', 'n', 't']));
Token!(preserve_content_token	(['P', 'r', 'e', 's', 'e', 'r', 'v', 'e', 'C', 'o', 'n', 't', 'e', 'n', 't']));
Token!(render_to_token			(['R', 'e', 'n', 'd', 'e', 'r', 'T', 'o']));
Token!(sampled_token			(['S', 'a', 'm', 'p', 'l', 'e', 'd']));
Token!(color_attachment_token	(['C', 'o', 'l', 'o', 'r', 'A', 't', 't', 'a', 'c', 'h', 'm', 'e', 'n', 't']));
Token!(input_attachment_token	(['I', 'n', 'p', 'u', 't', 'A', 't', 't', 'a', 'c', 'h', 'm', 'e', 'n', 't']));
Token!(device_local_token		(['D', 'e', 'v', 'i', 'c', 'e', 'L', 'o', 'c', 'a', 'l']));

// --- Parser Utils --- //
pub fn ignore_chars(c: char) -> bool { c == ' ' || c == '\t' }
pub fn ident_break(c: char) -> bool
{
	c == ':' || c == '-' || c == '[' || c == ']' || c == ',' || c == '<' || c == '>' || c == '/' || c == '.' || 
	c == '(' || c == ')' || ignore_chars(c)
}
pub trait SetOnce<T = Self>: Sized
{
	fn can_set(&self) -> bool;
	fn set(&mut self, value: T);

	fn assign_check_overriding<F>(&mut self, value: F) -> Result<(), ParseError> where F: FnOnce() -> Result<T, ParseError>
	{
		if self.can_set() { value().map(|v| self.set(v)) } else { Err(ParseError::DefinitionOverrided) }
	}
	fn assign_check_overriding_online<F>(&mut self, line: usize, value: F) -> Result<(), ParseErrorWithLine> where F: FnOnce() -> Result<T, ParseErrorWithLine>
	{
		if self.can_set() { value().map(|v| self.set(v)) } else { Err(ParseErrorWithLine(ParseError::DefinitionOverrided, line)) }
	}
}
pub trait SinkOnce: SetOnce<Self>
{
	fn sink_check_overriding<F>(&mut self, processor: F) -> Result<(), ParseError> where F: FnOnce(&mut Self) -> Result<(), ParseError>
	{
		if self.can_set() { processor(self) } else { Err(ParseError::DefinitionOverrided) }
	}
	fn sink_check_overriding_online<F>(&mut self, line: usize, processor: F) -> Result<(), ParseErrorWithLine>
		where F: FnOnce(&mut Self) -> Result<(), ParseErrorWithLine>
	{
		if self.can_set() { processor(self) } else { Err(ParseErrorWithLine(ParseError::DefinitionOverrided, line)) }
	}
}
impl<T> SetOnce<T> for Option<T>
{
	fn can_set(&self) -> bool { self.is_none() }
	fn set(&mut self, value: T) { *self = Some(value); }
}
impl<T> SetOnce for Vec<T>
{
	fn can_set(&self) -> bool { self.is_empty() }
	fn set(&mut self, value: Vec<T>) { *self = value; }
}
impl<T> SetOnce for NamedContents<T>
{
	fn can_set(&self) -> bool { self.is_empty() }
	fn set(&mut self, value: NamedContents<T>) { *self = value; }
}
impl<T> SinkOnce for Vec<T> {}
impl<T> SinkOnce for NamedContents<T> {}
pub fn acquire_line<'s>(lines: &mut LazyLines<'s>, level: usize) -> Option<ParseLine<'s>>
{
	const HEAD: [char; 3] = ['-'; 3];

	lines.next().and_then(|(l, s)| if s.front() == Some('#') || s.front() == None { lines.drop_line(); acquire_line(lines, level) }
		else if s.starts_with_trailing_opt(&HEAD[..level], |c| c != '-')
		{
			lines.drop_line();
			let mut s = ParseLine::wrap(&s[level..], level, l);
			s.drop_while(ignore_chars);
			Some(s)
		}
		else { None })
}
pub struct NamedConfigLine<C> { name: Option<String>, config: C }
impl<C> NamedConfigLine<C>
{
	fn parse<'s, F>(input: &mut ParseLine<'s>, argparser: F) -> Result<Self, ParseError> where F: FnOnce(&mut ParseLine<'s>) -> Result<C, ParseError>
	{
		if input.front() == Some('$')
		{
			let name = input.drop_opt(1).take_until(ident_break).require_content(|s| ParseError::NameRequired(s.current()))?;
			input.drop_while(ignore_chars).consume_delimiter(':', |s| argparser(s.drop_while(ignore_chars)))
				.map(|c| NamedConfigLine { name: Some(name.clone_as_string()), config: c })
		}
		else { argparser(input).map(|c| NamedConfigLine { name: None, config: c }) }
	}
}
impl NamedConfigLine<()>
{
	fn parse_noargs(input: &mut ParseLine) -> Result<Self, ParseError>
	{
		if input.front() == Some('$')
		{
			let name = input.drop_opt(1).take_until(ident_break).require_content(|s| ParseError::NameRequired(s.current()))?;
			input.drop_while(ignore_chars).consume_delimiter(':', |_| Ok(NamedConfigLine { name: Some(name.clone_as_string()), config: () }))
		}
		else { Ok(NamedConfigLine { name: None, config: () }) }
	}
}
/// Acquires the name of configuration(string before colon, ****: ~~)
pub fn acquire_config_name<'s>(source: &mut ParseLine<'s>) -> Result<ParseLine<'s>, ParseError>
{
	let name = source.take_until(ident_break);
	if source.drop_while(ignore_chars).front() != Some(':') { Err(ParseError::DelimiterRequired(source.current())) }
	else { source.drop_opt(1); Ok(name) }
}

#[cfg(test)] mod tests
{
	use super::*;
	use itertools::Itertools;

	#[test] fn parse_precise_renderpass_ref()
	{
		Testing!
		{
			PreciseRenderPassRef::parse; "0.1" => Ok(PreciseRenderPassRef
			{
				rp: LocationPacked(Location(1, 0), ConfigInt::Value(0)), subpass: LocationPacked(Location(1, 2), ConfigInt::Value(1))
			}),
			PreciseRenderPassRef::parse; "$First .1" => Ok(PreciseRenderPassRef
			{
				rp: LocationPacked(Location(1, 0), ConfigInt::Ref("First".into())), subpass: LocationPacked(Location(1, 8), ConfigInt::Value(1))
			}),
			PreciseRenderPassRef::parse; "$Final" => Err(ParseError::Expected("PreciseRenderPassRef".into(), 0))
		}
	}
	#[test] fn blendstate()
	{
		Testing!
		{
			AttachmentBlendState::parse; "Disabled" => Ok(AttachmentBlendState::Disabled),
			AttachmentBlendState::parse; "None" => Err(ParseError::Expected("AttachmentBlendState Constant".into(), 0)),
			AttachmentBlendState::parse_array; "[]" => Ok(Vec::new()),
			AttachmentBlendState::parse_array; "[Alpha]" => Ok(vec![AttachmentBlendState::AlphaBlend]),
			AttachmentBlendState::parse_array; "[" => Err(ParseError::Expected("AttachmentBlendState Constant".into(), 1)),
			AttachmentBlendState::parse_array; "" => Err(ParseError::Expected("Array of AttachmentBlendState Constant".into(), 0))
		}
	}
	#[test] fn parse_coord_primitives()
	{
		Testing!
		{
			Size3F::parse; "(0.0, 0.0, 1.0)" => Ok(Size3F(0.0f32, 0.0, 1.0)),
			Size3F::parse; "0.0" => Err(ParseError::Expected("Size3F".into(), 0)),
			Size3F::parse; "(0.0 " => Err(ParseError::DelimiterRequired(5)),
			Size3F::parse; "(0, 0.0, 0.0" => Err(ParseError::ClosingRequired(12)),
			Offset2::parse; "(0, 0)" => Ok(Offset2(0, 0)),
			Offset2::parse; "0" => Err(ParseError::Expected("Offset2".into(), 0)),
			Offset2::parse; "(0 " => Err(ParseError::DelimiterRequired(3)),
			Offset2::parse; "(0, 0, 0" => Err(ParseError::ClosingRequired(5)),
			Offset2::parse; "(0, 0 " => Err(ParseError::ClosingRequired(6))
		}
	}
	#[test] fn descriptor_entry()
	{
		Testing!
		{
			DescriptorEntry::parse; "UniformBuffer: Vertex" => Ok(DescriptorEntry
			{
				kind: DescriptorEntryKind::UniformBuffer(BufferDescriptorOption::None), count: 1, visibility: ShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT)
			}),
			DescriptorEntry::parse; "2 CombinedSampler : Geometry / Fragment" => Ok(DescriptorEntry
			{
				kind: DescriptorEntryKind::CombinedSampler, count: 2, visibility: ShaderStageFlags(VK_SHADER_STAGE_GEOMETRY_BIT | VK_SHADER_STAGE_FRAGMENT_BIT)
			}),
			DescriptorEntry::parse; "a: Vertex" => Err(ParseError::UnknownDescriptorKind(0))
		}
	}
	#[test] fn primitive_topology()
	{
		Testing!
		{
			VkPrimitiveTopology::parse; "PointList" => Ok(VkPrimitiveTopology::PointList),
			VkPrimitiveTopology::parse; "LineStrip with Adjacency" => Ok(VkPrimitiveTopology::LineStripWithAdjacency),
			VkPrimitiveTopology::parse; "PatchList with Adjacency" => Err(ParseError::UnknownPrimitiveTopology(true, 0)),
			VkPrimitiveTopology::parse; "PointStrip" => Err(ParseError::UnknownPrimitiveTopology(false, 0)),
			VkPrimitiveTopology::parse; "" => Err(ParseError::Expected("Primitive Topology".into(), 0))
		}
	}
	#[test] fn test_subpass_deps()
	{
		Testing!
		{
			RPSubpassDeps::parse; "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage, ByRegion" => Ok(RPSubpassDeps
			{
				passtrans: Transition { from: LocationPacked(Location(1, 0), ConfigInt::Value(0)), to: LocationPacked(Location(1, 5), ConfigInt::Value(1)) },
				access_mask: Transition
				{
					from: LocationPacked(Location(1, 8), AccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)),
					to: LocationPacked(Location(1, 32), AccessFlags(VK_ACCESS_SHADER_READ_BIT))
				},
				stage_bits: LocationPacked(Location(1, 45), VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT), by_region: true
			}),
			RPSubpassDeps::parse; "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage" => Ok(RPSubpassDeps
			{
				passtrans: Transition { from: LocationPacked(Location(1, 0), ConfigInt::Value(0)), to: LocationPacked(Location(1, 5), ConfigInt::Value(1)) },
				access_mask: Transition
				{
					from: LocationPacked(Location(1, 8), AccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)),
					to: LocationPacked(Location(1, 32), AccessFlags(VK_ACCESS_SHADER_READ_BIT))
				},
				stage_bits: LocationPacked(Location(1, 45), VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT), by_region: false
			}),
			RPSubpassDeps::parse; "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage," => Ok(RPSubpassDeps
			{
				passtrans: Transition { from: LocationPacked(Location(1, 0), ConfigInt::Value(0)), to: LocationPacked(Location(1, 5), ConfigInt::Value(1)) },
				access_mask: Transition
				{
					from: LocationPacked(Location(1, 8), AccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)),
					to: LocationPacked(Location(1, 32), AccessFlags(VK_ACCESS_SHADER_READ_BIT))
				},
				stage_bits: LocationPacked(Location(1, 45), VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT), by_region: false
			}),
			RPSubpassDeps::parse; "0 -> 1: ColorAttachmentWrite -> ShaderRead" => Err(ParseError::DelimiterRequired(42)),
			RPSubpassDeps::parse; "0 -> 1" => Err(ParseError::DelimiterRequired(6))
		}
	}
	#[test] fn test_from_token()
	{
		Testing!
		{
			from_token; "From" => true,
			from_token; "<--" => true,
			from_token; "From[" => true,
			from_token; "Fro" => false,
			from_token; "Fromt" => false
		}
	}
	#[test] fn test_at_token()
	{
		Testing!
		{
			at_token; "At" => true,
			at_token; "Att" => false,
			at_token; "@" => true,
			at_token; "@p" => true
		}
	}

	#[test] fn test_rp_attachment()
	{
		Testing!
		{
			RPAttachment::parse; "R8G8B8A8 UNORM, ShaderReadOnlyOptimal <- ColorAttachmentOptimal, PreserveContent" => Ok(RPAttachment
			{
				format: LocationPacked(Location(1, 0), Format::Value(VkFormat::R8G8B8A8_UNORM)), preserve_content: true, clear_on_load: None,
				layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
			}),
			RPAttachment::parse; "R32 SFLOAT, ShaderReadOnlyOptimal <- ColorAttachmentOptimal" => Ok(RPAttachment
			{
				format: LocationPacked(Location(1, 0), Format::Value(VkFormat::R32_SFLOAT)), preserve_content: false, clear_on_load: None,
				layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
			}),
			RPAttachment::parse; "R8G8B8A8 UNORM, ShaderReadOnlyOptimal, ClearOnLoad" => Ok(RPAttachment
			{
				format: LocationPacked(Location(1, 0), Format::Value(VkFormat::R8G8B8A8_UNORM)), preserve_content: false, clear_on_load: Some(true),
				layouts: Transition { from: VkImageLayout::ShaderReadOnlyOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
			}),
			RPAttachment::parse; "R8G8B8A8 SNORM, ShaderReadOnlyOptimal, LoadContent / PreserveContent" => Ok(RPAttachment
			{
				format: LocationPacked(Location(1, 0), Format::Value(VkFormat::R8G8B8A8_SNORM)), preserve_content: true, clear_on_load: Some(false),
				layouts: Transition { from: VkImageLayout::ShaderReadOnlyOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }	
			}),
			RPAttachment::parse; "R8G8B8A8 UNORM" => Err(ParseError::DelimiterRequired(14)),
			RPAttachment::parse; "R8G8B8A8 UNORM, ShaderReadOnlyOptimal, Hoge" => Err(ParseError::UnknownRenderPassAttachmentOptions(39))
		}
	}
	#[test] fn test_subpass_desc()
	{
		Testing!
		{
			RPSubpassDesc::parse; "RenderTo 0"
				=> Ok(RPSubpassDesc { color_outs: vec![LocationPacked(Location(1, 9), ConfigInt::Value(0))], inputs: Vec::new() }),
			RPSubpassDesc::parse; "RenderTo 0 From 1" => Ok(RPSubpassDesc
			{
				color_outs: vec![LocationPacked(Location(1, 9), ConfigInt::Value(0))], inputs: vec![LocationPacked(Location(1, 16), ConfigInt::Value(1))]
			}),
			RPSubpassDesc::parse; "<- [1, 2] RenderTo [0, 3]" => Ok(RPSubpassDesc
			{
				color_outs: vec![LocationPacked(Location(1, 20), ConfigInt::Value(0)), LocationPacked(Location(1, 23), ConfigInt::Value(3))],
				inputs: vec![LocationPacked(Location(1, 4), ConfigInt::Value(1)), LocationPacked(Location(1, 7), ConfigInt::Value(2))]
			}),
			RPSubpassDesc::parse; "Preserve 0" => Err(ParseError::CorruptedSubpassDesc(0)),
			RPSubpassDesc::parse; "RenderTo 0 RenderTo 1" => Err(ParseError::DefinitionOverrided)
		}
	}
	#[test] fn test_external_resources()
	{
		Testing!
		{
			ExternalResourceData::parse; "ImageView 1D \"HogeResource\""
				=> Ok(ExternalResourceData::ImageView { dim: ImageDimension::Linear, refname: LocationPacked(Location(1, 13), "HogeResource".into()) }),
			ExternalResourceData::parse; "ImageView Planar \"HogeResource\""
				=> Ok(ExternalResourceData::ImageView { dim: ImageDimension::Planar, refname: LocationPacked(Location(1, 17), "HogeResource".into()) }),
			ExternalResourceData::parse; "ImageView 3D \"HogeResource\""
				=> Ok(ExternalResourceData::ImageView { dim: ImageDimension::Cubic, refname: LocationPacked(Location(1, 13), "HogeResource".into()) }),
			ExternalResourceData::parse; "SwapChainViews" => Ok(ExternalResourceData::SwapChainViews),
			ExternalResourceData::parse; "Framebuffer" => Err(ParseError::UnknownExternalResource(0)),
			ExternalResourceData::parse; "ImageView \"HogeResource\"" => Err(ParseError::Expected("Image Dimension".into(), 10))
		}
	}
	#[test] fn test_framebuffer_rp()
	{
		Testing!
		{
			FramebufferRenderPassRef::parse; "<$FirstRP>" => Ok(FramebufferRenderPassRef::Int(LocationPacked(Location(1, 1), ConfigInt::Ref("FirstRP".into())))),
			FramebufferRenderPassRef::parse; "<Presented>" => Ok(FramebufferRenderPassRef::Presented),
			FramebufferRenderPassRef::parse; "n" => Ok(FramebufferRenderPassRef::None),
			FramebufferRenderPassRef::parse; "<0" => Err(ParseError::ClosingRequired(2)),
			FramebufferRenderPassRef::parse; "<AA>" => Err(ParseError::UnknownObjectRef("RenderPass", 1))
		}
	}

	#[test] fn string_literal()
	{
		Testing!
		{
			StringLiteral::parse; "\"HogeResource\"" => Ok(LocationPacked(Location(1, 0), "HogeResource".into())),
			StringLiteral::parse; "\"HogeResource" => Err(ParseError::ClosingRequired(13)),
			StringLiteral::parse; "A" => Err(ParseError::Expected("String Literal".into(), 0))
		}
	}
}
