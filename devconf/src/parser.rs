//! Postludium: Device Configuration Processor: Parser

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
			externs: NamedContents::new(), framebuffers: NamedContents::new(), ind_shaders: IndependentShaders::new()
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
	NameNotAllowed(usize)
}
trait DivergenceExt<T> { fn report_error(self, line: usize) -> T; }
impl<T> DivergenceExt<T> for Result<T, ParseError>
{
	fn report_error(self, line: usize) -> T
	{
		match self
		{
			Ok(t) => t,
			Err(ParseError::NameRequired(p)) => panic!("Name required following $ at line {}, col {}", line, p),
			Err(ParseError::UnknownDeviceResource(p)) => panic!("Unknown Device Resource was found at line {}, col {}", line, p),
			Err(ParseError::UnknownFormat(p)) => panic!("Unknown Image Format was found at line {}, col {}", line, p),
			Err(ParseError::UnknownImageLayout(p)) => panic!("Unknown Image Layout was found at line {}, col {}", line, p),
			Err(ParseError::UnknownRenderPassAttachmentOptions(p)) => panic!("Unknown Options for RenderPass Attachment was found at line {}, col {}", line, p),
			Err(ParseError::ImageLayoutRequired(p)) => panic!("Image Layout required at line {}, col {}", line, p),
			Err(ParseError::DirectionRequired(p)) => panic!("Direction Token(->, <-, To or From) required at line {}, col {}", line, p),
			Err(ParseError::DelimiterRequired(p)) => panic!("Delimiter required at line {}, col {}", line, p),
			Err(ParseError::ClosingRequired(p)) => panic!("Closing required at line {}, col {}", line, p),
			Err(ParseError::BytesizeRequired(p)) => panic!("Bytesize required at line {}, col {}", line, p),
			Err(ParseError::DefinitionOverrided) => panic!("Multiple definitions are found at line {}", line),
			Err(ParseError::CorruptedSubpassDesc(p)) => panic!("Some Error are found parsing SubpassDesc at line {}, col {}", line, p),
			Err(ParseError::IntValueRequired(p)) => panic!("Integer or ConfigRef required at line {}, col {}", line, p),
			Err(ParseError::UnknownPipelineStageFlag(p)) => panic!("Unknown Pipeline Stage Flag was found at line {}, col {}", line, p),
			Err(ParseError::UnknownAccessFlag(p)) => panic!("Unknown Access Mask Flag was found at line {}, col {}", line, p),
			Err(ParseError::UnknownExternalResource(p)) => panic!("Unknown External Resource was found at line {}, col {}", line, p),
			Err(ParseError::UnknownClearMode(p)) => panic!("Unknown Clear Mode was found at line {}, col {}", line, p),
			Err(ParseError::UnknownShaderStageFlag(p)) => panic!("Unknown Shader Stage Flag was found at line {}, col {}", line, p),
			Err(ParseError::UnknownDescriptorKind(p)) => panic!("Unknown Descriptor Kind was found at line {}, col {}", line, p),
			Err(ParseError::FormatRequired(p)) => panic!("Format required for RenderPass Attachment at line {}, col {}", line, p),
			Err(ParseError::UnknownPrimitiveTopology(true, p)) => panic!("Unknown Primitive Topology with Adjacency was found at line {}, col {}", line, p),
			Err(ParseError::UnknownPrimitiveTopology(false, p)) => panic!("Unknown Primitive Topology was found at line {}, col {}", line, p),
			Err(ParseError::UnknownObjectRef(n, p)) => panic!("Unknown {} ref at line {}, col {}", n, line, p),
			Err(ParseError::NumericParseError(n, p)) => panic!("NumericParseError: {} at line {}, col {}", n, line, p),
			Err(ParseError::FloatingParseError(n, p)) => panic!("FloatingParseError: {} at line {}, col {}", n, line, p),
			Err(ParseError::Expected(s, p)) => panic!("Expected {}, but it was not found at line {}, col {}", s, line, p),
			Err(ParseError::UnknownConfig(s)) => panic!("Unknown Config for {} was found at line {}", s, line),
			Err(ParseError::ConfigRequired(c)) => panic!("Configuration \"{}\" required at line {}", c, line),
			Err(ParseError::NameNotAllowed(p)) => panic!("Naming not allowed for the configuration at line {}, col {}", line, p)
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
/// The structure that can be constructed by lines, panics if there are some errors.
pub trait FromSourceBlock : Sized
{
	fn parse(enterline: &mut ParseLine, lines: &mut LazyLines) -> Self;
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
	fn location(&self) -> Location { Location(self.line(), self.current()) }
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

pub fn parse_device_resources(sink: &mut ParsedDeviceResources, includes: &mut Vec<LocationPacked<PathBuf>>, lines: &mut LazyLines)
{
	while let Some(mut source) = acquire_line(lines, 0)
	{
		let insource = source.current();
		let NamedConfigLine { name, .. } = NamedConfigLine::parse_noargs(&mut source).report_error(source.line());
		let name = name.map(From::from);
		let s = source.drop_while(ignore_chars).take_until(ident_break);
		match s.clone_as_string().as_ref()
		{
			/// Include <StringLiteral>
			"Include" => source.drop_while(ignore_chars).parse::<StringLiteral>().map(PartialApply1!(LocationPacked::apply; From::from))
				.and_then(|p| if name.is_some() { Err(ParseError::NameNotAllowed(insource)) } else { Ok(includes.push(p)) }).report_error(source.line()),
			"RenderPass" =>
				Let(RenderPassData::parse(&mut source, lines))._in(|p| sink.renderpasses.insert_uniq_auto(name, p)).report_error(source.line()),
			"SimpleRenderPass" =>
				Let(SimpleRenderPassData::parse(&mut source, lines))._in(|p| sink.simple_rps.insert_uniq_auto(name, p)).report_error(source.line()),
			"PresentedRenderPass" =>
				Let(PresentedRenderPassData::parse(&mut source, lines))._in(|p| sink.presented_rps.insert_uniq_auto(name, p)).report_error(source.line()),
			"DescriptorSetLayout" =>
				Let(DescriptorSetLayoutData::parse(&mut source, lines))._in(|p| sink.descriptor_set_layouts.insert_uniq_auto(name, p)).report_error(source.line()),
			"PushConstantLayout" =>
				Let(PushConstantLayout::parse(&mut source, lines))._in(|p| sink.push_constant_layouts.insert_uniq_auto(name, p)).report_error(source.line()),
			"PipelineLayout" =>
				Let(PipelineLayout::parse(&mut source, lines))._in(|p| sink.pipeline_layouts.insert_uniq_auto(name, p)).report_error(source.line()),
			"DescriptorSets" =>
				Let(DescriptorSetsInfo::parse(&mut source, lines))._in(|p| sink.descriptor_sets.insert_uniq_auto(name, p)).report_error(source.line()),
			"PipelineState" =>
				Let(PipelineStateInfo::parse(source.drop_while(ignore_chars), lines))._in(|p| sink.pipeline_states.insert_uniq_auto(name, p)).report_error(source.line()),
			"Extern" =>
				Let(source.drop_while(ignore_chars).parse::<ExternalResourceData>().report_error(source.line()))
					._in(|p| sink.externs.insert_uniq_auto(name, p)).report_error(source.line()),
			"Framebuffer" =>
				Let(FramebufferInfo::parse(source.drop_while(ignore_chars), lines))._in(|p| sink.framebuffers.insert_uniq_auto(name, p)).report_error(source.line()),
			"VertexShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_VERTEX_BIT, asset: asset, consts: consts };
				sink.ind_shaders.vertex.insert_uniq_auto(name, ss).report_error(source.line())
			},
			"FragmentShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_FRAGMENT_BIT, asset: asset, consts: consts };
				sink.ind_shaders.fragment.insert_uniq_auto(name, ss).report_error(source.line())
			},
			"GeometryShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_GEOMETRY_BIT, asset: asset, consts: consts };
				sink.ind_shaders.geometry.insert_uniq_auto(name, ss).report_error(source.line())
			},
			"TessellationControlShader" | "TessControlShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT, asset: asset, consts: consts };
				sink.ind_shaders.tesscontrol.insert_uniq_auto(name, ss).report_error(source.line())
			},
			"TessellationEvaluationShader" | "TessEvaluationShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT, asset: asset, consts: consts };
				sink.ind_shaders.tessevaluation.insert_uniq_auto(name, ss).report_error(source.line())
			},
			_ => Err(ParseError::UnknownDeviceResource(s.current())).report_error(source.line())
		}
	}
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
		match s.clone_as_string().as_ref()
		{
			"ImageView" => source.drop_while(ignore_chars).parse::<ImageDimension>()
				.and_then(|d| source.drop_while(ignore_chars).parse::<StringLiteral>().map(|n| ExternalResourceData::ImageView { dim: d, refname: n })),
			"SwapChainViews" => Ok(ExternalResourceData::SwapChainViews),
			_ => Err(ParseError::UnknownExternalResource(s.current()))
		}
	}
}

lazy_static!
{
	static ref ATTACHMENTS: Vec<char> = "Attachments".chars().collect();
	static ref SUBPASSES: Vec<char> = "Subpasses".chars().collect();
	static ref DEPENDENCIES: Vec<char> = "Dependencies".chars().collect();
}
impl FromSourceBlock for RenderPassData
{
	fn parse(_: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		let mut rpd = RenderPassData { attachments: NamedContents::new(), subpasses: NamedContents::new(), deps: Vec::new() };
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
			{
				ATTACHMENTS[..] => if !rpd.attachments.is_empty() { Err(ParseError::DefinitionOverrided) }
				else
				{
					while let Some(mut s) = acquire_line(source, 2)
					{
						let l = NamedConfigLine::parse(&mut s, RPAttachment::parse).report_error(s.line());
						rpd.attachments.insert_uniq_auto(l.name.map(From::from), l.config).report_error(s.line());
					}
					Ok(())
				},
				SUBPASSES[..] => if !rpd.subpasses.is_empty() { Err(ParseError::DefinitionOverrided) }
				else
				{
					while let Some(mut s) = acquire_line(source, 2)
					{
						let l = NamedConfigLine::parse(&mut s, RPSubpassDesc::parse).report_error(s.line());
						rpd.subpasses.insert_uniq_auto(l.name.map(From::from), l.config).report_error(s.line());
					}
					Ok(())
				},
				DEPENDENCIES[..] => if !rpd.deps.is_empty() { Err(ParseError::DefinitionOverrided) }
				else
				{
					while let Some(mut s) = acquire_line(source, 2)
					{
						let dep = s.parse::<RPSubpassDeps>().report_error(s.line());
						rpd.deps.push(dep);
					}
					Ok(())
				};
				_ => Err(ParseError::UnknownConfig("RenderPass"))
			})).report_error(s.line());
		}
		rpd
	}
}
lazy_static!
{
	static ref RPO_CLEAR_ON_LOAD: Vec<char> = "ClearOnLoad".chars().collect();
	static ref RPO_LOAD_CONTENT: Vec<char> = "LoadContent".chars().collect();
	static ref RPO_PRESERVE_CONTENT: Vec<char> = "PreserveContent".chars().collect();
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
				else if source.starts_with_trailing_opt(&RPO_CLEAR_ON_LOAD, ident_break)
				{
					rpa.clear_on_load = Some(true);
					if source.drop_opt(RPO_CLEAR_ON_LOAD.len()).drop_while(ignore_chars).front() == Some('/')
					{
						recurse(rpa, source.drop_opt(1).drop_while(ignore_chars))
					}
					else { Ok(()) }
				}
				else if source.starts_with_trailing_opt(&RPO_LOAD_CONTENT, ident_break)
				{
					rpa.clear_on_load = Some(false);
					if source.drop_opt(RPO_LOAD_CONTENT.len()).drop_while(ignore_chars).front() == Some('/')
					{
						recurse(rpa, source.drop_opt(1).drop_while(ignore_chars))
					}
					else { Ok(()) }
				}
				else if source.starts_with_trailing_opt(&RPO_PRESERVE_CONTENT, ident_break)
				{
					rpa.preserve_content = true;
					if source.drop_opt(RPO_PRESERVE_CONTENT.len()).drop_while(ignore_chars).front() == Some('/')
					{
						recurse(rpa, source.drop_opt(1).drop_while(ignore_chars))
					}
					else { Ok(()) }
				}
				else { Err(ParseError::UnknownRenderPassAttachmentOptions(source.current())) }
			}
			recurse(&mut rpa, source.drop_opt(1).drop_while(ignore_chars)).map(|_| rpa)
		}
	}
}
lazy_static!
{
	static ref SDI_RENDER_TO: Vec<char> = "RenderTo".chars().collect();
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
			else if input.starts_with_trailing_opt(&SDI_RENDER_TO, ident_break)
			{
				// RenderTo int/[ints...]
				let v = input.drop_opt(SDI_RENDER_TO.len()).drop_while(ignore_chars).array_loc::<ConfigInt>()?;
				if sink.color_outs.is_empty() { sink.color_outs = v; recurse(input.drop_while(ignore_chars), sink) }
				else { Err(ParseError::DefinitionOverrided) }
			}
			else if from_token(input)
			{
				// From int/[ints...]
				let v = input.drop_while(ignore_chars).array_loc::<ConfigInt>()?;
				if sink.inputs.is_empty() { sink.inputs = v; recurse(input.drop_while(ignore_chars), sink) }
				else { Err(ParseError::DefinitionOverrided) }
			}
			else { Err(ParseError::CorruptedSubpassDesc(input.current())) }
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
		let stage_bits = if at_token(source.drop_while(ignore_chars))
		{
			parse_pipeline_stage_bits(source.drop_while(ignore_chars))
		}
		else { Err(ParseError::DelimiterRequired(source.current())) }?;
		let by_region = if source.drop_while(ignore_chars).front() == Some(',')
		{
			source.drop_opt(1).drop_while(ignore_chars).starts_with_trailing_opt(&BY_REGION, ident_break)
		} else { false };

		Ok(RPSubpassDeps { passtrans: pass_trans, access_mask: access_mask_trans, stage_bits, by_region: by_region })
	}
}
fn parse_rp_clear_mode(source: &ParseLine) -> Result<Option<bool>, ParseError>
{
	if *source == ['N', 'o', 'n', 'e'][..] { Ok(None) }
	else if *source == ['O', 'n', 'L', 'o', 'a', 'd'][..] { Ok(Some(true)) }
	else if *source == ['P', 'r', 'e', 's', 'e', 'r', 'v', 'e'][..] { Ok(Some(false)) }
	else { Err(ParseError::UnknownClearMode(source.current())) }
}

lazy_static!
{
	static ref FORMAT: Vec<char> = "Format".chars().collect();
	static ref CLEARMODE: Vec<char> = "ClearMode".chars().collect();
}
impl FromSourceBlock for SimpleRenderPassData
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> SimpleRenderPassData
	{
		let (mut fmt, mut clear_mode) = (None, None);
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
			{
				FORMAT[..] => s.drop_while(ignore_chars).parse_loc::<Format>().and_then(|f| assign_check_overriding(&mut fmt, f)),
				CLEARMODE[..] => parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () });
				_ => Err(ParseError::UnknownConfig("SimpleRenderPass"))
			})).report_error(s.line());
		}
		fmt.ok_or(ParseError::ConfigRequired("Format")).map(|fmt| SimpleRenderPassData { format: fmt, clear_on_load: clear_mode })
			.report_error(enterline.line())
	}
}
impl FromSourceBlock for PresentedRenderPassData
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		let (mut fmt, mut clear_mode) = (None, None);
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
			{
				FORMAT[..] => s.drop_while(ignore_chars).parse_loc::<Format>().and_then(|f| assign_check_overriding(&mut fmt, f)),
				CLEARMODE[..] => parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () });
				_ => Err(ParseError::UnknownConfig("PresentedRenderPass"))
			})).report_error(s.line());
		}
		fmt.ok_or(ParseError::ConfigRequired("Format")).map(|fmt| PresentedRenderPassData { format: fmt, clear_on_load: clear_mode })
			.report_error(enterline.line())
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
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		let arg = enterline.drop_while(ignore_chars).parse::<FramebufferRenderPassRef>().report_error(enterline.line());
		let vs = enterline.drop_while(ignore_chars).array_loc::<ConfigInt>().report_error(enterline.line());
		
		let mut clear_mode = None;
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(s.drop_while(ignore_chars)).and_then(|name|
				if name == CLEARMODE[..] { parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; }) }
				else { Err(ParseError::UnknownConfig("Framebuffer")) }
			).report_error(s.line());
		}
		let style = match arg
		{
			FramebufferRenderPassRef::Int(rp) => FramebufferStyle::WithRenderPass(rp),
			FramebufferRenderPassRef::Presented => FramebufferStyle::Presented(clear_mode),
			FramebufferRenderPassRef::None => FramebufferStyle::Simple(clear_mode)
		};
		FramebufferInfo { style: style, views: vs }
	}
}
impl FromSourceBlock for DescriptorSetLayoutData
{
	fn parse(_: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		let mut entries = Vec::new();
		while let Some(mut s) = acquire_line(source, 1)
		{
			entries.push(s.parse::<DescriptorEntry>().report_error(s.line()));
		}
		DescriptorSetLayoutData { entries: entries }
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
lazy_static!
{
	static ref RANGE: Vec<char> = "Range".chars().collect();
	static ref VISIBILITY: Vec<char> = "Visibility".chars().collect();
}
impl FromSourceBlock for PushConstantLayout
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		let (mut range, mut vis) = (None, None);
		while let Some(mut s) = acquire_line(source, 1)
		{
			acquire_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
			{
				RANGE[..] => s.drop_while(ignore_chars).ranged::<usize>().and_then(|r| assign_check_overriding(&mut range, r)),
				VISIBILITY[..] => s.drop_while(ignore_chars).parse::<ShaderStageFlags>().and_then(|v| assign_check_overriding(&mut vis, v));
				_ => Err(ParseError::UnknownConfig("PushConstantLayout"))
			})).report_error(s.line());
		}
		let range = range.ok_or(ParseError::ConfigRequired("Range")).report_error(enterline.line());
		let visibility = vis.ok_or(ParseError::ConfigRequired("Visibility")).report_error(enterline.line());
		PushConstantLayout { range: range, visibility: visibility }
	}
}
lazy_static!
{
	static ref DESCRIPTORS: Vec<char> = "Descriptors".chars().collect();
	static ref PUSHCONSTANTLAYOUTS: Vec<char> = "PushConstantLayouts".chars().collect();
}
impl FromSourceBlock for PipelineLayout
{
	fn parse(_: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		let (mut desc, mut pcls) = (Vec::new(), Vec::new());
		while let Some(mut s) = acquire_line(source, 1)
		{
			/// - | "Descriptors" ints... / "PushConstantLayouts" ints...
			acquire_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
			{
				DESCRIPTORS[..] => s.drop_while(ignore_chars).array_loc::<ConfigInt>().and_then(|cv| vassign_check_overriding(&mut desc, cv)),
				PUSHCONSTANTLAYOUTS[..] => s.drop_while(ignore_chars).array_loc::<ConfigInt>().and_then(|cv| vassign_check_overriding(&mut pcls, cv));
				_ => Err(ParseError::UnknownConfig("PipelineLayout"))
			})).report_error(s.line());
		}
		PipelineLayout { descs: desc, pushconstants: pcls }
	}
}
impl FromSourceBlock for DescriptorSetsInfo
{
	fn parse(_: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		let mut entries = Vec::new();
		while let Some(mut s) = acquire_line(source, 1)
		{
			/// - [$Name :] $Name
			let l = NamedConfigLine::parse(&mut s, ConfigInt::parse_with_location)
				.map(|l| DescriptorSetEntry { name: l.name, layout: l.config }).report_error(s.line());
			entries.push(l);
		}
		DescriptorSetsInfo(entries)
	}
}
impl FromSourceBlock for PipelineStateInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		/// PipelineState | "for" precise_rpref "with" int
		let r = enterline.consume(for_token, || "\"for\"".into(), |s| s.drop_while(ignore_chars).parse::<PreciseRenderPassRef>()).report_error(enterline.line());
		let l = enterline.consume(with_token, || "\"with\"".into(), |s| s.drop_while(ignore_chars).parse_loc::<ConfigInt>()).report_error(enterline.line());

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
			acquire_config_name(&mut s).and_then(|name| match name.clone_as_string().as_ref()
			{
				"VertexShader" =>
					assign_check_overriding(&mut vsinfo, VertexShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"FragmentShader" =>
					assign_check_overriding(&mut shaderinfo.fragment, PipelineShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"GeometryShader" =>
					assign_check_overriding(&mut shaderinfo.geometry, PipelineShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"TessellationControlShader" =>
					assign_check_overriding(&mut shaderinfo.tesscontrol, PipelineShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"TessellationEvaluationShader" =>
					assign_check_overriding(&mut shaderinfo.tessevaluation, PipelineShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"PrimitiveTopology" => s.drop_while(ignore_chars).parse::<VkPrimitiveTopology>().and_then(|p| assign_check_overriding(&mut primt, p)),
				"ViewportScissors" => parse_viewport_scissors(s.drop_while(ignore_chars), source).and_then(|v| vassign_check_overriding(&mut vpsc, v)),
				"BlendStates" => AttachmentBlendState::parse_array(s.drop_while(ignore_chars)).and_then(|v| vassign_check_overriding(&mut blends, v)),
				_ => Err(ParseError::UnknownConfig("PipelineState"))
			}).report_error(s.line());
		}
		let vsinfo = vsinfo.ok_or(ParseError::ConfigRequired("VertexShader")).report_error(enterline.line());
		let primt = primt.ok_or(ParseError::ConfigRequired("PrimitiveTopology")).report_error(enterline.line());
		PipelineStateInfo
		{
			renderpass: r, layout_ref: l,
			vertex_shader: vsinfo, fragment_shader: shaderinfo.fragment, geometry_shader: shaderinfo.geometry,
			tesscontrol_shader: shaderinfo.tesscontrol, tessevaluation_shader: shaderinfo.tessevaluation,
			primitive_topology: primt, viewport_scissors: vpsc, blendstates: blends
		}
	}
}
impl FromSourceBlock for PipelineShaderStageInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		Self::parse_baseindent(enterline, source, 1)
	}
}
lazy_static!
{
	static ref CONSTANT: Vec<char> = "Constant".chars().collect();
	static ref STREAMBINDINGS: Vec<char> = "StreamBindings".chars().collect();
	static ref INPUT: Vec<char> = "Input".chars().collect();
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
fn sequence2<F, R, F2, R2, E>(c: &mut ParseLine, f1: F, f2: F2) -> Result<(R, R2), E>
	where F: FnOnce(&mut ParseLine) -> Result<R, E>, F2: FnOnce(&mut ParseLine) -> Result<R2, E>
{
	f1(c).and_then(|x1| f2(c).map(|x2| (x1, x2)))
}
fn sequence4<F, R, F2, R2, F3, R3, F4, R4, E>(c: &mut ParseLine, f1: F, f2: F2, f3: F3, f4: F4) -> Result<(R, R2, R3, R4), E>
	where F: FnOnce(&mut ParseLine) -> Result<R, E>, F2: FnOnce(&mut ParseLine) -> Result<R2, E>,
		F3: FnOnce(&mut ParseLine) -> Result<R3, E>, F4: FnOnce(&mut ParseLine) -> Result<R4, E>
{
	f1(c).and_then(|x1| f2(c).and_then(|x2| f3(c).and_then(|x3| f4(c).map(|x4| (x1, x2, x3, x4)))))
}
impl FromSourceArray for StreamBindingDesc {}
impl FromSourceBlock for VertexShaderStageInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		/// VertexShader | AssetResource
		let asset = enterline.parse_loc::<AssetResource>().report_error(enterline.line());

		let (mut consts, mut bindings, mut inputs) = (BTreeMap::new(), Vec::new(), BTreeMap::new());
		while let Some(mut s) = acquire_line(source, 1)
		{
			if s.starts_with(&CONSTANT)
			{
				// - "Constant" usize ":" value
				sequence2(s.drop_opt(CONSTANT.len()), 
					|s| s.drop_while(ignore_chars).parse::<usize>(),
					|s| s.drop_while(ignore_chars).consume_char(':', || "\":\"".into(), |s| NumericLiteral::parse(s.drop_while(ignore_chars), true))
				).map(|(index, value)| { consts.insert(index, value); })
			}
			else if s.starts_with(&STREAMBINDINGS)
			{
				// - "StreamBindings" ":" (StreamBindingDesc...)
				if !bindings.is_empty() { Err(ParseError::DefinitionOverrided) } else
				{
					s.drop_opt(STREAMBINDINGS.len()).drop_while(ignore_chars)
						.consume_delimiter(':', |s| s.drop_while(ignore_chars).array::<StreamBindingDesc>()).map(|v| { bindings = v; })
				}
			}
			else if s.starts_with(&INPUT)
			{
				// - "Input" usize "." usize To usize ":" Format
				sequence4(s.drop_opt(INPUT.len()),
					|s| s.drop_while(ignore_chars).parse::<usize>(),
					|s| s.drop_while(ignore_chars).consume_char('.', || "\".\"".into(), |s| s.drop_while(ignore_chars).parse::<usize>()),
					|s| if to_token(s.drop_while(ignore_chars)) { s.drop_while(ignore_chars).parse::<usize>() }
						else { Err(ParseError::Expected("\"To\" or \"->\"".into(), s.current())) },
					|s| s.drop_while(ignore_chars).consume_char(':', || "\":\"".into(), |s| s.drop_while(ignore_chars).parse::<Format>())
				).map(|(binding, offset, index, format)| { inputs.insert(index, VertexInputInfo { binding, offset, format }); })
			}
			else { Err(ParseError::UnknownConfig("Vertex Shader Module")) }.report_error(s.line());
		}
		VertexShaderStageInfo { asset: asset, consts: consts, stream_bindings: bindings, inputs: inputs }
	}
}
impl PipelineShaderStageInfo
{
	fn parse_baseindent(enterline: &mut ParseLine, source: &mut LazyLines, baseindent: usize) -> Self
	{
		let asset = enterline.parse_loc::<AssetResource>().report_error(enterline.line());

		let mut consts = BTreeMap::new();
		while let Some(mut s) = acquire_line(source, baseindent + 1)
		{
			/// - "Constant" usize : value
			if s.starts_with(&['C', 'o', 'n', 's', 't', 'a', 'n', 't'])
			{
				sequence2(&mut s,
					|s| s.drop_opt(8).drop_while(ignore_chars).parse::<usize>(),
					|s| s.drop_while(ignore_chars).consume_char(':', || "\":\"".into(), |s| NumericLiteral::parse(s.drop_while(ignore_chars), true))
				).map(|(index, value)| { consts.insert(index, value); })
			}
			else { Err(ParseError::UnknownConfig("Shader Module")) }.report_error(s.line());
		}
		PipelineShaderStageInfo { asset: asset, consts: consts }
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
fn parse_viewport_scissors(current: &mut ParseLine, source: &mut LazyLines) -> Result<Vec<ViewportScissorEntry>, ParseError>
{
	let mut v = Vec::new();
	if current.front() == None
	{
		// Descending
		while let Some(mut s) = acquire_line(source, 2)
		{
			v.push(ViewportScissorEntry::parse(&mut s).report_error(s.line()));
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
		}
	}
	else { Err(ParseError::Expected("Array or Children, of ViewportScissor".into(), current.current())) }
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
pub fn for_token(input: &mut ParseLine) -> bool
{
	// "for"
	if input.starts_with_trailing_opt(&['f', 'o', 'r'], ident_break) { input.drop_opt(3); true }
	else { false }
}
pub fn with_token(input: &mut ParseLine) -> bool
{
	// "with"
	if input.starts_with_trailing_opt(&['w', 'i', 't', 'h'], ident_break) { input.drop_opt(4); true }
	else { false }
}
pub fn adjacency_token(input: &mut ParseLine) -> bool
{
	// "Adjacency"
	if input.starts_with_trailing_opt(&['A', 'd', 'j', 'a', 'c', 'e', 'n', 'c', 'y'], ident_break) { input.drop_opt(9); true }
	else { false }
}

// --- Parser Utils --- //
pub fn ignore_chars(c: char) -> bool { c == ' ' || c == '\t' }
pub fn ident_break(c: char) -> bool
{
	c == ':' || c == '-' || c == '[' || c == ']' || c == ',' || c == '<' || c == '>' || c == '/' || c == '.' || ignore_chars(c)
}
fn assign_check_overriding<T>(dest: &mut Option<T>, value: T) -> Result<(), ParseError>
{
	if dest.is_none() { *dest = Some(value); Ok(()) } else { Err(ParseError::DefinitionOverrided) }
}
fn vassign_check_overriding<T>(dest: &mut Vec<T>, value: Vec<T>) -> Result<(), ParseError>
{
	if dest.is_empty() { *dest = value; Ok(()) } else { Err(ParseError::DefinitionOverrided) }
}
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
		let name_res = if input.front() == Some('$')
		{
			let name = input.drop_opt(1).take_until(ident_break);
			if name.is_empty() { Err(ParseError::NameRequired(name.current())) }
			else
			{
				input.drop_while(ignore_chars);
				if input.front() == Some(':') { input.drop_opt(1); Ok(Some(name.clone_as_string())) }
				else { Err(ParseError::DelimiterRequired(input.current())) }
			}
		}
		else { Ok(None) };

		name_res.and_then(|name_opt| argparser(input.drop_while(ignore_chars)).map(|v| NamedConfigLine { name: name_opt, config: v }))
	}
}
impl NamedConfigLine<()>
{
	fn parse_noargs(input: &mut ParseLine) -> Result<Self, ParseError>
	{
		if input.front() == Some('$')
		{
			let name = input.drop_opt(1).take_until(ident_break);
			if name.is_empty() { Err(ParseError::NameRequired(name.current())) }
			else
			{
				if input.drop_while(ignore_chars).front() == Some(':')
				{
					input.drop_opt(1); Ok(NamedConfigLine { name: Some(name.clone_as_string()), config: () })
				}
				else { Err(ParseError::DelimiterRequired(input.current())) }
			}
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
