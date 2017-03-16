//! Postludium: Device Configuration Processor: Parser

use std;
use std::ops::{Index, Deref, Range};
use std::num::{ParseIntError, ParseFloatError};
use std::collections::HashMap;
use parsetools::*;
use std::borrow::Cow;
use interlude::ffi::*;
use std::path::PathBuf;
use interlude::*;

pub use items::*;
pub use hobjects::*;

pub struct NamedContents<T>(HashMap<String, usize>, Vec<T>);
impl<T> Index<usize> for NamedContents<T>
{
	type Output = T;
	fn index(&self, s: usize) -> &T { &self.1[s] }
}
impl<'a, T> Index<&'a str> for NamedContents<T>
{
	type Output = T;
	fn index(&self, s: &'a str) -> &T { &self.1[self.0[s]] }
}
impl<T> Deref for NamedContents<T> { type Target = [T]; fn deref(&self) -> &[T] { &self.1 } }
impl<T> NamedContents<T>
{
	pub fn new() -> Self { NamedContents(HashMap::new(), Vec::new()) }
	pub fn insert(&mut self, name: Cow<str>, value: T) -> &mut T
	{
		let ref mut v = self.1;
		let p = *self.0.entry(name.into_owned()).or_insert_with(||
		{
			v.push(value);
			v.len() - 1
		});
		&mut v[p]
	}
	pub fn insert_unnamed(&mut self, value: T) -> &mut T
	{
		self.1.push(value);
		let p = self.1.len() - 1;
		&mut self.1[p]
	}
	pub fn reverse_index(&self, k: &str) -> Option<usize>
	{
		self.0.get(k).cloned()
	}
}

// Source Representations
pub struct ParsedDeviceResources
{
	pub renderpasses: NamedContents<RenderPassData>,
	pub simple_rps: NamedContents<SimpleRenderPassData>,
	pub presented_rps: NamedContents<PresentedRenderPassData>,
	pub descriptor_set_layouts: NamedContents<DescriptorSetLayoutData>,
	pub push_constant_layouts: NamedContents<PushConstantLayout>,
	pub pipeline_layouts: NamedContents<PipelineLayout>,
	pub descriptor_sets: NamedContents<DescriptorSetsInfo>,
	pub pipeline_states: NamedContents<PipelineStateInfo>,
	pub externs: NamedContents<ExternalResourceData>,
	pub framebuffers: NamedContents<FramebufferInfo>,
	pub ind_shaders: NamedContents<IndependentPipelineShaderStageInfo>
}
#[derive(Debug, PartialEq)]
pub struct ImageDescription
{
	pub dim: u32, pub format: LocationPacked<Format>, pub extent: LocationPacked<(f32, f32, f32)>, pub device_local: bool, pub usage: VkImageUsageFlags
}
#[derive(Debug, PartialEq)]
pub struct RPAttachment { pub format: LocationPacked<Format>, pub layouts: Transition<VkImageLayout>, pub clear_on_load: Option<bool>, pub preserve_content: bool }
#[derive(Debug, PartialEq)]
pub struct RPSubpassDesc { pub color_outs: Vec<LocationPacked<ConfigInt>>, pub inputs: Vec<LocationPacked<ConfigInt>> }
#[derive(Debug, PartialEq)]
pub struct RPSubpassDeps
{
	pub passtrans: Transition<LocationPacked<ConfigInt>>, pub access_mask: Transition<LocationPacked<VkAccessFlags>>,
	pub stage_bits: LocationPacked<VkPipelineStageFlags>, pub by_region: bool
}
pub struct RenderPassData { pub attachments: NamedContents<RPAttachment>, pub subpasses: NamedContents<RPSubpassDesc>, pub deps: Vec<RPSubpassDeps> }
#[derive(Debug, PartialEq)]
pub struct SimpleRenderPassData { pub format: LocationPacked<Format>, pub clear_on_load: Option<bool> }
#[derive(Debug, PartialEq)]
pub struct PresentedRenderPassData { pub format: LocationPacked<Format>, pub clear_on_load: Option<bool> }
#[derive(Debug, PartialEq)]
pub struct PreciseRenderPassRef { pub rp: LocationPacked<ConfigInt>, pub subpass: LocationPacked<ConfigInt> }
#[derive(Debug, PartialEq)]
pub enum ExternalResourceData
{
	ImageView { dim: u8, refname: LocationPacked<String> }, SwapChainViews
}
#[derive(Debug, PartialEq)]
pub enum FramebufferStyle
{
	WithRenderPass(LocationPacked<ConfigInt>), Simple(Option<bool>), Presented(Option<bool>)
}
#[derive(Debug, PartialEq)]
pub struct FramebufferInfo { style: FramebufferStyle, views: Vec<LocationPacked<ConfigInt>> }
#[derive(Debug, PartialEq)]
pub enum BufferDescriptorOption { None, TexelStore, DynamicOffset }
#[derive(Debug, PartialEq)]
pub enum DescriptorEntryKind
{
	Sampler, CombinedSampler, SampledImage, StorageImage,
	UniformBuffer(BufferDescriptorOption), StorageBuffer(BufferDescriptorOption), InputAttachment
}
#[derive(Debug, PartialEq)]
pub struct DescriptorEntry { kind: DescriptorEntryKind, count: usize, visibility: VkShaderStageFlags }
#[derive(Debug, PartialEq)]
pub struct DescriptorSetLayoutData { entries: Vec<DescriptorEntry> }
#[derive(Debug, PartialEq)]
pub struct PushConstantLayout { range: LocationPacked<Range<usize>>, visibility: VkShaderStageFlags }
#[derive(Debug, PartialEq)]
pub struct PipelineLayout { descs: Vec<LocationPacked<ConfigInt>>, pushconstants: Vec<LocationPacked<ConfigInt>> }
#[derive(Debug, PartialEq)]
pub struct DescriptorSetsInfo(Vec<DescriptorSetEntry>);
#[derive(Debug, PartialEq)]
pub struct DescriptorSetEntry { name: Option<String>, layout: LocationPacked<ConfigInt> }
/// ShaderStage Info
#[derive(Debug, PartialEq)]
pub struct PipelineShaderStageInfo
{
	asset: LocationPacked<AssetResource>, consts: HashMap<u32, LocationPacked<NumericLiteral>>
}
/// Independent(declared out of pipeline state declaration) ShaderStage Info
#[derive(Debug, PartialEq)]
pub struct IndependentPipelineShaderStageInfo
{
	pub stage: VkShaderStageFlags, pub asset: LocationPacked<AssetResource>, pub consts: HashMap<u32, LocationPacked<NumericLiteral>>
}
/// For Vertex Shader Declaration
#[derive(Debug, PartialEq)]
pub struct PipelineShaderStageInfoVertex
{
	pub asset: LocationPacked<AssetResource>, pub consts: HashMap<u32, LocationPacked<NumericLiteral>>,
	pub vbindings: Vec<VertexBindingRegistry>, pub vattributes: Vec<VertexAttributeInfo>
}
#[derive(Debug, PartialEq)]
pub struct PipelineStateInfo
{
	renderpass: PreciseRenderPassRef, layout_ref: LocationPacked<ConfigInt>,
	vertex_shader: PipelineShaderStageInfoVertex, geometry_shader: Option<PipelineShaderStageInfo>, fragment_shader: Option<PipelineShaderStageInfo>,
	tesscontrol_shader: Option<PipelineShaderStageInfo>, tessevaluation_shader: Option<PipelineShaderStageInfo>,
	primitive_topology: VkPrimitiveTopology, viewport_scissors: Vec<ViewportScissorEntry>, blendstates: Vec<AttachmentBlendState>
}
#[derive(Debug, PartialEq)]
pub enum ViewportScissorEntry { ScreenView, Custom(VkViewport, VkRect2D) }
#[derive(Debug, PartialEq)]
pub struct VertexAttributeInfo { binding: u32, format: LocationPacked<Format>, offset: u32 }
#[derive(Debug, PartialEq)]
pub enum VertexBindingRegistry { PerVertex(Option<u32>), PerInstance(Option<u32>), Empty }

// Misc
#[derive(Debug, PartialEq)]
enum FramebufferRenderPassRef { Int(LocationPacked<ConfigInt>), Presented, None }
impl ParsedDeviceResources
{
	pub fn empty() -> Self
	{
		ParsedDeviceResources
		{
			renderpasses: NamedContents::new(), simple_rps: NamedContents::new(), presented_rps: NamedContents::new(),
			descriptor_set_layouts: NamedContents::new(), push_constant_layouts: NamedContents::new(),
			pipeline_layouts: NamedContents::new(), descriptor_sets: NamedContents::new(), pipeline_states: NamedContents::new(),
			externs: NamedContents::new(), framebuffers: NamedContents::new(), ind_shaders: NamedContents::new()
		}
	}
}

// --- Parse Error --- //
#[derive(Debug, PartialEq)]
pub enum ParseError
{
	NameRequired(usize), UnknownDeviceResource(usize), UnknownFormat(usize), UnknownImageLayout(usize), IntValueRequired(usize),
	UnknownRenderPassAttachmentOptions(usize), ImageLayoutRequired(usize), DirectionRequired(usize),
	NumericParseError(ParseIntError, usize), FloatingParseError(ParseFloatError, usize),
	DelimiterRequired(usize), ClosingRequired(usize), DefinitionOverrided, CorruptedSubpassDesc(usize),
	UnknownPipelineStageFlag(usize), UnknownAccessFlag(usize), Expected(Cow<'static, str>, usize), UnknownConfig(&'static str), ConfigRequired(&'static str),
	UnknownExternalResource(usize), UnknownClearMode(usize), FormatRequired(usize), UnknownObjectRef(&'static str, usize), UnknownShaderStageFlag(usize),
	UnknownDescriptorKind(usize), BytesizeRequired(usize), UnknownPrimitiveTopology(bool, usize),
	NameNotAllowed(usize), VertexBindingIntegrityCheckingFailure(u32)
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
			Err(ParseError::NameNotAllowed(p)) => panic!("Naming not allowed for the configuration at line {}, col {}", line, p),
			Err(ParseError::VertexBindingIntegrityCheckingFailure(bi)) => panic!("Vertex Binding #{} (at line {}) is declared more than once and parser could not confirm the integrity.")
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

impl FromSource for PreciseRenderPassRef
{
	fn object_name() -> Cow<'static, str> { "PreciseRenderPassRef".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		let inloc = source.current();
		ConfigInt::parse(source).and_then(|r| if source.drop_while(ignore_chars).front() == Some('.')
		{
			ConfigInt::parse(source.drop_opt(1).drop_while(ignore_chars)).map(|s| PreciseRenderPassRef { rp: r, subpass: s })
		}
		else { Err(ParseError::Expected("PreciseRenderPassRef".into(), inloc)) })
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
			let vport = Size3F::parse(source)
				.and_then(|vs| if source.drop_while(ignore_chars).front() != Some('-') { Err(ParseError::Expected("\"-\"".into(), source.current())) }
				else { Size3F::parse(source.drop_opt(1).drop_while(ignore_chars)).map(|ve| (vs, ve)) });
			
			vport.and_then(|(Size3F(sx, sy, sz), Size3F(dx, dy, dz))| if source.drop_while(ignore_chars).front() == Some(':')
			{
				Offset2::parse(source.drop_opt(1).drop_while(ignore_chars))
					.and_then(|Offset2(ssx, ssy)| if source.drop_while(ignore_chars).front() != Some('-')
					{
						Err(ParseError::Expected("\"-\"".into(), source.current()))
					}
					else
					{
						Offset2::parse(source.drop_opt(1).drop_while(ignore_chars))
							.map(|Offset2(sdx, sdy)| VkRect2D(VkOffset2D(ssx, ssy), VkExtent2D((sdx - ssx) as u32, (sdy - ssy) as u32)))
					})
			} else { Ok(VkRect2D(VkOffset2D(sx as i32, sy as i32), VkExtent2D((dx - sx) as u32, (dy - sy) as u32))) }
			.map(|sc| ViewportScissorEntry::Custom(VkViewport(sx, sy, dx - sx, dy - sy, sz, dz), sc)))
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
		let x = source.take_while(|c| c.is_digit(10));
		if x.is_empty() { Err(ParseError::IntValueRequired(x.current())) }
		else { x.clone_as_string().parse::<u32>().map_err(|e| ParseError::NumericParseError(e, x.current())) }
	}
}
impl FromSource for Size3F
{
	fn object_name() -> Cow<'static, str> { "Size3F".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// "(" f32 "," f32 "," f32 ")"
		if source.front() != Some('(') { Err(ParseError::Expected(Self::object_name(), source.current())) }
		else { f32::parse(source.drop_opt(1).drop_while(ignore_chars)) }
		.and_then(|x| if source.drop_while(ignore_chars).front() != Some(',') { Err(ParseError::Expected("\",\"".into(), source.current())) }
		else { f32::parse(source.drop_opt(1).drop_while(ignore_chars)).map(|y| (x, y)) })
		.and_then(|(x, y)| if source.drop_while(ignore_chars).front() != Some(',') { Err(ParseError::Expected("\",\"".into(), source.current())) }
		else { f32::parse(source.drop_opt(1).drop_while(ignore_chars)).map(|z| Size3F(x, y, z)) })
		.and_then(|v| if source.drop_while(ignore_chars).front() != Some(')') { Err(ParseError::ClosingRequired(source.current())) }
		else { Ok(v) })
	}
}
impl FromSource for Offset2
{
	fn object_name() -> Cow<'static, str> { "Offset2".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// "(" i32 "," i32 ")"
		if source.front() != Some('(') { Err(ParseError::Expected(Self::object_name(), source.current())) }
		else { i32::parse(source.drop_opt(1).drop_while(ignore_chars)) }
		.and_then(|x| if source.drop_while(ignore_chars).front() != Some(',') { Err(ParseError::Expected("\",\"".into(), source.current())) }
		else { i32::parse(source.drop_opt(1).drop_while(ignore_chars)).map(|y| Offset2(x, y)) })
		.and_then(|v| if source.drop_while(ignore_chars).front() != Some(')') { Err(ParseError::ClosingRequired(source.current())) } else { Ok(v) })
	}
}

pub fn parse_device_resources(sink: &mut ParsedDeviceResources, includes: &mut Vec<LocationPacked<PathBuf>>, lines: &mut LazyLines)
{
	while let Some(mut source) = acquire_line(lines, 0)
	{
		let insource = source.current();
		let NamedConfigLine { name, .. } = NamedConfigLine::parse_noargs(&mut source).report_error(source.line());
		let s = source.drop_while(ignore_chars).take_until(ident_break);
		match s.clone_as_string().as_ref()
		{
			// Include <StringLiteral>
			"Include" => String::parse(source.drop_while(ignore_chars)).map(PartialApply1!(LocationPacked::rewrap; From::from))
				.and_then(|p| if name.is_some() { Err(ParseError::NameNotAllowed(insource)) } else { Ok(includes.push(p)) }).report_error(source.line()),
			"RenderPass" =>
			{
				let p = RenderPassData::parse(&mut source, lines);
				if let Some(name) = name { sink.renderpasses.insert(name.into(), p); } else { sink.renderpasses.insert_unnamed(p); }
			},
			"SimpleRenderPass" =>
			{
				let p = SimpleRenderPassData::parse(&mut source, lines);
				if let Some(name) = name { sink.simple_rps.insert(name.into(), p); } else { sink.simple_rps.insert_unnamed(p); }
			},
			"PresentedRenderPass" =>
			{
				let p = PresentedRenderPassData::parse(&mut source, lines);
				if let Some(name) = name { sink.presented_rps.insert(name.into(), p); } else { sink.presented_rps.insert_unnamed(p); }
			},
			"DescriptorSetLayout" =>
			{
				let p = DescriptorSetLayoutData::parse(&mut source, lines);
				if let Some(name) = name { sink.descriptor_set_layouts.insert(name.into(), p); } else { sink.descriptor_set_layouts.insert_unnamed(p); }
			},
			"PushConstantLayout" =>
			{
				let p = PushConstantLayout::parse(&mut source, lines);
				if let Some(name) = name { sink.push_constant_layouts.insert(name.into(), p); } else { sink.push_constant_layouts.insert_unnamed(p); }
			},
			"PipelineLayout" =>
			{
				let p = PipelineLayout::parse(&mut source, lines);
				if let Some(name) = name { sink.pipeline_layouts.insert(name.into(), p); } else { sink.pipeline_layouts.insert_unnamed(p); }
			},
			"DescriptorSets" =>
			{
				let p = DescriptorSetsInfo::parse(&mut source, lines);
				if let Some(name) = name { sink.descriptor_sets.insert(name.into(), p); } else { sink.descriptor_sets.insert_unnamed(p); }
			},
			"PipelineState" =>
			{
				let p = PipelineStateInfo::parse(source.drop_while(ignore_chars), lines);
				if let Some(name) = name { sink.pipeline_states.insert(name.into(), p); } else { sink.pipeline_states.insert_unnamed(p); }
			},
			"Extern" =>
			{
				let p = ExternalResourceData::parse(source.drop_while(ignore_chars)).report_error(source.line());
				if let Some(name) = name { sink.externs.insert(name.into(), p); } else { sink.externs.insert_unnamed(p); }
			},
			"Framebuffer" =>
			{
				let p = FramebufferInfo::parse(source.drop_while(ignore_chars), lines);
				if let Some(name) = name { sink.framebuffers.insert(name.into(), p); } else { sink.framebuffers.insert_unnamed(p); }
			},
			"VertexShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_VERTEX_BIT, asset: asset, consts: consts };
				if let Some(name) = name { sink.ind_shaders.insert(name.into(), ss); } else { sink.ind_shaders.insert_unnamed(ss); }
			},
			"FragmentShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_FRAGMENT_BIT, asset: asset, consts: consts };
				if let Some(name) = name { sink.ind_shaders.insert(name.into(), ss); } else { sink.ind_shaders.insert_unnamed(ss); }
			},
			"GeometryShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_GEOMETRY_BIT, asset: asset, consts: consts };
				if let Some(name) = name { sink.ind_shaders.insert(name.into(), ss); } else { sink.ind_shaders.insert_unnamed(ss); }
			},
			"TessellationControlShader" | "TessControlShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT, asset: asset, consts: consts };
				if let Some(name) = name { sink.ind_shaders.insert(name.into(), ss); } else { sink.ind_shaders.insert_unnamed(ss); }
			},
			"TessellationEvaluationShader" | "TessEvaluationShader" =>
			{
				let PipelineShaderStageInfo { asset, consts } = PipelineShaderStageInfo::parse_baseindent(source.drop_while(ignore_chars), lines, 0);
				let ss = IndependentPipelineShaderStageInfo { stage: VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT, asset: asset, consts: consts };
				if let Some(name) = name { sink.ind_shaders.insert(name.into(), ss); } else { sink.ind_shaders.insert_unnamed(ss); }
			},
			_ => Err(ParseError::UnknownDeviceResource(s.current())).report_error(source.line())
		};
	}
}
impl FromSource for ExternalResourceData
{
	fn object_name() -> Cow<'static, str> { "ExternalResourceData".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		fn image_dimension(input: &ParseLine) -> Result<u8, ParseError>
		{
			match input.front().and_then(|c| c.to_digit(10))
			{
				Some(d) if input.peek(1) == Some('D') => Ok(d as u8),
				_ => Err(ParseError::Expected("Image Dimension".into(), input.current()))
			}
		}

		let s = source.take_until(ident_break);
		match s.clone_as_string().as_ref()
		{
			"ImageView" =>
			{
				let d = source.drop_while(ignore_chars).take_until(ident_break);
				image_dimension(&d).and_then(|d|
					String::parse(source.drop_while(ignore_chars)).map(|n| ExternalResourceData::ImageView { dim: d, refname: n }))
			},
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
						match NamedConfigLine::parse(&mut s, RPAttachment::parse).report_error(s.line())
						{
							NamedConfigLine { name: Some(name), config } => { rpd.attachments.insert(name.into(), config); },
							NamedConfigLine { config, .. } => { rpd.attachments.insert_unnamed(config); }
						}
					}
					Ok(())
				},
				SUBPASSES[..] => if !rpd.subpasses.is_empty() { Err(ParseError::DefinitionOverrided) }
				else
				{
					while let Some(mut s) = acquire_line(source, 2)
					{
						match NamedConfigLine::parse(&mut s, RPSubpassDesc::parse).report_error(s.line())
						{
							NamedConfigLine { name: Some(name), config } => { rpd.subpasses.insert(name.into(), config); },
							NamedConfigLine { config, .. } => { rpd.subpasses.insert_unnamed(config); }
						}
					}
					Ok(())
				},
				DEPENDENCIES[..] => if !rpd.deps.is_empty() { Err(ParseError::DefinitionOverrided) }
				else
				{
					while let Some(mut s) = acquire_line(source, 2)
					{
						RPSubpassDeps::parse(&mut s).map(|c| rpd.deps.push(c)).report_error(s.line());
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
		Format::parse(source).and_then(|format|
		{
			if source.drop_while(ignore_chars).front() != Some(',') { Err(ParseError::DelimiterRequired(source.current())) }
			else
			{
				Transition::parse_opt(source.drop_opt(1).drop_while(ignore_chars), |x| parse_image_layout(x).map(LocationPacked::unwrap)).map(|layouts| (format, layouts))
			}
		}).map(|(format, layouts)| RPAttachment { format: format, layouts: layouts, clear_on_load: None, preserve_content: false })
		.and_then(|mut rpa|
		{
			if source.drop_while(ignore_chars).front() != Some(',') { Ok(rpa) }
			else
			{
				fn recursive(rpa: &mut RPAttachment, source: &mut ParseLine) -> Result<(), ParseError>
				{
					if source.is_empty() { Ok(()) }
					else if source.starts_with_trailing_opt(&RPO_CLEAR_ON_LOAD, ident_break)
					{
						rpa.clear_on_load = Some(true);
						if source.drop_opt(RPO_CLEAR_ON_LOAD.len()).drop_while(ignore_chars).front() == Some('/')
						{
							recursive(rpa, source.drop_opt(1).drop_while(ignore_chars))
						}
						else { Ok(()) }
					}
					else if source.starts_with_trailing_opt(&RPO_LOAD_CONTENT, ident_break)
					{
						rpa.clear_on_load = Some(false);
						if source.drop_opt(RPO_LOAD_CONTENT.len()).drop_while(ignore_chars).front() == Some('/')
						{
							recursive(rpa, source.drop_opt(1).drop_while(ignore_chars))
						}
						else { Ok(()) }
					}
					else if source.starts_with_trailing_opt(&RPO_PRESERVE_CONTENT, ident_break)
					{
						rpa.preserve_content = true;
						if source.drop_opt(RPO_PRESERVE_CONTENT.len()).drop_while(ignore_chars).front() == Some('/')
						{
							recursive(rpa, source.drop_opt(1).drop_while(ignore_chars))
						}
						else { Ok(()) }
					}
					else { Err(ParseError::UnknownRenderPassAttachmentOptions(source.current())) }
				}
				recursive(&mut rpa, source.drop_opt(1).drop_while(ignore_chars)).map(|_| rpa)
			}
		})
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
		fn recursive<'s>(input: &mut ParseLine<'s>, sink: &mut RPSubpassDesc) -> Result<(), ParseError>
		{
			if input.is_empty() { Ok(()) }
			else if input.starts_with_trailing_opt(&SDI_RENDER_TO, ident_break)
			{
				// RenderTo int/[ints...]
				ConfigInt::parse_array(input.drop_opt(SDI_RENDER_TO.len()).drop_while(ignore_chars)).and_then(|v|
				{
					if sink.color_outs.is_empty()
					{
						sink.color_outs = v;
						recursive(input.drop_while(ignore_chars), sink)
					}
					else { Err(ParseError::DefinitionOverrided) }
				})
			}
			else if from_token(input)
			{
				// From int/[ints...]
				ConfigInt::parse_array(input.drop_while(ignore_chars)).and_then(|v|
				{
					if sink.inputs.is_empty()
					{
						sink.inputs = v;
						recursive(input.drop_while(ignore_chars), sink)
					}
					else { Err(ParseError::DefinitionOverrided) }
				})
			}
			else { Err(ParseError::CorruptedSubpassDesc(input.current())) }
		}
		let mut rpsd = RPSubpassDesc { color_outs: Vec::new(), inputs: Vec::new() };
		recursive(source, &mut rpsd).map(|_| rpsd)
	}
}
lazy_static!
{
	static ref BY_REGION: Vec<char> = "ByRegion".chars().collect();
}
impl FromSource for RPSubpassDeps
{
	fn object_name() -> Cow<'static, str> { "RenderPass Subpass Dependencies".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// int (From/To) int ":" transition#access_mask At stage_bits ["," ["ByRegion"]]
		Transition::parse(source, ConfigInt::parse).and_then(|pt|
		{
			if source.drop_while(ignore_chars).front() == Some(':') { source.drop_opt(1).drop_while(ignore_chars); Ok(pt) }
			else { Err(ParseError::DelimiterRequired(source.current())) }
		})
		.and_then(|pt| Transition::parse(source, parse_access_mask).map(|amt| (pt, amt)))
		.and_then(|(pt, amt)|
		{
			if at_token(source.drop_while(ignore_chars))
			{
				parse_pipeline_stage_bits(source.drop_while(ignore_chars)).map(|sf| (pt, amt, sf))
			}
			else { Err(ParseError::DelimiterRequired(source.current())) }
		})
		.map(|(pt, amt, sf)|
		{
			if source.drop_while(ignore_chars).front() == Some(',')
			{
				let mut r2 = source.clone();
				r2.drop_opt(1).drop_while(ignore_chars);
				if r2.starts_with_trailing_opt(&BY_REGION, ident_break)
				{
					*source = r2;
					RPSubpassDeps { passtrans: pt, access_mask: amt, stage_bits: sf, by_region: true }
				}
				else
				{
					RPSubpassDeps { passtrans: pt, access_mask: amt, stage_bits: sf, by_region: false }
				}
			}
			else { RPSubpassDeps { passtrans: pt, access_mask: amt, stage_bits: sf, by_region: false } }
		})
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
				FORMAT[..] => Format::parse(s.drop_while(ignore_chars))
					.and_then(|f| if fmt.is_none() { fmt = Some(f); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
				CLEARMODE[..] => parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () });
				_ => Err(ParseError::UnknownConfig("SimpleRenderPass"))
			})).report_error(s.line());
		}
		if fmt.is_none() { Err(ParseError::ConfigRequired("Format")).report_error(enterline.line()) }
		else
		{
			SimpleRenderPassData { format: fmt.unwrap(), clear_on_load: clear_mode }
		}
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
				FORMAT[..] => Format::parse(s.drop_while(ignore_chars))
					.and_then(|f| if fmt.is_none() { fmt = Some(f); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
				CLEARMODE[..] => parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () });
				_ => Err(ParseError::UnknownConfig("PresentedRenderPass"))
			})).report_error(s.line());
		}
		if fmt.is_none() { Err(ParseError::ConfigRequired("Format")).report_error(enterline.line()) }
		else
		{
			PresentedRenderPassData { format: fmt.unwrap(), clear_on_load: clear_mode }
		}
	}
}
lazy_static!
{
	static ref PRESENTED: Vec<char> = "Presented".chars().collect();
}
impl FromSource for FramebufferRenderPassRef
{
	fn object_name() -> Cow<'static, str> { "<FRenderPassRef>".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// "<" ("Presented" / int) ">" / <EMPTY>
		if source.front() == Some('<')
		{
			// Which Parameter: "Presented" / int
			source.drop_opt(1).drop_while(ignore_chars);
			let p = if source.front().map(|c| ('0' <= c && c <= '9') || c == '$').unwrap_or(false)
			{
				// int
				ConfigInt::parse(source).map(FramebufferRenderPassRef::Int)
			}
			else if source.starts_with_trailing_opt(&PRESENTED, ident_break)
			{
				source.drop_opt(PRESENTED.len());
				Ok(FramebufferRenderPassRef::Presented)
			}
			else { Err(ParseError::UnknownObjectRef("RenderPass", source.current())) };
			
			p.and_then(|v|
			{
				if source.drop_while(ignore_chars).front() == Some('>') { source.drop_opt(1); Ok(v) }
				else { Err(ParseError::ClosingRequired(source.current())) }
			})
		}
		else { Ok(FramebufferRenderPassRef::None) }
	}
}
impl FromSourceBlock for FramebufferInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		let (arg, vs) = FramebufferRenderPassRef::parse(enterline.drop_while(ignore_chars))
			.and_then(|arg| ConfigInt::parse_array(enterline.drop_while(ignore_chars)).map(|vs| (arg, vs))).report_error(enterline.line());
		
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
			entries.push(DescriptorEntry::parse(&mut s).report_error(s.line()));
		}
		DescriptorSetLayoutData { entries: entries }
	}
}
impl FromSource for DescriptorEntry
{
	fn object_name() -> Cow<'static, str> { "DescriptorSet Entry".into() }
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		fn descriptor_entry_kind(input: ParseLine) -> Result<DescriptorEntryKind, ParseError>
		{
			match input.clone_as_string().as_ref()
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
				_ => Err(ParseError::UnknownDescriptorKind(input.current()))
			}
		}

		let count = if source.front().map(|c| '0' <= c && c <= '9').unwrap_or(false)
		{
			// count type : visibility
			let count_str = source.take_while(|c| '0' <= c && c <= '9');
			assert!(!count_str.is_empty());
			count_str.clone_as_string().parse::<usize>().map_err(|e| ParseError::NumericParseError(e, count_str.current()))
		}
		else { Ok(1) };
		count.and_then(|count| acquire_config_name(source.drop_while(ignore_chars)).and_then(descriptor_entry_kind)
			.and_then(|ntype| parse_shader_stage_bits(source.drop_while(ignore_chars)).map(|ss| DescriptorEntry
			{
				count: count, kind: ntype, visibility: ss.unwrap()
			})))
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
				RANGE[..] => parse_usize_range(s.drop_while(ignore_chars)).and_then(|r| assign_check_overriding(&mut range, r)),
				VISIBILITY[..] => parse_shader_stage_bits(s.drop_while(ignore_chars)).and_then(|v| assign_check_overriding(&mut vis, v));
				_ => Err(ParseError::UnknownConfig("PushConstantLayout"))
			})).report_error(s.line());
		}
		range.ok_or(ParseError::ConfigRequired("Range")).and_then(|range|
		vis.ok_or(ParseError::ConfigRequired("Visibility")).map(|vis| PushConstantLayout { range: range, visibility: vis.unwrap() }))
			.report_error(enterline.line())
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
			acquire_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
			{
				DESCRIPTORS[..] => ConfigInt::parse_array(s.drop_while(ignore_chars)).and_then(|cv| vassign_check_overriding(&mut desc, cv)),
				PUSHCONSTANTLAYOUTS[..] => ConfigInt::parse_array(s.drop_while(ignore_chars)).and_then(|cv| vassign_check_overriding(&mut pcls, cv));
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
			entries.push(NamedConfigLine::parse(&mut s, ConfigInt::parse)
				.map(|NamedConfigLine { name, config }| DescriptorSetEntry { name: name, layout: config }).report_error(s.line()));
		}
		DescriptorSetsInfo(entries)
	}
}
impl FromSourceBlock for PipelineStateInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		// "PipelineState" v"for" precise_rpref "with" int
		let r = if enterline.starts_with_trailing_opt(&['f', 'o', 'r'], ident_break)
		{
			PreciseRenderPassRef::parse(enterline.drop_opt(3).drop_while(ignore_chars))
		}
		else { Err(ParseError::Expected("\"for\"".into(), enterline.current())) };
		let (rp, l) = r.and_then(|rp| if enterline.drop_while(ignore_chars).starts_with_trailing_opt(&['w', 'i', 't', 'h'], ident_break)
		{
			ConfigInt::parse(enterline.drop_opt(4).drop_while(ignore_chars)).map(|l| (rp, l))
		} else { Err(ParseError::Expected("\"with\"".into(), enterline.current())) }).report_error(enterline.line());

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
					assign_check_overriding(&mut vsinfo, PipelineShaderStageInfoVertex::parse(s.drop_while(ignore_chars), source)),
				"FragmentShader" =>
					assign_check_overriding(&mut shaderinfo.fragment, PipelineShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"GeometryShader" =>
					assign_check_overriding(&mut shaderinfo.geometry, PipelineShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"TessellationControlShader" =>
					assign_check_overriding(&mut shaderinfo.tesscontrol, PipelineShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"TessellationEvaluationShader" =>
					assign_check_overriding(&mut shaderinfo.tessevaluation, PipelineShaderStageInfo::parse(s.drop_while(ignore_chars), source)),
				"PrimitiveTopology" => parse_primitive_topology(s.drop_while(ignore_chars)).and_then(|p| assign_check_overriding(&mut primt, p)),
				"ViewportScissors" => parse_viewport_scissors(s.drop_while(ignore_chars), source).and_then(|v| vassign_check_overriding(&mut vpsc, v)),
				"BlendStates" => AttachmentBlendState::parse_array(s.drop_while(ignore_chars)).and_then(|v| vassign_check_overriding(&mut blends, v)),
				_ => Err(ParseError::UnknownConfig("PipelineState"))
			}).report_error(s.line());
		}
		vsinfo.ok_or(ParseError::ConfigRequired("VertexShader")).and_then(move |vsinfo|
		primt.ok_or(ParseError::ConfigRequired("PrimitiveTopology")).map(move |primt| PipelineStateInfo
		{
			renderpass: rp, layout_ref: l,
			vertex_shader: vsinfo, fragment_shader: shaderinfo.fragment, geometry_shader: shaderinfo.geometry,
			tesscontrol_shader: shaderinfo.tesscontrol, tessevaluation_shader: shaderinfo.tessevaluation,
			primitive_topology: primt, viewport_scissors: vpsc, blendstates: blends
		})).report_error(enterline.line())
	}
}
impl FromSourceBlock for PipelineShaderStageInfo
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self
	{
		Self::parse_baseindent(enterline, source, 1)
	}
}
impl PipelineShaderStageInfo
{
	fn parse_baseindent(enterline: &mut ParseLine, source: &mut LazyLines, baseindent: usize) -> Self
	{
		let asset = AssetResource::parse(enterline).report_error(enterline.line());

		let mut consts = HashMap::new();
		while let Some(mut s) = acquire_line(source, baseindent + 1)
		{
			if s.starts_with(&['C', 'o', 'n', 's', 't', 'a', 'n', 't'])
			{
				let n = s.drop_opt(8).drop_while(ignore_chars).take_while(|c| c.is_digit(10));
				if n.is_empty() { Err(ParseError::IntValueRequired(n.current())) }
				else
				{
					n.clone_as_string().parse::<u32>().map_err(|e| ParseError::NumericParseError(e, n.current()))
						.and_then(|n| if s.drop_while(ignore_chars).front() == Some(':')
						{
							NumericLiteral::parse(s.drop_opt(1).drop_while(ignore_chars), true).and_then(|v| if consts.contains_key(&n)
							{
								Err(ParseError::DefinitionOverrided)
							}
							else { consts.insert(n, v); Ok(()) })
						}
						else { Err(ParseError::Expected(":".into(), s.current())) })
				}
			}
			else { Err(ParseError::UnknownConfig("Shader Module")) }.report_error(s.line());
		}
		PipelineShaderStageInfo { asset: asset, consts: consts }
	}
}
impl FromSourceBlock for PipelineShaderStageInfoVertex
{
	fn parse(enterline: &mut ParseLine, source: &mut LazyLines) -> Self { Self::parse_baseindent(enterline, source, 1) }
}
impl PipelineShaderStageInfoVertex
{
	fn parse_baseindent(enterline: &mut ParseLine, source: &mut LazyLines, baseindent: usize) -> Self
	{
		use std::collections::hash_map::Entry;
		let asset = AssetResource::parse(enterline).report_error(enterline.line());

		let mut consts = HashMap::new();
		let (mut vb_registry, mut max_binding) = (HashMap::new(), 0);
		let mut valist = Vec::new();
		while let Some(mut s) = acquire_line(source, baseindent + 1)
		{
			if s.starts_with(&['P', 'e', 'r', 'I', 'n', 's', 't', 'a', 'n', 'c', 'e']) || s.front() == Some('i')
			{
				let stride_opt = if if s.front() == Some('i') { s.drop_opt(1) } else { s.drop_opt(11) }.drop_while(ignore_chars).front() == Some('(')
				{
					// Structure Stride
					u32::parse(s.drop_opt(1).drop_while(ignore_chars)).and_then(|n|
						if s.drop_while(ignore_chars).front() == Some(')') { s.drop_opt(1); Ok(Some(n)) } else { Err(ParseError::ClosingRequired(s.current())) })
				}
				else { Ok(None) };

				stride_opt.and_then(|stride_opt|
				{
					u32::parse(s.drop_while(ignore_chars)).and_then(|bindex| if s.drop_while(ignore_chars).front() == Some(':')
					{
						Format::parse(s.drop_opt(1).drop_while(ignore_chars)).map(|form| (bindex, form))
					} else { Err(ParseError::DelimiterRequired(s.current())) }).and_then(|(bindex, form)| if s.drop_while(ignore_chars).front() == Some('(')
					{
						u32::parse(s.drop_opt(1).drop_while(ignore_chars)).and_then(|offs|
							if s.drop_while(ignore_chars).front() == Some(')') { s.drop_opt(1); Ok((stride_opt, bindex, form, offs)) }
							else { Err(ParseError::ClosingRequired(s.current())) }
						)
					} else { Err(ParseError::Expected("Offset Bytes".into(), s.current())) })
				}).and_then(|(stride_opt, bindex, form, offs)|
				{
					max_binding = std::cmp::max(max_binding, bindex);
					match vb_registry.entry(bindex)
					{
						Entry::Vacant(v) => { v.insert(VertexBindingRegistry::PerInstance(stride_opt)); Ok(()) },
						Entry::Occupied(mut o) => match o.get_mut()
						{
							&mut VertexBindingRegistry::PerInstance(ref mut st) => if (*st == None && stride_opt != None) || *st == stride_opt
							{
								*st = stride_opt;
								Ok(())
							}
							else
							{
								Err(ParseError::VertexBindingIntegrityCheckingFailure(bindex))
							},
							&mut VertexBindingRegistry::PerVertex(_) => Err(ParseError::VertexBindingIntegrityCheckingFailure(bindex)),
							_ => unreachable!()
						}
					}.and_then(|_| Ok(valist.push(VertexAttributeInfo { binding: bindex, format: form, offset: offs })))
				})
			}
			else if s.starts_with(&['P', 'e', 'r', 'V', 'e', 'r', 't', 'e', 'x']) || s.front() == Some('v')
			{
				let stride_opt = if if s.front() == Some('v') { s.drop_opt(1) } else { s.drop_opt(9) }.drop_while(ignore_chars).front() == Some('(')
				{
					// Structure Stride
					u32::parse(s.drop_opt(1).drop_while(ignore_chars)).and_then(|n|
						if s.drop_while(ignore_chars).front() == Some(')') { s.drop_opt(1); Ok(Some(n)) } else { Err(ParseError::ClosingRequired(s.current())) })
				}
				else { Ok(None) };

				stride_opt.and_then(|stride_opt|
				{
					u32::parse(s.drop_while(ignore_chars)).and_then(|bindex| if s.drop_while(ignore_chars).front() == Some(':')
					{
						Format::parse(s.drop_opt(1).drop_while(ignore_chars)).map(|form| (bindex, form))
					} else { Err(ParseError::DelimiterRequired(s.current())) }).and_then(|(bindex, form)| if s.drop_while(ignore_chars).front() == Some('(')
					{
						u32::parse(s.drop_opt(1).drop_while(ignore_chars)).and_then(|offs|
							if s.drop_while(ignore_chars).front() == Some(')') { s.drop_opt(1); Ok((stride_opt, bindex, form, offs)) }
							else { Err(ParseError::ClosingRequired(s.current())) }
						)
					} else { Err(ParseError::Expected("Offset Bytes".into(), s.current())) })
				}).and_then(|(stride_opt, bindex, form, offs)|
				{
					max_binding = std::cmp::max(max_binding, bindex);
					match vb_registry.entry(bindex)
					{
						Entry::Vacant(v) => { v.insert(VertexBindingRegistry::PerVertex(stride_opt)); Ok(()) },
						Entry::Occupied(mut o) => match o.get_mut()
						{
							&mut VertexBindingRegistry::PerVertex(ref mut st) => if (*st == None && stride_opt != None) || *st == stride_opt
							{
								*st = stride_opt;
								Ok(())
							}
							else
							{
								Err(ParseError::VertexBindingIntegrityCheckingFailure(bindex))
							},
							&mut VertexBindingRegistry::PerInstance(_) => Err(ParseError::VertexBindingIntegrityCheckingFailure(bindex)),
							_ => unreachable!()
						}
					}.and_then(|_| Ok(valist.push(VertexAttributeInfo { binding: bindex, format: form, offset: offs })))
				})
			}
			else if let Some('0' ... '9') = s.front()
			{
				// PerVertex, auto-structure strides
				u32::parse(&mut s).and_then(|bindex| if s.drop_while(ignore_chars).front() == Some(':')
				{
					Format::parse(s.drop_opt(1).drop_while(ignore_chars)).map(|form| (bindex, form))
				} else { Err(ParseError::DelimiterRequired(s.current())) }).and_then(|(bindex, form)| if s.drop_while(ignore_chars).front() == Some('(')
				{
					u32::parse(s.drop_opt(1).drop_while(ignore_chars)).and_then(|offs|
						if s.drop_while(ignore_chars).front() == Some(')') { s.drop_opt(1); Ok((bindex, form, offs)) }
						else { Err(ParseError::ClosingRequired(s.current())) }
					)
				} else { Err(ParseError::Expected("Offset Bytes".into(), s.current())) })
				.and_then(|(bindex, form, offs)|
				{
					max_binding = std::cmp::max(max_binding, bindex);
					match vb_registry.entry(bindex)
					{
						Entry::Vacant(v) => { v.insert(VertexBindingRegistry::PerVertex(None)); Ok(()) },
						Entry::Occupied(o) => match o.get()
						{
							&VertexBindingRegistry::PerInstance(_) => Err(ParseError::VertexBindingIntegrityCheckingFailure(bindex)),
							_ => Ok(())
						}
					}.and_then(|_| Ok(valist.push(VertexAttributeInfo { binding: bindex, format: form, offset: offs })))
				})
			}
			else if s.starts_with(&['C', 'o', 'n', 's', 't', 'a', 'n', 't'])
			{
				let n = s.drop_opt(8).drop_while(ignore_chars).take_while(|c| c.is_digit(10));
				if n.is_empty() { Err(ParseError::IntValueRequired(n.current())) }
				else
				{
					n.clone_as_string().parse::<u32>().map_err(|e| ParseError::NumericParseError(e, n.current()))
						.and_then(|n| if s.drop_while(ignore_chars).front() == Some(':')
						{
							NumericLiteral::parse(s.drop_opt(1).drop_while(ignore_chars), true).and_then(|v| if consts.contains_key(&n)
							{
								Err(ParseError::DefinitionOverrided)
							}
							else { consts.insert(n, v); Ok(()) })
						}
						else { Err(ParseError::Expected(":".into(), s.current())) })
				}
			}
			else { Err(ParseError::UnknownConfig("Shader Module")) }.report_error(s.line());
		}
		let mut vbs = (0 .. max_binding + 1).map(|n| vb_registry.remove(&n).unwrap_or(VertexBindingRegistry::Empty)).collect();
		PipelineShaderStageInfoVertex { asset: asset, consts: consts, vbindings: vbs, vattributes: valist }
	}
}
fn parse_primitive_topology(source: &mut ParseLine) -> Result<VkPrimitiveTopology, ParseError>
{
	// ident ["with" "Adjacency"]
	let prim = source.take_until(ident_break);
	if prim.is_empty() { Err(ParseError::Expected("Primitive Topology".into(), prim.current())) }
	else if source.drop_while(ignore_chars).starts_with_trailing_opt(&['w', 'i', 't', 'h'], ident_break)
	{
		if source.drop_opt(4).drop_while(ignore_chars).starts_with_trailing_opt(&['A', 'd', 'j', 'a', 'c', 'e', 'n', 'c', 'y'], ident_break)
		{
			match prim.clone_as_string().as_ref()
			{
				"LineList" => Ok(VkPrimitiveTopology::LineListWithAdjacency),
				"LineStrip" => Ok(VkPrimitiveTopology::LineStripWithAdjacency),
				"TriangleList" => Ok(VkPrimitiveTopology::TriangleListWithAdjacency),
				"TriangleStrip" => Ok(VkPrimitiveTopology::TriangleStripWithAdjacency),
				_ => Err(ParseError::UnknownPrimitiveTopology(true, prim.current()))
			}
		} else { Err(ParseError::Expected("\"Adjacency\"".into(), source.current())) }
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

pub fn from_token(input: &mut ParseLine) -> bool
{
	if input.starts_with_trailing_opt(&['F', 'r', 'o', 'm'], ident_break) { input.drop_opt(4); true }
	else if input.starts_with(&['<', '-']) { input.drop_opt(2); true }
	else { false }
}
pub fn at_token(input: &mut ParseLine) -> bool
{
	if input.starts_with_trailing_opt(&['A', 't'], ident_break) { input.drop_opt(2); true }
	else if input.front() == Some('@') { input.drop_opt(1); true }
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
			PreciseRenderPassRef::parse; "0.1"
				=> Ok(PreciseRenderPassRef { rp: LocationPacked(1, 0, ConfigInt::Value(0)), subpass: LocationPacked(1, 2, ConfigInt::Value(1)) }),
			PreciseRenderPassRef::parse; "$First .1"
				=> Ok(PreciseRenderPassRef { rp: LocationPacked(1, 0, ConfigInt::Ref("First".into())), subpass: LocationPacked(1, 8, ConfigInt::Value(1)) }),
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
			Size3F::parse; "(0.0 " => Err(ParseError::Expected("\",\"".into(), 5)),
			Size3F::parse; "(0, 0.0, 0.0" => Err(ParseError::ClosingRequired(12)),
			Offset2::parse; "(0, 0)" => Ok(Offset2(0, 0)),
			Offset2::parse; "0" => Err(ParseError::Expected("Offset2".into(), 0)),
			Offset2::parse; "(0 " => Err(ParseError::Expected("\",\"".into(), 3)),
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
				kind: DescriptorEntryKind::UniformBuffer(BufferDescriptorOption::None), count: 1, visibility: VK_SHADER_STAGE_VERTEX_BIT
			}),
			DescriptorEntry::parse; "2 CombinedSampler : Geometry / Fragment" => Ok(DescriptorEntry
			{
				kind: DescriptorEntryKind::CombinedSampler, count: 2, visibility: VK_SHADER_STAGE_GEOMETRY_BIT | VK_SHADER_STAGE_FRAGMENT_BIT
			}),
			DescriptorEntry::parse; "a: Vertex" => Err(ParseError::UnknownDescriptorKind(0))
		}
	}
	#[test] fn primitive_topology()
	{
		Testing!
		{
			parse_primitive_topology; "PointList" => Ok(VkPrimitiveTopology::PointList),
			parse_primitive_topology; "LineStrip with Adjacency" => Ok(VkPrimitiveTopology::LineStripWithAdjacency),
			parse_primitive_topology; "PatchList with Adjacency" => Err(ParseError::UnknownPrimitiveTopology(true, 0)),
			parse_primitive_topology; "PointStrip" => Err(ParseError::UnknownPrimitiveTopology(false, 0)),
			parse_primitive_topology; "" => Err(ParseError::Expected("Primitive Topology".into(), 0))
		}
	}
	#[test] fn test_subpass_deps()
	{
		Testing!
		{
			RPSubpassDeps::parse; "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage, ByRegion" => Ok(RPSubpassDeps
			{
				passtrans: Transition { from: LocationPacked(1, 0, ConfigInt::Value(0)), to: LocationPacked(1, 5, ConfigInt::Value(1)) },
				access_mask: Transition { from: LocationPacked(1, 8, VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT), to: LocationPacked(1, 32, VK_ACCESS_SHADER_READ_BIT) },
				stage_bits: LocationPacked(1, 45, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT), by_region: true
			}),
			RPSubpassDeps::parse; "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage" => Ok(RPSubpassDeps
			{
				passtrans: Transition { from: LocationPacked(1, 0, ConfigInt::Value(0)), to: LocationPacked(1, 5, ConfigInt::Value(1)) },
				access_mask: Transition { from: LocationPacked(1, 8, VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT), to: LocationPacked(1, 32, VK_ACCESS_SHADER_READ_BIT) },
				stage_bits: LocationPacked(1, 45, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT), by_region: false
			}),
			RPSubpassDeps::parse; "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage," => Ok(RPSubpassDeps
			{
				passtrans: Transition { from: LocationPacked(1, 0, ConfigInt::Value(0)), to: LocationPacked(1, 5, ConfigInt::Value(1)) },
				access_mask: Transition { from: LocationPacked(1, 8, VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT), to: LocationPacked(1, 32, VK_ACCESS_SHADER_READ_BIT) },
				stage_bits: LocationPacked(1, 45, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT), by_region: false
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
				format: LocationPacked(1, 0, Format::Value(VkFormat::R8G8B8A8_UNORM)), preserve_content: true, clear_on_load: None,
				layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
			}),
			RPAttachment::parse; "R32 SFLOAT, ShaderReadOnlyOptimal <- ColorAttachmentOptimal" => Ok(RPAttachment
			{
				format: LocationPacked(1, 0, Format::Value(VkFormat::R32_SFLOAT)), preserve_content: false, clear_on_load: None,
				layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
			}),
			RPAttachment::parse; "R8G8B8A8 UNORM, ShaderReadOnlyOptimal, ClearOnLoad" => Ok(RPAttachment
			{
				format: LocationPacked(1, 0, Format::Value(VkFormat::R8G8B8A8_UNORM)), preserve_content: false, clear_on_load: Some(true),
				layouts: Transition { from: VkImageLayout::ShaderReadOnlyOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
			}),
			RPAttachment::parse; "R8G8B8A8 SNORM, ShaderReadOnlyOptimal, LoadContent / PreserveContent" => Ok(RPAttachment
			{
				format: LocationPacked(1, 0, Format::Value(VkFormat::R8G8B8A8_SNORM)), preserve_content: true, clear_on_load: Some(false),
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
				=> Ok(RPSubpassDesc { color_outs: vec![LocationPacked(1, 9, ConfigInt::Value(0))], inputs: Vec::new() }),
			RPSubpassDesc::parse; "RenderTo 0 From 1"
				=> Ok(RPSubpassDesc { color_outs: vec![LocationPacked(1, 9, ConfigInt::Value(0))], inputs: vec![LocationPacked(1, 16, ConfigInt::Value(1))] }),
			RPSubpassDesc::parse; "<- [1, 2] RenderTo [0, 3]" => Ok(RPSubpassDesc
			{
				color_outs: vec![LocationPacked(1, 20, ConfigInt::Value(0)), LocationPacked(1, 23, ConfigInt::Value(3))],
				inputs: vec![LocationPacked(1, 4, ConfigInt::Value(1)), LocationPacked(1, 7, ConfigInt::Value(2))]
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
				=> Ok(ExternalResourceData::ImageView { dim: 1, refname: LocationPacked(1, 13, "HogeResource".into()) }),
			ExternalResourceData::parse; "ImageView 2D \"HogeResource\""
				=> Ok(ExternalResourceData::ImageView { dim: 2, refname: LocationPacked(1, 13, "HogeResource".into()) }),
			ExternalResourceData::parse; "ImageView 3D \"HogeResource\""
				=> Ok(ExternalResourceData::ImageView { dim: 3, refname: LocationPacked(1, 13, "HogeResource".into()) }),
			ExternalResourceData::parse; "SwapChainViews" => Ok(ExternalResourceData::SwapChainViews),
			ExternalResourceData::parse; "Framebuffer" => Err(ParseError::UnknownExternalResource(0)),
			ExternalResourceData::parse; "ImageView \"HogeResource\"" => Err(ParseError::Expected("Image Dimension".into(), 10))
		}
	}
	#[test] fn test_framebuffer_rp()
	{
		Testing!
		{
			FramebufferRenderPassRef::parse; "<$FirstRP>" => Ok(FramebufferRenderPassRef::Int(LocationPacked(1, 1, ConfigInt::Ref("FirstRP".into())))),
			FramebufferRenderPassRef::parse; "<Presented>" => Ok(FramebufferRenderPassRef::Presented),
			FramebufferRenderPassRef::parse; "n" => Ok(FramebufferRenderPassRef::None),
			FramebufferRenderPassRef::parse; "<0" => Err(ParseError::ClosingRequired(2)),
			FramebufferRenderPassRef::parse; "<AA>" => Err(ParseError::UnknownObjectRef("RenderPass", 1))
		}
	}
}
