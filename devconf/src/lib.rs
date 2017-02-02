//! Postludium: Device Configuration Processor

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate parsetools;
extern crate interlude_vkdefs as vk;
#[cfg(test)] extern crate itertools;

use std::collections::HashMap;
use parsetools::*;
use std::borrow::Cow;
#[cfg(test)] use itertools::Itertools;
use vk::*;

#[macro_use] mod items;
use self::items::*;
mod hobjects;
use self::hobjects::*;

pub struct NamedContents<T>(HashMap<String, usize>, Vec<T>);
impl<T> std::ops::Index<usize> for NamedContents<T>
{
	type Output = T;
	fn index(&self, s: usize) -> &T { &self.1[s] }
}
impl<'a, T> std::ops::Index<&'a str> for NamedContents<T>
{
	type Output = T;
	fn index(&self, s: &'a str) -> &T { &self.1[self.0[s]] }
}
impl<T> std::ops::Deref for NamedContents<T> { type Target = [T]; fn deref(&self) -> &[T] { &self.1 } }
impl<T> NamedContents<T>
{
	fn new() -> Self { NamedContents(HashMap::new(), Vec::new()) }
	fn insert(&mut self, name: Cow<str>, value: T) -> &mut T
	{
		let ref mut v = self.1;
		let p = *self.0.entry(name.into_owned()).or_insert_with(||
		{
			v.push(value);
			v.len() - 1
		});
		&mut v[p]
	}
	fn insert_unnamed(&mut self, value: T) -> &mut T
	{
		self.1.push(value);
		let p = self.1.len() - 1;
		&mut self.1[p]
	}
}
/*
pub struct DeviceResources
{
	pub render_passes: NamedContents<RenderPass>,
	pub descriptor_set_layouts: NamedContents<DescriptorSetLayout>,
	pub descriptor_sets: DescriptorSets,
	pub pipeline_layouts: NamedContents<PipelineLayout>,
	pub pipeline_states: NamedContents<GraphicsPipeline>
}
*/

// Source Representations
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct RPAttachment { format: PixelFormat, layouts: Transition<VkImageLayout>, clear_on_load: Option<bool>, preserve_content: bool }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct RPSubpassDesc { color_outs: Vec<ConfigInt>, inputs: Vec<ConfigInt> }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct RPSubpassDeps { passtrans: Transition<ConfigInt>, access_mask: Transition<VkAccessFlags>, stage_bits: VkPipelineStageFlags, by_region: bool }
pub struct RenderPassData { attachments: NamedContents<RPAttachment>, passes: NamedContents<RPSubpassDesc>, deps: Vec<RPSubpassDeps> }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct SimpleRenderPassData { format: PixelFormat, clear_on_load: Option<bool> }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct PresentedRenderPassData { format: PixelFormat, clear_on_load: Option<bool> }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct PreciseRenderPassRef { rp: ConfigInt, subpass: ConfigInt }
impl PreciseRenderPassRef
{
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		ConfigInt::parse(source).and_then(|r| if source.drop_while(ignore_chars).front() == Some('.')
		{
			ConfigInt::parse(source.drop_opt(1).drop_while(ignore_chars)).map(|s| PreciseRenderPassRef { rp: r, subpass: s })
		}
		else { Err(ParseError::Expected("PreciseRenderPassRef", source.current())) })
	}
}
#[test] fn parse_precise_renderpass_ref()
{
	Testing!
	{
		PreciseRenderPassRef::parse: "0.1" => Ok(PreciseRenderPassRef { rp: ConfigInt::Value(0), subpass: ConfigInt::Value(1) }),
		PreciseRenderPassRef::parse: "$First .1" => Ok(PreciseRenderPassRef { rp: ConfigInt::Ref("First".into()), subpass: ConfigInt::Value(1) }),
		PreciseRenderPassRef::parse: "$Final" => Err(ParseError::Expected("PreciseRenderPassRef", source.current()))
	}
}
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum ExternalResourceData
{
	ImageView { dim: u8, refname: String }, SwapChainViews
}
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum FramebufferStyle
{
	WithRenderPass(ConfigInt), Simple(Option<bool>), Presented(Option<bool>)
}
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct FramebufferInfo
{
	style: FramebufferStyle, views: Vec<ConfigInt>
}
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum BufferDescriptorOption { None, TexelStore, DynamicOffset }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum DescriptorEntryKind
{
	Sampler, CombinedSampler, SampledImage, StorageImage,
	UniformBuffer(BufferDescriptorOption), StorageBuffer(BufferDescriptorOption), InputAttachment
}
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct DescriptorEntry { kind: DescriptorEntryKind, count: usize, visibility: VkShaderStageFlags }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct PushConstantLayout { range: std::ops::Range<usize>, visibility: VkShaderStageFlags }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct PipelineLayout { descs: Vec<ConfigInt>, pushconstants: Vec<ConfigInt> }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct DescriptorSetsInfo(Vec<DescriptorSetEntry>);
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct DescriptorSetEntry { name: Option<String>, layout: ConfigInt }
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct PipelineShaderStageInfo
{
	asset: AssetResource, consts: HashMap<u32, NumericLiteral>
}
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct PipelineStateInfo
{
	vertex_shader: PipelineShaderStageInfo, geometry_shader: Option<PipelineShaderStageInfo>, fragment_shader: Option<PipelineShaderStageInfo>,
	tesscontrol_shader: Option<PipelineShaderStageInfo>, tessevaluation_shader: Option<PipelineShaderStageInfo>,
	primitive_topology: VkPrimitiveTopology, viewport_scissors: Vec<ViewportScissorEntry>, blendstates: Vec<AttachmentBlendState>
}
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum ViewportScissorEntry
{
	ScreenView,
	Custom(VkViewport, VkRect2D)
}
lazy_static!
{
	static ref SCREENVIEW: Vec<char> = "ScreenView".chars().collect();
}
impl ViewportScissorEntry
{
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// Size3F "-" Size3F [":" Offset2D "-" Offset2D] / "ScreenView"
		if source.starts_with_trailing_opt(&SCREENVIEW, ident_break) { source.drop_opt(SCREENVIEW.len()); Ok(ViewportScissorEntry::ScreenView) }
		else
		{
			let vport = parse_size3f(source)
				.and_then(|vs| if source.drop_while(ignore_chars).front() != Some('-') { Err(ParseError::Expected("\"-\"", source.current())) }
				else { parse_size3f(source.drop_opt(1).drop_while(ignore_chars)).map(|ve| (vs, ve)) });
			
			vport.and_then(|((sx, sy, sz), (dx, dy, dz))| if source.drop_while(ignore_chars).front() == Some(':')
			{
				parse_offset2(source.drop_opt(1).drop_while(ignore_chars))
					.and_then(|(ssx, ssy)| if source.drop_while(ignore_chars).front() != Some('-') { Err(ParseError::Expected("\"-\"", source.current())) }
					else
					{
						parse_offset2(source.drop_opt(1).drop_while(ignore_chars))
							.map(|(sdx, sdy)| VkRect2D(VkOffset2D(ssx, ssy), VkExtent2D((sdx - ssx) as u32, (sdy - ssy) as u32)))
					})
			} else { Ok(VkRect2D(VkOffset2D(sx as i32, sy as i32), VkExtent2D((dx - sx) as u32, (dy - sy) as u32))) }
			.map(|sc| ViewportScissorEntry::Custom(VkViewport(sx, sy, dx - sx, dy - sy, sz, dz), sc)))
		}
	}
}
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum AttachmentBlendState
{
	Disabled, Alpha, PremultipliedAlpha
}
lazy_static!
{
	static ref DISABLED: Vec<char> = "Disabled".chars().collect();
	static ref ALPHA: Vec<char> = "Alpha".chars().collect();
	static ref PREMULTIPLIEDALPHA: Vec<char> = "PremultipliedAlpha".chars().collect();
}
impl AttachmentBlendState
{
	fn parse(source: &mut ParseLine) -> Result<Self, ParseError>
	{
		// "Disabled" / "Alpha" / "PremultipliedAlpha"
		let s = source.take_until(ident_break);
		PartialEqualityMatchMap!(s;
		{
			DISABLED[..] => Ok(AttachmentBlendState::Disabled),
			ALPHA[..] => Ok(AttachmentBlendState::Alpha),
			PREMULTIPLIEDALPHA[..] => Ok(AttachmentBlendState::PremultipliedAlpha);
			_ => Err(ParseError::Expected("AttachmentBlendState Constant", s.current()))
		})
	}
	fn parse_array(source: &mut ParseLine) -> Result<Vec<Self>, ParseError>
	{
		// "[" blendstate,* "]"
		if source.front() != Some('[') { Err(ParseError::Expected("Array of AttachmentBlendState Constant", source.current())) }
		else
		{
			fn recursive(source: &mut ParseLine, sink: &mut Vec<AttachmentBlendState>) -> Result<(), ParseError>
			{
				AttachmentBlendState::parse(source).and_then(|abs|
				{
					sink.push(abs);
					if source.drop_while(ignore_chars).front() == Some(',') { recursive(source.drop_opt(1).drop_while(ignore_chars), sink) }
					else { Ok(()) }
				})
			}
			if source.drop_opt(1).drop_while(ignore_chars).front() == Some(']') { Ok(Vec::new()) }
			else
			{
				let mut v = Vec::new();
				match recursive(source, &mut v)
				{
					Ok(_) => Ok(v),
					Err(e) => Err(e)
				}
			}
		}
	}
}
#[test] fn blendstate()
{
	Testing!
	{
		AttachmentBlendState::parse: "Disabled" => Ok(AttachmentBlendState::Disabled),
		AttachmentBlendState::parse: "None" => Err(ParseError::Expected("AttachmentBlendState Constant", 0)),
		AttachmentBlendState::parse_array: "[]" => Ok(Vec::new()),
		AttachmentBlendState::parse_array: "[Alpha]" => Ok(vec![AttachmentBlendState::Alpha]),
		AttachmentBlendState::parse_array: "[" => Err(ParseError::ClosingRequired(1)),
		AttachmentBlendState::parse_array: "" => Err(ParseError::Expected("Array of AttachmentBlendState Constant", 0))
	}
}

fn parse_size3f(source: &mut ParseLine) -> Result<(f32, f32, f32), ParseError>
{
	// "(" f32 "," f32 "," f32 ")"
	if source.front() != Some('(') { Err(ParseError::Expected("Size3F", source.current())) }
	else
	{
		let x = source.drop_opt(1).drop_while(ignore_chars).take_while(|c| c.is_digit(10));
		let s = if source.front() == Some('.')
		{
			let fp = source.drop_opt(1).take_while(|c| c.is_digit(10));
			x.chars().iter().chain(&['.']).chain(fp.chars().iter()).cloned().collect()
		}
		else { x.clone_as_string() };
		s.parse::<f32>().map_err(|e| ParseError::FloatingParseError(e, x.current()))
	}.and_then(|x| if source.drop_while(ignore_chars).front() != Some(',') { Err(ParseError::Expected("\",\"", source.current())) }
	else
	{
		let y = source.drop_opt(1).drop_while(ignore_chars).take_while(|c| c.is_digit(10));
		let s = if source.front() == Some('.')
		{
			let fp = source.drop_opt(1).take_while(|c| c.is_digit(10));
			y.chars().iter().chain(&['.']).chain(fp.chars().iter()).cloned().collect()
		}
		else { y.clone_as_string() };
		s.parse::<f32>().map_err(|e| ParseError::FloatingParseError(e, y.current())).map(|y| (x, y))
	}).and_then(|(x, y)| if source.drop_while(ignore_chars).front() != Some(',') { Err(ParseError::Expected("\",\"", source.current())) }
	else
	{
		let z = source.drop_opt(1).drop_while(ignore_chars).take_while(|c| c.is_digit(10));
		let s = if source.front() == Some('.')
		{
			let fp = source.drop_opt(1).take_while(|c| c.is_digit(10));
			z.chars().iter().chain(&['.']).chain(fp.chars().iter()).cloned().collect()
		}
		else { z.clone_as_string() };
		s.parse::<f32>().map_err(|e| ParseError::FloatingParseError(e, z.current())).map(|z| (x, y, z))
	}).and_then(|v| if source.drop_while(ignore_chars).front() != Some(')') { Err(ParseError::ClosingRequired(source.current())) }
	else { Ok(v) })
}
fn parse_offset2(source: &mut ParseLine) -> Result<(i32, i32), ParseError>
{
	// "(" i32 "," i32 ")"
	if source.front() != Some('(') { Err(ParseError::Expected("Offset2", source.current())) }
	else
	{
		let inv = source.drop_opt(1).drop_while(ignore_chars).front() == Some('-');
		if inv { source.drop_opt(1); }
		let x = source.drop_while(ignore_chars).take_while(|c| c.is_digit(10));
		x.clone_as_string().parse::<i32>().map_err(|e| ParseError::NumericParseError(e, x.current())).map(|e| if inv { -e } else { e })
	}.and_then(|x| if source.drop_while(ignore_chars).front() != Some(',') { Err(ParseError::Expected("\",\"", source.current())) }
	else
	{
		let inv = source.drop_opt(1).drop_while(ignore_chars).front() == Some('-');
		if inv { source.drop_opt(1); }
		let y = source.drop_while(ignore_chars).take_while(|c| c.is_digit(10));
		y.clone_as_string().parse::<i32>().map_err(|e| ParseError::NumericParseError(e, y.current())).map(|e| if inv { (x, -e) } else { (x, e) })
	}).and_then(|v| if source.drop_while(ignore_chars).front() != Some(')') { Err(ParseError::ClosingRequired(source.current())) }
	else { Ok(v) })
}
#[test] fn parse_coord_primitives()
{
	Testing!
	{
		parse_size3f: "(0.0, 0.0, 1.0)" => Ok((0.0f32, 0.0, 1.0)),
		parse_size3f: "0.0" => Err(ParseError::Expected("Size3F", 0)),
		parse_size3f: "(0.0 " => Err(ParseError::Expected("\",\"", 5)),
		parse_size3f: "(0, 0.0, 0.0" => Err(ParseError::ClosingRequired(14)),
		parse_offset2: "(0, 0)" => Ok((0, 0)),
		parse_offset2: "0" => Err(ParseError::Expected("Offset2", 0)),
		parse_offset2: "(0 " => Err(ParseError::Expected("\",\"", 3)),
		parse_offset2: "(0, 0, 0" => Err(ParseError::ClosingRequired(5)),
		parse_offset2: "(0, 0 " => Err(ParseError::ClosingRequired(6))
	}
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

// Parse Error
#[derive(Debug, PartialEq)]
pub enum ParseError
{
	UnexpectedHead, NameRequired(usize), UnknownDeviceResource(usize), UnknownFormat(usize), UnknownImageLayout(usize), IntValueRequired(usize),
	UnknownRenderPassAttachmentOptions(usize), ImageLayoutRequired(usize), DirectionRequired(usize),
	NumericParseError(std::num::ParseIntError, usize), FloatingParseError(std::num::ParseFloatError, usize),
	DelimiterRequired(usize), ClosingRequired(usize), DefinitionOverrided, CorruptedSubpassDesc(usize),
	UnknownPipelineStageFlag(usize), UnknownAccessFlag(usize), Expected(&'static str, usize), UnknownConfig(&'static str), ConfigRequired(&'static str),
	UnknownExternalResource(usize), UnknownClearMode(usize), FormatRequired(usize), UnknownObjectRef(&'static str, usize), UnknownShaderStageFlag(usize),
	UnknownDescriptorKind(usize), BytesizeRequired(usize), UnknownPrimitiveTopology(bool, usize)
}
trait DivergenceExt<T> { fn report_error(self, line: usize) -> T; }
impl<T> DivergenceExt<T> for Result<T, ParseError>
{
	fn report_error(self, line: usize) -> T
	{
		match self
		{
			Ok(t) => t,
			Err(ParseError::UnexpectedHead) => panic!("Unexpected character at head of line {}", line),
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
			Err(ParseError::ConfigRequired(c)) => panic!("Configuration \"{}\" required at line {}", c, line)
		}
	}
}

fn acquire_line<'s>(lines: &mut LazyLines<'s>, level: usize) -> Option<(usize, ParseLine<'s>)>
{
	const HEAD: [char; 3] = ['-'; 3];

	lines.next().and_then(|(l, s)| if s.front() == Some('#') || s.front() == None { lines.drop_line(); acquire_line(lines, level) }
		else if s.starts_with_trailing_opt(&HEAD[..level], |c| c != '-')
		{
			lines.drop_line();
			let mut s = ParseLine(&s[level..], level);
			s.drop_while(ignore_chars);
			Some((l, s))
		}
		else { None })
}

fn ignore_chars(c: char) -> bool { c == ' ' || c == '\t' }
fn ident_break(c: char) -> bool
{
	c == ':' || c == '-' || c == '[' || c == ']' || c == ',' || c == '<' || c == '>' || c == '/' || ignore_chars(c)
}
fn parse_device_resource(mut source: LazyLines)
{
	if let Some((l, s)) = source.pop()
	{
		match s.front()
		{
			Some('#') => parse_device_resource(source),
			Some('$') => parse_named_device_resource(l, &mut ParseLine(s, 0), source),
			Some(_) => parse_unnamed_device_resource(l, &mut ParseLine(s, 0), source),
			_ => Err(ParseError::UnexpectedHead).report_error(l)
		}
	}
}
fn parse_named_device_resource(current: usize, source: &mut ParseLine, mut restlines: LazyLines)
{
	NamedConfigLine::parse(source, |source|
	{
		parse_unnamed_device_resource(current, source.drop_while(ignore_chars), restlines);
		Ok(())
	}).report_error(current);
}
fn parse_unnamed_device_resource(current: usize, source: &mut ParseLine, mut rest: LazyLines)
{
	let s = source.take_until(ident_break);
	match s.clone_as_string().as_ref()
	{
		"RenderPass" => { parse_renderpass(&mut rest); },
		"SimpleRenderPass" => { parse_simple_renderpass(current, &mut rest); },
		"PresentedRenderPass" => { parse_presented_renderpass(current, &mut rest); },
		"DescriptorSetLayout" => { parse_descriptor_set_layout(&mut rest); },
		"PushConstantLayout" => { parse_push_constant_layout(&mut rest).report_error(current); },
		"PipelineLayout" => { parse_pipeline_layout(&mut rest); },
		"DescriptorSets" => { parse_descriptor_sets(&mut rest); },
		"PipelineState" => { parse_pipeline_state(current, source.drop_while(ignore_chars), &mut rest); },
		"Extern" => { parse_extern_resources(source.drop_while(ignore_chars)).report_error(current); },
		"Framebuffer" => { parse_framebuffer(source.drop_while(ignore_chars), &mut rest).report_error(current); }
		_ => Err(ParseError::UnknownDeviceResource(s.current())).report_error(current)
	};
}
fn parse_extern_resources(input: &mut ParseLine) -> Result<ExternalResourceData, ParseError>
{
	fn image_dimension(input: &ParseLine) -> Result<u8, ParseError>
	{
		if *input == ['1', 'D'][..] { Ok(1) }
		else if *input == ['2', 'D'][..] { Ok(2) }
		else if *input == ['3', 'D'][..] { Ok(3) }
		else { Err(ParseError::Expected("Image Dimension", input.current())) }
	}

	let s = input.take_until(ident_break);
	match s.clone_as_string().as_ref()
	{
		"ImageView" =>
		{
			let d = input.drop_while(ignore_chars).take_until(ident_break);
			image_dimension(&d).and_then(|d|
				parse_string_literal(input.drop_while(ignore_chars)).map(|n| ExternalResourceData::ImageView { dim: d, refname: n })
			)
		},
		"SwapChainViews" => Ok(ExternalResourceData::SwapChainViews),
		_ => Err(ParseError::UnknownExternalResource(s.current()))
	}
}
fn parse_config_name<'s>(source: &mut ParseLine<'s>) -> Result<ParseLine<'s>, ParseError>
{
	let name = source.take_until(ident_break);
	if source.drop_while(ignore_chars).front() != Some(':') { Err(ParseError::DelimiterRequired(source.current())) }
	else { source.drop_opt(1); Ok(name) }
}
lazy_static!
{
	static ref ATTACHMENTS: Vec<char> = "Attachments".chars().collect();
	static ref SUBPASSES: Vec<char> = "Subpasses".chars().collect();
	static ref DEPENDENCIES: Vec<char> = "Dependencies".chars().collect();
}
fn parse_renderpass(source: &mut LazyLines) -> RenderPassData
{
	let mut rpd = RenderPassData { attachments: NamedContents::new(), passes: NamedContents::new(), deps: Vec::new() };
	while let Some((l, mut s)) = acquire_line(source, 1)
	{
		parse_config_name(&mut s).and_then(|name|
			if name == ATTACHMENTS[..]
			{
				if !rpd.attachments.is_empty() { Err(ParseError::DefinitionOverrided).report_error(l) }
				else
				{
					while let Some((l, mut s)) = acquire_line(source, 2)
					{
						match NamedConfigLine::parse(&mut s, parse_rp_attachment)
						{
							Ok(NamedConfigLine { name: Some(name), config }) => { rpd.attachments.insert(name.into(), config); },
							Ok(NamedConfigLine { config, .. }) => { rpd.attachments.insert_unnamed(config); },
							Err(ParseError::UnexpectedHead) => break,
							e => { e.report_error(l); }
						}
					}
				}
				Ok(())
			}
			else if name == SUBPASSES[..]
			{
				if !rpd.passes.is_empty() { Err(ParseError::DefinitionOverrided).report_error(l) }
				else
				{
					while let Some((l, mut s)) = acquire_line(source, 2)
					{
						match NamedConfigLine::parse(&mut s, parse_subpass_desc)
						{
							Ok(NamedConfigLine { name: Some(name), config }) => { rpd.passes.insert(name.into(), config); },
							Ok(NamedConfigLine { config, .. }) => { rpd.passes.insert_unnamed(config); },
							Err(ParseError::UnexpectedHead) => break,
							e => { e.report_error(l); }
						}
					}
				}
				Ok(())
			}
			else if name == DEPENDENCIES[..]
			{
				if !rpd.deps.is_empty() { Err(ParseError::DefinitionOverrided).report_error(l) }
				else
				{
					while let Some((l, mut s)) = acquire_line(source, 2)
					{
						match parse_subpass_deps(&mut s)
						{
							Ok(c) => { rpd.deps.push(c); },
							e => { e.report_error(l); }
						}
					}
				}
				Ok(())
			}
			else { Err(ParseError::UnknownConfig("RenderPass")) }
		).report_error(l);
	}
	rpd
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
fn parse_simple_renderpass(line_in: usize, source: &mut LazyLines) -> SimpleRenderPassData
{
	let (mut fmt, mut clear_mode) = (None, None);
	while let Some((l, mut s)) = acquire_line(source, 1)
	{
		parse_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
		{
			FORMAT[..] => PixelFormat::parse(s.drop_while(ignore_chars))
				.and_then(|f| if fmt.is_none() { fmt = Some(f); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			CLEARMODE[..] => parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () });
			_ => Err(ParseError::UnknownConfig("SimpleRenderPass"))
		})).report_error(l);
	}
	if fmt.is_none() { Err(ParseError::ConfigRequired("Format")).report_error(line_in) }
	else
	{
		SimpleRenderPassData { format: fmt.unwrap(), clear_on_load: clear_mode }
	}
}
fn parse_presented_renderpass(line_in: usize, source: &mut LazyLines) -> PresentedRenderPassData
{
	let (mut fmt, mut clear_mode) = (None, None);
	while let Some((l, mut s)) = acquire_line(source, 1)
	{
		parse_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
		{
			FORMAT[..] => PixelFormat::parse(s.drop_while(ignore_chars))
				.and_then(|f| if fmt.is_none() { fmt = Some(f); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			CLEARMODE[..] => parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () });
			_ => Err(ParseError::UnknownConfig("PresentedRenderPass"))
		})).report_error(l);
	}
	if fmt.is_none() { Err(ParseError::ConfigRequired("Format")).report_error(line_in) }
	else
	{
		PresentedRenderPassData { format: fmt.unwrap(), clear_on_load: clear_mode }
	}
}
lazy_static!
{
	static ref PRESENTED: Vec<char> = "Presented".chars().collect();
}
#[cfg_attr(test, derive(Debug, PartialEq))]
enum FramebufferRenderPassRef { Int(ConfigInt), Presented, None }
fn parse_framebuffer_rp(input: &mut ParseLine) -> Result<FramebufferRenderPassRef, ParseError>
{
	if input.front() == Some('<')
	{
		// Which Parameter: "Presented" / int
		input.drop_opt(1).drop_while(ignore_chars);
		let p = if input.front().map(|c| ('0' <= c && c <= '9') || c == '$').unwrap_or(false)
		{
			// int
			ConfigInt::parse(input).map(FramebufferRenderPassRef::Int)
		}
		else if input.starts_with_trailing_opt(&PRESENTED, ident_break)
		{
			input.drop_opt(PRESENTED.len());
			Ok(FramebufferRenderPassRef::Presented)
		}
		else { Err(ParseError::UnknownObjectRef("RenderPass", input.current())) };
		
		p.and_then(|v|
		{
			if input.drop_while(ignore_chars).front() == Some('>') { input.drop_opt(1); Ok(v) }
			else { Err(ParseError::ClosingRequired(input.current())) }
		})
	}
	else { Ok(FramebufferRenderPassRef::None) }
}
fn parse_framebuffer(rest: &mut ParseLine, mut source: &mut LazyLines) -> Result<FramebufferInfo, ParseError>
{
	parse_framebuffer_rp(rest.drop_while(ignore_chars)).and_then(|arg| ConfigInt::parse_array(rest.drop_while(ignore_chars)).map(|vs| (arg, vs)))
		.and_then(|(arg, vs)|
		{
			let mut clear_mode = None;
			while let Some((l, mut s)) = acquire_line(source, 1)
			{
				parse_config_name(s.drop_while(ignore_chars)).and_then(|name|
					if name == CLEARMODE[..]
					{
						parse_rp_clear_mode(s.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () })
					}
					else { Err(ParseError::UnknownConfig("Framebuffer")) }
				).report_error(l)
			}
			let style = match arg
			{
				FramebufferRenderPassRef::Int(rp) => FramebufferStyle::WithRenderPass(rp),
				FramebufferRenderPassRef::Presented => FramebufferStyle::Presented(clear_mode),
				FramebufferRenderPassRef::None => FramebufferStyle::Simple(clear_mode)
			};
			Ok(FramebufferInfo { style: style, views: vs })
		})
}
fn parse_descriptor_set_layout(source: &mut LazyLines) -> Vec<DescriptorEntry>
{
	let mut entries = Vec::new();
	while let Some((l, mut s)) = acquire_line(source, 1)
	{
		entries.push(parse_descriptor_entry(&mut s).report_error(l));
	}
	entries
}
fn parse_descriptor_entry(source: &mut ParseLine) -> Result<DescriptorEntry, ParseError>
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
	count.and_then(|count| parse_config_name(source.drop_while(ignore_chars)).and_then(descriptor_entry_kind)
		.and_then(|ntype| parse_shader_stage_bits(source.drop_while(ignore_chars)).map(|ss| DescriptorEntry { count: count, kind: ntype, visibility: ss })))
}
#[test] fn descriptor_entry()
{
	Testing!
	{
		parse_descriptor_entry: "UniformBuffer: Vertex" => Ok(DescriptorEntry
		{
			kind: DescriptorEntryKind::UniformBuffer(BufferDescriptorOption::None), count: 1, visibility: VK_SHADER_STAGE_VERTEX_BIT
		}),
		parse_descriptor_entry: "2 CombinedSampler : Geometry / Fragment" => Ok(DescriptorEntry
		{
			kind: DescriptorEntryKind::CombinedSampler, count: 2, visibility: VK_SHADER_STAGE_GEOMETRY_BIT | VK_SHADER_STAGE_FRAGMENT_BIT
		}),
		parse_descriptor_entry: "a: Vertex" => Err(ParseError::UnknownDescriptorKind(0))
	}
}
lazy_static!
{
	static ref RANGE: Vec<char> = "Range".chars().collect();
	static ref VISIBILITY: Vec<char> = "Visibility".chars().collect();
}
fn parse_push_constant_layout(source: &mut LazyLines) -> Result<PushConstantLayout, ParseError>
{
	let (mut range, mut vis) = (None, None);
	while let Some((l, mut s)) = acquire_line(source, 1)
	{
		parse_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
		{
			RANGE[..] => parse_usize_range(s.drop_while(ignore_chars))
				.and_then(|r| if range.is_none() { range = Some(r); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			VISIBILITY[..] => parse_shader_stage_bits(s.drop_while(ignore_chars))
				.and_then(|r| if vis.is_none() { vis = Some(r); Ok(()) } else { Err(ParseError::DefinitionOverrided) });
			_ => Err(ParseError::UnknownConfig("PushConstantLayout"))
		})).report_error(l);
	}
	range.ok_or(ParseError::ConfigRequired("Range"))
	.and_then(|range| vis.ok_or(ParseError::ConfigRequired("Visibility")).map(|vis| PushConstantLayout { range: range, visibility: vis }))
}
lazy_static!
{
	static ref DESCRIPTORS: Vec<char> = "Descriptors".chars().collect();
	static ref PUSHCONSTANTLAYOUTS: Vec<char> = "PushConstantLayouts".chars().collect();
}
fn parse_pipeline_layout(source: &mut LazyLines) -> PipelineLayout
{
	let (mut desc, mut pcls) = (Vec::new(), Vec::new());
	while let Some((l, mut s)) = acquire_line(source, 1)
	{
		parse_config_name(&mut s).and_then(|name| PartialEqualityMatchMap!(name;
		{
			DESCRIPTORS[..] => ConfigInt::parse_array(s.drop_while(ignore_chars))
				.and_then(|cv| if desc.is_empty() { desc = cv; Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			PUSHCONSTANTLAYOUTS[..] => ConfigInt::parse_array(s.drop_while(ignore_chars))
				.and_then(|cv| if pcls.is_empty() { pcls = cv; Ok(()) } else { Err(ParseError::DefinitionOverrided) });
			_ => Err(ParseError::UnknownConfig("PipelineLayout"))
		})).report_error(l);
	}
	PipelineLayout { descs: desc, pushconstants: pcls }
}
fn parse_descriptor_sets(source: &mut LazyLines) -> DescriptorSetsInfo
{
	let mut entries = Vec::new();
	while let Some((l, mut s)) = acquire_line(source, 1)
	{
		entries.push(NamedConfigLine::parse(&mut s, ConfigInt::parse)
			.map(|NamedConfigLine { name, config }| DescriptorSetEntry { name: name, layout: config })
		.report_error(l));
	}
	DescriptorSetsInfo(entries)
}
fn parse_pipeline_state(current: usize, source: &mut ParseLine, restlines: &mut LazyLines) -> PipelineStateInfo
{
	// "PipelineState" v"for" precise_rpref "with" int <LF>
	let r = if source.starts_with_trailing_opt(&['f', 'o', 'r'], ident_break)
	{
		PreciseRenderPassRef::parse(source.drop_opt(3).drop_while(ignore_chars))
	}
	else { Err(ParseError::Expected("\"for\"", source.current())) };
	let rpl = r.and_then(|rp| if source.drop_while(ignore_chars).starts_with_trailing_opt(&['w', 'i', 't', 'h'], ident_break)
	{
		ConfigInt::parse(source.drop_opt(4).drop_while(ignore_chars)).map(|l| (rp, l))
	}
	else { Err(ParseError::Expected("\"with\"", source.current())) }).report_error(current);

	let (mut vsinfo, mut fsinfo, mut gsinfo, mut tcsinfo, mut tesinfo) = (None, None, None, None, None);
	let (mut primt, mut vpsc, mut blends) = (None, Vec::new(), Vec::new());
	while let Some((l, mut s)) = acquire_line(restlines, 1)
	{
		parse_config_name(&mut s).and_then(|name| match name.clone_as_string().as_ref()
		{
			"VertexShader" => parse_pipeline_shaderstage_info(s.drop_while(ignore_chars), restlines)
				.and_then(|s| if vsinfo.is_none() { vsinfo = Some(s); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			"FragmentShader" => parse_pipeline_shaderstage_info(s.drop_while(ignore_chars), restlines)
				.and_then(|s| if fsinfo.is_none() { fsinfo = Some(s); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			"GeometryShader" => parse_pipeline_shaderstage_info(s.drop_while(ignore_chars), restlines)
				.and_then(|s| if gsinfo.is_none() { gsinfo = Some(s); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			"TessellationControlShader" => parse_pipeline_shaderstage_info(s.drop_while(ignore_chars), restlines)
				.and_then(|s| if tcsinfo.is_none() { tcsinfo = Some(s); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			"TessellationEvaluationShader" => parse_pipeline_shaderstage_info(s.drop_while(ignore_chars), restlines)
				.and_then(|s| if tesinfo.is_none() { tesinfo = Some(s); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			"PrimitiveTopology" => parse_primitive_topology(s.drop_while(ignore_chars))
				.and_then(|s| if primt.is_none() { primt = Some(s); Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			"ViewportScissors" => parse_viewport_scissors(s.drop_while(ignore_chars), restlines)
				.and_then(|v| if vpsc.is_empty() { vpsc = v; Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			"BlendStates" => AttachmentBlendState::parse_array(s.drop_while(ignore_chars))
				.and_then(|v| if blends.is_empty() { blends = v; Ok(()) } else { Err(ParseError::DefinitionOverrided) }),
			_ => Err(ParseError::UnknownConfig("PipelineState"))
		}).report_error(l);
	}

	vsinfo.ok_or(ParseError::ConfigRequired("VertexShader")).and_then(|vsinfo|
	primt.ok_or(ParseError::ConfigRequired("PrimitiveTopology")).map(|primt| PipelineStateInfo
	{
		vertex_shader: vsinfo, fragment_shader: fsinfo, geometry_shader: gsinfo,
		tesscontrol_shader: tcsinfo, tessevaluation_shader: tesinfo,
		primitive_topology: primt, viewport_scissors: vpsc, blendstates: blends
	})).report_error(current)
}
fn parse_pipeline_shaderstage_info(source: &mut ParseLine, lines: &mut LazyLines) -> Result<PipelineShaderStageInfo, ParseError>
{
	AssetResource::parse(source).map(|a|
	{
		let mut consts = HashMap::new();
		while let Some((l, mut s)) = acquire_line(lines, 2)
		{
			if s.starts_with(&['C', 'o', 'n', 's', 't', 'a', 'n', 't'])
			{
				let n = s.drop_opt(8).drop_while(ignore_chars).take_while(|c| c.is_digit(10));
				if n.is_empty() { Err(ParseError::IntValueRequired(n.current())) }
				else
				{
					n.clone_as_string().parse::<u32>().map_err(|e| ParseError::NumericParseError(e, n.current())).and_then(|n|
					{
						if s.drop_while(ignore_chars).front() == Some(':')
						{
							NumericLiteral::parse(s.drop_opt(1).drop_while(ignore_chars), true).and_then(|v| if consts.contains_key(&n)
							{
								Err(ParseError::DefinitionOverrided)
							} else { consts.insert(n, v); Ok(()) })
						}
						else { Err(ParseError::Expected(":", s.current())) }
					})
				}
			}
			else { Err(ParseError::UnknownConfig("Shader Module")) }.report_error(l);
		}
		PipelineShaderStageInfo { asset: a, consts: consts }
	})
}
fn parse_primitive_topology(source: &mut ParseLine) -> Result<VkPrimitiveTopology, ParseError>
{
	// ident ["with" "Adjacency"]
	let prim = source.take_until(ident_break);
	if prim.is_empty() { Err(ParseError::Expected("Primitive Topology", prim.current())) }
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
		} else { Err(ParseError::Expected("\"Adjacency\"", source.current())) }
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
		while let Some((l, mut s)) = acquire_line(source, 2)
		{
			v.push(ViewportScissorEntry::parse(&mut s).report_error(l));
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
	else { Err(ParseError::Expected("Array or Children, of ViewportScissor", current.current())) }
}
#[test] fn parse_primitive_topology()
{
	Testing!
	{
		parse_primitive_topology: "PointList" => Ok(VkPrimitiveTopology::PointList),
		parse_primitive_topology: "LineStrip with Adjacency" => Ok(VkPrimitiveTopology::LineStripWithAdjacency),
		parse_primitive_topology: "PatchList with Adjacency" => Err(ParseError::UnknownPrimitiveTopology(true, 0)),
		parse_primitive_topology: "PointStrip" => Err(ParseError::UnknownPrimitiveTopology(false, 0)),
		parse_primitive_topology: "" => Err(ParseError::Expected("Primitive Topology", 0))
	}
}

lazy_static!
{
	static ref RPO_CLEAR_ON_LOAD: Vec<char> = "ClearOnLoad".chars().collect();
	static ref RPO_LOAD_CONTENT: Vec<char> = "LoadContent".chars().collect();
	static ref RPO_PRESERVE_CONTENT: Vec<char> = "PreserveContent".chars().collect();
}
// pixel_format "," transition_opt#image_layout "," option,*
fn parse_rp_attachment(source: &mut ParseLine) -> Result<RPAttachment, ParseError>
{
	PixelFormat::parse(source).and_then(|format|
	{
		if source.drop_while(ignore_chars).front() != Some(',') { Err(ParseError::DelimiterRequired(source.current())) }
		else
		{
			Transition::parse_opt(source.drop_opt(1).drop_while(ignore_chars), parse_image_layout).map(|layouts| (format, layouts))
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
lazy_static!
{
	static ref SDI_RENDER_TO: Vec<char> = "RenderTo".chars().collect();
}
// ("RenderTo" (int/ints) / From (int/ints))*
fn parse_subpass_desc(input: &mut ParseLine) -> Result<RPSubpassDesc, ParseError>
{
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
	recursive(input, &mut rpsd).map(|_| rpsd)
}
lazy_static!
{
	static ref BY_REGION_FLAG: Vec<char> = "ByRegion".chars().collect();
}
// int (From/To) int ":" transition#access_mask At stage_bits ["," ["ByRegion"]]
fn parse_subpass_deps(input: &mut ParseLine) -> Result<RPSubpassDeps, ParseError>
{
	Transition::parse(input, ConfigInt::parse).and_then(|pt|
	{
		if input.drop_while(ignore_chars).front() == Some(':') { input.drop_opt(1).drop_while(ignore_chars); Ok(pt) }
		else { Err(ParseError::DelimiterRequired(input.current())) }
	})
	.and_then(|pt| Transition::parse(input, parse_access_mask).map(|amt| (pt, amt)))
	.and_then(|(pt, amt)|
	{
		if at_token(input.drop_while(ignore_chars))
		{
			parse_pipeline_stage_bits(input.drop_while(ignore_chars)).map(|sf| (pt, amt, sf))
		}
		else { Err(ParseError::DelimiterRequired(input.current())) }
	})
	.map(|(pt, amt, sf)|
	{
		if input.drop_while(ignore_chars).front() == Some(',')
		{
			let mut r2 = input.clone();
			r2.drop_opt(1).drop_while(ignore_chars);
			if r2.starts_with_trailing_opt(&BY_REGION_FLAG, ident_break)
			{
				*input = r2;
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
#[test] fn test_subpass_deps()
{
	Testing!
	{
		parse_subpass_deps: "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage, ByRegion" => Ok(RPSubpassDeps
		{
			passtrans: Transition { from: ConfigInt::Value(0), to: ConfigInt::Value(1) },
			access_mask: Transition { from: VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, to: VK_ACCESS_SHADER_READ_BIT },
			stage_bits: VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT, by_region: true
		}),
		parse_subpass_deps: "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage" => Ok(RPSubpassDeps
		{
			passtrans: Transition { from: ConfigInt::Value(0), to: ConfigInt::Value(1) },
			access_mask: Transition { from: VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, to: VK_ACCESS_SHADER_READ_BIT },
			stage_bits: VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT, by_region: false
		}),
		parse_subpass_deps: "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage," => Ok(RPSubpassDeps
		{
			passtrans: Transition { from: ConfigInt::Value(0), to: ConfigInt::Value(1) },
			access_mask: Transition { from: VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, to: VK_ACCESS_SHADER_READ_BIT },
			stage_bits: VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT, by_region: false
		}),
		parse_subpass_deps: "0 -> 1: ColorAttachmentWrite -> ShaderRead" => Err(ParseError::DelimiterRequired(42)),
		parse_subpass_deps: "0 -> 1" => Err(ParseError::DelimiterRequired(6))
	}
}

fn from_token(input: &mut ParseLine) -> bool
{
	if input.starts_with_trailing_opt(&['F', 'r', 'o', 'm'], ident_break) { input.drop_opt(4); true }
	else if input.starts_with(&['<', '-']) { input.drop_opt(2); true }
	else { false }
}
#[test] fn test_from_token()
{
	Testing!
	{
		from_token: "From" => true,
		from_token: "<--" => true,
		from_token: "From[" => true,
		from_token: "Fro" => false,
		from_token: "Fromt" => false
	}
}
fn at_token(input: &mut ParseLine) -> bool
{
	if input.starts_with_trailing_opt(&['A', 't'], ident_break) { input.drop_opt(2); true }
	else if input.front() == Some('@') { input.drop_opt(1); true }
	else { false }
}
#[test] fn test_at_token()
{
	Testing!
	{
		at_token: "At" => true,
		at_token: "Att" => false,
		at_token: "@" => true,
		at_token: "@p" => true
	}
}

#[test] fn test_rp_attachment()
{
	Testing!
	{
		parse_rp_attachment: "R8G8B8A8 UNORM, ShaderReadOnlyOptimal <- ColorAttachmentOptimal, PreserveContent" => Ok(RPAttachment
		{
			format: PixelFormat::Value(VkFormat::R8G8B8A8_UNORM), preserve_content: true, clear_on_load: None,
			layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
		}),
		parse_rp_attachment: "R32 SFLOAT, ShaderReadOnlyOptimal <- ColorAttachmentOptimal" => Ok(RPAttachment
		{
			format: PixelFormat::Value(VkFormat::R32_SFLOAT), preserve_content: false, clear_on_load: None,
			layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
		}),
		parse_rp_attachment: "R8G8B8A8 UNORM, ShaderReadOnlyOptimal, ClearOnLoad" => Ok(RPAttachment
		{
			format: PixelFormat::Value(VkFormat::R8G8B8A8_UNORM), preserve_content: false, clear_on_load: Some(true),
			layouts: Transition { from: VkImageLayout::ShaderReadOnlyOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
		}),
		parse_rp_attachment: "R8G8B8A8 SNORM, ShaderReadOnlyOptimal, LoadContent / PreserveContent" => Ok(RPAttachment
		{
			format: PixelFormat::Value(VkFormat::R8G8B8A8_SNORM), preserve_content: true, clear_on_load: Some(false),
			layouts: Transition { from: VkImageLayout::ShaderReadOnlyOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }	
		}),
		parse_rp_attachment: "R8G8B8A8 UNORM" => Err(ParseError::DelimiterRequired(14)),
		parse_rp_attachment: "R8G8B8A8 UNORM, ShaderReadOnlyOptimal, Hoge" => Err(ParseError::UnknownRenderPassAttachmentOptions(39))
	}
}
#[test] fn test_subpass_desc()
{
	Testing!
	{
		parse_subpass_desc: "RenderTo 0" => Ok(RPSubpassDesc { color_outs: vec![ConfigInt::Value(0)], inputs: Vec::new() }),
		parse_subpass_desc: "RenderTo 0 From 1" => Ok(RPSubpassDesc { color_outs: vec![ConfigInt::Value(0)], inputs: vec![ConfigInt::Value(1)] }),
		parse_subpass_desc: "<- [1, 2] RenderTo [0, 3]" => Ok(RPSubpassDesc
		{
			color_outs: vec![ConfigInt::Value(0), ConfigInt::Value(3)], inputs: vec![ConfigInt::Value(1), ConfigInt::Value(2)]
		}),
		parse_subpass_desc: "Preserve 0" => Err(ParseError::CorruptedSubpassDesc(0)),
		parse_subpass_desc: "RenderTo 0 RenderTo 1" => Err(ParseError::DefinitionOverrided)
	}
}
#[test] fn test_external_resources()
{
	Testing!
	{
		parse_extern_resources: "ImageView 1D \"HogeResource\"" => Ok(ExternalResourceData::ImageView { dim: 1, refname: "HogeResource".into() }),
		parse_extern_resources: "ImageView 2D \"HogeResource\"" => Ok(ExternalResourceData::ImageView { dim: 2, refname: "HogeResource".into() }),
		parse_extern_resources: "ImageView 3D \"HogeResource\"" => Ok(ExternalResourceData::ImageView { dim: 3, refname: "HogeResource".into() }),
		parse_extern_resources: "SwapChainViews" => Ok(ExternalResourceData::SwapChainViews),
		parse_extern_resources: "Framebuffer" => Err(ParseError::UnknownExternalResource(0)),
		parse_extern_resources: "ImageView \"HogeResource\"" => Err(ParseError::Expected("Image Dimension", 10))
	}
}
#[test] fn test_framebuffer_rp()
{
	Testing!
	{
		parse_framebuffer_rp: "<$FirstRP>" => Ok(FramebufferRenderPassRef::Int(ConfigInt::Ref("FirstRP".into()))),
		parse_framebuffer_rp: "<Presented>" => Ok(FramebufferRenderPassRef::Presented),
		parse_framebuffer_rp: "n" => Ok(FramebufferRenderPassRef::None),
		parse_framebuffer_rp: "<0" => Err(ParseError::ClosingRequired(2)),
		parse_framebuffer_rp: "<AA>" => Err(ParseError::UnknownObjectRef("RenderPass", 1))
	}
}
