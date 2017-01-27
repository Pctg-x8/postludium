//! Postludium: Device Configuration Processor

use std;
use interlude::*;
use interlude::ffi::*;
use std::collections::HashMap;
use super::parsetools::*;
use std::borrow::Cow;
use lazylines::*;
use itertools::Itertools;

mod items;
use self::items::*;

struct Let<F: FnOnce() -> T, T>(F);
impl<F: FnOnce() -> T, T> Let<F, T>
{
	fn _in<G: FnOnce(T) -> R, R>(self, f: G) -> R { f(self.0()) }
}

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
pub struct DeviceResources
{
	pub render_passes: NamedContents<RenderPass>,
	pub descriptor_set_layouts: NamedContents<DescriptorSetLayout>,
	pub descriptor_sets: DescriptorSets,
	pub pipeline_layouts: NamedContents<PipelineLayout>,
	pub pipeline_states: NamedContents<GraphicsPipeline>
}

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
pub enum ExternalResourceData
{
	ImageView { dim: u8, refname: String }, SwapChainViews
}

#[cfg_attr(test, derive(Debug, PartialEq))] pub struct Transition<T> { from: T, to: T }
impl<T> Transition<T>
{
	fn parse<'s, F>(input: &'s [char], childparser: F) -> Result<(Self, &'s [char]), ParseError> where F: Fn(&[char]) -> Result<(T, &[char]), ParseError>
	{
		childparser(input).and_then(|(a, r)|
		{
			let r = r.skip_while(ignore_chars);
			if r.starts_with_trailing_opt(&['T', 'o'], ident_break) || r.starts_with(&['-', '>'])
			{
				childparser(r.drop(2).skip_while(ignore_chars)).map(|(b, r)| (Transition { from: a, to: b }, r))
			}
			else if let Some(r) = from_token(r)
			{
				childparser(r.skip_while(ignore_chars)).map(|(b, r)| (Transition { to: a, from: b }, r))
			}
			else { Err(ParseError::DirectionRequired) }
		})
	}
}
impl<T> Transition<T> where T: Copy
{
	fn parse_opt<'s, F>(input: &'s [char], childparser: F) -> Result<(Self, &'s [char]), ParseError> where F: Fn(&[char]) -> Result<(T, &[char]), ParseError>
	{
		childparser(input).and_then(|(a, r)|
		{
			let rb = r.skip_while(ignore_chars);
			if rb.starts_with_trailing_opt(&['T', 'o'], ident_break) || rb.starts_with(&['-', '>'])
			{
				childparser(rb.drop(2).skip_while(ignore_chars)).map(|(b, r)| (Transition { from: a, to: b }, r))
			}
			else if let Some(r) = from_token(rb)
			{
				childparser(r.skip_while(ignore_chars)).map(|(b, r)| (Transition { to: a, from: b }, r))
			}
			else { Ok((Transition { to: a, from: a }, r)) }
		})
	}
}
pub struct NamedConfigLine<C> { name: Option<String>, config: C }
impl<C> NamedConfigLine<C>
{
	fn parse0<F>(input: &[char], argparser: F) -> Result<Self, ParseError> where F: FnOnce(&[char]) -> Result<C, ParseError>
	{
		let name_res = if input.front() == Some('$')
		{
			let (name, rest) = input.drop(1).take_while(ident_break);
			if name.is_empty() { Err(ParseError::NameRequired) }
			else
			{
				let rest = rest.skip_while(ignore_chars);
				if rest.front() == Some(':') { Ok((Some(name.iter().cloned().collect::<String>()), rest.drop(1).skip_while(ignore_chars))) }
				else { Err(ParseError::DelimiterRequired) }
			}
		}
		else { Ok((None, input)) };

		name_res.and_then(|(name_opt, rest)| argparser(rest).map(|v| NamedConfigLine { name: name_opt, config: v }))
	}
	fn parse1<F>(input: &[char], argparser: F) -> Result<Self, ParseError> where F: FnOnce(&[char]) -> Result<C, ParseError>
	{
		if input.starts_with_trailing_opt(&['-'], |c| c != '-') { Self::parse0(input.drop(1).skip_while(ignore_chars), argparser) }
		else { Err(ParseError::UnexpectedHead) }
	}
	fn parse2<F>(input: &[char], argparser: F) -> Result<Self, ParseError> where F: FnOnce(&[char]) -> Result<C, ParseError>
	{
		if input.starts_with_trailing_opt(&['-'; 2], |c| c != '-') { Self::parse0(input.drop(2).skip_while(ignore_chars), argparser) }
		else { Err(ParseError::UnexpectedHead) }
	}
	fn parse3<F>(input: &[char], argparser: F) -> Result<Self, ParseError> where F: FnOnce(&[char]) -> Result<C, ParseError>
	{
		if input.starts_with_trailing_opt(&['-'; 3], |c| c != '-') { Self::parse0(input.drop(3).skip_while(ignore_chars), argparser) }
		else { Err(ParseError::UnexpectedHead) }
	}
}

pub struct ObjectConfigArg<'s> { name: String, value: &'s [char] }
macro_rules! ParseObjectConfigArgN
{
	($t: ident = $n: expr) =>
	{
		fn $t(source: &'s [char]) -> Result<Self, ParseError>
		{
			use self::ParseError::*;

			if source.starts_with(&['-'; $n]) && source.peek($n + 1) != Some('-')
			{
				let rest = source.drop($n).skip_while(ignore_chars);
				let (name, rest) = rest.take_until(|c| c == ':' || ignore_chars(c));
				if name.is_empty() { Err(NameRequired) }
				else
				{
					let rest = rest.skip_while(ignore_chars);
					if rest.front() != Some(':') { Err(DelimiterRequired) }
					else { Ok(ObjectConfigArg { name: name.clone_as_string(), value: rest.drop(1).skip_while(ignore_chars) }) }
				}
			}
			else { Err(UnexpectedHead) }
		}
	}
}
impl<'s> ObjectConfigArg<'s>
{
	ParseObjectConfigArgN!(parse1 = 1);
	ParseObjectConfigArgN!(parse2 = 2);
	ParseObjectConfigArgN!(parse3 = 3);
}

// Parse Error
#[derive(Debug, PartialEq)]
pub enum ParseError
{
	UnexpectedHead, NameRequired, UnknownDeviceResource, UnknownFormat, UnknownImageLayout, IntValueRequired,
	UnknownRenderPassAttachmentOptions, ImageLayoutRequired, DirectionRequired, NumericParseError(std::num::ParseIntError),
	DelimiterRequired, ClosingRequired, DefinitionOverrided, CorruptedSubpassDesc, UnknownPipelineStageFlag, UnknownAccessFlag,
	Expected(&'static str), UnknownConfig(&'static str), UnknownExternalResource, UnknownClearMode, FormatRequired
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
			Err(ParseError::NameRequired) => panic!("Name required following $ at line {}", line),
			Err(ParseError::UnknownDeviceResource) => panic!("Unknown Device Resource was found at line {}", line),
			Err(ParseError::UnknownFormat) => panic!("Unknown Image Format was found at line {}", line),
			Err(ParseError::UnknownImageLayout) => panic!("Unknown Image Layout was found at line {}", line),
			Err(ParseError::UnknownRenderPassAttachmentOptions) => panic!("Unknown Options for RenderPass Attachment was found at line {}", line),
			Err(ParseError::ImageLayoutRequired) => panic!("Image Layout required at line {}", line),
			Err(ParseError::DirectionRequired) => panic!("Direction Token(->, <-, To or From) required at line {}", line),
			Err(ParseError::DelimiterRequired) => panic!("Delimiter required at line {}", line),
			Err(ParseError::ClosingRequired) => panic!("Closing required at line {}", line),
			Err(ParseError::DefinitionOverrided) => panic!("Multiple definitions are found at line {}", line),
			Err(ParseError::CorruptedSubpassDesc) => panic!("Some Error are found parsing SubpassDesc at line {}", line),
			Err(ParseError::IntValueRequired) => panic!("Integer or ConfigRef required at line {}", line),
			Err(ParseError::UnknownPipelineStageFlag) => panic!("Unknown Pipeline Stage Flag was found at line {}", line),
			Err(ParseError::UnknownAccessFlag) => panic!("Unknown Access Mask Flag was found at line {}", line),
			Err(ParseError::UnknownExternalResource) => panic!("Unknown External Resource was found at line {}", line),
			Err(ParseError::UnknownClearMode) => panic!("Unknown Clear Mode was found at line {}", line),
			Err(ParseError::FormatRequired) => panic!("Format required for RenderPass Attachment at line {}", line),
			Err(ParseError::NumericParseError(n)) => panic!("NumericParseError: {} at line {}", n, line),
			Err(ParseError::Expected(s)) => panic!("Expected {}, but it was not found at line {}", s, line),
			Err(ParseError::UnknownConfig(s)) => panic!("Unknown Config for {} was found at line {}", s, line)
		}
	}
}

fn ignore_chars(c: char) -> bool { c == ' ' || c == '\t' }
fn ident_break(c: char) -> bool
{
	c == ':' || c == '-' || c == '[' || c == ']' || c == ',' || c == '<' || c == '>' || ignore_chars(c)
}
fn parse_device_resource(mut source: LazyLines)
{
	if let Some((l, s)) = source.pop()
	{
		match s.front()
		{
			Some('#') => parse_device_resource(source),
			Some('$') => parse_named_device_resource(l, s, source),
			Some(_) => parse_unnamed_device_resource(l, s, source),
			_ => Err(ParseError::UnexpectedHead).report_error(l)
		}
	}
}
fn acquire_name(source: &[char]) -> Result<(&[char], &[char]), ParseError>
{
	assert_eq!(source[0], '$');
	let (name, rest) = source.drop(1).take_while(|c| c != ':');
	if !name.is_empty() { Ok((name, rest.drop(1))) } else { Err(ParseError::NameRequired) }
}
fn parse_named_device_resource(current: usize, source: &[char], mut restlines: LazyLines)
{
	let (name, rest) = acquire_name(source).report_error(current);
	parse_unnamed_device_resource(current, rest.drop(1).skip_while(ignore_chars), restlines)
}
fn parse_unnamed_device_resource(current: usize, source: &[char], mut rest: LazyLines)
{
	let (s, r) = source.take_until(ident_break);
	match s.clone_as_string().as_ref()
	{
		"RenderPass" => { parse_renderpass(&mut rest); },
		"SimpleRenderPass" => { parse_simple_renderpass(current, &mut rest); },
		"PresentedRenderPass" => { parse_presented_renderpass(current, &mut rest); },
		"DescriptorSetLayout" => parse_descriptor_set_layout(rest),
		"PushConstantLayout" => parse_push_constant_layout(rest),
		"PipelineLayout" => parse_pipeline_layout(rest),
		"DescriptorSets" => parse_descriptor_sets(rest),
		"PipelineState" => parse_pipeline_state(current, source, rest),
		"Extern" => { parse_extern_resources(r.skip_while(ignore_chars)).report_error(current); },
		"Framebuffer" => { parse_framebuffer(s).report_error(current); }
		_ => Err(ParseError::UnknownDeviceResource).report_error(current)
	};
}
fn parse_extern_resources(source: &[char]) -> Result<ExternalResourceData, ParseError>
{
	fn image_dimension(source: &[char]) -> Result<u8, ParseError>
	{
		if source == &['1', 'D'] { Ok(1) }
		else if source == &['2', 'D'] { Ok(2) }
		else if source == &['3', 'D'] { Ok(3) }
		else { Err(ParseError::Expected("Image Dimension")) }
	}

	let (s, r) = source.take_until(ident_break);
	match s.clone_as_string().as_ref()
	{
		"ImageView" => Let(|| r.skip_while(ignore_chars).take_until(ident_break))._in(|(d, r)|
			image_dimension(d).and_then(|d| parse_string_literal(r.skip_while(ignore_chars)).map(|(rn, _)| ExternalResourceData::ImageView { dim: d, refname: rn }))
		),
		"SwapChainViews" => Ok(ExternalResourceData::SwapChainViews),
		_ => Err(ParseError::UnknownExternalResource)
	}
}
fn parse_renderpass(source: &mut LazyLines) -> RenderPassData
{
	let mut rpd = RenderPassData { attachments: NamedContents::new(), passes: NamedContents::new(), deps: Vec::new() };
	while let Some((l, s)) = source.next()
	{
		match ObjectConfigArg::parse1(s)
		{
			Ok(ObjectConfigArg { name, .. }) => match name.as_ref()
			{
				"Attachments" => if !rpd.attachments.is_empty() { Err(ParseError::DefinitionOverrided).report_error(l) }
				else
				{
					source.drop_line();
					while let Some((l, s)) = source.next()
					{
						match NamedConfigLine::parse2(s, parse_rp_attachment)
						{
							Ok(NamedConfigLine { name: Some(name), config }) => { source.drop_line(); rpd.attachments.insert(name.into(), config); },
							Ok(NamedConfigLine { config, .. }) => { source.drop_line(); rpd.attachments.insert_unnamed(config); },
							Err(ParseError::UnexpectedHead) => break,
							e => { e.report_error(l); }
						}
					}
				},
				"Subpasses" => if !rpd.passes.is_empty() { Err(ParseError::DefinitionOverrided).report_error(l) }
				else
				{
					source.drop_line();
					while let Some((l, s)) = source.next()
					{
						match NamedConfigLine::parse2(s, parse_subpass_desc)
						{
							Ok(NamedConfigLine { name: Some(name), config }) => { source.drop_line(); rpd.passes.insert(name.into(), config); },
							Ok(NamedConfigLine { config, .. }) => { source.drop_line(); rpd.passes.insert_unnamed(config); },
							Err(ParseError::UnexpectedHead) => break,
							e => { e.report_error(l); }
						}
					}
				},
				"Dependencies" => if !rpd.deps.is_empty() { Err(ParseError::DefinitionOverrided).report_error(l) }
				else
				{
					source.drop_line();
					while let Some((l, s)) = source.next()
					{
						if s.starts_with_trailing_opt(&['-'; 2], |c| c != '-')
						{
							match parse_subpass_deps(s.drop(2).skip_while(ignore_chars))
							{
								Ok(c) => { source.drop_line(); rpd.deps.push(c); },
								e => { e.report_error(l); }
							}
						}
						else { break; }
					}
				},
				_ => Err(ParseError::UnknownConfig("RenderPass")).report_error(l)
			},
			Err(ParseError::UnexpectedHead) => break,
			e => { e.report_error(l); }
		}
	}
	rpd
}
fn parse_rp_clear_mode(source: &[char]) -> Result<Option<bool>, ParseError>
{
	if source == &['N', 'o', 'n', 'e'] { Ok(None) }
	else if source == &['O', 'n', 'L', 'o', 'a', 'd'] { Ok(Some(true)) }
	else if source == &['P', 'r', 'e', 's', 'e', 'r', 'v', 'e'] { Ok(Some(false)) }
	else { Err(ParseError::UnknownClearMode) }
}
fn parse_simple_renderpass(line_in: usize, source: &mut LazyLines) -> SimpleRenderPassData
{
	let (mut fmt, mut clear_mode) = (None, None);
	while let Some((l, s)) = source.next()
	{
		match ObjectConfigArg::parse1(s)
		{
			Ok(ObjectConfigArg { name, value }) => match name.as_ref()
			{
				"Format" => parse_pixel_format(value)
					.and_then(|(f, _)| if fmt.is_none() { fmt = Some(f); Ok(()) } else { Err(ParseError::DefinitionOverrided) }).report_error(l),
				"ClearMode" => { clear_mode = parse_rp_clear_mode(value).report_error(l); },
				_ => Err(ParseError::UnknownConfig("SimpleRenderPass")).report_error(l)
			},
			Err(ParseError::UnexpectedHead) => break,
			e => { e.report_error(l); }
		}
	}
	if fmt.is_none() { Err(ParseError::FormatRequired).report_error(line_in) }
	else
	{
		SimpleRenderPassData { format: fmt.unwrap(), clear_on_load: clear_mode }
	}
}
fn parse_presented_renderpass(line_in: usize, source: &mut LazyLines) -> PresentedRenderPassData
{
	let (mut fmt, mut clear_mode) = (None, None);
	while let Some((l, s)) = source.next()
	{
		match ObjectConfigArg::parse1(s)
		{
			Ok(ObjectConfigArg { name, value }) =>
			{
				source.drop_line();
				match name.as_ref()
				{
					"Format" => parse_pixel_format(value)
						.and_then(|(f, _)| if fmt.is_none() { fmt = Some(f); Ok(()) } else { Err(ParseError::DefinitionOverrided) }).report_error(l),
					"ClearMode" => { clear_mode = parse_rp_clear_mode(value).report_error(l); },
					_ => Err(ParseError::UnknownConfig("SimpleRenderPass")).report_error(l)
				}
			},
			Err(ParseError::UnexpectedHead) => break,
			e => { e.report_error(l); }
		}
	}
	if fmt.is_none() { Err(ParseError::FormatRequired).report_error(line_in) }
	else
	{
		PresentedRenderPassData { format: fmt.unwrap(), clear_on_load: clear_mode }
	}
}
fn parse_framebuffer(source: &[char]) -> Result<(), ParseError>
{
	unimplemented!();
}
fn parse_descriptor_set_layout(source: LazyLines)
{
	unimplemented!();
}
fn parse_push_constant_layout(source: LazyLines)
{
	unimplemented!();
}
fn parse_pipeline_layout(source: LazyLines)
{
	unimplemented!();
}
fn parse_descriptor_sets(source: LazyLines)
{
	unimplemented!();
}
fn parse_pipeline_state(current: usize, source: &[char], restlines: LazyLines)
{
	unimplemented!();
}

lazy_static!
{
	static ref RPO_CLEAR_ON_LOAD: Vec<char> = "ClearOnLoad".chars().collect();
	static ref RPO_LOAD_CONTENT: Vec<char> = "LoadContent".chars().collect();
	static ref RPO_PRESERVE_CONTENT: Vec<char> = "PreserveContent".chars().collect();
}
// pixel_format "," image_layout [(To/From) image_layout] "," option,*
fn parse_rp_attachment(source: &[char]) -> Result<RPAttachment, ParseError>
{
	parse_pixel_format(source).and_then(|(format, rest)|
	{
		let rest = rest.skip_while(ignore_chars);
		if rest.is_front_of(',')
		{
			let rest = rest.drop(1).skip_while(ignore_chars);
			Transition::parse_opt(rest, parse_image_layout).and_then(|(l, rest)|
			{
				let rest = rest.skip_while(ignore_chars);
				let mut rpa = RPAttachment
				{
					format: format, layouts: l, clear_on_load: None, preserve_content: false
				};
				if rest.is_front_of(',')
				{
					fn recursive<'s>(rpa: &mut RPAttachment, source: &'s [char]) -> Result<(), ParseError>
					{
						if source.is_empty() { Ok(()) }
						else if source.starts_with(&RPO_CLEAR_ON_LOAD)
						{
							rpa.clear_on_load = Some(true);
							let rest = source.drop(RPO_CLEAR_ON_LOAD.len()).skip_while(ignore_chars);
							if rest.is_front_of('/') { recursive(rpa, rest.drop(1).skip_while(ignore_chars)) }
							else { Ok(()) }
						}
						else if source.starts_with(&RPO_LOAD_CONTENT)
						{
							rpa.clear_on_load = Some(false);
							let rest = source.drop(RPO_CLEAR_ON_LOAD.len()).skip_while(ignore_chars);
							if rest.is_front_of('/') { recursive(rpa, rest.drop(1).skip_while(ignore_chars)) }
							else { Ok(()) }
						}
						else if source.starts_with(&RPO_PRESERVE_CONTENT)
						{
							rpa.preserve_content = true;
							let rest = source.drop(RPO_CLEAR_ON_LOAD.len()).skip_while(ignore_chars);
							if rest.is_front_of('/') { recursive(rpa, rest.drop(1).skip_while(ignore_chars)) }
							else { Ok(()) }
						}
						else { Err(ParseError::UnknownRenderPassAttachmentOptions) }
					}
					recursive(&mut rpa, rest.drop(1).skip_while(ignore_chars)).map(|_| rpa)
				}
				else { Ok(rpa) }
			})
		}
		else { Err(ParseError::DelimiterRequired) }
	})
}
lazy_static!
{
	static ref SDI_RENDER_TO: Vec<char> = "RenderTo".chars().collect();
}
// ("RenderTo" (int/ints) / "From" (int/ints))*
fn parse_subpass_desc(input: &[char]) -> Result<RPSubpassDesc, ParseError>
{
	fn recursive<'s>(input: &'s [char], sink: &mut RPSubpassDesc) -> Result<&'s [char], ParseError>
	{
		if input.is_empty() { Ok(input) }
		else if input.starts_with_trailing_opt(&SDI_RENDER_TO, ident_break)
		{
			// RenderTo int/[ints...]
			ConfigInt::parse_array(input.drop(SDI_RENDER_TO.len()).skip_while(ignore_chars)).and_then(|(v, r)|
			{
				if sink.color_outs.is_empty()
				{
					sink.color_outs = v;
					recursive(r.skip_while(ignore_chars), sink)
				}
				else { Err(ParseError::DefinitionOverrided) }
			})
		}
		else if let Some(r) = from_token(input)
		{
			// From int/[ints...]
			ConfigInt::parse_array(r.skip_while(ignore_chars)).and_then(|(v, r)|
			{
				if sink.inputs.is_empty()
				{
					sink.inputs = v;
					recursive(r.skip_while(ignore_chars), sink)
				}
				else { Err(ParseError::DefinitionOverrided) }
			})
		}
		else { Err(ParseError::CorruptedSubpassDesc) }
	}
	let mut rpsd = RPSubpassDesc { color_outs: Vec::new(), inputs: Vec::new() };
	recursive(input, &mut rpsd).map(|_| rpsd)
}
lazy_static!
{
	static ref BY_REGION_FLAG: Vec<char> = "ByRegion".chars().collect();
}
// int (From/To) int ":" access_mask (From/To) access_mask At stage_bits ["," ["ByRegion"]]
fn parse_subpass_deps(input: &[char]) -> Result<RPSubpassDeps, ParseError>
{
	use self::ParseError::*;

	Transition::parse(input, ConfigInt::parse).and_then(|(pt, r)|
	{
		let r = r.skip_while(ignore_chars);
		if r.front() == Some(':') { Ok((pt, r.drop(1).skip_while(ignore_chars))) } else { Err(DelimiterRequired) }
	})
	.and_then(|(pt, r)| Transition::parse(r, parse_access_mask).map(|(amt, r)| ((pt, amt), r.skip_while(ignore_chars))))
	.and_then(|(vp, r)| if let Some(r) = at_token(r) { Ok((vp, r.skip_while(ignore_chars))) } else { Err(DelimiterRequired) })
	.and_then(|((pt, amt), r)| parse_pipeline_stage_bits(r).map(|(sf, r)| ((pt, amt, sf), r.skip_while(ignore_chars))))
	.map(|((pt, amt, sf), r)| if r.front() == Some(',')
	{
		let r = r.drop(1).skip_while(ignore_chars);
		if r.starts_with_trailing_opt(&BY_REGION_FLAG, ident_break)
		{
			RPSubpassDeps { passtrans: pt, access_mask: amt, stage_bits: sf, by_region: true }
		}
		else { RPSubpassDeps { passtrans: pt, access_mask: amt, stage_bits: sf, by_region: false } }
	}
	else { RPSubpassDeps { passtrans: pt, access_mask: amt, stage_bits: sf, by_region: false } })
}

fn from_token(input: &[char]) -> Option<&[char]>
{
	if input.starts_with_trailing_opt(&['F', 'r', 'o', 'm'], ident_break) { Some(input.drop(4)) }
	else if input.starts_with(&['<', '-']) { Some(input.drop(2)) }
	else { None }
}
#[test] fn test_from_token()
{
	assert_eq!(from_token(&['F', 'r', 'o', 'm']), Some(&[][..]));
	assert_eq!(from_token(&['<', '-', '-']), Some(&['-'][..]));
	assert_eq!(from_token(&['F', 'r', 'o']), None);
	assert_eq!(from_token(&['F', 'r', 'o', 'm', 't']), None);
	assert_eq!(from_token(&['F', 'r', 'o', 'm', '[']), Some(&['['][..]));
}
fn at_token(input: &[char]) -> Option<&[char]>
{
	if input.starts_with_trailing_opt(&['A', 't'], ident_break) { Some(input.drop(2)) }
	else if input.front() == Some('@') { Some(input.drop(1)) }
	else { None }
}
#[test] fn test_at_token()
{
	assert_eq!(at_token(&['A', 't']), Some(&[][..]));
	assert_eq!(at_token(&['A', 't', 't']), None);
	assert_eq!(at_token(&['@']), Some(&[][..]));
	assert_eq!(at_token(&['@', 'p']), Some(&['p'][..]));
}

#[test] fn test_rp_attachment()
{
	assert_eq!(parse_rp_attachment(&"R8G8B8A8 UNORM, ShaderReadOnlyOptimal <- ColorAttachmentOptimal, PreserveContent".chars().collect_vec()), Ok(RPAttachment
	{
		format: PixelFormat::User(VkFormat::R8G8B8A8_UNORM),
		layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal },
		preserve_content: true, clear_on_load: None
	}));
	assert_eq!(parse_rp_attachment(&"R8G8B8A8 UNORM, ShaderReadOnlyOptimal , PreserveContent".chars().collect_vec()), Ok(RPAttachment
	{
		format: PixelFormat::User(VkFormat::R8G8B8A8_UNORM),
		layouts: Transition { from: VkImageLayout::ShaderReadOnlyOptimal, to: VkImageLayout::ShaderReadOnlyOptimal },
		preserve_content: true, clear_on_load: None
	}));
	assert_eq!(parse_rp_attachment(&"R32 SFLOAT, ShaderReadOnlyOptimal <- ColorAttachmentOptimal".chars().collect_vec()), Ok(RPAttachment
	{
		format: PixelFormat::User(VkFormat::R32_SFLOAT),
		layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal },
		preserve_content: false, clear_on_load: None
	}));
	assert_eq!(parse_rp_attachment(&"R8G8B8A8 UNORM, ShaderReadOnlyOptimal -> ColorAttachmentOptimal, ClearOnLoad".chars().collect_vec()), Ok(RPAttachment
	{
		format: PixelFormat::User(VkFormat::R8G8B8A8_UNORM),
		layouts: Transition { from: VkImageLayout::ShaderReadOnlyOptimal, to: VkImageLayout::ColorAttachmentOptimal },
		preserve_content: false, clear_on_load: Some(true)
	}));
	assert_eq!(parse_rp_attachment(&"R8G8B8A8 SNORM, ShaderReadOnlyOptimal -> ColorAttachmentOptimal, LoadContent / PreserveContent".chars().collect_vec()),
		Ok(RPAttachment
		{
			format: PixelFormat::User(VkFormat::R8G8B8A8_SNORM),
			layouts: Transition { from: VkImageLayout::ShaderReadOnlyOptimal, to: VkImageLayout::ColorAttachmentOptimal },
			preserve_content: true, clear_on_load: Some(false)
		}));
	assert_eq!(parse_rp_attachment(&"R8G8B8A8 UNORM".chars().collect_vec()), Err(ParseError::DelimiterRequired));
	assert_eq!(parse_rp_attachment(&"R8G8B8A8 UNORM, ShaderReadOnlyOptimal, Hoge".chars().collect_vec()), Err(ParseError::UnknownRenderPassAttachmentOptions));
}
#[test] fn test_subpass_desc()
{
	assert_eq!(parse_subpass_desc(&"RenderTo 0".chars().collect_vec()), Ok(RPSubpassDesc { color_outs: vec![ConfigInt::Value(0)], inputs: Vec::new() }));
	assert_eq!(parse_subpass_desc(&"RenderTo 0 From 1".chars().collect_vec()), Ok(RPSubpassDesc { color_outs: vec![ConfigInt::Value(0)], inputs: vec![ConfigInt::Value(1)] }));
	assert_eq!(parse_subpass_desc(&"<- [1, 2] RenderTo [0, 3]".chars().collect_vec()),
		Ok(RPSubpassDesc { color_outs: vec![ConfigInt::Value(0), ConfigInt::Value(3)], inputs: vec![ConfigInt::Value(1), ConfigInt::Value(2)] }));
	assert_eq!(parse_subpass_desc(&"Preserve 0".chars().collect_vec()), Err(ParseError::CorruptedSubpassDesc));
	assert_eq!(parse_subpass_desc(&"RenderTo 0 RenderTo 1".chars().collect_vec()), Err(ParseError::DefinitionOverrided));
}
#[test] fn test_subpass_deps()
{
	fn test(case: &str, expect: Result<RPSubpassDeps, ParseError>)
	{
		assert_eq!(parse_subpass_deps(&case.chars().collect_vec()), expect);
	}
	test("0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage, ByRegion", Ok(RPSubpassDeps
	{
		passtrans: Transition { from: ConfigInt::Value(0), to: ConfigInt::Value(1) },
		access_mask: Transition { from: VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, to: VK_ACCESS_SHADER_READ_BIT },
		stage_bits: VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT, by_region: true
	}));
}
#[test] fn test_external_resources()
{
	fn test(case: &str, expect: Result<ExternalResourceData, ParseError>)
	{
		assert_eq!(parse_extern_resources(&case.chars().collect_vec()), expect);
	}
	test("ImageView 1D \"HogeResource\"", Ok(ExternalResourceData::ImageView { dim: 1, refname: "HogeResource".into() }));
	test("ImageView 2D \"HogeResource\"", Ok(ExternalResourceData::ImageView { dim: 2, refname: "HogeResource".into() }));
	test("ImageView 3D \"HogeResource\"", Ok(ExternalResourceData::ImageView { dim: 3, refname: "HogeResource".into() }));
	test("SwapChainViews ", Ok(ExternalResourceData::SwapChainViews));
	test("Framebuffer", Err(ParseError::UnknownExternalResource));
	test("ImageView \"HogeResource\"", Err(ParseError::Expected("Image Dimension")));
}