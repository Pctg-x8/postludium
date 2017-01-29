//! Postludium: Device Configuration Processor

use std;
use interlude::*;
use interlude::ffi::*;
use std::collections::HashMap;
use super::parsetools::*;
use std::borrow::Cow;
use lazylines::*;
use itertools::Itertools;

#[macro_use] mod items;
use self::items::*;

struct Let<F: FnOnce() -> T, T>(F);
impl<F: FnOnce() -> T, T> Let<F, T>
{
	fn _in<G: FnOnce(T) -> R, R>(self, f: G) -> R { f(self.0()) }
}
fn id<T>(t: T) -> T { t }

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
pub enum PartialResult<'s, T>
{
	Success(T, ParseLine<'s>), Failed(ParseError)
}
pub use self::PartialResult::*;
impl<'s, T> PartialResult<'s, T>
{
	pub fn from_opt(o: Option<T>, rest: ParseLine<'s>, err: ParseError) -> Self
	{
		if let Some(o) = o { Success(o, rest) } else { Failed(err) }
	}
	pub fn vmap<F, U>(self, map: F) -> PartialResult<'s, U> where F: FnOnce(T) -> U
	{
		match self { Success(v, r) => Success(map(v), r), Failed(e) => Failed(e) }
	}
	pub fn result<F, U>(self, map: F) -> Result<U, ParseError> where F: FnOnce(T, ParseLine<'s>) -> U
	{
		match self { Success(v, r) => Ok(map(v, r)), Failed(e) => Err(e) }
	}
	pub fn and_then<F, U>(self, map: F) -> PartialResult<'s, U> where F: FnOnce(T, ParseLine<'s>) -> PartialResult<'s, U>
	{
		match self { Success(v, r) => map(v, r), Failed(e) => Failed(e) }
	}
	pub fn pipe_result<F, U>(self, pipe: F) -> Result<U, ParseError> where F: FnOnce(T, ParseLine<'s>) -> Result<U, ParseError>
	{
		match self { Success(v, r) => pipe(v, r), Failed(e) => Err(e) }
	}
}

#[cfg_attr(test, derive(Debug, PartialEq))] pub struct Transition<T> { from: T, to: T }
impl<T> Transition<T>
{
	fn parse<'s, F>(input: ParseLine<'s>, childparser: F) -> PartialResult<'s, Self> where F: Fn(ParseLine<'s>) -> PartialResult<'s, T>
	{
		childparser(input).and_then(|a, r|
		{
			let r = r.drop_while(ignore_chars);
			if r.starts_with_trailing_opt(&['T', 'o'], ident_break) || r.starts_with(&['-', '>'])
			{
				childparser(r.drop_opt(2).drop_while(ignore_chars)).vmap(|b| Transition { from: a, to: b })
			}
			else if let Some(r) = from_token(r.clone()) { childparser(r.drop_while(ignore_chars)).vmap(|b| Transition { from: b, to: a }) }
			else { Failed(ParseError::DirectionRequired(r.current())) }
		})
	}
}
impl<T> Transition<T> where T: Copy
{
	fn parse_opt<'s, F>(input: ParseLine<'s>, childparser: F) -> PartialResult<'s, Self> where F: Fn(ParseLine<'s>) -> PartialResult<'s, T>
	{
		childparser(input).and_then(|a, r|
		{
			let r = r.drop_while(ignore_chars);
			if r.starts_with_trailing_opt(&['T', 'o'], ident_break) || r.starts_with(&['-', '>'])
			{
				childparser(r.drop_opt(2).drop_while(ignore_chars)).vmap(|b| Transition { from: a, to: b })
			}
			else if let Some(r) = from_token(r.clone())
			{
				childparser(r.drop_while(ignore_chars)).vmap(|b| Transition { to: a, from: b })
			}
			else { Success(Transition { to: a, from: a }, r) }
		})
	}
}
pub struct NamedConfigLine<C> { name: Option<String>, config: C }
impl<C> NamedConfigLine<C>
{
	fn parse0<'s, F>(input: ParseLine<'s>, argparser: F) -> Result<Self, ParseError> where F: FnOnce(ParseLine<'s>) -> Result<C, ParseError>
	{
		let name_res = if input.front() == Some('$')
		{
			let (name, rest) = input.drop_opt(1).take_until(ident_break);
			if name.is_empty() { Err(ParseError::NameRequired(name.current())) }
			else
			{
				let rest = rest.drop_while(ignore_chars);
				if rest.front() == Some(':') { Ok((Some(name.clone_as_string()), rest.drop_opt(1))) }
				else { Err(ParseError::DelimiterRequired(rest.current())) }
			}
		}
		else { Ok((None, input)) };

		name_res.and_then(|(name_opt, rest)| argparser(rest.drop_while(ignore_chars)).map(|v| NamedConfigLine { name: name_opt, config: v }))
	}
	fn parse1<'s, F>(input: ParseLine<'s>, argparser: F) -> Result<Self, ParseError> where F: FnOnce(ParseLine<'s>) -> Result<C, ParseError>
	{
		if input.starts_with_trailing_opt(&['-'], |c| c != '-') { Self::parse0(input.drop_opt(1).drop_while(ignore_chars), argparser) }
		else { Err(ParseError::UnexpectedHead) }
	}
	fn parse2<'s, F>(input: ParseLine<'s>, argparser: F) -> Result<Self, ParseError> where F: FnOnce(ParseLine<'s>) -> Result<C, ParseError>
	{
		if input.starts_with_trailing_opt(&['-'; 2], |c| c != '-') { Self::parse0(input.drop_opt(2).drop_while(ignore_chars), argparser) }
		else { Err(ParseError::UnexpectedHead) }
	}
	fn parse3<'s, F>(input: ParseLine<'s>, argparser: F) -> Result<Self, ParseError> where F: FnOnce(ParseLine<'s>) -> Result<C, ParseError>
	{
		if input.starts_with_trailing_opt(&['-'; 3], |c| c != '-') { Self::parse0(input.drop_opt(2).drop_while(ignore_chars), argparser) }
		else { Err(ParseError::UnexpectedHead) }
	}
}

// Parse Error
#[derive(Debug, PartialEq)]
pub enum ParseError
{
	UnexpectedHead, NameRequired(usize), UnknownDeviceResource(usize), UnknownFormat(usize), UnknownImageLayout(usize), IntValueRequired(usize),
	UnknownRenderPassAttachmentOptions(usize), ImageLayoutRequired(usize), DirectionRequired(usize), NumericParseError(std::num::ParseIntError, usize),
	DelimiterRequired(usize), ClosingRequired(usize), DefinitionOverrided, CorruptedSubpassDesc(usize),
	UnknownPipelineStageFlag(usize), UnknownAccessFlag(usize), Expected(&'static str, usize), UnknownConfig(&'static str), ConfigRequired(&'static str),
	UnknownExternalResource(usize), UnknownClearMode(usize), FormatRequired(usize), UnknownObjectRef(&'static str, usize)
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
			Err(ParseError::DefinitionOverrided) => panic!("Multiple definitions are found at line {}", line),
			Err(ParseError::CorruptedSubpassDesc(p)) => panic!("Some Error are found parsing SubpassDesc at line {}, col {}", line, p),
			Err(ParseError::IntValueRequired(p)) => panic!("Integer or ConfigRef required at line {}, col {}", line, p),
			Err(ParseError::UnknownPipelineStageFlag(p)) => panic!("Unknown Pipeline Stage Flag was found at line {}, col {}", line, p),
			Err(ParseError::UnknownAccessFlag(p)) => panic!("Unknown Access Mask Flag was found at line {}, col {}", line, p),
			Err(ParseError::UnknownExternalResource(p)) => panic!("Unknown External Resource was found at line {}, col {}", line, p),
			Err(ParseError::UnknownClearMode(p)) => panic!("Unknown Clear Mode was found at line {}, col {}", line, p),
			Err(ParseError::FormatRequired(p)) => panic!("Format required for RenderPass Attachment at line {}, col {}", line, p),
			Err(ParseError::UnknownObjectRef(n, p)) => panic!("Unknown {} ref at line {}, col {}", n, line, p),
			Err(ParseError::NumericParseError(n, p)) => panic!("NumericParseError: {} at line {}, col {}", n, line, p),
			Err(ParseError::Expected(s, p)) => panic!("Expected {}, but it was not found at line {}, col {}", s, line, p),
			Err(ParseError::UnknownConfig(s)) => panic!("Unknown Config for {} was found at line {}", s, line),
			Err(ParseError::ConfigRequired(c)) => panic!("Configuration \"{}\" required at line {}", c, line)
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
			Some('$') => parse_named_device_resource(l, ParseLine(s, 0), source),
			Some(_) => parse_unnamed_device_resource(l, ParseLine(s, 0), source),
			_ => Err(ParseError::UnexpectedHead).report_error(l)
		}
	}
}
fn parse_named_device_resource(current: usize, source: ParseLine, mut restlines: LazyLines)
{
	NamedConfigLine::parse0(source, |r|
	{
		parse_unnamed_device_resource(current, r.drop_while(ignore_chars), restlines);
		Ok(())
	}).report_error(current);
}
fn parse_unnamed_device_resource(current: usize, source: ParseLine, mut rest: LazyLines)
{
	let (s, sr) = source.take_until(ident_break);
	match s.clone_as_string().as_ref()
	{
		"RenderPass" => { parse_renderpass(&mut rest); },
		"SimpleRenderPass" => { parse_simple_renderpass(current, &mut rest); },
		"PresentedRenderPass" => { parse_presented_renderpass(current, &mut rest); },
		"DescriptorSetLayout" => parse_descriptor_set_layout(rest),
		"PushConstantLayout" => parse_push_constant_layout(rest),
		"PipelineLayout" => parse_pipeline_layout(rest),
		"DescriptorSets" => parse_descriptor_sets(rest),
		"PipelineState" => parse_pipeline_state(current, sr, rest),
		"Extern" => { parse_extern_resources(sr.drop_while(ignore_chars)).report_error(current); },
		"Framebuffer" => { parse_framebuffer(sr, &mut rest).report_error(current); }
		_ => Err(ParseError::UnknownDeviceResource(s.current())).report_error(current)
	};
}
fn parse_extern_resources(input: ParseLine) -> Result<ExternalResourceData, ParseError>
{
	fn image_dimension(input: &ParseLine) -> Result<u8, ParseError>
	{
		if *input == ['1', 'D'][..] { Ok(1) }
		else if *input == ['2', 'D'][..] { Ok(2) }
		else if *input == ['3', 'D'][..] { Ok(3) }
		else { Err(ParseError::Expected("Image Dimension", input.current())) }
	}

	let (s, r) = input.take_until(ident_break);
	match s.clone_as_string().as_ref()
	{
		"ImageView" =>
		{
			let (d, r) = r.drop_while(ignore_chars).take_until(ident_break);
			image_dimension(&d).and_then(|d| parse_string_literal(r.drop_while(ignore_chars)).result(|n, _| ExternalResourceData::ImageView { dim: d, refname: n }))
		},
		"SwapChainViews" => Ok(ExternalResourceData::SwapChainViews),
		_ => Err(ParseError::UnknownExternalResource(s.current()))
	}
}
fn parse_config_name<'s>(source: ParseLine<'s>) -> PartialResult<ParseLine<'s>>
{
	let (name, rest) = source.take_until(ident_break);
	let rest = rest.drop_while(ignore_chars);
	if rest.front() != Some(':') { Failed(ParseError::DelimiterRequired(rest.current())) }
	else { Success(name, rest.drop_opt(1)) }
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
	while let Some((l, s)) = source.next()
	{
		let s = ParseLine(s, 0);
		if s.front() == Some('-') && s.peek(1) != Some('-')
		{
			parse_config_name(s.drop_opt(2)).pipe_result(|name, _|
				if name == ATTACHMENTS[..]
				{
					if !rpd.attachments.is_empty() { Err(ParseError::DefinitionOverrided).report_error(l) }
					else
					{
						source.drop_line();
						while let Some((l, s)) = source.next()
						{
							match NamedConfigLine::parse2(ParseLine(s, 0), parse_rp_attachment)
							{
								Ok(NamedConfigLine { name: Some(name), config }) => { source.drop_line(); rpd.attachments.insert(name.into(), config); },
								Ok(NamedConfigLine { config, .. }) => { source.drop_line(); rpd.attachments.insert_unnamed(config); },
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
						source.drop_line();
						while let Some((l, s)) = source.next()
						{
							match NamedConfigLine::parse2(ParseLine(s, 0), parse_subpass_desc)
							{
								Ok(NamedConfigLine { name: Some(name), config }) => { source.drop_line(); rpd.passes.insert(name.into(), config); },
								Ok(NamedConfigLine { config, .. }) => { source.drop_line(); rpd.passes.insert_unnamed(config); },
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
						source.drop_line();
						while let Some((l, s)) = source.next()
						{
							let s = ParseLine(s, 0);
							if s.starts_with_trailing_opt(&['-'; 2], |c| c != '-')
							{
								let r = s.drop_opt(2).drop_while(ignore_chars);
								match parse_subpass_deps(r)
								{
									Ok(c) => { source.drop_line(); rpd.deps.push(c); },
									e => { e.report_error(l); }
								}
							}
							else { break; }
						}
					}
					Ok(())
				}
				else { Err(ParseError::UnknownConfig("RenderPass")) }
			).report_error(l);
		}
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
	while let Some((l, s)) = source.next()
	{
		let mut s = ParseLine(s, 0);
		if s.front() == Some('-') && s.peek(1) != Some('-')
		{
			parse_config_name(s.drop_opt(1)).pipe_result(|name, r|
				if name == FORMAT[..]
				{
					PixelFormat::parse(r.drop_while(ignore_chars))
						.pipe_result(|f, _| if fmt.is_none() { fmt = Some(f); Ok(()) } else { Err(ParseError::DefinitionOverrided) })
				}
				else if name == CLEARMODE[..]
				{
					parse_rp_clear_mode(&r.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () })
				}
				else { Err(ParseError::UnknownConfig("SimpleRenderPass")) }
			).report_error(l);
		}
		else { break; }
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
	while let Some((l, s)) = source.next()
	{
		if s.front() == Some('-') && s.peek(1) != Some('-')
		{
			let s = ParseLine(&s[1..], 1);
			parse_config_name(s.drop_while(ignore_chars)).pipe_result(|name, r|
				if name == FORMAT[..]
				{
					PixelFormat::parse(r.drop_while(ignore_chars))
						.pipe_result(|f, _| if fmt.is_none() { fmt = Some(f); Ok(()) } else { Err(ParseError::DefinitionOverrided) })
				}
				else if name == CLEARMODE[..]
				{
					parse_rp_clear_mode(&r.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () })
				}
				else { Err(ParseError::UnknownConfig("PresentedRenderPass")) }
			).report_error(l);
		}
		else { break; }
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
fn parse_framebuffer_rp(input: ParseLine) -> PartialResult<FramebufferRenderPassRef>
{
	if input.front() == Some('<')
	{
		let r = input.drop_opt(1);
		// Which Parameter: "Presented" / int
		let p = if r.front().map(|c| ('0' <= c && c <= '9') || c == '$').unwrap_or(false)
		{
			// int
			ConfigInt::parse(r).vmap(FramebufferRenderPassRef::Int)
		}
		else if r.starts_with_trailing_opt(&PRESENTED, ident_break)
		{
			Success(FramebufferRenderPassRef::Presented, r.drop_opt(PRESENTED.len()))
		}
		else { Failed(ParseError::UnknownObjectRef("RenderPass", r.current())) };
		
		p.and_then(|v, r|
		{
			let r = r.drop_while(ignore_chars);
			if r.front() == Some('>') { Success(v, r.drop_opt(1)) } else { Failed(ParseError::ClosingRequired(r.current())) }
		})
	}
	else { Success(FramebufferRenderPassRef::None, input) }
}
fn parse_framebuffer(rest: ParseLine, mut source: &mut LazyLines) -> Result<FramebufferInfo, ParseError>
{
	match parse_framebuffer_rp(rest.drop_while(ignore_chars)).and_then(|arg, r| ConfigInt::parse_array(r.drop_while(ignore_chars)).vmap(|vs| (arg, vs)))
	{
		Success((arg, vs), _) =>
		{
			let mut clear_mode = None;
			while let Some((l, s)) = source.next()
			{
				if s.front() == Some('-') && s.peek(1) != Some('-')
				{
					source.drop_line();
					parse_config_name(ParseLine(&s[1..], 1).drop_while(ignore_chars)).pipe_result(|name, r|
						if name == CLEARMODE[..]
						{
							parse_rp_clear_mode(&r.drop_while(ignore_chars)).map(|cm| { clear_mode = cm; () })
						}
						else { Err(ParseError::UnknownConfig("Framebuffer")) }
					).report_error(l)
				}
				else { break; }
			}
			let style = match arg
			{
				FramebufferRenderPassRef::Int(rp) => FramebufferStyle::WithRenderPass(rp),
				FramebufferRenderPassRef::Presented => FramebufferStyle::Presented(clear_mode),
				FramebufferRenderPassRef::None => FramebufferStyle::Simple(clear_mode)
			};
			Ok(FramebufferInfo { style: style, views: vs })
		}
		Failed(e) => Err(e)
	}
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
fn parse_pipeline_state(current: usize, mut source: ParseLine, restlines: LazyLines)
{
	unimplemented!();
}

lazy_static!
{
	static ref RPO_CLEAR_ON_LOAD: Vec<char> = "ClearOnLoad".chars().collect();
	static ref RPO_LOAD_CONTENT: Vec<char> = "LoadContent".chars().collect();
	static ref RPO_PRESERVE_CONTENT: Vec<char> = "PreserveContent".chars().collect();
}
// pixel_format "," transition_opt#image_layout "," option,*
fn parse_rp_attachment(source: ParseLine) -> Result<RPAttachment, ParseError>
{
	PixelFormat::parse(source).and_then(|format, r|
	{
		let r = r.drop_while(ignore_chars);
		if r.front() != Some(',') { Failed(ParseError::DelimiterRequired(r.current())) }
		else
		{
			Transition::parse_opt(r.drop_opt(1).drop_while(ignore_chars), parse_image_layout).vmap(|layouts| (format, layouts))
		}
	}).vmap(|(format, layouts)| RPAttachment { format: format, layouts: layouts, clear_on_load: None, preserve_content: false })
	.pipe_result(|mut rpa, r|
	{
		let r = r.drop_while(ignore_chars);
		if r.front() != Some(',') { Ok(rpa) }
		else
		{
			fn recursive(rpa: &mut RPAttachment, source: ParseLine) -> Result<(), ParseError>
			{
				if source.is_empty() { Ok(()) }
				else if source.starts_with_trailing_opt(&RPO_CLEAR_ON_LOAD, ident_break)
				{
					rpa.clear_on_load = Some(true);
					let r = source.drop_opt(RPO_CLEAR_ON_LOAD.len()).drop_while(ignore_chars);
					if r.front() == Some('/') { recursive(rpa, r.drop_opt(1).drop_while(ignore_chars)) } else { Ok(()) }
				}
				else if source.starts_with_trailing_opt(&RPO_LOAD_CONTENT, ident_break)
				{
					rpa.clear_on_load = Some(false);
					let r = source.drop_opt(RPO_LOAD_CONTENT.len()).drop_while(ignore_chars);
					if r.front() == Some('/') { recursive(rpa, r.drop_opt(1).drop_while(ignore_chars)) } else { Ok(()) }
				}
				else if source.starts_with_trailing_opt(&RPO_PRESERVE_CONTENT, ident_break)
				{
					rpa.preserve_content = true;
					let r = source.drop_opt(RPO_PRESERVE_CONTENT.len()).drop_while(ignore_chars);
					if r.front() == Some('/') { recursive(rpa, r.drop_opt(1).drop_while(ignore_chars)) } else { Ok(()) }
				}
				else { Err(ParseError::UnknownRenderPassAttachmentOptions(source.current())) }
			}
			recursive(&mut rpa, r.drop_opt(1).drop_while(ignore_chars)).map(|_| rpa)
		}
	})
}
lazy_static!
{
	static ref SDI_RENDER_TO: Vec<char> = "RenderTo".chars().collect();
}
// ("RenderTo" (int/ints) / From (int/ints))*
fn parse_subpass_desc(input: ParseLine) -> Result<RPSubpassDesc, ParseError>
{
	fn recursive<'s>(input: ParseLine<'s>, sink: &mut RPSubpassDesc) -> Result<(), ParseError>
	{
		if input.is_empty() { Ok(()) }
		else if input.starts_with_trailing_opt(&SDI_RENDER_TO, ident_break)
		{
			// RenderTo int/[ints...]
			ConfigInt::parse_array(input.drop_opt(SDI_RENDER_TO.len()).drop_while(ignore_chars)).pipe_result(|v, r|
			{
				if sink.color_outs.is_empty()
				{
					sink.color_outs = v;
					recursive(r.drop_while(ignore_chars), sink)
				}
				else { Err(ParseError::DefinitionOverrided) }
			})
		}
		else if let Some(r) = from_token(input.clone())
		{
			// From int/[ints...]
			ConfigInt::parse_array(r.drop_while(ignore_chars)).pipe_result(|v, r|
			{
				if sink.inputs.is_empty()
				{
					sink.inputs = v;
					recursive(r.drop_while(ignore_chars), sink)
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
fn parse_subpass_deps(input: ParseLine) -> Result<RPSubpassDeps, ParseError>
{
	Transition::parse(input, ConfigInt::parse).and_then(|pt, r|
	{
		let r = r.drop_while(ignore_chars);
		if r.front() == Some(':') { Success(pt, r.drop_opt(1).drop_while(ignore_chars)) }
		else { Failed(ParseError::DelimiterRequired(r.current())) }
	})
	.and_then(|pt, r| Transition::parse(r, parse_access_mask).vmap(|amt| (pt, amt)))
	.and_then(|(pt, amt), r|
	{
		let ep = r.current();
		if let Some(r) = at_token(r.drop_while(ignore_chars))
		{
			parse_pipeline_stage_bits(r.drop_while(ignore_chars)).vmap(|sf| (pt, amt, sf))
		}
		else { Failed(ParseError::DelimiterRequired(ep)) }
	})
	.result(|(pt, amt, sf), r|
	{
		let r = r.drop_while(ignore_chars);
		if r.front() == Some(',')
		{
			let r2 = r.clone().drop_opt(1).drop_while(ignore_chars);
			if r2.starts_with_trailing_opt(&BY_REGION_FLAG, ident_break)
			{
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

fn from_token<'s>(input: ParseLine<'s>) -> Option<ParseLine<'s>>
{
	if input.starts_with_trailing_opt(&['F', 'r', 'o', 'm'], ident_break) { Some(input.drop_opt(4)) }
	else if input.starts_with(&['<', '-']) { Some(input.drop_opt(2)) }
	else { None }
}
#[test] fn test_from_token()
{
	Testing!
	{
		from_token: "From" => Some(ParseLine(&[], 4)),
		from_token: "<--" => Some(ParseLine(&['-'], 2)),
		from_token: "From[" => Some(ParseLine(&['['], 4)),
		from_token: "Fro" => None,
		from_token: "Fromt" => None
	}
}
fn at_token(input: ParseLine) -> Option<ParseLine>
{
	if input.starts_with_trailing_opt(&['A', 't'], ident_break) { Some(input.drop_opt(2)) }
	else if input.front() == Some('@') { Some(input.drop_opt(1)) }
	else { None }
}
#[test] fn test_at_token()
{
	Testing!
	{
		at_token: "At" => Some(ParseLine(&[], 2)),
		at_token: "Att" => None,
		at_token: "@" => Some(ParseLine(&[], 1)),
		at_token: "@p" => Some(ParseLine(&['p'], 1))
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
#[test] fn test_subpass_deps()
{
	Testing!
	{
		parse_subpass_deps: "0 -> 1: ColorAttachmentWrite -> ShaderRead @ FragmentShaderStage, ByRegion" => Ok(RPSubpassDeps
		{
			passtrans: Transition { from: ConfigInt::Value(0), to: ConfigInt::Value(1) },
			access_mask: Transition { from: VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, to: VK_ACCESS_SHADER_READ_BIT },
			stage_bits: VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT, by_region: true
		})
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
		parse_framebuffer_rp: "<$FirstRP>" => Success(FramebufferRenderPassRef::Int(ConfigInt::Ref("FirstRP".into())), ParseLine(&[], 10)),
		parse_framebuffer_rp: "<Presented>" => Success(FramebufferRenderPassRef::Presented, ParseLine(&[], 11)),
		parse_framebuffer_rp: "n" => Success(FramebufferRenderPassRef::None, ParseLine(&['n'], 0)),
		parse_framebuffer_rp: "<0" => Failed(ParseError::ClosingRequired(2)),
		parse_framebuffer_rp: "<AA>" => Failed(ParseError::UnknownObjectRef("RenderPass", 1))
	}
}
