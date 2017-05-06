//! Syntax Tree Elements

use std;
use std::ops::{Index, Deref, Range};
use std::fmt::Debug;
use std::borrow::Cow;
use std::collections::{HashMap, BTreeMap};
use std::path::PathBuf;
use interlude::ffi::*;
use interlude::*;
use interlude;
use error::ParseError;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum OperationResult { Success, Failed(Cow<'static, str>) }
impl OperationResult
{
	pub fn propagate_failure(self, other: Self) -> Self { if let OperationResult::Failed(_) = other { other } else { self } }
	pub fn report_error(self, line: usize) { if let OperationResult::Failed(reason) = self { panic!("Operation has failed({}) in processing at line {}", reason, line) } }
	pub fn rewrap_err<F, R>(self, wrapper: F) -> Result<(), R> where F: FnOnce(Cow<'static, str>) -> R
	{
		if let OperationResult::Failed(n) = self { Err(wrapper(n)) } else { Ok(()) }
	}
}

pub enum InsertionResult<'s> { Success, Duplicated(Cow<'s, str>) }
impl<'s> InsertionResult<'s>
{
	pub fn into_parse_result(self) -> Result<(), ParseError>
	{
		match self
		{
			InsertionResult::Success => Ok(()),
			InsertionResult::Duplicated(n) => Err(ParseError::EntryDuplicated(n.into_owned().into()))
		}
	}
}

pub struct NamedContents<T>(pub HashMap<String, usize>, pub Vec<T>);
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
impl<T> NamedContents<T>
{
	pub fn new() -> Self { NamedContents(HashMap::new(), Vec::new()) }

	pub fn insert<'s>(&mut self, name: Cow<'s, str>, value: T) -> InsertionResult<'s>
	{
		if self.0.contains_key(name.deref()) { InsertionResult::Duplicated(name) }
		else { let index = self.insert_unnamed(value); self.0.insert(name.into_owned(), index); InsertionResult::Success }
	}
	pub fn insert_unnamed(&mut self, value: T) -> usize
	{
		self.1.push(value); self.1.len() - 1
	}
	pub fn insert_auto<'s>(&mut self, name: Option<Cow<'s, str>>, value: T) -> InsertionResult<'s>
	{
		if let Some(n) = name { self.insert(n, value) } else { self.insert_unnamed(value); InsertionResult::Success }
	}
	pub fn make_link(&mut self, name: Cow<str>, linkref: usize)
	{
		if self.0.contains_key(name.deref()) { *self.0.get_mut(name.as_ref()).unwrap() = linkref; }
		else { self.0.insert(name.into_owned(), linkref); }
	}
	pub fn iter(&self) -> std::slice::Iter<T> { self.1.iter() }
	pub fn into_iter(self) -> std::vec::IntoIter<T> { self.1.into_iter() }
	pub fn is_empty(&self) -> bool { self.1.is_empty() }

	pub fn reverse_index(&self, k: &str) -> Option<usize>
	{
		self.0.get(k).cloned()
	}
	pub fn make_reverse_dict(&self) -> HashMap<usize, &str>
	{
		let mut h = HashMap::new();
		for (n, &x) in self.0.iter()
		{
			h.insert(x, n.as_ref());
		}
		h
	}
	pub fn drain_to_reverse_dict(&mut self) -> (BTreeMap<usize, Vec<String>>, &mut Vec<T>)
	{
		let mut h = BTreeMap::new();
		for (n, x) in self.0.drain()
		{
			h.entry(x).or_insert_with(Vec::new).push(n)
		}
		(h, &mut self.1)
	}
}
impl<T: Eq> NamedContents<T> where T: std::borrow::Borrow<T> + std::borrow::ToOwned<Owned = T>
{
	pub fn insert_vunique<'s>(&mut self, name: Cow<'s, str>, value: Cow<T>) -> InsertionResult<'s>
	{
		if self.0.contains_key(name.deref()) { InsertionResult::Duplicated(name) }
		else { let index = self.insert_unnamed_vunique(value); self.0.insert(name.into_owned(), index); InsertionResult::Success }
	}
	pub fn insert_unnamed_vunique(&mut self, value: Cow<T>) -> usize
	{
		if let Some((i, _)) = self.1.iter().enumerate().find(|&(_, o)| o == value.as_ref()) { i }
		else { self.1.push(value.into_owned()); self.1.len() - 1 }
	}
	pub fn insert_auto_vunique<'s>(&mut self, name: Option<Cow<'s, str>>, value: Cow<T>) -> InsertionResult<'s>
	{
		if let Some(n) = name { self.insert_vunique(n, value) } else { self.insert_unnamed_vunique(value); InsertionResult::Success }
	}
}
impl<T: Debug> std::fmt::Debug for NamedContents<T>
{
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		self.1.iter().enumerate().map(|(x, c)| format!("{:?}: {:?}", self.0.iter().filter(|&(_, &y)| y == x).map(|(n, _)| n).collect::<Vec<_>>(), c))
			.collect::<Vec<_>>().fmt(fmt)
	}
}

/// The location information in source: Element Order = Line, Column
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Location(pub usize, pub usize);
impl Location
{
	pub fn line(&self) -> usize { self.0 }
	pub fn column(&self) -> usize { self.1 }
}
impl std::fmt::Display for Location
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{}:{}", self.0, self.1)
	}
}
/// SynTree Container: Packed value with the location which was in source
#[derive(Debug, PartialEq)]
pub struct LocationPacked<T: Debug + PartialEq>(pub Location, pub T);
impl<T: Clone + Debug + PartialEq> Clone for LocationPacked<T>
{
	fn clone(&self) -> Self { LocationPacked(self.0.clone(), self.1.clone()) }
}
impl<T: Debug + PartialEq> LocationPacked<T>
{
	pub fn unwrap(self) -> T { self.1 }
	pub fn value_ref(&self) -> &T { &self.1 }

	pub fn apply<F, U: Debug + PartialEq>(self, func: F) -> LocationPacked<U> where F: FnOnce(T) -> U
	{
		LocationPacked(self.0, func(self.1))
	}
}
/// SynTree Container: To-From Pair
#[derive(Debug)]
pub struct Transition<T> { pub from: T, pub to: T }
impl<T: Clone> Clone for Transition<T> { fn clone(&self) -> Self { Transition { from: self.from.clone(), to: self.to.clone() } } }
impl<T: PartialEq> PartialEq for Transition<T> { fn eq(&self, other: &Self) -> bool { self.from == other.from && self.to == other.to } }
impl<T: Eq> Eq for Transition<T> {}

// Generic Elements
#[derive(Debug, PartialEq)]
pub enum Format { Ref(String), Value(VkFormat) }
/// Integer Literal or $~~
#[derive(Debug, PartialEq)]
pub enum ConfigInt<N = u32> { Value(N), Ref(String) }
#[derive(Debug, PartialEq)]
pub enum NumericLiteral { Integer(i64), Floating(f64), Floating32(f32) }
#[derive(Debug, PartialEq)]
pub enum AssetResource { IntRef(ConfigInt), PathRef(Vec<String>) }

/// Image Dimension 1D / 2D / 3D
#[derive(Debug, PartialEq)]
pub enum ImageDimension
{
	/// 1D
	Linear,
	/// 2D
	Planar,
	/// 3D
	Cubic
}

pub struct ParsedDeviceResources
{
	pub includes: Vec<LocationPacked<PathBuf>>,
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
	pub images: NamedContents<ImageDescription>,
	pub samplers: NamedContents<SamplerDescription>,
	pub ind_shaders: IndependentShaders
}
pub struct IndependentShaders
{
	pub vertex: NamedContents<VertexShaderStageInfo>,
	pub tesscontrol: NamedContents<IndependentShaderStageInfo>,
	pub tessevaluation: NamedContents<IndependentShaderStageInfo>,
	pub geometry: NamedContents<IndependentShaderStageInfo>,
	pub fragment: NamedContents<IndependentShaderStageInfo>
}
impl IndependentShaders
{
	pub fn new() -> Self
	{
		IndependentShaders
		{
			vertex: NamedContents::new(), tesscontrol: NamedContents::new(), tessevaluation: NamedContents::new(),
			geometry: NamedContents::new(), fragment: NamedContents::new()
		}
	}
}
#[derive(Debug, PartialEq)]
pub struct ImageDescription
{
	pub extent: Vec<ConfigInt<usize>>, pub format: Format, pub device_local: bool, pub usage: VkImageUsageFlags, pub mapping: ComponentMapping
}
#[derive(Debug, PartialEq)]
pub struct SamplerDescription { pub mag_filter: Filter, pub min_filter: Filter }
#[derive(Debug, PartialEq)]
pub struct RPAttachment { pub format: LocationPacked<Format>, pub layouts: Transition<VkImageLayout>, pub clear_on_load: Option<bool>, pub preserve_content: bool }
#[derive(Debug, PartialEq)]
pub struct RPSubpassDesc { pub color_outs: Vec<LocationPacked<ConfigInt>>, pub inputs: Vec<LocationPacked<ConfigInt>> }
#[derive(Debug, PartialEq)]
pub struct RPSubpassDeps
{
	pub passtrans: Transition<LocationPacked<ConfigInt>>, pub access_mask: Transition<LocationPacked<AccessFlags>>,
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
	ImageView { dim: ImageDimension, refname: LocationPacked<String> }, SwapChainViews,
	Uint(LocationPacked<String>)
}
#[derive(Debug, PartialEq)]
pub enum FramebufferStyle
{
	WithRenderPass(LocationPacked<ConfigInt>), Simple(Option<bool>), Presented(Option<bool>)
}
#[derive(Debug, PartialEq)]
pub struct FramebufferInfo { pub style: FramebufferStyle, pub views: Vec<LocationPacked<ConfigInt>> }
#[derive(Debug, PartialEq, Eq)]
pub enum BufferDescriptorOption { None, TexelStore, DynamicOffset }
#[derive(Debug, PartialEq, Eq)]
pub enum DescriptorEntryKind
{
	Sampler, CombinedSampler, SampledImage, StorageImage,
	UniformBuffer(BufferDescriptorOption), StorageBuffer(BufferDescriptorOption), InputAttachment
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DescriptorSetLayoutData { pub entries: Vec<interlude::Descriptor> }
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PushConstantLayout { pub range: Range<usize>, pub visibility: interlude::ShaderStage }
#[derive(Debug, PartialEq)]
pub struct PipelineLayout { pub descs: Vec<LocationPacked<ConfigInt>>, pub pushconstants: Vec<LocationPacked<ConfigInt>> }
#[derive(Debug, PartialEq)]
pub struct DescriptorSetsInfo(pub Vec<DescriptorSetEntry>);
#[derive(Debug, PartialEq)]
pub struct DescriptorSetEntry { pub name: Option<String>, pub layout: LocationPacked<ConfigInt> }
#[derive(Debug, PartialEq)] pub enum StreamBindingDesc { PerVertex(usize), PerInstance(usize) }
#[derive(Debug, PartialEq)] pub struct VertexInputInfo { pub binding: usize, pub offset: usize, pub format: Format }
#[derive(Debug, PartialEq)]
pub struct VertexShaderStageInfo
{
	pub asset: LocationPacked<AssetResource>, pub consts: BTreeMap<usize, LocationPacked<NumericLiteral>>,
	pub stream_bindings: Vec<StreamBindingDesc>, pub inputs: BTreeMap<usize, VertexInputInfo>
}
#[derive(Debug, PartialEq)]
pub struct PipelineShaderStageInfo
{
	pub asset: LocationPacked<AssetResource>, pub consts: BTreeMap<usize, LocationPacked<NumericLiteral>>
}
#[derive(Debug, PartialEq)]
pub struct IndependentShaderStageInfo
{
	pub stage: VkShaderStageFlags, pub asset: LocationPacked<AssetResource>, pub consts: BTreeMap<usize, LocationPacked<NumericLiteral>>
}
#[derive(Debug, PartialEq)]
pub struct PipelineStateInfo
{
	pub renderpass: PreciseRenderPassRef, pub layout_ref: LocationPacked<ConfigInt>,
	pub vertex_shader			: VertexShaderStageInfo,
	pub geometry_shader			: Option<PipelineShaderStageInfo>,
	pub fragment_shader			: Option<PipelineShaderStageInfo>,
	pub tesscontrol_shader		: Option<PipelineShaderStageInfo>,
	pub tessevaluation_shader	: Option<PipelineShaderStageInfo>,
	pub primitive_topology		: VkPrimitiveTopology,
	pub viewport_scissors		: Vec<ViewportScissorEntry>,
	pub blendstates				: Vec<AttachmentBlendState>
}
#[derive(Debug, PartialEq)]
pub enum ViewportScissorEntry { ScreenView, Custom(VkViewport, VkRect2D) }
#[derive(Debug, PartialEq)]
pub struct VertexAttributeInfo { binding: u32, format: LocationPacked<Format>, offset: u32 }
#[derive(Debug, PartialEq)]
pub enum VertexBindingRegistry { PerVertex(Option<u32>), PerInstance(Option<u32>), Empty }

/// Shader Conversions
impl IndependentShaderStageInfo
{
	pub fn from_pss(pss: PipelineShaderStageInfo, stage: VkShaderStageFlags) -> Self
	{
		IndependentShaderStageInfo { stage: stage, asset: pss.asset, consts: pss.consts }
	}
}

/// Element Trait: Indicates that the element has location information.
pub trait LocationProvider
{
	fn location(&self) -> &Location;

	// Shorthands
	fn line(&self) -> usize { self.location().line() }
	fn column(&self) -> usize { self.location().column() }
}
impl<T: PartialEq + Debug> LocationProvider for LocationPacked<T> { fn location(&self) -> &Location { &self.0 } }
impl<T> LocationProvider for [T] where T: LocationProvider { fn location(&self) -> &Location { self[0].location() } }
impl<T> LocationProvider for Transition<T> where T: LocationProvider { fn location(&self) -> &Location { self.from.location() } }
