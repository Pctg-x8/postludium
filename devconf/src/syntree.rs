//! Syntax Tree Elements

use std;
use std::ops::{Index, Deref, Range};
use std::fmt::Debug;
use std::borrow::Cow;
use std::collections::{HashMap, BTreeMap};
use std::path::PathBuf;
use interlude::ffi::*;
use interlude::*;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum OperationResult { Success, Failed(Cow<'static, str>) }
impl OperationResult
{
	pub fn propagate_failure(self, other: Self) -> Self { if let OperationResult::Failed(_) = other { other } else { self } }
	pub fn report_error(self, line: usize) { if let OperationResult::Failed(reason) = self { panic!("Operation has failed({}) in processing at line {}", reason, line) } }
}

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
	pub fn insert(&mut self, name: Cow<str>, value: T)
	{
		let ref mut v = self.1;
		*self.0.entry(name.into_owned()).or_insert_with(||
		{
			v.push(value);
			v.len() - 1
		});
	}
	pub fn insert_unique(&mut self, name: Cow<str>, value: T) -> OperationResult
	{
		if self.0.contains_key(name.deref()) { OperationResult::Failed(format!("Duplicated Entry: {}", name).into()) }
		else { self.1.push(value); self.0.insert(name.into_owned(), self.1.len() - 1); OperationResult::Success }
	}
	pub fn insert_unnamed(&mut self, value: T)
	{
		self.1.push(value);
	}
	pub fn insert_auto(&mut self, name: Option<Cow<str>>, value: T)
	{
		if let Some(n) = name { self.insert(n, value) } else { self.insert_unnamed(value) }
	}
	pub fn insert_uniq_auto(&mut self, name: Option<Cow<str>>, value: T) -> OperationResult
	{
		if let Some(n) = name { self.insert_unique(n, value) } else { self.insert_unnamed(value); OperationResult::Success }
	}
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
#[derive(Debug, PartialEq)]
pub struct Transition<T> { pub from: T, pub to: T }

// Generic Elements
#[derive(Debug, PartialEq)]
pub enum Format { Ref(String), Value(VkFormat) }
/// Integer Literal or $~~
#[derive(Debug, PartialEq)]
pub enum ConfigInt { Value(u32), Ref(String) }
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
#[derive(Clone, Copy, PartialEq, Debug)] pub struct AccessFlags(pub VkAccessFlags);
#[derive(Debug, PartialEq, Clone, Copy)] pub struct ShaderStageFlags(pub VkShaderStageFlags);
impl std::cmp::PartialEq<VkAccessFlags> for AccessFlags { fn eq(&self, t: &VkAccessFlags) -> bool { self.0 == *t } }
impl std::cmp::PartialEq<VkShaderStageFlags> for ShaderStageFlags { fn eq(&self, t: &VkShaderStageFlags) -> bool { self.0 == *t } }

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
	pub ind_shaders: IndependentShaders
}
pub struct IndependentShaders
{
	pub vertex: NamedContents<IndependentPipelineShaderStageInfo>,
	pub tesscontrol: NamedContents<IndependentPipelineShaderStageInfo>,
	pub tessevaluation: NamedContents<IndependentPipelineShaderStageInfo>,
	pub geometry: NamedContents<IndependentPipelineShaderStageInfo>,
	pub fragment: NamedContents<IndependentPipelineShaderStageInfo>
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
	pub dim: u32, pub format: LocationPacked<Format>, pub extent: LocationPacked<(f32, f32, f32)>, pub device_local: bool, pub usage: VkImageUsageFlags
}
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
	ImageView { dim: ImageDimension, refname: LocationPacked<String> }, SwapChainViews
}
#[derive(Debug, PartialEq)]
pub enum FramebufferStyle
{
	WithRenderPass(LocationPacked<ConfigInt>), Simple(Option<bool>), Presented(Option<bool>)
}
#[derive(Debug, PartialEq)]
pub struct FramebufferInfo { pub style: FramebufferStyle, pub views: Vec<LocationPacked<ConfigInt>> }
#[derive(Debug, PartialEq)]
pub enum BufferDescriptorOption { None, TexelStore, DynamicOffset }
#[derive(Debug, PartialEq)]
pub enum DescriptorEntryKind
{
	Sampler, CombinedSampler, SampledImage, StorageImage,
	UniformBuffer(BufferDescriptorOption), StorageBuffer(BufferDescriptorOption), InputAttachment
}
#[derive(Debug, PartialEq)]
pub struct DescriptorEntry { pub kind: DescriptorEntryKind, pub count: usize, pub visibility: ShaderStageFlags }
#[derive(Debug, PartialEq)]
pub struct DescriptorSetLayoutData { pub entries: Vec<DescriptorEntry> }
#[derive(Debug, PartialEq)]
pub struct PushConstantLayout { pub range: Range<usize>, pub visibility: ShaderStageFlags }
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
pub struct IndependentPipelineShaderStageInfo
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
