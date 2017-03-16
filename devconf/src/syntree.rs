// Syntax Elements

use std::ops::{ Index, Deref, Range };
use std::fmt::Debug;
use std::borrow::Cow;
use std::collections::HashMap;
use std::path::PathBuf;
use interlude::ffi::*;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum OperationResult { Success, Failed }
impl OperationResult
{
	pub fn propagate_failure(self, other: Self) -> Self { if other == OperationResult::Failed { other } else { self } }
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
		if self.0.contains_key(name.deref()) { OperationResult::Failed }
		else { self.1.push(value); self.0.insert(name.into_owned(), self.1.len() - 1); OperationResult::Success }
	}
	pub fn insert_unnamed(&mut self, value: T)
	{
		self.1.push(value);
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

/// SynTree Container: Packed value with the location which was in source
#[derive(Debug, PartialEq)]
pub struct LocationPacked<T: Debug + PartialEq>(pub usize, pub usize, pub T);
impl<T: Clone + Debug + PartialEq> Clone for LocationPacked<T>
{
	fn clone(&self) -> Self { LocationPacked(self.0, self.1, self.2.clone()) }
}
impl<T: Debug + PartialEq> LocationPacked<T>
{
	pub fn line(&self) -> usize { self.0 }
	pub fn col(&self) -> usize { self.1 }
	pub fn unwrap(self) -> T { self.2 }
	pub fn value_ref(&self) -> &T { &self.2 }

	pub fn rewrap<CF, U: Debug + PartialEq>(self, constructor: CF) -> LocationPacked<U> where CF: FnOnce(T) -> U
	{
		LocationPacked(self.0, self.1, constructor(self.2))
	}
}
impl<T: Debug + PartialEq> Deref for LocationPacked<T>
{
	type Target = T;
	fn deref(&self) -> &T { &self.2 }
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
#[derive(Debug, PartialEq)]
pub struct VertexShaderStageInfo
{
	asset: LocationPacked<AssetResource>, consts: HashMap<u32, LocationPacked<NumericLiteral>>
}
#[derive(Debug, PartialEq)]
pub struct PipelineShaderStageInfo
{
	asset: LocationPacked<AssetResource>, consts: HashMap<u32, LocationPacked<NumericLiteral>>
}
#[derive(Debug, PartialEq)]
pub struct IndependentPipelineShaderStageInfo
{
	stage: VkShaderStageFlags, asset: LocationPacked<AssetResource>, consts: HashMap<u32, LocationPacked<NumericLiteral>>
}
#[derive(Debug, PartialEq)]
pub struct PipelineStateInfo
{
	renderpass: PreciseRenderPassRef, layout_ref: LocationPacked<ConfigInt>,
	vertex_shader: PipelineShaderStageInfo, geometry_shader: Option<PipelineShaderStageInfo>, fragment_shader: Option<PipelineShaderStageInfo>,
	tesscontrol_shader: Option<PipelineShaderStageInfo>, tessevaluation_shader: Option<PipelineShaderStageInfo>,
	primitive_topology: VkPrimitiveTopology, viewport_scissors: Vec<ViewportScissorEntry>, blendstates: Vec<AttachmentBlendState>
}
#[derive(Debug, PartialEq)]
pub enum ViewportScissorEntry { ScreenView, Custom(VkViewport, VkRect2D) }
#[derive(Debug, PartialEq)]
pub enum AttachmentBlendState { Disabled, Alpha, PremultipliedAlpha }
